-----------------------------------------------------------------------------
-- |
-- Module      :  Core
-- Copyright   :  (C) 2022 Christopher Norris
-- License     :  MIT-style (see the file LICENSE)
-- Maintainer  :  Christopher Norris <@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------

module Core
  ( 
  -- provisionally
    getMetaInfo

  , run
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.Lens
import           Control.Concurrent             ( forkIO )

import           Control.Monad
import           Data.BEncode                  as Bencode
import           Data.IP                        ( toIPv4 )
import           Data.Binary
import           Data.Binary.Builder            ( toLazyByteString )
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Byteable                  ( toBytes )
import           Data.ByteString.Lens
import           Data.Int
import           Data.Map                      as Map
import           Data.Map.Lens
import           Data.Maybe
import           Data.Monoid                    ( mappend
                                                , (<>)
                                                )
import           Data.Proxy
import qualified Data.Text                     as TT
import           Data.Word                      ( Word16 )
import           GHC.Generics            hiding ( from )
import           Network.HTTP.Simple     hiding ( Proxy )
import           Network.Socket          hiding ( recv )
import           Network.Socket.ByteString
import           Network.HTTP.Client            ( path )
import           Network.HTTP.Types             ( urlEncode )
import           Say                            ( say
                                                , sayShow
                                                )
import           System.IO
import           URI.ByteString                 ( urlEncode )
import qualified "cryptohash" Crypto.Hash      as CH
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as CL8
import qualified Data.Maybe                    as DM
import qualified Data.ByteString.Base16        as B16

makePrisms ''Bencode.BEncode

protocolString :: B.ByteString
protocolString = "BitTorrent protocol"

mkPeerId = "ABCDEFGHIJKLMNOPQRST"
torrentName = "kubuntu-20.04.4-desktop-amd64.iso.torrent"
  --"kubuntu-18.04.5-desktop-amd64.iso.torrent"

downloadFolder = "/Users/imac/Downloads/"
type PeerId = String
type InfoHash = CH.Digest CH.SHA1
type TorrentData = BL.ByteString
type TorrentLength = Integer

data SimpleMessageType = ChokeM | UnChokeM | InterestedM | NotInterestedM | HaveM | BitfieldM | RequestM | PieceM | CancelM | PortM deriving (Show, Eq, Enum)

decodeMessageType :: SimpleMessageType -> Int8
decodeMessageType = fromIntegral . fromEnum

data MetaInfo = MetaInfo
  { info     :: SingleFileInfoDictionary
  , infoHash :: InfoHash
  , announce :: TorrentData
  }
  deriving Show

data SingleFileInfoDictionary = SingleFileInfoDictionary
  { torrentLength :: TorrentLength
  , name          :: TorrentData
  , pieceLength   :: TorrentLength

  {- string consisting of the concatenation of all 20-byte SHA1 hash values,
     one per piece (byte string, i.e. not urlencoded) -}
  , pieces        :: TorrentData
  }
  deriving Show

data BHandshake = BHandshake
  { handshakeInfoHash :: B.ByteString
  , handshakePeerId   :: B.ByteString
  , reserved          :: B.ByteString
  }
  deriving (Show, Eq, Generic)

instance Binary BHandshake where
  put (BHandshake infoHash peerId reserved) = do
    putWord8 (fromIntegral $ B.length protocolString)
    putByteString protocolString
    replicateM_ 8 (putWord8 0)
    putByteString infoHash
    putByteString peerId

  get = do
    protoSize <- get :: Get Word8
    replicateM_ (fromIntegral protoSize) getWord8
    reserved <- getByteString 8
    infoHash <- getByteString 20
    peerId   <- getByteString 20
    return $ BHandshake infoHash peerId reserved


-- interested: <len=0001><id=2>
data Interested = Interested
  { lengthI  :: Int32
  , messageI :: Int8
  }
  deriving (Show, Eq, Generic)
instance Binary Interested where
  put (Interested len msgIdBF) = do
    putInt32be $ 1
    putInt8 $ decodeMessageType InterestedM

  get = do
    lengthI  <- getInt32be
    messageI <- getInt8
    return $ Interested lengthI messageI

-- The unchoke message is fixed-length and has no payload.
-- >>> unchoke: <len=0001><id=1>
data Unchoke = Unchoke
  { lengthUnChk :: Int32
  , messageIDUC :: Int8
  }
  deriving (Show, Eq, Generic)
instance Binary Unchoke where
  put (Unchoke len msgIdBF) = do
    putInt32be $ 1
    putInt8 $ decodeMessageType UnChokeM

  get = do
    lengthUnChk <- getInt32be
    messageIDUC <- getInt8
    return $ Unchoke lengthUnChk messageIDUC

-- >>> bitfield: <len=0001+X><id=5><bitfield>
data Bitfield = Bitfield
  { lengthBitfield :: Int32
  , messageIDBF    :: Int8
  , bitfield       :: B.ByteString
  }
  deriving (Show, Eq, Generic)

instance Binary Bitfield where
  put (Bitfield len msgIdBF bitfield) = do
    putInt32be $ fromInteger $ toInteger $ 1 + B.length bitfield
    putInt8 $ decodeMessageType BitfieldM
    putByteString bitfield

  get = do
    lengthPrefixP <- getInt32be
    messageIDP    <- getInt8
    bitfield      <- getByteString $ fromInteger (toInteger lengthPrefixP - 1)
    return $ Bitfield lengthPrefixP messageIDP bitfield

-- The request message is fixed length, and is used to request a block. The payload 
-- contains the following information:
-- >>> request: <len=0013><id=6><index><begin><length>
data RequestBlock = RequestBlock
  { lengthPrefixPR :: Int32
  , messageIDPR    :: SimpleMessageType
  , index          :: Int32
  , begin          :: Int32
  , length         :: Int32
  }
  deriving (Show, Eq, Generic)

-- (B.unpack . CL8.toStrict . encode) (RequestBlock 0x0d Request 0x000001a0 0x0002c000 0x00004000)
-- == [0,0,0,13,6,0,0,1,160,0,2,192,0,0,0,64,0]
instance Binary RequestBlock where
  -- put (RequestBlock lengthPrefixPR msgId index begin length) = do
  put (RequestBlock _ _ index begin length) = do
    putInt32be 13
    putInt8 $ decodeMessageType RequestM
    putInt32be index
    putInt32be begin
    putInt32be length

  -- get = RequestBlock <$> getInt32le <*> (toEnum . fromIntegral <$> getInt8) <*> getInt32le <*> getInt32le <*> getInt32le
  get =
    RequestBlock
      <$> getInt32be
      <*> (toEnum . fromIntegral <$> getInt8)
      <*> getInt32be
      <*> getInt32be
      <*> getInt32be

-- The piece message is variable length, where X is the length of the block.
-- >>> piece: <len=0009+X><id=7><index><begin><block>
data Piece = Piece
  {
   --length prefix is a four byte big-endian value
    lengthPrefixP :: Int32
  -- The message ID is a single decimal byte, Request
  , messageIDP    :: Int8
  -- index: integer specifying the zero-based piece index
  , indexP        :: Int32
  -- begin: integer specifying the zero-based byte offset within the piece
  , beginP        :: Int32
  --  block of data, which is a subset of the piece specified by index. 
  , blockP        :: B.ByteString
  }
  deriving (Show, Eq, Generic)

instance Binary Piece where
  put (Piece lengthPrefixP msgIdP indexP beginP blockP) = do
    putInt32be $ fromInteger $ toInteger $ 9 + B.length blockP
    putInt8 $ decodeMessageType PieceM
    putInt32be indexP
    putInt32be beginP
    putByteString blockP

  -- get = Piece <$> getInt32le <*> getInt8 <*> getInt32le <*> getInt32le <*> getByteString
  get = do
    lengthPrefixP <- getInt32be
    messageIDP    <- getInt8
    indexP        <- getInt32be
    beginP        <- getInt32be
    blockP        <- getByteString 255 -- TODO: get correct length $ fromInteger (toInteger lengthPrefixP - 19)
    return $ Piece lengthPrefixP messageIDP indexP beginP blockP

data KeepAlive = KeepAlive
  { lengthPrefixKA :: B.ByteString
  }
  deriving (Show, Eq, Generic)

data SimpleMsg = SimpleMsg
  { lengthPrefix :: Int32
  , messageId    :: SimpleMessageType
  , indexSM      :: Int32
  , indexSM2     :: Int32
  }
  deriving (Show, Eq, Generic)

instance Binary SimpleMsg where
  put (SimpleMsg length msgId i j) = do
    putInt32be length
    putInt8 $ fromIntegral $ fromEnum msgId
    putInt32be i
    putInt32be j

  get =
    SimpleMsg
      <$> getInt32be
      <*> (toEnum . fromIntegral <$> getInt8)
      <*> getInt32be
      <*> getInt32be

run = getMetaInfo >>= getPeers

-- from https://github.com/peti/hsdns/blob/master/ADNS/Resolver.hs
toPTR :: HostAddress -> String
toPTR ha =
  shows b4
    . ('.' :)
    . shows b3
    . ('.' :)
    . shows b2
    . ('.' :)
    . shows b1
    $ mempty
  where (b1, b2, b3, b4) = hostAddressToTuple ha

getMetaInfo = makeMetaInfo . decode <$> input
 where
  input  = BL.readFile $ downloadFolder <> torrentName
  decode = fromJust . Bencode.bRead
  makeMetaInfo parsedTorrent =
    let infoDictionary = fromJust $ parsedTorrent ^. _BDict ^. at "info"
        singleFileInfoDictionary = SingleFileInfoDictionary
          (key "length" _BInt)
          (key "name" _BString)
          (key "piece length" _BInt)
          (key "pieces" _BString)
           where
            key name prism =
              fromJust (infoDictionary ^. _BDict ^. at name) ^?! prism
    in  MetaInfo
          singleFileInfoDictionary
          (CH.hash . BL.toStrict $ bPack infoDictionary)
          (fromJust (parsedTorrent ^. _BDict ^. at "announce") ^. _BString)

getPeers meta@MetaInfo {..} = do
  request' <- parseRequest $ CL8.unpack announce
  let sha1DigestAsC8 = toBytes infoHash
      urlEncodeBS =
        CL8.toStrict . toLazyByteString . URI.ByteString.urlEncode []

  say "torrent size.."
  sayShow $ torrentLength info
  sayShow $ pieceLength info
  -- BITFIELD response interpretation:-
  -- lengthBitfield = 956, which is one more than length so we'll have 955 of which 954 are 0xFF, 1 is 0xE0 (224) 11100000 (i.e. 3 pieces)
  -- 954 * 8 + 3 = 7,635, piecelength is 262144 so 262144 * 7,635 = 2,001,469,440 which is our filesize. 

  -- pieces is a string consisting of the concatenation of all 20-byte SHA1 hash values, one per piece (byte string, i.e. not urlencoded)
  -- sayShow $ pieces info
  say "contacting announce.."
  sayShow $ "infohash for " <> CL8.unpack announce <> " is " <> C8.unpack
    (urlEncodeBS sha1DigestAsC8)
  say "----------------------"
  request' <- parseRequest $ CL8.unpack announce <> "?info_hash=" <> C8.unpack
    (urlEncodeBS sha1DigestAsC8)
  let request = setRequestMethod "GET" $ addToRequestQueryString
        [ ("peer_id"      , Just mkPeerId)
        , ("port"         , Just $ C8.pack "39183")
        , ("uploaded"     , Just "0")
        , ("downloaded"   , Just "0")
        , ("left", Just $ (C8.pack . show) $ torrentLength info)
        , ("event"        , Just $ C8.pack "started")
        , ("numwant"      , Just "200")
        , ("no_peer_id"   , Just "1")
        , ("compact"      , Just "1")
        , ("supportcrypto", Just "1")
        , ("redundant"    , Just "0")
        ]
        request'
  responseB <- getResponseBody <$> httpLBS request
  say "bendecoded body.."
  let bendecodedBody = DM.fromJust (Bencode.bRead responseB)
      peers =
        chunksOf 6
          .  fromJust
          $  fromJust (bendecodedBody ^. _BDict ^. at "peers")
          ^? _BString
       where
        chunksOf _ (BL.uncons -> Nothing) = []
        chunksOf n l                      = first : chunksOf n rest
          where (first, rest) = BL.splitAt n l

      peersToIPAndPort =
        (\(SockAddrInet p h) -> (toPTR h, show p)) . toIPPortTuple <$> peers
       where
        toIPPortTuple lc8bs =
          let (ipAddress, portNumber) =
                runGet (liftM2 (,) getWord32be getWord16be) lc8bs
          in  SockAddrInet (fromIntegral portNumber :: PortNumber) ipAddress

  forM_ peersToIPAndPort $ forkIO . handShakePeer meta
  return (bendecodedBody, peers, peersToIPAndPort)

runScrape = getMetaInfo >>= scrape

scrape meta@MetaInfo {..} = do
  request <- parseRequest $ CL8.unpack announce
  let requestScrape =
        setRequestMethod "GET" $ setRequestQueryString [] $ request
          { path = "/scrape"
          }
  responseS <- Bencode.bRead . getResponseBody <$> httpLBS requestScrape
  let scrapeResponse = fromJust responseS
  let parsedFilenames = over traverse
                             (view _BString . fromJust)
                             bdecodedFilenames
       where
        bdecodedFilenames =
          over traverse (view (at "name") . view _BDict)
            $  Map.elems
            $  fromJust (Map.lookup "files" $ scrapeResponse ^. _BDict)
            ^. _BDict

  mapMOf_ traverse sayShow parsedFilenames
  return (scrapeResponse, tup scrapeResponse ^. _2 . _BString)
 where
  tup x =
    Map.elemAt 3
      $  head (Map.elems (head (Map.elems $ x ^. _BDict) ^. _BDict))
      ^. _BDict

handShakePeer metaInfo (addr, port) = do
  let
    handshakeaddress = addr <> ":" <> port
    sha1DigestAsC8   = toBytes $ infoHash metaInfo
    urlEncodeBS = CL8.toStrict . toLazyByteString . URI.ByteString.urlEncode []
    handShake        = BHandshake sha1DigestAsC8 mkPeerId B.empty

  addrInfo <- resolve addr port
  sock     <- socket (addrFamily addrInfo)
                     (addrSocketType addrInfo)
                     (addrProtocol addrInfo)

  connect sock $ addrAddress addrInfo
  let encodedHandShake = encode handShake
  say
    $  "sending handshake for "
    <> TT.pack handshakeaddress
    <> " encoded as-"
    <> TT.pack (CL8.unpack encodedHandShake)
  sendAll sock $ BL.toStrict encodedHandShake
  input <- recv sock 68
  case
      decodeOrFail (CL8.fromStrict input) :: Either
        (CL8.ByteString, ByteOffset, String)
        (CL8.ByteString, ByteOffset, BHandshake)
    of
      Left  (_, _, err) -> say $ "error receiving handshake-" <> TT.pack err

      -- successfully received handshake
      Right (_, _, hs ) -> do
        sayShow $ "handshake received from peer with id" <> handshakePeerId hs

        say "post-handshake pieces requested.."
        input <- recv sock 65536

        case
            decodeOrFail (CL8.fromStrict input) :: Either
              (CL8.ByteString, ByteOffset, String)
              (Decoded Bitfield)
          of
            Left (_, _, err) ->
              say $ "post-handshake BITFIELD request error-" <> TT.pack err
            Right (_, _, hs) ->
              say
                $  "post-handshake BITFIELD request SUCCESS-"
                <> (TT.pack . show) hs

        let interested = Interested 1 2
        sendAll sock $ BL.toStrict (encode interested)

        -- ----------------------------------
        input <- CL8.fromStrict <$> recv sock 65536

        case
            decodeOrFail input :: Either
              (CL8.ByteString, ByteOffset, String)
              (Decoded Unchoke)
          of
            Left  (_, _, err) -> say $ "Unchoke error-" <> TT.pack err
            Right (_, _, hs ) -> do
              say $ "Unchoke SUCCESS-" <> (TT.pack . show) hs


        let pieceRequest =
              RequestBlock 0x0d RequestM 0x00000001 0x00004000 0x00004000

        sendAll sock $ BL.toStrict (encode pieceRequest)
        input <- CL8.fromStrict <$> recv sock 65536


-- ------------------DELME --------------
        case (decodeOrFail @SimpleMsgR) input of
          Left  (_, _, err) -> say $ "DECODING error-" <> TT.pack err
          Right (_, _, hs ) -> say $ (TT.pack . show) hs
-- ------------------DELME --------------

        decodeResponse input sock

 where
  resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

type Decoded a = (CL8.ByteString, ByteOffset, a)
type Decoded' a = Either (CL8.ByteString, ByteOffset, String) (Decoded a)

type PieceR = Decoded' Piece
type SimpleMsgR = Decoded' SimpleMsg

type M a
  =  CL8.ByteString
  -> Either
       (CL8.ByteString, ByteOffset, String)
       (CL8.ByteString, ByteOffset, a)

decodeResponse inp sock = case (decodeOrFail @PieceR) inp of
  Left  (_, _, err) -> say $ "first PIECE request error-" <> TT.pack err
  Right (_, _, hs ) -> do
    say $ "first PIECE request SUCCESS-" <> (TT.pack . show) hs

    let pieceRequest =
          RequestBlock 0x0d RequestM 0x00000000 0x00024000 0x00004000
    sendAll sock $ BL.toStrict (encode pieceRequest)
    input <- CL8.fromStrict <$> recv sock 65536
    case
        decodeOrFail input :: Either
          (CL8.ByteString, ByteOffset, String)
          (Decoded Piece)
      of
        Left (_, _, err) -> say $ "2nd PIECE request error-" <> TT.pack err
        Right (_, _, hs) ->
          say $ "2nd PIECE request SUCCESS-" <> (TT.pack . show) hs

data Wrap a = forall a . (Binary a) => Wrap (M a)
deriving instance Functor Wrap
