-----------------------------------------------------------------------------
-- |
-- Module      :  ProgressBar
-- Copyright   :  (C) 2022 Christopher Norris
-- License     :  MIT-style (see the file LICENSE)
-- Maintainer  :  Christopher Norris <@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------

module ProgressBar
  ( viewBar
  )
where


import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8         as C8
import           Data.ByteString.UTF8
import           Text.Printf


viewBar step fileSize = do
  downloadedMVar <- newEmptyMVar >>= performDownload 0
  putStrLn "Begining download for: linuxmint-17-kde-dvd-32bit.iso.torrent"
  forM_ [0 .. step] $ const $ takeMVar downloadedMVar >>= progressBar fileSize
  putStrLn ""
  putStrLn "Torrent downloaded, pls check ~/Downloads/horrent/store/section/1/"
 where
  performDownload :: RealFrac t => p -> MVar t -> IO (MVar t)
  performDownload n mvar = do
    forkIO $ loop 0
    return mvar
   where
    loop n = do
      putMVar mvar n
      threadDelay $ 100000 * round n
      loop $ n + 0.031

  progressBar :: (RealFrac a, PrintfArg a) => a -> a -> IO ()
  progressBar max progress = C8.putStr $ C8.concat
    (  [ C8.pack
         $  "Downloading "
         <> format4dp progress
         <> "Gb of "
         <> format4dp max
         <> "Gb ["
         <> format4dp (progress / max * 100)
         <> "%] "
       ]
    <> replicate (round $ progress * 100 / max)         (fromString "▓")
    <> replicate (round $ (max - progress) * 100 / max) (fromString "░")
    <> [C8.pack " (Peers 17, 8.923bit/s)\r"]
    )
    where format4dp = printf "%.4g"

