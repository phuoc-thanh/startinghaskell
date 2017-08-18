{-# LANGUAGE OverloadedStrings #-}

module Hunt where

import Injector
import PacketGenerator
import Parser hiding (encode, decode)
import Data.ByteString.Base16
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.List
import Data.List.Split
import Data.Maybe
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad
import Control.Concurrent

-- HD ZM40, CBT ZM41, VNT ZM43, VTM ZM44, HT ZM45, TVK ZM48, KP ZM49, TDLT ZM51
huntTarget :: ByteString
huntTarget = "ZM49|ZM51"

main = do pls <- cPls
          forM_ pls $ \u -> do
            forkIO $ do
                tid <- myThreadId
                conn <- joinWorld u
                sendAll conn $ iniHunt
                splitM conn tid

splitM :: Socket -> ThreadId -> IO ()              
splitM conn t = do
    threadDelay 1000000
    msg <- recv conn 1024
    if C.isInfixOf "ZM" msg
    then do
        let h = tail . map (C.pack . take 4) $ split (startsWith "ZM") $ C.unpack msg
        findHr h conn t
    else splitM conn t

hrVerify :: [ByteString] -> Maybe ByteString
hrVerify heroes
    | (C.isInfixOf (heroes !! 0) huntTarget) = Just "0"
    | (C.isInfixOf (heroes !! 1) huntTarget) = Just "1"
    | (C.isInfixOf (heroes !! 2) huntTarget) = Just "2"
    | otherwise = Nothing

findHr :: [ByteString] -> Socket -> ThreadId -> IO ()   
findHr heroes conn t = case hrVerify heroes of
                Just idx -> do C.putStrLn $ C.append "found hero at " idx
                               sendAll conn $ openRoom idx
                               hunt idx conn t
                Nothing  -> do threadDelay 1000000
                               sendAll conn $ renewHunt
                               splitM conn t

hunt' :: ByteString -> Socket -> IO ()                               
hunt' idx conn = do threadDelay 1000000
                    msg <- recv conn 1024
                    unless (C.isInfixOf "TuHaTienTu" msg) $ hunt' idx conn
                    when (C.isInfixOf "TuHaTienTu" msg) $ do
                        threadDelay 3000000
                        sendAll conn $ goHunt idx
                        msg <- recv conn 1024
                        threadDelay 3000000
                        close conn


hunt :: ByteString -> Socket -> ThreadId -> IO ()                               
hunt idx conn t = do threadDelay 1000000
                     msg <- recv conn 1024
                     unless (C.isInfixOf "1b14010102" $ encode msg) $ hunt idx conn t
                     when (C.isInfixOf "1b14010102" $ encode msg) $ do
                         threadDelay 7000000
                         sendAll conn $ goHunt idx
                         msg <- recv conn 1024
                         threadDelay 3000000
                         close conn
                         killThread t
                