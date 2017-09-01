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

main = do pls <- cPls
          forM_ pls $ \u -> do
            forkIO $ do
                tid <- myThreadId
                conn <- joinWorld u
                sendAll conn $ iniHunt
                waitfor_ "ZM" conn tid splitM

splitM :: ByteString -> Socket -> ThreadId -> IO ()              
splitM msg conn t = do
    let h = tail . map (C.pack . take 4) $ split (startsWith "ZM") $ C.unpack msg
    findHr h conn t

hrVerify :: [ByteString] -> ByteString -> Maybe ByteString
hrVerify heroes huntTarget
    | (C.isInfixOf (heroes !! 0) huntTarget) = Just "0"
    | (C.isInfixOf (heroes !! 1) huntTarget) = Just "1"
    | (C.isInfixOf (heroes !! 2) huntTarget) = Just "2"
    | otherwise = Nothing

findHr :: [ByteString] -> Socket -> ThreadId -> IO ()   
findHr heroes conn t = do 
    cf <- getConfig
    case hrVerify heroes (C.pack $ huntTarget cf) of
        Just idx -> do C.putStrLn $ C.append "found hero at " idx
                       sendAll conn $ openRoom idx
                       hunt idx conn t
        Nothing  -> do threadDelay 1000000
                       sendAll conn $ renewHunt
                       waitfor_ "ZM" conn t splitM

hunt :: ByteString -> Socket -> ThreadId -> IO ()                               
hunt idx conn t = waitfor "1b14010102" conn t $ do
    threadDelay 7000000
    sendAll conn $ goHunt idx
    msg <- recv conn 1024
    threadDelay 3000000
    close conn
    killThread t     