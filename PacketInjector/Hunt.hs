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

-- HD 40, MDP ZM39, CBT ZM41, VTM ZM44, HT ZM45, TVK ZM48, KP ZM49, TDLT ZM51
huntTarget :: ByteString
huntTarget = "ZM49"

main = do pls <- players
          forM_ pls $ \u -> do
            forkIO $ do
                conn <- joinWorld u
                sendAll conn $ iniHunt
                listenM conn

listenM :: Socket -> IO ()              
listenM conn   = do msg <- recv conn 2048
                    unless (C.isInfixOf "ZM" msg) $ listenM conn
                    when (C.isInfixOf "ZM" msg) $ do
                        let h = tail . map (C.pack . take 4) $ split (startsWith "ZM") $ C.unpack msg
                        findHr h conn

hrVerify :: [ByteString] -> Maybe ByteString
hrVerify heroes
    | (C.isInfixOf (heroes !! 0) huntTarget) = Just "0"
    | (C.isInfixOf (heroes !! 1) huntTarget) = Just "1"
    | (C.isInfixOf (heroes !! 2) huntTarget) = Just "2"
    | otherwise = Nothing

findHr :: [ByteString] -> Socket -> IO ()   
findHr heroes conn = case hrVerify heroes of
                Just idx -> do C.putStrLn $ C.append "found hero at " idx
                               sendAll conn $ openRoom idx
                               hunt idx conn
                Nothing  -> do threadDelay 2000000
                               sendAll conn $ renewHunt
                               listenM conn

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


hunt :: ByteString -> Socket -> IO ()                               
hunt idx conn  = do threadDelay 100000
                    msg <- recv conn 1024
                    unless (C.isInfixOf "1b14010102" $ encode msg) $ hunt idx conn
                    when (C.isInfixOf "1b14010102" $ encode msg) $ do
                        threadDelay 7000000
                        sendAll conn $ goHunt idx
                        msg <- recv conn 1024
                        threadDelay 3000000
                        close conn                        

quickHunt idx = do  u <- getPlayer "kdqq001"
                    conn <- joinWorld u
                    sendAll conn $ openRoom idx
                    hunt' idx conn

-- 31001b14010102000000000000005a0c00008d13000005000000190000000b00547548615469656e54750005000000f7960000
-- 33001b14010102000000000000000601000089130000010000001e0000000d0043e1baa975232154e1bab76300040000002a270100
-- 30001b1401010200000000000000ab0b00008913000001000000380000000a005265706c7931393839000900000029dd0600
-- 30001b1401010200000000000000ab0b00008913000001000000380000000a005265706c7931393839000900000029dd0600                    