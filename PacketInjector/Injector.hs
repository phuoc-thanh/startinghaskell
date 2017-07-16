{-# LANGUAGE OverloadedStrings #-}



module Injector where

import GameData  
import LoginData
import HttpRq
import PlayGame
import Network.Socket hiding (send, close)
-- import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Connection
import Data.Maybe
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Control.Concurrent
import Data.ByteString.Base16.Lazy


-- Server Info
kd01Host = "210.245.26.188"
kd01Port = 8001
kdt28Host = "125.212.242.98"
kdt28Port = 8003
--kdt55,kdt 72
kdt55Host = "123.31.25.77"
kdt55Port = 8001
kdt72Port = 8007
kdt153Host = "123.31.25.73"
kdt153Port = 8065

sendNTimes :: Integer -> Connection (Socket, SockAddr) -> IO ()
sendNTimes 1 c = send c $ bet100
sendNTimes n c = do send c $ bet100
                    sendNTimes (n - 1) c
                 
rankRewards :: Integer -> Connection (Socket, SockAddr) -> IO ()
rankRewards 1 c = send c $ rankReward
rankRewards n c = do send c $ rankReward
                     rankRewards (n - 1) c                 

injectWorld :: IO ()
injectWorld = do res <- loginVerify
                 conn <- TCP.connect kdt55Host kdt55Port
                 send conn $ getLoginData res
                 msg <- Streams.read (source conn)
                 forkIO . send conn . enterWorld res . C.fromStrict $ fromJust msg
                 Streams.read (source conn)
                 C.putStrLn "enter world!"
                --  forkIO $ rankRewards 99999 conn
                --  msg3 <- Streams.read (source conn)
                --  C.putStrLn "bet 1000"