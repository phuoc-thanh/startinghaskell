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


--tcpPacket Base length: 54

-- Server Info
kd01Host = "210.245.26.188"
kd01Port = 8001
kdt116Host = "125.212.242.101"
kdt116Port = 8006
                      
injectWorld :: IO ()
injectWorld = do res <- loginVerify
                 conn <- TCP.connect kdt116Host kdt116Port
                 send conn $ getLoginData res
                 msg <- Streams.read (source conn)
                 forkIO . send conn . enterWorld res . C.fromStrict $ fromJust msg
                 Streams.read (source conn)
                 C.putStrLn "enter world !"
                 forkIO . send conn $ bet100
                 msg3 <- Streams.read (source conn)
                 C.putStrLn . "bet 100"