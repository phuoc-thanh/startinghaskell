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
import Data.HexString
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Control.Concurrent

--tcpPacket Base length: 54

-- Server Info
kd01Host = "210.245.26.188"
kd01Port = 8001

hexNetTime =  "140003ff10006f6e6c696e6574696d6520696e666f00"
                      

injectWorld :: IO ()
injectWorld = do res <- loginVerify
                 conn <- TCP.connect kd01Host kd01Port
                 send conn $ getLoginData res
                 msg <- Streams.read (source conn)
                 C.putStrLn $ C.fromStrict (fromJust msg)
                 forkIO $ send conn $ enterWorld res
                 msg2 <- Streams.read (source conn)
                 C.putStrLn "succesful"
                 forkIO $ send conn $ enterChap01B01
                 msg3 <- Streams.read (source conn)
                 C.putStrLn "succesful"
                 forkIO $ send conn $ copyBlock
                 msg4 <- Streams.read (source conn)
                 C.putStrLn "succesful"