{-# LANGUAGE OverloadedStrings #-}



module Injector where

import GameData  
import LoginData
import HttpRq
import Network.Socket hiding (send, close)
-- import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Connection
import Data.Maybe
import Data.HexString
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams

--tcpPacket Base length: 54

-- Server Info
kd01Host = "210.245.26.188"
kd01Port = 8001

hexNetTime =  "140003ff10006f6e6c696e6574696d6520696e666f00"

hexLogin :: HexString
hexLogin = hexString "4d0001ff49004c4f47494e20302e302e312031302031203131313131203132332031343939393334343735203864646531343435663266646335393063636163386636643263333630343637203000"

hexEnter :: HexString
hexEnter = hexString "140002ff1000454e5445522031313131312031333000"
                      

injectWorld :: IO ()
injectWorld = do conn <- TCP.connect kd01Host kd01Port
                 send conn $ C.fromStrict $ toBytes hexEnter
                 res <- Streams.read (source conn)
                 C.putStrLn $ C.fromStrict (fromJust res)
                 close conn

injectLogin :: IO ()
injectLogin = do res <- loginVerify
                 sock <- TCP.connectSocket kd01Host kd01Port
                 setSocketOption (fst sock) KeepAlive 1
                 conn <- TCP.socketToConnection 1024 sock
                 send conn $ getLoginData res
                 send conn $ enterWorld res
                 msg <- Streams.read (source conn)
                 C.putStrLn $ C.fromStrict (fromJust msg)