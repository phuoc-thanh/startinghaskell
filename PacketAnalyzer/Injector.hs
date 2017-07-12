{-# LANGUAGE OverloadedStrings #-}



module Injector where

import GameData  
-- import Network.Socket hiding (recv)
-- import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.HexString
import Data.Connection
import Data.Maybe
import Data.Time.Clock.POSIX

import qualified System.IO.Streams.TCP as TCP
-- import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams


--tcpPacket Base length: 54

-- Server Info
kd01Host = "210.245.26.188"
kd01Port = 8001

hexLogin = "4d0001ff49004c4f47494e20302e302e312031302031203131313131203132332031343939373838303833206465333234323030343430333933636163663534653536346132353365323330203000"

hexEnter = "140002ff1000454e5445522031313131312031333000"

hexNetTime =  "140003ff10006f6e6c696e6574696d6520696e666f00"

--current time
cTime :: IO Integer
cTime = fmap round getPOSIXTime


-- simpleTCPSend :: IO ()
-- simpleTCPSend = withSocketsDo $
--     do addrinfos <- getAddrInfo Nothing (Just "210.245.26.188") (Just "8001")
--        let serveraddr = head addrinfos
--        putStrLn $ show serveraddr
--        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
--        setSocketOption sock KeepAlive 1
--        connect sock (addrAddress serveraddr)
--        sendAll sock $ toBytes $ hexB
--        msg <- recv sock 1024
--        close sock
--        putStrLn $ show $ fromBytes msg 
--        C.putStrLn "Received "
--        C.putStrLn $ msg

main :: IO ()
main = do conn <- TCP.connect kd01Host kd01Port
          -- send conn getGameData
          send conn $ C.pack hexLogin
          res <- Streams.read (source conn)
          C.putStrLn $ C.fromStrict (fromJust res)
        --   close conn