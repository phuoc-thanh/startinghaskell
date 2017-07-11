{-# LANGUAGE OverloadedStrings #-}



module PacketInject where

-- import Network.Socket hiding (recv)
-- import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.HexString
import Data.Connection
import Data.Maybe
import qualified System.IO.Streams.TCP as TCP
-- import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams


--tcpPacket Base length: 54

--kd01
--ip:210.245.26.188:8001

--first bit: = data length - 2
--second bit: 00 (flag)
--third bit: may be first package
--fourth bit: ff (255)
--fifth bit: = data length - 6 (the real data)
--sixth bit: = 00 (flag)

hexB :: HexString
hexB = hexString "160001ff12006564656e547265652066696e697368203000"

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
main = do conn <- TCP.connect "210.245.26.188" 8001
          send conn $ C.fromStrict $ toBytes hexB
          res <- Streams.read (source conn)
          C.putStrLn $ C.fromStrict (fromJust res)
          close conn