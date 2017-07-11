{-# LANGUAGE OverloadedStrings #-}



module PacketInject where

-- import Network.Socket hiding (recv)
-- import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.HexString
import Data.Connection
import Data.Maybe
import Data.Time
import Data.ByteString.From.Hex
import Data.ByteString.Base16
import Numeric (showHex, readHex, readDec, showIntAtBase)
import Data.Time.Clock.POSIX

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

hexLogin :: HexString
hexLogin = hexString "4d0001ff49004c4f47494e20302e302e312031302031203131313131203132332031343939373838303833206465333234323030343430333933636163663534653536346132353365323330203000"

hexEnter :: HexString
hexEnter = hexString "140002ff1000454e5445522031313131312031333000"

hexNetTime :: HexString
hexNetTime = hexString "140003ff10006f6e6c696e6574696d6520696e666f00"

hexStreams = hexString "0a0004ff0600.626f617264.001f0005ff1b00.72616e6b4c6973742067657452616e6b4c697374446174612030.00"

--packet04: board
streamLength = fromHex 0x0a

--626f617264
boardS = encode "board"

--72616e6b4c6973742067657452616e6b4c697374446174612030
getRankList = encode "rankList getRankListData 0" 

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
main = do conn <- TCP.connect "210.245.26.188" 8001
          send conn $ C.fromStrict $ toBytes hexLogin
          send conn $ C.fromStrict $ toBytes hexEnter
          res <- Streams.read (source conn)
          C.putStrLn $ C.fromStrict (fromJust res)
        --   close conn