{-# LANGUAGE OverloadedStrings #-}



module PacketInject where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Data.HexString

--tcpPacket Base length: 54

--kd01
--ip:210.245.26.188:8001

--first bit: = data length - 2
--second bit: 00 (flag)
--third bit: may be first package
--fourth bit: ff (255)
--fifth bit: = data length - 6 (the real data)
--sixth bit: = 00 (flag)

hexB :: Data.HexString.HexString
hexB = hexString "160001ff12006564656e547265652066696e697368203000"

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just "210.245.26.188") (Just "8001")
       let serveraddr = head addrinfos
       putStrLn $ show serveraddr
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       sendAll sock $ toBytes $ hexB
       msg <- recv sock 1024
       close sock
       C.putStrLn "Received "
       C.putStrLn msg