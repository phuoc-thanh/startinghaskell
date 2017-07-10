{-# LANGUAGE OverloadedStrings #-}



module TcpPacketInject where


import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

--kd01
--ip:210.245.26.188:8001

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just "210.245.26.188") (Just "8001")
       let serveraddr = head addrinfos
       putStrLn $ show serveraddr
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       sendAll sock $ C.pack "tesr"
       msg <- recv sock 1024
       close sock
       putStr "Received "
       C.putStrLn msg