{-# LANGUAGE OverloadedStrings #-}

module Injector where
import Authenticator
import HttpRq
import PacketGenerator (newUser)
import Parser hiding (encode)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Data.ByteString.Base16
import Data.Maybe
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)

getServerInfo :: String -> IO Server
getServerInfo i = do
    serverinfos <- parseFile "ServerInfoQQ.json" :: IO (Maybe [Server])
    return $ head $ filter (\s -> (sid s) == i) (fromJust $ serverinfos)

getMatch :: String -> IO Match
getMatch i = do
    match <- parseFile "Match.json" :: IO (Maybe [Match])
    return $ head $ filter (\s -> (mid s) == i) (fromJust $ match)

players :: IO [Player]
players = do
    pls <- parseFile "Players.json" :: IO (Maybe [Player])
    return $ fromJust pls

buffPls :: IO [Player]
buffPls = do
    pls <- parseFile "Buffs.json" :: IO (Maybe [Player])
    return $ fromJust pls
    
cPls :: IO [Player]
cPls = do
    pls <- parseFile "Clone.json" :: IO (Maybe [Player])
    return $ fromJust pls

getPlayer :: String -> IO Player    
getPlayer uname = do
    users <- parseFile "Players.json" :: IO (Maybe [Player])
    return $ head $ filter (\u -> (acc u) == uname) (fromJust $ users)    

sendNTimes :: Integer -> Socket -> ByteString -> IO ()
sendNTimes 1 c s = sendAll c s
sendNTimes n c s = do sendAll c s
                      sendNTimes (n - 1) c s 
                               
adp :: String -> String -> IO ()
adp u p = do pl <- login u p
             pls <- players
             appendJSON "Players.json" (pl:pls)
            
adb :: String -> String -> IO ()
adb u p = do pl <- login u p
             pls <- buffPls
             appendJSON "Buffs.json" (pl:pls)
             
adc :: String -> String -> IO ()
adc u p = do pl <- login u p
             pls <- cPls
             appendJSON "Clone.json" (pl:pls)             

login :: String -> String -> IO Player
login u p = do res <- loginVerify u p
               uServer <- getServerInfo (defaultsid res)
               addrinfos <- getAddrInfo Nothing (Just $ ip uServer) (Just $ port uServer)
               let serveraddr = head addrinfos
               sock <- socket (addrFamily serveraddr) Stream defaultProtocol
               connect sock (addrAddress serveraddr)
               sendAll sock $ loginData res
               msg <- recv sock 256
               close sock
               return $ Player u (uid res) (opname res) (defaultsid res)
                               (displayNovice res) (create_time res) (key res) 
                               (show . getChNumber $ encode msg)
                               (amount res)

reg u p s = do res <- regAccount u p
               uServer <- getServerInfo s
               addrinfos <- getAddrInfo Nothing (Just $ ip uServer) (Just $ port uServer)
               let serveraddr = head addrinfos
               sock <- socket (addrFamily serveraddr) Stream defaultProtocol
               connect sock (addrAddress serveraddr)
               sendAll sock $ regData res (C.pack s)
               recv sock 256
               sendAll sock $ newUser (C.pack u)
               msg <- recv sock 256
               return $ Player u (uid res) (opname res) s
                               (displayNovice res) (create_time res) (key res)
                               (show . newChNumber $ encode msg)
                               (amount res)

joinWorld :: Player -> IO Socket
joinWorld user = do uServer <- getServerInfo (defaultsid $ user)
                    addrinfos <- getAddrInfo Nothing (Just $ ip uServer) (Just $ port uServer)
                    let serveraddr = head addrinfos
                    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
                    connect sock (addrAddress serveraddr)
                    sendAll sock $ loginData user
                    msg <- recv sock 1024
                    sendAll sock $ enterW (C.pack $ uid user) (C.pack $ chNumber user)
                    C.putStrLn $ C.append (C.pack $ acc user) " has joined the KD world!"
                    return sock
