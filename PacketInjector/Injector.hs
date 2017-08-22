{-# LANGUAGE OverloadedStrings #-}

module Injector where
import Authenticator
import HttpRq
import Parser hiding (encode)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Data.ByteString.Base16
import Data.Maybe
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)

getServerInfo :: String -> IO Server
getServerInfo i = do
    serverinfos <- parseFile "ServerInfo.json" :: IO (Maybe [Server])
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

connect_ :: HostName -> ServiceName -> IO Socket
connect_ host port = do 
    addrinfos <- getAddrInfo Nothing (Just $ host) (Just $ port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    return sock

login :: String -> String -> IO Player
login u p = do res <- loginVerify u p
               uServer <- getServerInfo (defaultsid res)
               sock <- connect_ (ip uServer) (port uServer)
               sendAll sock $ loginData res (C.pack $ defaultsid res)
               msg <- recv sock 256
               close sock
               return $ Player u (uid res) (opname res) (defaultsid res)
                               (displayNovice res) (create_time res) (key res) 
                               (show . getChNumber $ encode msg)
                               (amount res)

reg :: String -> String -> String -> IO (Socket, Player)                    
reg u p s = do res <- regAccount u p
               uServer <- getServerInfo s
               sock <- connect_ (ip uServer) (port uServer)
               sendAll sock $ loginData res (C.pack s)
               recv sock 256
               return (sock, res)

joinWorld :: Player -> IO Socket
joinWorld user = do uServer <- getServerInfo (defaultsid $ user)
                    sock <- connect_ (ip uServer) (port uServer)
                    sendAll sock $ loginData user (C.pack $ defaultsid user)
                    msg <- recv sock 1024
                    sendAll sock $ enterW (C.pack $ uid user) (C.pack $ chNumber user)
                    C.putStrLn $ C.append (C.pack $ acc user) " has joined the KD world!"
                    return sock
