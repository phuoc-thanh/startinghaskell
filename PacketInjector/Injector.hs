{-# LANGUAGE OverloadedStrings #-}

module Injector where
import Authenticator
import HttpRq
import Parser hiding (encode)
import Network.Socket hiding (send, close)
import Data.ByteString.Base16.Lazy
import Data.Connection
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams

getServerInfo :: String -> IO Server
getServerInfo i = do
    serverinfos <- parseFile "ServerInfoQQ.json" :: IO (Maybe [Server])
    return $ head $ filter (\s -> (sid s) == i) (fromJust $ serverinfos)

players :: IO ([Player])
players = do
    players <- parseFile "qqPlayers.json" :: IO (Maybe [Player])
    return $ fromJust players
    
getPlayer :: String -> IO Player    
getPlayer uname = do
    users <- parseFile "qqPlayers.json" :: IO (Maybe [Player])
    return $ head $ filter (\u -> (acc u) == uname) (fromJust $ users)    

sendNTimes :: Integer -> Connection (Socket, SockAddr) -> C.ByteString -> IO ()
sendNTimes 1 c s = send c s
sendNTimes n c s = do send c s
                      sendNTimes (n - 1) c s 
                               
login :: String -> String -> IO Player
login u p = do res <- loginVerify (C.pack u) (C.pack p)
               uServer <- getServerInfo (defaultsid res)
               conn <- TCP.connect (ip uServer) (fromInteger $ port uServer)
               send conn $ loginData res
               msg <- Streams.read (source conn)
               close conn
               return $ Player u (uid res) (opname res) (defaultsid res) (displayNovice res) 
                               (create_time res) (key res) 
                               (show . getChNumber . encode . C.fromStrict $ fromJust msg)
                               (amount res)

joinWorld :: Player -> IO TCP.TCPConnection
joinWorld user = do uServer <- getServerInfo (defaultsid $ user)
                    conn <- TCP.connect (ip uServer) (fromInteger $ port uServer)
                    send conn $ loginData user
                    msg <- Streams.read (source conn)
                    send conn $ enterW (C.pack $ uid user) (C.pack $ chNumber user)
                    C.putStrLn $ C.append (C.pack $ acc user) " has joined the KD world!"
                    return conn
                     