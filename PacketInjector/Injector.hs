{-# LANGUAGE OverloadedStrings #-}

module Injector where
import Authenticator
import HttpRq
import PacketGenerator
import Parser hiding (encode)
import Network.Socket hiding (send, close)
import Data.ByteString.Base16.Lazy
import Data.Connection
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Control.Concurrent

getServerInfo :: String -> IO KDServer
getServerInfo i = do
    serverinfos <- parseFile "ServerInfoQQ.json" :: IO (Maybe [KDServer])
    return $ head $ filter (\s -> (sid s) == i) (fromJust $ serverinfos)

buffUsers :: IO ([KDUser])
buffUsers = do
    buffUsers <- parseFile "BuffUsers.json" :: IO (Maybe [KDUser])
    return $ fromJust buffUsers
    
getUser :: String -> IO KDUser    
getUser uname = do
    users <- parseFile "BuffUsers.json" :: IO (Maybe [KDUser])
    return $ head $ filter (\u -> (acc u) == uname) (fromJust $ users)    

sendNTimes :: Integer -> Connection (Socket, SockAddr) -> C.ByteString -> IO ()
sendNTimes 1 c s = send c s
sendNTimes n c s = do send c s
                      sendNTimes (n - 1) c s 
                               
login :: String -> String -> IO ()
login u p = do res <- loginVerify (C.pack u) (C.pack p)
               C.putStrLn $ C.pack $ show $ res
               uServer <- getServerInfo (defaultsid res)
               conn <- TCP.connect (ip uServer) (fromInteger $ port uServer)
               send conn $ loginData res
               msg <- Streams.read (source conn)
               C.putStrLn $ C.append "chNumber:" $ C.pack $ show (getChNumber . encode . C.fromStrict $ fromJust msg)
               bu <- buffUsers
               appendJSON "BuffUsers.json" (res : bu)
               close conn

joinWorld :: KDUser -> IO TCP.TCPConnection
joinWorld user = do uServer <- getServerInfo (defaultsid $ user)
                    conn <- TCP.connect (ip uServer) (fromInteger $ port uServer)
                    send conn $ loginData user
                    msg <- Streams.read (source conn)
                    send conn $ enterW (C.pack $ uid user) (C.pack $ chNumber user)
                    C.putStrLn $ C.append (C.pack $ acc user) " has joined the KD world!"
                    return conn

rankR :: String -> Integer -> IO ()
rankR uname n = do user <- getUser uname
                   conn <- joinWorld user
                   sendNTimes n conn rankReward
                   close conn                    

sendP :: String -> Integer -> IO ()                   
sendP uname n = do user <- getUser uname
                   conn <- joinWorld user
                   sendNTimes n conn tPacket
                   close conn

-- idx: the betting id of character in a cross server war.                   
buff :: String -> IO ()                  
buff idx = do bUsers <- buffUsers
              forM_ bUsers $ \u -> do
                forkIO $ do
                    conn <- joinWorld u
                    sendNTimes (amount u) conn (bet100 $ C.pack idx)
                    -- close conn
            --  return $ C.putStrLn "Done !"
       
armyRewards :: IO ()                  
armyRewards = do bUsers <- buffUsers
                 forM_ bUsers $ \u -> do
                    forkIO $ do
                        conn <- joinWorld u
                        send conn armyReward

armyMisAwards :: IO ()                  
armyMisAwards = do bUsers <- buffUsers
                   forM_ bUsers $ \u -> do
                      forkIO $ do
                        conn <- joinWorld u
                        send conn (armyMisAward "1")
                        send conn (armyMisAward "2")
                        send conn (armyMisAward "3")
                        send conn (armyMisAward "4")

armyMisAccepts :: IO ()                  
armyMisAccepts = do bUsers <- buffUsers
                    forM_ bUsers $ \u -> do
                       forkIO $ do
                         conn <- joinWorld u
                         send conn (armyMisAccept "1")
                         send conn (armyMisAccept "2")
                         send conn (armyMisAccept "3")
                         send conn (armyMisAccept "4")                        