{-# LANGUAGE OverloadedStrings #-}

module Injector where
import GameData  
import LoginData
import HttpRq
import PlayGame
import Parser hiding (encode)
import Network.Socket hiding (send, close)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Connection
import Data.Maybe
import Control.Monad
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Control.Concurrent
import Data.ByteString.Base16.Lazy

getServerInfo :: Int -> IO KDServer
getServerInfo i = do
    serverinfos <- parseFile "ServerInfo.json" :: IO (Maybe [KDServer])
    let server = (!!) (fromJust $ serverinfos) (i - 1)
    return $ server

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
               uServer <- getServerInfo (read $ defaultsid res)
               conn <- TCP.connect (ip uServer) (fromInteger $ port uServer)
               send conn $ loginData res
               msg <- Streams.read (source conn)
               C.putStrLn $ C.append "chNumber:" $ C.pack $ show (getChNumber . encode . C.fromStrict $ fromJust msg)
               close conn

joinWorld :: KDUser -> IO TCP.TCPConnection
joinWorld user = do uServer <- getServerInfo (read $ defaultsid $ user)
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
                   sendNTimes n conn store82
                   close conn

-- buff :: IO ThreadId()                  
buff n =  do bUsers <- buffUsers
             forM_ bUsers $ \u -> do
                forkIO $ sendP (acc u) n
            --  return $ C.putStrLn "Done !"
       
