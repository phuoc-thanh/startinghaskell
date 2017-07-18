{-# LANGUAGE OverloadedStrings #-}

module Injector where
import GameData  
import LoginData
import HttpRq
import PlayGame
import Parser hiding (encode)
import Network.Socket hiding (send, close)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lazy (ByteString)
import Data.Connection
import Data.Maybe
import Data.Aeson hiding (encode)
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Control.Concurrent
import Control.Monad
import Data.ByteString.Base16.Lazy

getServerInfo :: Int -> IO (String, Integer)
getServerInfo i = do
    serverinfos <- parseFile "ServerInfo.json" :: IO (Maybe [KDServer])
    let server = (!!) (fromJust $ serverinfos) (i - 1)
    return $ (ip server, port server)

-- buffUsers :: IO ([KDUser])
buffUsers i = do
    buffUsers <- parseFile "BuffUsers.json" :: IO (Maybe [KDUser])
    let user = (!!) (fromJust $ buffUsers) (i - 1)
    return user

sendNTimes :: Integer -> Connection (Socket, SockAddr) -> ByteString -> IO ()
sendNTimes 1 c s = send c s
sendNTimes n c s = do send c s
                      sendNTimes (n - 1) c s 
                               
fullJoin :: ByteString -> ByteString -> IO TCP.TCPConnection
fullJoin u p = do  res <- loginVerify u p
                   uServer <- getServerInfo (read $ defaultsid res)
                   conn <- TCP.connect (fst uServer) (fromInteger $ snd uServer)
                   send conn $ loginData res
                   msg <- Streams.read (source conn)
                   send conn $ enterW (C.pack $ uid res) (getChNumber . encode . C.fromStrict $ fromJust msg)
                   C.putStrLn $ C.append u " has joined the KD world!"
                   return conn

joinWorld :: Int -> IO TCP.TCPConnection
joinWorld idx = do  user <- buffUsers idx 
                    uServer <- getServerInfo (read $ defaultsid user)
                    conn <- TCP.connect (fst uServer) (fromInteger $ snd uServer)
                    send conn $ loginData user
                    msg <- Streams.read (source conn)
                    send conn $ enterW (C.pack $ uid user) (getChNumber . encode . C.fromStrict $ fromJust msg)
                    C.putStrLn $ C.append (C.pack $ acc user) " has joined the KD world!"
                    return conn
                    
getReward u p = do conn <- joinWorld 0
                   sendNTimes 9999 conn rankReward
                   close conn
 
-- main = do
--   hSetBuffering stdout NoBuffering            -- 1
--   forkIO (replicateM_ 10000 (putChar 'A'))   -- 2
--   forkIO (replicateM_ 10000 (putChar '9'))   -- 2
--   replicateM_ 10000 (putChar 'B')            -- 3         
