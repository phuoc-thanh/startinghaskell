{-# LANGUAGE OverloadedStrings #-}



module Injector where
import GameData  
import LoginData
import HttpRq
import PlayGame
import Parser
import Network.Socket hiding (send, close)
-- import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as BS
import Data.Connection
import Data.Maybe
import Data.Aeson
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Control.Concurrent
import Control.Monad


getServerInfo :: Int -> IO (String, Integer)
getServerInfo i = do
    serverinfos <- parseFile "ServerInfoQQ.json" :: IO (Maybe [KDServer])
    let server = (!!) (fromJust $ serverinfos) (i - 1)
    return $ (ip server, port server)

-- buffUsers :: IO ([BuffUser])
buffUsers i = do
    buffUsers <- parseFile "BuffUsers.json" :: IO (Maybe [BuffUser])
    let user = (!!) (fromJust $ buffUsers) (i - 1)
    return user

sendNTimes :: Integer -> Connection (Socket, SockAddr) -> IO ()
sendNTimes 1 c = send c $ bet100
sendNTimes n c = do send c $ bet100
                    sendNTimes (n - 1) c
                 
rankRewards :: Integer -> Connection (Socket, SockAddr) -> IO ()
rankRewards 1 c = send c $ rankReward
rankRewards n c = do send c $ rankReward
                     rankRewards (n - 1) c
                     
storeR :: Integer -> Connection (Socket, SockAddr) -> IO ()
storeR 1 c = send c $ store82
storeR n c = do send c $ store82
                storeR (n - 1) c             
                               
-- joinWorld :: String -> String -> IO TCP.TCPConnection
joinWorld u p = do res <- loginVerify u p
                   uServer <- getServerInfo (read $ defaultsid res)
                   conn <- TCP.connect (fst uServer) (fromInteger $ snd uServer)
                   send conn $ getLoginData res
                   msg <- Streams.read (source conn)
                   send conn . enterW res . C.fromStrict $ fromJust msg
                   C.putStrLn $ C.append (C.pack u) " has joined world!"
                   return conn
                    
getReward u p = do conn <- joinWorld u p
                   rankRewards 9999 conn
                   close conn

bet100x n = do u <- buffUsers n
               conn <- joinWorld (uname u) (pass u)
               storeR 1 conn
               close conn

buff = do
    bet100x 1
    bet100x 2
    bet100x 10
    C.putStrLn "bet 100"             
-- main = do
--   hSetBuffering stdout NoBuffering            -- 1
--   forkIO (replicateM_ 10000 (putChar 'A'))   -- 2
--   forkIO (replicateM_ 10000 (putChar '9'))   -- 2
--   replicateM_ 10000 (putChar 'B')            -- 3         
