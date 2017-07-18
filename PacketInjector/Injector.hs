{-# LANGUAGE OverloadedStrings #-}



module Injector where
import GameData  
import LoginData
import HttpRq
import PlayGame
import Parser
import Network.Socket hiding (send, close)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lazy (ByteString)
import Data.Connection
import Data.Maybe
import Data.Aeson
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Control.Concurrent
import Control.Monad


getServerInfo :: Int -> IO (String, Integer)
getServerInfo i = do
    serverinfos <- parseFile "ServerInfo.json" :: IO (Maybe [KDServer])
    let server = (!!) (fromJust $ serverinfos) (i - 1)
    return $ (ip server, port server)

-- buffUsers :: IO ([BuffUser])
buffUsers i = do
    buffUsers <- parseFile "BuffUsers.json" :: IO (Maybe [BuffUser])
    let user = (!!) (fromJust $ buffUsers) (i - 1)
    return user

sendNTimes :: Integer -> Connection (Socket, SockAddr) -> ByteString -> IO ()
sendNTimes 1 c s = send c s
sendNTimes n c s = do send c s
                      sendNTimes (n - 1) c s 
                               
joinWorld :: ByteString -> ByteString -> IO TCP.TCPConnection
joinWorld u p = do res <- loginVerify u p
                   uServer <- getServerInfo (read $ defaultsid res)
                   conn <- TCP.connect (fst uServer) (fromInteger $ snd uServer)
                   send conn $ getLoginData res
                   msg <- Streams.read (source conn)
                   send conn . enterW res . C.fromStrict $ fromJust msg
                   C.putStrLn $ C.append u " has joined the KD world!"
                   return conn
                    
getReward u p = do conn <- joinWorld u p
                   sendNTimes 9999 conn rankReward
                   close conn
 
-- main = do
--   hSetBuffering stdout NoBuffering            -- 1
--   forkIO (replicateM_ 10000 (putChar 'A'))   -- 2
--   forkIO (replicateM_ 10000 (putChar '9'))   -- 2
--   replicateM_ 10000 (putChar 'B')            -- 3         
