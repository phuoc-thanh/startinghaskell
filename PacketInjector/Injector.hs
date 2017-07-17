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


parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    contents <- BS.readFile file
    return $ decode contents

getServerInfo :: Int -> IO (String, Integer)
getServerInfo i = do
    serverinfos <- parseFile "ServerInfo.json" :: IO (Maybe [KDServer])
    let server = (!!) (fromJust $ serverinfos) (i - 1)
    return $ (ip server, port server)

sendNTimes :: Integer -> Connection (Socket, SockAddr) -> IO ()
sendNTimes 1 c = send c $ bet100
sendNTimes n c = do send c $ bet100
                    sendNTimes (n - 1) c
                 
rankRewards :: Integer -> Connection (Socket, SockAddr) -> IO ()
rankRewards 1 c = send c $ rankReward
rankRewards n c = do send c $ rankReward
                     rankRewards (n - 1) c                 

injectWorld :: IO ()
injectWorld = do res <- loginVerify
                 uServer <- getServerInfo (read $ defaultsid res)
                 conn <- TCP.connect (fst uServer) (fromInteger $ snd uServer)
                 send conn $ getLoginData res
                 msg <- Streams.read (source conn)
                 send conn . enterWorld res . C.fromStrict $ fromJust msg
                 Streams.read (source conn)
                 C.putStrLn "enter world!"
                 close conn
                --  forkIO $ rankRewards 99999 conn
                --  msg3 <- Streams.read (source conn)
                --  C.putStrLn "bet 1000"