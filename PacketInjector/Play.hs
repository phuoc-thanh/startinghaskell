{-# LANGUAGE OverloadedStrings #-}

module Play where

import Injector
import PacketGenerator
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad
import Control.Concurrent

          
sendP :: String -> Integer -> IO ()
sendP uname n = do user <- getPlayer uname
                   conn <- joinWorld user
                   sendNTimes n conn tPacket
                   close conn

armyMis :: IO ()                  
armyMis = do bUsers <- players
             forM_ bUsers $ \u -> do
                forkIO $ do
                    conn <- joinWorld u
                    sendAll conn armyRequest
                    requestA conn

armyMis' :: IO ()                  
armyMis' = do bUsers <- players
              forM_ bUsers $ \u -> do
                 forkIO $ do
                    conn <- joinWorld u
                    sendAll conn armyReward
                    sendAll conn armyBase
                    listenA' conn

requestA :: Socket -> IO ()
requestA conn  = do threadDelay 2000000
                    msg <- recv conn 2048
                    unless (C.isInfixOf "0300aa0801" $ encode msg) $ requestA conn
                    when (C.isInfixOf "0300aa0801" $ encode msg) $ do
                        sendAll conn armyReward
                        sendAll conn armyBase
                        listenA conn
                    

listenA :: Socket -> IO ()              
listenA conn   = do threadDelay 1000000
                    msg <- recv conn 2048
                    when (C.isInfixOf "0300a12905" $ encode msg) $ do
                        sendAll conn armyExit
                        recv conn 2048
                        threadDelay 2000000
                        C.putStrLn "exit Army!"
                        close conn
                    unless (C.isInfixOf "boss" msg) $ listenA conn
                    when (C.isInfixOf "boss" msg) $ do
                        sendAll conn (armyMisAward "1")
                        sendAll conn (armyMisAward "2")
                        sendAll conn (armyMisAward "3")
                        sendAll conn (armyMisAward "4")
                        threadDelay 1000000
                        recv conn 2048
                        sendAll conn (armyMisAccept "1")
                        sendAll conn (armyMisAccept "2")
                        sendAll conn (armyMisAccept "3")
                        sendAll conn (armyMisAccept "4")
                        threadDelay 2000000
                        sendAll conn armyMisSpdUp
                        sendAll conn armyBase
                        listenA conn
                    
listenA' :: Socket -> IO ()              
listenA' conn  = do threadDelay 1000000
                    msg <- recv conn 2048
                    unless (C.isInfixOf "boss" msg) $ listenA conn
                    when (C.isInfixOf "boss" msg) $ do
                        sendAll conn (armyMisAward "1")
                        sendAll conn (armyMisAward "2")
                        sendAll conn (armyMisAward "3")
                        sendAll conn (armyMisAward "4")
                        threadDelay 1000000
                        recv conn 2048
                        sendAll conn (armyMisAccept "1")
                        sendAll conn (armyMisAccept "2")
                        sendAll conn (armyMisAccept "3")
                        sendAll conn (armyMisAccept "4")
                        threadDelay 2000000
                        sendAll conn armyMisSpdUp
                        sendAll conn armyBase
                        listenA conn