{-# LANGUAGE OverloadedStrings #-}

module Play where

import Injector
import PacketGenerator
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
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
                    m <- recv conn 2048
                    sendAll conn armyReward
                    sendAll conn (armyMisAward "1")
                    sendAll conn (armyMisAward "2")
                    sendAll conn (armyMisAward "3")
                    sendAll conn (armyMisAward "4")
                    msg <- recv conn 2048
                    threadDelay 2000000
                    sendAll conn (armyMisAccept "1")
                    sendAll conn (armyMisAccept "2")
                    sendAll conn (armyMisAccept "3")
                    sendAll conn (armyMisAccept "4")                   