{-# LANGUAGE OverloadedStrings #-}

module Play where

import Injector
import PacketGenerator
import Parser
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Concurrent
import Data.Connection
import Control.Monad

adp :: String -> String -> IO ()
adp u p = do pl <- login u p
             pls <- players
             appendJSON "Players.json" (pl:pls)
            

sendP :: String -> Integer -> IO ()                   
sendP uname n = do user <- getPlayer uname
                   conn <- joinWorld user
                   sendNTimes n conn tPacket
                   close conn

rankR :: String -> Integer -> IO ()
rankR uname n = do user <- getPlayer uname
                   conn <- joinWorld user
                   sendNTimes n conn rankReward
                   close conn

-- idx: the betting id of character in a cross server war.                   
buff :: String -> IO ()                  
buff idx = do bUsers <- players
              forM_ bUsers $ \u -> do
                forkIO $ do
                    conn <- joinWorld u
                    sendNTimes (amount u) conn (bet100 $ C.pack idx)

armyMis :: IO ()                  
armyMis = do bUsers <- players
             forM_ bUsers $ \u -> do
                forkIO $ do
                    conn <- joinWorld u
                    send conn armyReward
                    send conn (armyMisAward "1")
                    send conn (armyMisAward "2")
                    send conn (armyMisAward "3")
                    send conn (armyMisAward "4")
                    send conn (armyMisAccept "1")
                    send conn (armyMisAccept "2")
                    send conn (armyMisAccept "3")
                    send conn (armyMisAccept "4")