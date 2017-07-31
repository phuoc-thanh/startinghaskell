{-# LANGUAGE OverloadedStrings #-}

module Play where

import Injector
import PacketGenerator
import Parser
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv, sendAll)
import Data.Maybe
import Control.Monad
import Control.Concurrent
-- import Control.Concurrent.Async

main :: IO ()
main = do match <- getMatch "1"
          winBet match
          loseBet match

adp :: String -> String -> IO ()
adp u p = do pl <- login u p
             pls <- players
             appendJSON "Players.json" (pl:pls)
            
adb :: String -> String -> IO ()
adb u p = do pl <- login u p
             pls <- players
             appendJSON "Buffs.json" (pl:pls)

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
---------------------------------------------------------
-- the last 1000 xu should bet 10 times 100xu
bet :: Integer -> Socket -> ByteString -> IO ()
bet 0 c idx = sendAll c (bet100 idx)
bet 1 c idx = sendNTimes 10 c (bet100 idx)
bet n c idx = do sendAll c (bet1000 idx)
                 bet (n - 1) c idx

-- buff players will lose money                 
loseBet :: Match -> IO ()                  
loseBet m = do pls <- buffPls
               forM_ pls $ \u -> do
                 forkIO $ do
                   conn <- joinWorld u
                   bet (amount u) conn (C.pack $ lose m)

-- players will win money                           
winBet :: Match -> IO ()                  
winBet m = do pls <- players
              forM_ pls $ \u -> do
                forkIO $ do
                  conn <- joinWorld u
                  bet (amount u) conn (C.pack $ win m)

armyMis :: IO ()                  
armyMis = do bUsers <- players
             forM_ bUsers $ \u -> do
                forkIO $ do
                    conn <- joinWorld u
                    sendAll conn armyReward
                    sendAll conn (armyMisAward "1")
                    sendAll conn (armyMisAward "2")
                    sendAll conn (armyMisAward "3")
                    sendAll conn (armyMisAward "4")
                    sendAll conn (armyMisAccept "1")
                    sendAll conn (armyMisAccept "2")
                    sendAll conn (armyMisAccept "3")
                    sendAll conn (armyMisAccept "4")

tRefresh = do u <- getPlayer "reply0001"
              conn <- joinWorld u
              sendAll conn $ huntRefresh
              listenM conn
              -- msg <- recv conn 2048
              -- C.putStrLn msg
              close conn

listenM :: Socket -> IO ()              
listenM conn = do msg <- recv conn 2048
                  C.putStrLn msg
                  unless (C.isInfixOf "ZM" msg) C.putStrLn "ended"