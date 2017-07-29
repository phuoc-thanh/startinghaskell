{-# LANGUAGE OverloadedStrings #-}

module Play where

import Injector
import PacketGenerator
import Parser
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as BS
import Network.Socket hiding (send, close)
import Data.Connection
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import qualified System.IO.Streams as Streams

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
bet :: Integer -> Connection (Socket, SockAddr) -> C.ByteString -> IO ()
bet 0 c idx = send c (bet100 idx)
bet 1 c idx = sendNTimes 10 c (bet100 idx)
bet n c idx = do send c (bet1000 idx)
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
                    send conn armyReward
                    send conn (armyMisAward "1")
                    send conn (armyMisAward "2")
                    send conn (armyMisAward "3")
                    send conn (armyMisAward "4")
                    send conn (armyMisAccept "1")
                    send conn (armyMisAccept "2")
                    send conn (armyMisAccept "3")
                    send conn (armyMisAccept "4")

tRefresh = do u <- getPlayer "kdqq011"
              conn <- joinWorld u
              threadDelay 3000000
              ms <- Streams.read (source conn)
            --   Streams.unRead (fromJust ms) (source conn)
              BS.putStrLn $ fromJust $ ms

