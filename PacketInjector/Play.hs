{-# LANGUAGE OverloadedStrings #-}

module Play where

import Injector
import PacketGenerator
import Parser
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.List
import Data.List.Split
import Data.Maybe
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
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
            --   C.putStrLn zm
              -- close conn

-- listenM :: Socket -> t -> IO ()              
listenM conn   = do msg <- recv conn 2048
                    unless (C.isInfixOf "ZM" msg) $ listenM conn
                    when (C.isInfixOf "ZM" msg) $ do
                        let h = tail . map (C.pack . take 4) $ split (startsWith "ZM") $ C.unpack msg
                        hunt h conn


-- CBT ZM39, VNT ZM43, VTM ZM44, HT ZM45, TVK ZM49, KP ZM50, TDLT ZM51
huntTarget = "ZM39|ZM44|ZM49|ZM50|ZM51"

huntVerify heroes
    | (C.isInfixOf (heroes !! 0) huntTarget) = Just "0"
    | (C.isInfixOf (heroes !! 1) huntTarget) = Just "1"
    | (C.isInfixOf (heroes !! 2) huntTarget) = Just "2"
    | otherwise = Nothing

hunt h conn = case huntVerify h of
                Just idx -> do C.putStrLn $ C.append "found hero at " idx
                               sendAll conn $ openRoom idx
                               huntStart idx conn
                Nothing -> do threadDelay 2000000
                              sendAll conn $ huntRefresh
                              listenM conn

huntStart idx conn = do threadDelay 5000000
                        msg <- recv conn 2048
                        unless (C.isInfixOf "Reply1989" msg) $ huntStart idx conn
                        when (C.isInfixOf "Reply1989" msg) $ do
                            sendAll conn $ startRoom idx
                            msg <- recv conn 2048
                            threadDelay 3000000
                            close conn