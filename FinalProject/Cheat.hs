{-# LANGUAGE OverloadedStrings #-}

module Cheat where

import Injector
import PacketGenerator
import Parser hiding (encode, decode)
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad
import Control.Concurrent

main :: IO ()
main = undefined

reward uname n = do 
    user <- getPlayer uname
    conn <- joinWorld user
    sendNTimes n conn rankReward


gift uname 0 = print "done"
gift uname x = do
    user <- getPlayer uname
    conn <- joinWorld user
    preload conn 32768
    sendAll conn $ activityItem "11091" --tlv 15633
    -- tlv ["149126", "149128"]
    forM_ ["86333","86334","86335","86336","86360","86361"] $ \n 
        -> sendAll conn $ activityReward n
    threadDelay 2000000
    gift uname (x - 1)

train uname n = do 
    user <- getPlayer uname
    conn <- joinWorld user
    sendNTimes n conn riderTrain

--ndd HERO_PATCH_NAME40/38
--hd HERO_PATCH_NAME15/41
hcode h
    | h == "qt" = ("HERO_PATCH_NAME41", "46")
    | h == "vnt" = ("HERO_PATCH_NAME46", "43")
    | h == "vtm" = ("HERO_PATCH_NAME08", "45")
    | h == "kp" = ("HERO_PATCH_NAME21", "93")
    | h == "tdlt" = ("HERO_PATCH_NAME47", "95")
    | h == "dd" = ("HERO_PATCH_NAME11", "98")
    | h == "vtd" = ("HERO_PATCH_NAME19", "97")
    | h == "dccb" = ("HERO_PATCH_NAME16", "96")
    | h == "dpbb" = ("HERO_PATCH_NAME39", "99")
    | h == "tdt" = ("HERO_PATCH_NAME48", "100")
    | otherwise = ("HERO_PATCH_NAME54", "6")

patch u h = do
    p <- getPlayer u
    conn <- joinWorld p
    threadDelay 2000000    
    preload conn 36864
    refStore conn (hcode h)

refStore conn (str, idx) = do
    sendAll conn store8
    threadDelay 960000
    msg <- recv conn 1024
    if (C.isInfixOf str msg) then do
        C.putStrLn "Patch found"
        sendAll conn $ store9 idx
        msg2 <- recv conn 512
        if (C.isInfixOf "0300a17b" $ encode msg2) then do
            C.putStrLn "Done!"
            close conn
            else refStore conn (str, idx)
        else refStore conn (str, idx)