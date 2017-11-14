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

train uname n = do 
    user <- getPlayer uname
    conn <- joinWorld user
    sendNTimes n conn riderTrain

hcode h
    | h == "kp" = ("HERO_PATCH_NAME21", "93")
    | h == "dd" = ("HERO_PATCH_NAME11", "98")
    | h == "vtd" = ("HERO_PATCH_NAME19", "97")
    | h == "dccb" = ("HERO_PATCH_NAME16", "96")
    | h == "dpbb" = ("HERO_PATCH_NAME39", "99")
    | otherwise = ("HERO_PATCH_NAME48", "100")

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