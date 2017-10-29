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

--vtm: HERO_PATCH_NAME08/store 9 91 1/store 9 45 1 (2)
--dccb: HERO_PATCH_NAME16/ store 9 96 1
--dd: HERO_PATCH_NAME11/ store 9 98 1
--dpbb: HERO_PATCH_NAME39/ store 9 99 1
--tdt: HERO_PATCH_NAME48/ store 9 100 1



patch u = do
    p <- getPlayer u
    conn <- joinWorld p
    threadDelay 2000000    
    preload conn 36864
    refStore conn ("HERO_PATCH_NAME11", "98")

refStore conn (str, idx) = do
    sendAll conn store8
    threadDelay 1000000
    msg <- recv conn 1024
    when (C.isInfixOf "0300a17b" $ encode msg) $ do
        C.putStrLn "Done!"
        close conn
    if (C.isInfixOf str msg)
        then C.putStrLn "found"
        else refStore conn (str, idx)