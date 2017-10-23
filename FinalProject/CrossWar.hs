{-# LANGUAGE OverloadedStrings #-}

module CrossWar where

import Injector
import PacketGenerator
import Parser hiding (encode, decode)
import Serializer (hexDeserialize)
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do match <- getMatch "1"
          winBet match
          loseBet match

---------------------------------------------------------
-- the last 1000 xu should bet 10 times 100xu
bet :: Integer -> Integer -> Socket -> ByteString -> IO ()
bet 0 lim c idx = bet lim lim c idx
bet 1 lim c idx = sendNTimes 10 c (bet100 idx)
bet n lim c idx
    | n > lim = bet lim lim c idx
    | otherwise = do sendNTimes (n - 1) c (bet1000 idx)
                     bet 1 lim c idx

-- buff players will lose money                 
loseBet :: Match -> IO ()
loseBet m = do pls <- buffPls
               forM_ pls $ \u -> forkIO $ do
                   conn <- joinWorld u
                   msg <- recv conn 80
                   bet (amount u) (div (coinInfo msg) 1000) conn (C.pack $ lose m)

-- players will win money
winBet :: Match -> IO ()
winBet m = do pls <- players
              forM_ pls $ \u -> forkIO $ do
                  conn <- joinWorld u
                  msg <- recv conn 80
                  bet (amount u) (div (coinInfo msg) 1000) conn (C.pack $ win m)


---------------------------------------------------------
flower idx n = do
    pls <- buffPls
    forM_ pls $ \p -> do
        conn <- joinWorld p
        sendAll conn $ cwarflower (C.pack idx) (C.pack n)

---------------------------------------------------------
-- looking coin info, get 80 bytes from socket after join and return number of coin
coinInfo :: ByteString -> Integer
coinInfo msg = hexDeserialize $ C.drop 152 $ encode msg

info = do
    pls <- buffPls
    forM_ pls $ \p -> do
        conn <- joinWorld p
        msg <- recv conn 80
        C.putStrLn $ C.append "coin info: " (C.pack $ show $ coinInfo msg)
        close conn