{-# LANGUAGE OverloadedStrings #-}

module CrossWar where

import Injector
import PacketGenerator
import Parser
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do match <- getMatch "1"
          winBet match
          threadDelay 48000000
          loseBet match

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

