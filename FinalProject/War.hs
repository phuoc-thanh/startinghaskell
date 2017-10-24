{-# LANGUAGE OverloadedStrings #-}

module War where

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
main = do 
    winBet
    loseBet

---------------------------------------------------------
-- the last 1000 xu should bet 10 times 100xu
bet :: Integer -> Socket -> ByteString -> IO ()
bet 0 c idx = close c
bet 1 c idx = sendNTimes 10 c (bet100 idx)
bet n c idx = do sendNTimes (n - 1) c (bet1000 idx)
                 bet 1 c idx

-- bet 3 match same time
quickBet :: Player -> [ByteString] -> IO ()                  
quickBet p codes = do
    conn <- joinWorld p
    forM_ codes $ \c -> forkIO $ do
        bet (amount p) conn c

---------------------------------------------------------
winBet :: IO ()
winBet = do
    pls <- players
    forM_ pls $ \p -> forkIO $ quickBet p winC
                  
loseBet :: IO ()
loseBet = do 
    pls <- buffPls
    forM_ pls $ \p -> forkIO $ quickBet p loseC                 

winC = ["28001474", "114000433", "109000079"]
loseC = ["28001474", "114000433", "109000079"]