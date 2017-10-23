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
    pls <- players
    forM_ pls $ \p -> forkIO $ quickBet p

---------------------------------------------------------
-- the last 1000 xu should bet 10 times 100xu
bet :: Integer -> Socket -> ByteString -> IO ()
bet 0 c idx = sendAll c (bet100 idx)
bet 1 c idx = sendNTimes 10 c (bet100 idx)
bet n c idx = do sendNTimes (n - 1) c (bet1000 idx)
                 bet 1 c idx

-- bet 3 match same time
quickBet :: Player -> IO ()                  
quickBet p = do
    conn <- joinWorld p
    forM_ codeName $ \c -> forkIO $ do
        bet (amount p) conn c

codeName = ["28001474", "114000433", "109000079"]