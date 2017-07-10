
-- {-# LANGUAGE OverloadedStrings #-}

import Network
import System.IO
 
sampleReq = withSocketsDo $ do
    h <- connectTo "api.kimdungqq.com" (PortNumber 80)
    hSetBuffering h LineBuffering
    hPutStr h "GET /jinyong/api/client_api/defaultSid.php?acc=11111&sid=1&opname=10&sign=8270b3084d30ab3d95a7d58dbb15ed73 HTTP/1.1\nhost: api.kimdungqq.com\n\n"
    contents <- hGetContents h
    putStrLn contents
    hClose h

--https://wiki.haskell.org/Cookbook/Network_programming
