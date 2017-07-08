
{-# LANGUAGE OverloadedStrings #-}

import Network
import Network.URI
-- import Network.HTTP
import Network.Browser
import System.IO

import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
 
sampleReq = withSocketsDo $ do
    h <- connectTo "api.kimdungqq.com" (PortNumber 80)
    hSetBuffering h LineBuffering
    hPutStr h "GET /jinyong/api/client_api/defaultSid.php?acc=11111&sid=1&opname=10&sign=8270b3084d30ab3d95a7d58dbb15ed73 HTTP/1.1\nhost: api.kimdungqq.com\n\n"
    contents <- hGetContents h
    putStrLn contents
    hClose h

--https://wiki.haskell.org/Cookbook/Network_programming


Just keepAliveURI = parseURI "http://api.kimdungqq.com/jinyong/vega/loginVerify"
body = "uid=11111&token=6892aea2-d3da-48d8-84c6-fcc4782f3d21&os=and&version=75896"
-- body = "partnerId=0&userName=reply1988&deviceId=d80f998b16396187&serverMode=UNKNOWN&password=haxinhdep&refcode=0&gameId=46"
-- headers = [Header HdrContentType "application/x-www-form-urlencoded",
--            Header HdrContentLength (show . length $ body)]

-- keepAliveRq = Request  {
--     rqURI = keepAliveURI, rqMethod = POST,
--     rqHeaders = headers,
--     rqBody = body
-- }

-- keepAliveRq = setRequestPath "/payclient.ashx?op=CheckUser"
keepAliveRq = setRequestPath "/jinyong/vega/loginVerify"
            $ setRequestHost "api.kimdungqq.com"
            -- $ setRequestHost "m-pay.kimdungqq.com"
            $ setRequestBodyLBS body
            $ setRequestMethod "POST"
            $ defaultRequest

main = do
    response <- httpLBS keepAliveRq
    putStrLn $ "The Header: " ++
               show (getResponseHeaders response)
    putStrLn $ show $ response
-- main = browse $ do
--      res <- request keepAliveRq
--      putStrLn show $ getResponseBody res