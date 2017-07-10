{-# LANGUAGE OverloadedStrings #-}



module Login where

import System.IO
import Data.List
import Data.List.Split
import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS

import           Data.Aeson            (Value)
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple


loginVerifyURI = "/jinyong/vega/loginVerify"
checkUserURI = "/payclient.ashx?op=CheckUser"
getUserURI = "/payclient.ashx?op=GetUser"

-- 210.245.26.186
apiHost = "api.kimdungqq.com"
-- 104.28.17.37
payHost = "m-pay.kimdungqq.com"

checkUserRq     = setRequestPath checkUserURI
                $ setRequestHost payHost
                $ setRequestBodyLBS "partnerId=0&userName=reply1988&deviceId=d80f998b16396187&serverMode=UNKNOWN&password=haxinhdep&refcode=0&gameId=46"
                $ setRequestMethod "POST"
                $ defaultRequest
--"result=1&fullName=reply1988&userId=11111&session=9929ea82-151f-492d-8a2c-94346d35f14f&IP=172.31.16.101"

getSession :: ByteString -> ByteString
getSession = fst . BS.splitAt 36 . snd . BS.splitAt 49 

getUserRq       = setRequestPath getUserURI
                $ setRequestHost payHost
                $ setRequestBodyLBS "session=9929ea82-151f-492d-8a2c-94346d35f14f&serverMode=UNKNOWN&hash=e02ea6746a05b4fd4a765fd65cb8a5c6&time=1499617306527"
                $ setRequestMethod "POST"
                $ defaultRequest                
--"fullname=reply1988&email=thanhdo89se%40gmail.com&phone=0982393901&cash=17000&idcard=241096330&IP=172.31.16.101&result=1"
getUid :: ByteString -> ByteString
getUid u = BS.append (BS.append "uid=" u) "&token="

getLoginRqBody :: String -> ByteString -> ByteString
getLoginRqBody u b = BS.append (BS.append (BS.append "uid=" (S8.pack u)) "&token=") (BS.append "&os=and&version=75896" b)

loginVerifyRq   = setRequestPath loginVerifyURI
                $ setRequestHost apiHost
                -- $ setRequestBodyLBS "uid=11111&token=9929ea82-151f-492d-8a2c-94346d35f14f&os=and&version=75896"
                $ setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
                $ defaultRequest
main = do
    uid <- getLine
    cResponse <- httpLBS checkUserRq
    response <- httpJSON (setRequestBodyLBS (getLoginRqBody uid $ getSession (getResponseBody cResponse)) $ loginVerifyRq)
    -- putStrLn $ getResponseHeaders response
    putStrLn $ getResponseBody response
