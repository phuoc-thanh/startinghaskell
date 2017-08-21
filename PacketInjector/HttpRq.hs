{-# LANGUAGE OverloadedStrings #-}



module HttpRq where

import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C

import Parser
import Network.HTTP.Simple

-- loginVerifyURI = "/sgwww/vega/loginVerify"


loginVerifyURI = "/jinyong/vega/loginVerify"
checkUserURI = "/payclient.ashx?op=CheckUser"
getUserURI = "/payclient.ashx?op=GetUser"
regUserURI = "/payclient.ashx?op=RegisterUser"

-- apiHost = "api.alv.gaba.vn"
-- apiHost = "api.kimdungqq.com"
apiHost = "api.kd.gaba.vn"

-- payHost = "m-pay.kimdungqq.com"
payHost = "m-pay.gaba.vn"

checkUserRq :: ByteString -> ByteString -> Request
checkUserRq u p = setRequestPath checkUserURI
                $ setRequestHost payHost
                $ setRequestBodyLBS (userRqBody u p)
                $ setRequestMethod "POST"
                $ defaultRequest

newUserRq :: ByteString -> ByteString -> Request
newUserRq u p   = setRequestPath regUserURI
                $ setRequestHost payHost
                $ setRequestBodyLBS (userRqBody u p)
                $ setRequestMethod "POST"
                $ defaultRequest                

userRqBody :: ByteString -> ByteString -> ByteString
userRqBody u p   = C.append "partnerId=0&userName="
                 $ C.append u 
                 $ C.append "&deviceId=d80f998b16396187&serverMode=UNKNOWN&password="
                 $ C.append p "&refcode=0&gameId=46"

loginRqBody :: ByteString -> ByteString
loginRqBody s = C.append "uid=" . C.append userId . C.append "&token=" $ C.append session "&os=and&version=75896"
    where session = C.drop 8 $ (!!) (C.split '&' s) 3
          userId  = C.drop 7 $ (!!) (C.split '&' s) 2

loginVerifyRq :: ByteString -> Request       
loginVerifyRq s = setRequestPath loginVerifyURI
                $ setRequestHost apiHost
                $ setRequestBodyLBS (loginRqBody s)
                $ setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
                $ defaultRequest

getUserData :: FromJSON a => ByteString -> Maybe a
getUserData s = decode $ C.append (C.drop 9 $ head $ C.split '}' s) ",\"chNumber\" : \"unknow\",\"amount\" : 0}"

loginVerify :: String -> String -> IO Player
loginVerify u p = do
    cResponse <- httpLBS $ checkUserRq (C.pack u) (C.pack p)
    response <- httpLBS $ loginVerifyRq (getResponseBody cResponse)
    return $ fromJust $ getUserData $ getResponseBody response

regAccount :: String -> String -> IO Player
regAccount u p = do
    nResponse <- httpLBS $ newUserRq (C.pack u) (C.pack p)
    cResponse <- httpLBS $ checkUserRq (C.pack u) (C.pack p)
    response <- httpLBS $ loginVerifyRq (getResponseBody cResponse)
    return $ fromJust $ getUserData $ getResponseBody response    