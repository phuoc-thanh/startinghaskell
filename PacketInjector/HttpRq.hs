{-# LANGUAGE OverloadedStrings #-}



module HttpRq where

import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import Data.Maybe
import Parser
import Network.HTTP.Simple

import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C

loginVerifyURI = "/jinyong/vega/loginVerify"
checkUserURI = "/payclient.ashx?op=CheckUser"
getUserURI = "/payclient.ashx?op=GetUser"

-- 210.245.26.186
apiHost = "api.kimdungqq.com"

-- 123.31.25.80 KDT
-- apiHost = "api.kd.gaba.vn"
-- 104.28.17.37
-- 104.28.16.37
payHost = "m-pay.kimdungqq.com"
-- 123.31.25.71 KDT
-- payHost = "m-pay.gaba.vn"

checkUserRq u p = setRequestPath checkUserURI
                $ setRequestHost payHost
                $ setRequestBodyLBS (userRqBody u p)
                $ setRequestMethod "POST"
                $ defaultRequest

userRqBody :: ByteString -> ByteString -> ByteString
userRqBody u p   = C.append "partnerId=0&userName="
                 $ C.append u 
                 $ C.append "&deviceId=d80f998b16396187&serverMode=UNKNOWN&password="
                 $ C.append p "&refcode=0&gameId=46"

getSession :: ByteString -> ByteString
-- getSession = fst . BS.splitAt 36 . snd . BS.splitAt 49
-- KDT
getSession = fst . BS.splitAt 36 . snd . BS.splitAt 54

getId :: C.ByteString -> C.ByteString
getId = last . C.split '=' . head . tail . tail . C.split '&'

getLoginRqBody :: ByteString -> ByteString -> ByteString
getLoginRqBody u b = BS.append (BS.append u "&token=") (BS.append "&os=and&version=75896" b)

loginVerifyRq   = setRequestPath loginVerifyURI
                $ setRequestHost apiHost
                $ setRequestMethod "POST"
                $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
                $ defaultRequest

getName :: C.ByteString -> C.ByteString
getName = head . C.split '/'

getPassword :: C.ByteString -> C.ByteString
getPassword = last . C.split '/'

getUserData :: FromJSON a => C.ByteString -> Maybe a
getUserData s = Parser.decode $ C.append (C.drop 9 $ head $ C.split '}' s) "}"


-- loginVerify :: String -> String -> IO KDUser
loginVerify u p = do
    cResponse <- httpLBS $ checkUserRq (getName $ C.pack u) (getPassword $ C.pack p)
    let uidString = C.append "uid=" $ getId (getResponseBody cResponse)
        session = getSession (getResponseBody cResponse)
    response <- httpLBS (setRequestBodyLBS (getLoginRqBody uidString session) $ loginVerifyRq)
    return $ fromJust $ getUserData $ getResponseBody response
    -- return cResponse
