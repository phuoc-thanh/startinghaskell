{-# LANGUAGE OverloadedStrings #-}



module HttpRq where

import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as C

import Parser
import Network.HTTP.Simple


checkUserURI = "/payclient.ashx?op=CheckUser"
getUserURI = "/payclient.ashx?op=GetUser"
regUserURI = "/payclient.ashx?op=RegisterUser"

checkUserRq :: ByteString -> ByteString -> S.ByteString -> Request
checkUserRq u p h = setRequestPath checkUserURI
                  $ setRequestHost h
                  $ setRequestBodyLBS (userRqBody u p)
                  $ setRequestMethod "POST"
                  $ defaultRequest

newUserRq :: ByteString -> ByteString -> S.ByteString -> Request
newUserRq u p h = setRequestPath regUserURI
                $ setRequestHost h
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

-- loginVerifyRq :: ByteString -> ByteString -> Request       
loginVerifyRq s host path = setRequestHost host
                          $ setRequestPath path
                          $ setRequestBodyLBS (loginRqBody s)
                          $ setRequestMethod "POST"
                          $ setRequestHeader "Content-Type" ["application/x-www-form-urlencoded"]
                          $ defaultRequest

getUserData :: FromJSON a => ByteString -> Maybe a
getUserData s = decode $ C.append (C.drop 9 $ head $ C.split '}' s) ",\"chNumber\" : \"unknow\",\"amount\" : 0}"

loginVerify :: String -> String -> IO Player
loginVerify u p = do
    cf <- getConfig
    cResponse <- httpLBS $ checkUserRq (C.pack u) (C.pack p) (S.pack $ payHost cf)
    response <- httpLBS $ loginVerifyRq (getResponseBody cResponse)
                                        (S.pack $ apiHost cf) 
                                        (S.pack $ loginPath cf)
    return $ fromJust $ getUserData $ getResponseBody response

regAccount :: String -> String -> IO Player
regAccount u p = do
    cf <- getConfig
    nResponse <- httpLBS $ newUserRq (C.pack u) (C.pack p) (S.pack $ payHost cf)
    cResponse <- httpLBS $ checkUserRq (C.pack u) (C.pack p) (S.pack $ payHost cf)
    response <- httpLBS $ loginVerifyRq (getResponseBody cResponse)
                                        (S.pack $ apiHost cf) 
                                        (S.pack $ loginPath cf)
    return $ fromJust $ getUserData $ getResponseBody response