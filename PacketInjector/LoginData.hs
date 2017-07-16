{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module LoginData where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy
import Data.List.Split
import Serialize

loginPrefix :: C.ByteString
loginPrefix = "LOGIN 0.0.1 10 55 "

enterPrefix :: C.ByteString
enterPrefix = "ENTER "

d123String :: C.ByteString
d123String = " 123 "

getChNumber :: C.ByteString -> Integer
getChNumber = read . concat . ("0x":) . reverse . chunksOf 2 . C.unpack . C.take 8 . C.drop 14

getUid :: C.ByteString -> C.ByteString
-- getUid = head . C.split '\"' . C.drop 22
-- kdt 10char
getUid = head . C.split '\"' . C.drop 27
-- kdt 6char
-- getUid = head . C.split '\"' . C.drop 23

getKey :: C.ByteString -> C.ByteString
-- getKey = head . C.split '\"' . C.drop 110
-- KDT --(sId>99, uid=10) -> 122 (sId<99, uid=10) -> 121, (sId<99, uid=6) -> 113
getKey = head . C.split '\"' . C.drop 121

getTime :: C.ByteString -> C.ByteString
-- getTime = head . C.split ',' . C.drop 92
-- KDT --(sId>99, uid=10) -> 104 (sId<99, uid=10) -> 103, (sId<99, uid=6) -> 95
getTime = head . C.split ',' . C.drop 103

getLoginData :: C.ByteString -> C.ByteString
getLoginData d = C.append (hexLoginSerialize $ C.length loginString) loginString
                 where loginString = C.append loginPrefix
                                   $ C.append (getUid d)
                                   $ C.append d123String
                                   $ C.append (getTime d)
                                   $ C.append " "
                                   $ C.append (getKey d) " 0\NUL"

enterWorld :: C.ByteString -> C.ByteString -> C.ByteString
enterWorld d m = C.append (hexEnterSerialize $ C.length enterString) enterString
                 where enterString = C.append enterPrefix
                                   $ C.append (getUid d)
                                   $ C.append " "
                                   $ C.append (C.pack . show . getChNumber $ encode m) "\NUL"
                  

