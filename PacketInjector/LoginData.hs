{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module LoginData where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy
import Data.List.Split
import Serialize
import Parser hiding (encode, decode)

loginPrefix :: C.ByteString
loginPrefix = "LOGIN 0.0.1 10 "

enterPrefix :: C.ByteString
enterPrefix = "ENTER "

d123String :: C.ByteString
d123String = " 123 "
-- KDUser {acc = "232012", uid = "232012", opname = "10", defaultsid = "28", displayNovice = 1, create_time = 1500275785, key = "66ed8b606d2204985a30ba30b59a773b"}
getChNumber :: C.ByteString -> Integer
getChNumber = read . concat . ("0x":) . reverse . chunksOf 2 . C.unpack . C.take 8 . C.drop 14

getLoginData :: KDUser -> C.ByteString
getLoginData d = C.append (hexLoginSerialize $ C.length loginString) loginString
                 where loginString = C.append loginPrefix
                                   $ C.append (C.pack $ defaultsid d)
                                   $ C.append " "
                                   $ C.append (C.pack $ uid d)
                                   $ C.append d123String
                                   $ C.append (C.pack $ show $ create_time d)
                                   $ C.append " "
                                   $ C.append (C.pack $ key d) " 0\NUL"

enterWorld :: KDUser -> C.ByteString -> C.ByteString
enterWorld d m = C.append (hexEnterSerialize $ C.length enterString) enterString
                 where enterString = C.append enterPrefix
                                   $ C.append (C.pack $ uid d)
                                   $ C.append " "
                                   $ C.append (C.pack . show . getChNumber $ encode m) "\NUL"
                  

