{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module Authenticator where

import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.List.Split
import Serializer
import Parser hiding (encode, decode)

loginPrefix :: ByteString
loginPrefix = "LOGIN 0.0.1 "

enterPrefix :: ByteString
enterPrefix = "ENTER "

d123String :: ByteString
d123String = " 123 "

getChNumber :: ByteString -> Integer
getChNumber = read . concat . ("0x":) . reverse . chunksOf 2 . C.unpack . C.take 8 . C.drop 14

loginData :: Player -> ByteString
loginData u = C.append (hexLoginSerialize $ C.length loginString) loginString
                 where loginString = C.append loginPrefix
                                   $ C.append (C.pack $ opname u)
                                   $ C.append " "
                                   $ C.append (C.pack $ defaultsid u)
                                   $ C.append " "
                                   $ C.append (C.pack $ uid u)
                                   $ C.append d123String
                                   $ C.append (C.pack $ show $ create_time u)
                                   $ C.append " "
                                   $ C.append (C.pack $ key u) " 0\NUL"

enterW :: ByteString -> ByteString -> ByteString
enterW idx cN = C.append (hexEnterSerialize $ C.length enterString) enterString
                    where enterString = C.append enterPrefix
                                      $ C.append idx
                                      $ C.append " "
                                      $ C.append cN "\NUL"
                     

