{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module Authenticator where

import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Serializer
import Parser hiding (encode, decode)

loginPrefix :: ByteString
loginPrefix = "LOGIN 0.0.1 "

enterPrefix :: ByteString
enterPrefix = "ENTER "

d123String :: ByteString
d123String = " 123 "

getChNumber :: ByteString -> Integer
getChNumber = hexDeserialize . C.take 8 . C.drop 14

-- newChNumber :: ByteString -> Integer
-- newChNumber = hexDeserialize . C.take 6 . C.drop 6

newChNumber :: ByteString -> Integer
newChNumber = hexDeserialize . C.take 8 . C.drop 6

loginData :: Player -> ByteString -> ByteString
loginData u sver = C.append (hexLoginSerialize $ C.length loginString) loginString
    where loginString = C.append loginPrefix
                      $ C.append (C.pack $ opname u) $ C.append " "
                      $ C.append sver $ C.append " "
                      $ C.append (C.pack $ uid u) $ C.append d123String
                      $ C.append (C.pack $ show $ create_time u) $ C.append " "
                      $ C.append (C.pack $ key u) " 0\NUL"

enterW :: ByteString -> ByteString -> ByteString
enterW idx cN = C.append (hexEnterSerialize $ C.length enterString) enterString
    where enterString = C.append enterPrefix
                      $ C.append idx $ C.append " "
                      $ C.append cN "\NUL"
                     

