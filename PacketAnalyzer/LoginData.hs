{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module LoginData where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy


-- 0x00
flagByte :: C.ByteString
flagByte = "00"
-- 0xff
indexfByte :: C.ByteString
indexfByte = "ff"

-- Packet 01-02 structure
-- packet info = 2 bytes (1 byte info + 1 flag byte) represents the length of data packet
-- index info  = 2 bytes (1 index byte + 1 byte: ff) represents the index of this packet in the streams
-- data info   = 2 bytes (1 byte info + 1 flag byte) represents the length of data
-- login data  = 73 bytes (72 bytes data + 1 flag byte) contains the string:
--      "LOGIN 0.0.1 10 1 11111 123 1499788083 de324200440393cacf54e564a253e230 0"
-- enter world = 16 bytes (15 bytes data + 1 flag byte) contains the string:
--      "ENTER 11111 130"

-- Login Packet Info == 0x4d : 0x00 the length of packet (2bytes + 2bytes + 73bytes)
loginPacketInfo :: C.ByteString
loginPacketInfo = C.append "4d" flagByte
-- Login Index Info == 0x04 : 0xff (constant)
loginIndex      ::  C.ByteString
loginIndex      = C.append "01" indexfByte
-- Login Data Info == "Login Data length" + flagByte
loginDataInfo   :: C.ByteString
loginDataInfo   = C.append "49" flagByte

-- Enter World Info == 0x14 : 0x00 the length of packet (2bytes + 2bytes + 16bytes)
joinWInfo :: C.ByteString
joinWInfo = C.append "14" flagByte
-- Login Index Info == 0x02 : 0xff (constant)
joinWIndex      ::  C.ByteString
joinWIndex      = C.append "02" indexfByte
-- Login Data Info == "Login Data length" + flagByte
joinWDataInfo   :: C.ByteString
joinWDataInfo   = C.append "10" flagByte

serialLoginBytes :: C.ByteString
serialLoginBytes = fst . decode . C.append loginPacketInfo $ C.append loginIndex loginDataInfo

serialEnterBytes :: C.ByteString
serialEnterBytes = fst . decode . C.append joinWInfo $ C.append joinWIndex joinWDataInfo

loginString :: C.ByteString
loginString = "LOGIN 0.0.1 10 1 "

enterWString :: C.ByteString
enterWString = "ENTER "

d123String :: C.ByteString
d123String = " 123 "

d130String :: C.ByteString
d130String = " 130"

getUid :: C.ByteString -> C.ByteString
getUid = head . C.split '\"' . C.drop 22

getKey :: C.ByteString -> C.ByteString
getKey = head . C.split '\"' . C.drop 110

getTime :: C.ByteString -> C.ByteString
getTime = head . C.split ',' . C.drop 92

getLoginData :: C.ByteString -> C.ByteString
getLoginData d = C.append (serialLoginBytes)
               $ C.append loginString
               $ C.append (getUid d)
               $ C.append d123String
               $ C.append (getTime d)
               $ C.append " "
               $ C.append (getKey d)
               $ C.append " 0" 
               $ fst $ decode flagByte

enterWorld :: C.ByteString -> C.ByteString
enterWorld d = C.append (serialEnterBytes)   
             $ C.append enterWString
             $ C.append (getUid d)
             $ C.append d130String
             $ fst $ decode flagByte    

