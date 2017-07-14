{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module LoginData where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy
import Data.List.Split
import Numeric
import Data.Char (intToDigit)

-- 0x00
flagByte :: C.ByteString
flagByte = "00"
-- 0xff
indexfByte :: C.ByteString
indexfByte = "ff"

--length at byte05

-- numBytes revert byte8+9
-- 02 00 00 00 1d 00 01 82 00 00 00 02 00 00 00 32
-- 00 00 00 01 00 00 00 0a 00 52 65 70 6c 79 31 39
-- 38 38 00

-- 02 00 00 00 20 00 01 06 01 00 00 02 00 00 00 1b
-- 00 00 00 01 00 00 00 0d 00 43 e1 ba a9 75 23 21
-- 54 e1 ba b7 63 00

--serv2 revert byte8-9-10
-- 02 00 00 00 1b 00 01 89 86 1e 00 02 00 00 00 0a
-- 00 00 00 01 00 00 00 08 00 4b 6f 6d 70 61 6e 79
-- 00

-- 02 00 00 00 1b 00 01 90 0b 00 00 02 00 00 00 03
-- 00 00 00 01 00 00 00 08 00 54 68 6f 69 48 6f 61
-- 00

-- Packet 01-02 structure
-- packet info = 2 bytes (1 byte info + 1 flag byte) represents the length of data packet
-- index info  = 2 bytes (1 index byte + 1 byte: ff) represents the index of this packet in the streams
-- data info   = 2 bytes (1 byte info + 1 flag byte) represents the length of data
-- login data  = 73 bytes (72 bytes data + 1 flag byte) contains the string:
--      "LOGIN 0.0.1 10 1 11111 123 1499788083 de324200440393cacf54e564a253e230 0"
-- enter world = 16 bytes (15 bytes data + 1 flag byte) contains the string:
--      "ENTER 11111 130" -> reply1988
--      "ENTER 11107 2960" -> reply1988


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

getChNumber :: C.ByteString -> Integer
getChNumber = read . concat . ("0x":) . reverse . chunksOf 2 . C.unpack . C.take 8 . C.drop 14

getUid :: C.ByteString -> C.ByteString
getUid = head . C.split '\"' . C.drop 22

getKey :: C.ByteString -> C.ByteString
getKey = head . C.split '\"' . C.drop 110

getTime :: C.ByteString -> C.ByteString
getTime = head . C.split ',' . C.drop 92

getLoginData :: C.ByteString -> C.ByteString
getLoginData d = C.append serialLoginBytes
               $ C.append loginString
               $ C.append (getUid d)
               $ C.append d123String
               $ C.append (getTime d)
               $ C.append " "
               $ C.append (getKey d)
               $ C.append " 0" 
               $ fst $ decode flagByte

enterWorld :: C.ByteString -> C.ByteString -> C.ByteString
enterWorld d m = C.append serialEnterBytes
               $ C.append enterWString
               $ C.append (getUid d)
               $ C.append " "
               $ C.append (C.pack . show . getChNumber $ encode m)
               $ fst $ decode flagByte    

