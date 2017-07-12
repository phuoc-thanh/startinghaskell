{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module LoginData where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import System.Locale



-- 0x00
flagByte :: C.ByteString
flagByte = "00"
-- 0xff
indexfByte :: C.ByteString
indexfByte = "ff"

-- Packet 01 structure
-- packet info = 2 bytes (1 byte info + 1 flag byte) represents the length of data packet
-- index info  = 2 bytes (1 index byte + 1 byte: ff) represents the index of this packet in the streams
-- data info   = 2 bytes (1 byte info + 1 flag byte) represents the length of data
-- login data   = 73 bytes (72 bytes data + 1 flag byte) contains the string:
--      "LOGIN 0.0.1 10 1 11111 123 1499788083 de324200440393cacf54e564a253e230 0"

-- Login Packet Info == 0x4d : 0x00 the length of packet (2bytes + 2bytes + 73bytes)
loginPacketInfo :: C.ByteString
loginPacketInfo = C.append "4d" flagByte
-- Login Index Info == 0x04 : 0xff (constant)
loginIndex      ::  C.ByteString
loginIndex      = C.append "01" indexfByte
-- Login Data Info == "Login Data length" + flagByte
loginDataInfo   :: C.ByteString
loginDataInfo   = C.append "49" flagByte

loginData       :: C.ByteString
loginData = C.append (encode "LOGIN 0.0.1 10 1 11111 123 1499788083 de324200440393cacf54e564a253e230 0") flagByte

--current time
-- cTime :: IO Integer
cTime = getSystemTime  