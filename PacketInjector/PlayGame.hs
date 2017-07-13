{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module PlayGame where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy


-- 0x00
flagByte :: C.ByteString
flagByte = "00"
-- 0xff
indexfByte :: C.ByteString
indexfByte = "ff"

--hex stream : 180006ff1400636f707920656e74657220433031423031203100
--hex stream : 0f0007ff0b00636f707920626c6f636b00

-- Packet 01-02 structure
-- packet info = 2 bytes (1 byte info + 1 flag byte) represents the length of data packet
-- index info  = 2 bytes (1 index byte + 1 byte: ff) represents the index of this packet in the streams
-- data info   = 2 bytes (1 byte info + 1 flag byte) represents the length of data
-- login data  = 73 bytes (72 bytes data + 1 flag byte) contains the string:
--      "LOGIN 0.0.1 10 1 11111 123 1499788083 de324200440393cacf54e564a253e230 0"
-- enter world = 16 bytes (15 bytes data + 1 flag byte) contains the string:
--      "ENTER 11111 130" -> reply1988
--      "ENTER 11107 2960" -> reply1988


-- Chap1b1 Packet Info == 0x18 : 0x00 the length of packet (2bytes + 2bytes + 73bytes)
chap1b1Info :: C.ByteString
chap1b1Info = C.append "18" flagByte
-- Chap1b1 Index Info == 0x06 : 0xff (first packet after enter world)
chap1b1Index      ::  C.ByteString
chap1b1Index      = C.append "06" indexfByte
-- Chap1b1 Data Info == "Chap1-1 Data length" + flagByte
chap1b1DataInfo   :: C.ByteString
chap1b1DataInfo   = C.append "14" flagByte

serialChap1b1Bytes :: C.ByteString
serialChap1b1Bytes = fst . decode . C.append chap1b1Info $ C.append chap1b1Index chap1b1DataInfo

-- copyBlockInfo Packet Info == 0x18 : 0x00 the length of packet (2bytes + 2bytes + 73bytes)
copyBlockInfo :: C.ByteString
copyBlockInfo = C.append "0f" flagByte
-- copyBlockInfo Index Info == 0x06 : 0xff (first packet after enter world)
copyBlockIndex      ::  C.ByteString
copyBlockIndex      = C.append "07" indexfByte
-- copyBlockInfo Data Info == "Chap1-1 Data length" + flagByte
copyBlockDataInfo   :: C.ByteString
copyBlockDataInfo   = C.append "0b" flagByte

serialCopyBlockBytes :: C.ByteString
serialCopyBlockBytes = fst . decode . C.append copyBlockInfo $ C.append copyBlockIndex copyBlockDataInfo

chap1b1String :: C.ByteString
chap1b1String = "copy enter C01B01 1"

copyBlockString :: C.ByteString
copyBlockString = "copy block"

enterChap01B01 :: C.ByteString
enterChap01B01 = C.append serialChap1b1Bytes  
                $ C.append chap1b1String
                $ fst $ decode flagByte

copyBlock :: C.ByteString
copyBlock = C.append serialCopyBlockBytes
            $ C.append copyBlockString
            $ fst $ decode flagByte