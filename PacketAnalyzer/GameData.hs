{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module GameData where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy


-- 0x00
flagByte :: C.ByteString
flagByte = "00"
-- 0xff
indexfByte :: C.ByteString
indexfByte = "ff"


-- Packet 04 : 05 structure
-- packet info = 2 bytes (1 byte info + 1 flag byte) represents the length of data packet
-- index info  = 2 bytes (1 index byte + 1 byte: ff) represents the index of this packet in the streams
-- data info   = 2 bytes (1 byte info + 1 flag byte) represents the length of data
-- board data  = 6 bytes (5 bytes data + 1 flag byte) contains the string: "board"
-- rank data   = 27 bytes (5 bytes data + 1 flag byte) contains the string: "rankList getRankListData 0"

-- Board Packet Info == 0x0a : 0x00 the length of packet (2bytes + 2bytes + 6bytes)
boardPacketInfo :: C.ByteString
boardPacketInfo = C.append "0a" flagByte
-- Board Index Info == 0x04 : 0xff (constant)
boardIndex      ::  C.ByteString
boardIndex      = C.append "04" indexfByte
-- Board Data Info == "board Data length" + flagByte
boardDataInfo   :: C.ByteString
boardDataInfo   = C.append "06" flagByte
-- Board Data: 626f61726400
boardData       :: C.ByteString
boardData       = C.append (encode "board") flagByte


-- Rank Packet Info == 0x1f : 0x00 the length of packet (2bytes + 2bytes + 27bytes)
rankPacketInfo  :: C.ByteString
rankPacketInfo  = C.append "1f" flagByte
-- Rank Index Info == 0x05 : 0xff (constant)
rankIndex       ::  C.ByteString
rankIndex       = C.append "05" indexfByte
-- Rank Data Info == "rank Data length" + flagByte
rankDataInfo    :: C.ByteString
rankDataInfo    = C.append "1b" flagByte
-- Rank Data : 72616e6b4c6973742067657452616e6b4c69737444617461203000
rankData        :: C.ByteString
rankData        = C.append (encode "rankList getRankListData 0") flagByte


getBoardBytes :: C.ByteString
getBoardBytes = C.append boardPacketInfo
                $ C.append boardIndex
                $ C.append boardDataInfo
                $ boardData

getRankBytes  :: C.ByteString
getRankBytes  = C.append rankPacketInfo
                $ C.append rankIndex
                $ C.append rankDataInfo
                $ rankData
                
getGameData :: C.ByteString
getGameData = C.append getBoardBytes getRankBytes