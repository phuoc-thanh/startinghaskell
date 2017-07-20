{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module PacketGenerator where

import Serializer
import qualified Data.ByteString.Lazy.Char8 as C

rankReward :: C.ByteString
rankReward = C.append (hexSerialize $ C.length s) s
                where s = "rankList reward 1\NUL"

tPacket :: C.ByteString
tPacket = C.append (hexSerialize $ C.length s) s
             where s = "store 8 2\NUL"

bet100 :: C.ByteString -> C.ByteString
bet100 idx = C.append (hexSerialize $ C.length s) s
                where s = C.append "crossserverwar betting "
                        $ C.append idx " 100\NUL"