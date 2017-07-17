{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module PlayGame where

import Serialize
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base16.Lazy


bet100h :: C.ByteString
-- bet100h = fst $ decode "270004ff2300"
bet100h = fst $ decode "290003ff2500"

rankReward :: C.ByteString
rankReward = C.append (hexSerialize $ C.length s) s
             where s = "rankList reward 1\NUL"

bet1000h :: C.ByteString
bet1000h = fst $ decode "280003ff2400"

bet100 :: C.ByteString
bet100 = C.append bet100h "crossserverwar betting 109000079 100\NUL"

bet1000 :: C.ByteString
bet1000 = C.append bet1000h "crossserverwar betting 6000276 500\NUL"

gete0 :: C.ByteString
gete0 = fst $ decode "130003ff0f00"

getEdenTree0 :: C.ByteString
getEdenTree0 = C.append gete0 "edenTree get 0\NUL"

chap1b1h :: C.ByteString
chap1b1h = fst $ decode "180003ff1400"

chap1b1 :: C.ByteString
chap1b1 = C.append chap1b1h "copy enter C01B01 1\NUL"