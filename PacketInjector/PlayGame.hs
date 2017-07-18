{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables , OverloadedStrings #-}

module PlayGame where

import Serialize
import qualified Data.ByteString.Lazy.Char8 as C

rankReward :: C.ByteString
rankReward = C.append (hexSerialize $ C.length s) s
                where s = "rankList reward 8\NUL"

store82 :: C.ByteString
store82 = C.append (hexSerialize $ C.length s) s
             where s = "crossserverwar getgamereward 51\NUL"

bet100 :: C.ByteString
bet100 = C.append (hexSerialize $ C.length s) s
                where s = "crossserverwar betting 109000079 100\NUL"