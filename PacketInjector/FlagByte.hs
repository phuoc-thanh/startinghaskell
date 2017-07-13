{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}


module FlagByte where

import qualified Data.ByteString.Lazy.Char8 as C


class FlagByte a where
    -- 0x00
    flagByte :: a -> C.ByteString
    -- 0xff
    indexfByte :: a -> C.ByteString

instance FlagByte C.ByteString where
    flagByte s = C.append s "00"
    indexfByte s = C.append s"ff"
    
