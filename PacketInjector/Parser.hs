{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Parser ( encode
              , decode
              , parseFile
              , appendJSON
              , Server(..)
              , Player(..)
              , FromJSON(..)
              , ToJSON(..)
              ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy       as BSL


parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    contents <- BS.readFile file
    return $ decodeStrict contents

appendJSON :: ToJSON a => FilePath -> a -> IO ()
appendJSON fp a = do BSL.writeFile fp $ encodePretty a    

data Server = Server { sid   :: String,
                       sname :: String,
                       ip    :: String,
                       port  :: Integer }
                   deriving (Show, Eq, Generic)
data Player = Player { acc           :: String,
                       uid           :: String,
                       opname        :: String,
                       defaultsid    :: String,
                       displayNovice :: Integer,
                       create_time   :: Integer,
                       key           :: String,
                       chNumber      :: String,
                       amount        :: Integer }
                   deriving (Show, Eq, Generic)

instance FromJSON Server
instance ToJSON Server

instance FromJSON Player
instance ToJSON Player