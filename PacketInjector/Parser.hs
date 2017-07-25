{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Parser ( encode
              , decode
              , parseFile
              , appendJSON
              , KDServer(..)
              , KDUser(..)
              , FromJSON(..)
              , ToJSON(..)
              ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy       as BS


parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    contents <- BS.readFile file
    return $ decode contents

appendJSON :: ToJSON a => FilePath -> a -> IO ()
appendJSON fp a = do BS.appendFile fp $ encodePretty a    

data KDServer = KDServer { sid   :: String,
                           sname :: String,
                           ip    :: String,
                           port  :: Integer }
                   deriving (Show, Eq, Generic)
data KDUser = KDUser { acc           :: String,
                       uid           :: String,
                       opname        :: String,
                       defaultsid    :: String,
                       displayNovice :: Integer,
                       create_time   :: Integer,
                       key           :: String,
                       chNumber      :: String,
                       amount        :: Integer }
                   deriving (Show, Eq, Generic)

instance FromJSON KDServer
instance ToJSON KDServer

instance FromJSON KDUser
instance ToJSON KDUser