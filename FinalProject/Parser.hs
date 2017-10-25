{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Parser ( encode
              , decode
              , parseFile
              , appendJSON
              , getConfig
              , Server(..)
              , Player(..)
              , Match(..)
              , PreConfig (..)
              , FromJSON(..)
              , ToJSON(..)
              ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL


parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    contents <- BS.readFile file
    return $ decodeStrict contents

appendJSON :: ToJSON a => FilePath -> a -> IO ()
appendJSON fp a = do BSL.writeFile fp $ encodePretty a    

getConfig :: IO PreConfig
getConfig = do
    cf <- parseFile "Config.json" :: IO (Maybe PreConfig)
    return $ fromJust cf

data Server = Server { sid   :: String,
                       sname :: String,
                       ip    :: String,
                       port  :: String,
                       army  :: String }
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

data Match = Match { mid  :: String,
                     win  :: String,
                     lose :: String }
                   deriving (Show, Eq, Generic)

data PreConfig = PreConfig { apiHost      :: String,
                             payHost      :: String,
                             loginPath    :: String,
                             armyGroup    :: Int,
                             cloneCamp    :: String,
                             cloneKeyword :: String,
                             cloneServer  :: String,
                             defaultPass  :: String,
                             huntTarget   :: String,
                             huntDelay    :: Int }                   
                    deriving (Show, Generic)

instance FromJSON Server
instance ToJSON Server

instance FromJSON Player
instance ToJSON Player

instance FromJSON Match
instance ToJSON Match

instance FromJSON PreConfig
instance ToJSON PreConfig