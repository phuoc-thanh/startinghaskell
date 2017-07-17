{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Parser ( encode
              , decode
              , KDServer(..)
              , KDUser(..)
              , FromJSON(..)
              , ToJSON(..)
              ) where

import Data.Aeson
import Data.Monoid
import Control.Applicative

-- type SId = String

data KDServer = KDServer { sid   :: String,
                           sname   :: String,
                           ip :: String,
                           port :: String,
                           isopen :: Integer,
                           update_time :: String
                         }
                   deriving (Show, Eq)
data KDUser = KDUser { acc   :: String,
                           uid   :: String,
                           opname :: String,
                           defaultsid :: String,
                           displayNovice :: Integer,
                           create_time :: Integer,
                           key :: String
                         }
                   deriving (Show, Eq)

instance FromJSON KDServer where
    parseJSON (Object v) = KDServer   <$>
                           v .: "sid"   <*>
                           v .: "sname"     <*>
                           v .: "ip" <*>
                           v .: "port" <*>
                           v .: "isopen" <*>
                           v .: "update_time"
    parseJSON _ = mempty

instance ToJSON KDServer where
    toJSON KDServer{..} = object [ "sid"   .= sid
                                    , "sname"     .= sname
                                    , "port" .= port
                                    , "ip" .= ip
                                    , "isopen"    .= isopen
                                    , "update_time" .= update_time
                                    ]

instance FromJSON KDUser where
    parseJSON (Object u) = KDUser   <$>
                           u .: "acc" <*>
                           u .: "uid"   <*>
                           u .: "opname"     <*>
                           u .: "defaultsid" <*>
                           u .: "displayNovice" <*>
                           u .: "create_time" <*>
                           u .: "key"
    parseJSON _ = mempty

instance ToJSON KDUser where
    toJSON KDUser{..} = object [ "acc" .= acc
                                    , "uid"   .= uid
                                    , "opname"     .= opname
                                    , "defaultsid" .= defaultsid
                                    , "displayNovice" .= displayNovice
                                    , "create_time"    .= create_time
                                    , "key" .= key
                                    ]