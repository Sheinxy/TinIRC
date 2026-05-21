{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Yaml (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml as Y

data Server = Server
  { hostname :: String,
    port :: Int,
    useTls :: Bool,
    checkCertificates :: Bool,
    channels :: [String]
  }
  deriving (Eq, Show)

instance FromJSON Server where
  parseJSON (Y.Object v) =
    Server
      <$> v .: "hostname"
      <*> v .: "port"
      <*> v .:? "use_tls" .!= False
      <*> v .:? "check_certificates" .!= True
      <*> v .:? "channels" .!= []
  parseJSON _ = fail "Expected Object for Server value"

data User = User
  { username :: String,
    mode :: Int,
    realName :: String
  }
  deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Y.Object v) =
    User
      <$> v .: "username"
      <*> v .:? "mode" .!= 0
      <*> v .: "realname"
  parseJSON _ = fail "Expected Object for User value"

data Client = Client
  { sentHistoryLen :: Int,
    channelHistoryLen :: Int
  }
  deriving (Eq, Show)

instance FromJSON Client where
  parseJSON (Y.Object v) =
    Client
      <$> v .:? "sent_history_length" .!= 2048
      <*> v .:? "channel_history_length" .!= 2048
  parseJSON _ = fail "Expected Object for Client value"

data Config = Config
  { server :: Server,
    user :: User,
    client :: Client
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config
      <$> v .: "server"
      <*> v .: "user"
      <*> v .:? "client" .!= Client 2048 2048
  parseJSON _ = fail "Expected Object for Config"

parseConfig :: FilePath -> IO Config
parseConfig = Y.decodeFileThrow
