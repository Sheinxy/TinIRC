module IrcParsing.Types where

data Prefix = ServerName String | UserName {nickname :: String, user :: String, host :: String} deriving (Eq, Show)

data Message = Message {prefix :: Prefix, command :: String, params :: [String]} deriving (Eq, Show)
