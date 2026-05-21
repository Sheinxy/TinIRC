{-# LANGUAGE OverloadedStrings #-}

module Client (Client (..), runClient) where

import Brick.BChan
import Config (Config)
import qualified Config
import Control.Concurrent.Async
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.List
import Data.Text (Text, append, pack)
import Data.Text.Encoding (encodeUtf8)
import UserInterface.Types (RecvChan, SendChan)

-- | The network client
data Client = Client
  { -- | Send the given text to the server
    send :: Text -> IO (),
    -- | Receive data from the server
    receive :: IO ByteString,
    -- | Configuration from the configuration file
    config :: Config,
    -- | The channel used to pass received data to the UI
    recvChan :: RecvChan,
    -- | The channel used to retrieve messages to send from the UI
    sendChan :: SendChan
  }

sendLoop :: Client -> IO ()
sendLoop client = do
  msg <- readBChan (sendChan client)
  send client (append msg "\r\n")
  sendLoop client

receiveLoop :: Client -> IO ()
receiveLoop client = do
  response <- receive client
  let clientConfig = config client
      username = Config.username $ Config.user clientConfig
      hostname = Config.hostname $ Config.server clientConfig
  writeBChan (recvChan client) response
  when (response == C8.pack ("PING " ++ username ++ "\r\n")) $ do
    let pong = pack ("PONG " ++ hostname ++ "\r\n")
    send client pong
    writeBChan (recvChan client) (encodeUtf8 pong)
  receiveLoop client

-- | Run the client:
-- Send the connection information to the IRC server and enter the sending and receiving loops
runClient :: Client -> IO ()
runClient client = do
  send client $ pack $ "USER " ++ username ++ " " ++ show usermode ++ " * :" ++ realname ++ "\r\n"
  send client $ pack $ "NICK " ++ username ++ "\r\n"
  send client $ pack $ "JOIN " ++ intercalate "," channels ++ "\r\n"
  concurrently_ (sendLoop client) (receiveLoop client)
  where
    cfg = config client
    user = Config.user cfg
    username = Config.username user
    usermode = Config.mode user
    realname = Config.realName user
    server = Config.server cfg
    channels = Config.channels server
