{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UserInterface.Types where

import Brick.BChan
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Lens.Micro.TH

-- | Channel type used by the client module to pass received
-- messages to the user interface module.
type RecvChan = BChan ByteString

-- | Channel type used by the user interface module to pass messages
-- to be sent to the client module.
type SendChan = BChan Text

-- | The UI configuration.
data UiConfig = UiConfig
  { -- | The sending channel used by the UI to pass messages to be sent
    -- to the client.
    sendChannel :: SendChan,
    -- | The maximum number of sent messages in the history.
    -- Set in the configuration file.
    sentHistoryLen :: Int,
    -- | The maximum number of messages in a channel's history
    -- Set in the configuration file.
    channelHistoryLen :: Int,
    -- | The channels to be initially joined
    initialChannels :: [String],
    -- | The displayed name for the user
    -- Set in the configuration file.
    displayName :: String
  }

-- | The current state of the App
data AppState = AppState
  { -- | The channel's history
    _messages :: Map String [ByteString],
    -- | The joined channels
    _channels :: [String],
    -- | The content on the input box
    _input :: Text,
    -- | The content of the last sent message
    _sentHistory :: [Text],
    -- | The currently selected message in the history (grows backward)
    _sentHistoryCurrent :: Int,
    -- | The currently selected channel
    _currentChannel :: Int,
    -- | List of currently unread channels
    _unreadChannels :: Set String
  }

makeLenses ''AppState

-- | Reset the input field of the AppState and update the history
resetAndMemorizeInput :: UiConfig -> AppState -> AppState
resetAndMemorizeInput cfg st = st {_sentHistory = take (sentHistoryLen cfg) (_input st : _sentHistory st), _input = "", _sentHistoryCurrent = -1}
