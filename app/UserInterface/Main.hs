{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module UserInterface.Main (runUserInterface) where

import Brick.AttrMap (attrMap, attrName)
import Brick.Forms
import Brick.Main
import Brick.Types
import Brick.Util (bg)
import Brick.Widgets.Border
import Brick.Widgets.Core
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import UserInterface.Events
import UserInterface.Types
import UserInterface.Widgets

drawUI :: AppForm -> [Widget Name]
drawUI f = [ui]
  where
    st = formState f
    chans = st ^. channels
    ui = hBox [channelCenter, msgCenter]
    msgCenter = border $ vBox [padBottom Max msgBox, hBorder, vLimit 1 $ renderForm f]
    channelCenter = border $ hLimitPercent 10 $ vBox [channelBox chans (st ^. currentChannel) (st ^. unreadChannels), hBorder, vLimit 1 joinButton]
    joinButton = viewport JoinButton Horizontal $ padLeft (Pad 1) $ str "+ Join/DM"
    currentChanName = chans !! (st ^. currentChannel)
    chatHistory = st ^. messages
    msgBox = messageBox (Map.findWithDefault (chatHistory Map.! "- Raw") currentChanName chatHistory)

app :: UiConfig -> App AppForm ByteString Name
app config =
  App
    { appDraw = drawUI,
      appStartEvent = do
        vty <- getVtyHandle
        liftIO $ V.setMode (V.outputIface vty) V.Mouse True,
      appHandleEvent = appEvent config,
      appAttrMap = const $ attrMap V.defAttr [(attrName "unread", bg V.magenta)],
      appChooseCursor = showFirstCursor
    }

-- | Create and run the UI
runUserInterface :: UiConfig -> RecvChan -> IO ()
runUserInterface config recvChan = do
  let initialState =
        AppState
          { _messages = Map.fromList (map (,[]) (initialChannels config)),
            _channels = initialChannels config,
            _input = "",
            _sentHistory = [],
            _sentHistoryCurrent = -1,
            _currentChannel = 0,
            _unreadChannels = Set.empty
          }
      form = makeForm initialState
  void $ customMainWithDefaultVty (Just recvChan) (app config) form
