{-# LANGUAGE OverloadedStrings #-}

module UserInterface.Events (appEvent) where

import Brick.BChan
import Brick.Forms
import Brick.Main
import Brick.Types
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Search (split)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (append, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Graphics.Vty as V
import Lens.Micro (over, set)
import UserInterface.JoinDialog
import UserInterface.MessageHandling
import UserInterface.Types
import UserInterface.Widgets
  ( AppForm,
    Name (..),
    getFormField,
    modifyForm,
  )

sendMessage :: UiConfig -> EventM Name AppForm ()
sendMessage config = do
  rawMsg <- getFormField input
  currentChan <- getFormField currentChannel
  chans <- getFormField channels
  let channelName = chans !! currentChan
  -- If the selected channel is "Raw" then send the raw message. Otherwise, send this as a private message for the current channel
  let msg = if currentChan == 0 then rawMsg else pack ("PRIVMSG " ++ channelName ++ " :") `append` rawMsg
  liftIO $ writeBChan (sendChannel config) msg
  modifyForm (resetAndMemorizeInput config)
  addNewMessages config [encodeUtf8 msg]
  vScrollToEnd (viewportScroll MessageBox)

receiveMessage :: UiConfig -> ByteString -> EventM Name AppForm ()
receiveMessage config event = do
  let newMessages = filter (/= "") $ split "\r\n" event
  addNewMessages config newMessages

rewindHistory :: EventM Name AppForm ()
rewindHistory = do
  history <- getFormField sentHistory
  current <- getFormField sentHistoryCurrent
  let hLen = length history
      current' = current + 1
  unless (current' >= hLen) $ do
    let msg = history !! current'
    modifyForm (set input msg)
    modifyForm (set sentHistoryCurrent current')

forwardHistory :: EventM Name AppForm ()
forwardHistory = do
  history <- getFormField sentHistory
  current <- getFormField sentHistoryCurrent
  let current' = current - 1
  unless (current' < -1) $ do
    let msg = if current' == -1 then "" else history !! current'
    modifyForm (set input msg)
    modifyForm (set sentHistoryCurrent current')

markChannelAsRead :: EventM Name AppForm ()
markChannelAsRead = do
  curChannelName <- (!!) <$> getFormField channels <*> getFormField currentChannel
  modifyForm (over unreadChannels $ Set.delete curChannelName)

changeSelectedChannel :: Location -> EventM Name AppForm ()
changeSelectedChannel (Location (_, y)) = do
  maxIdx <- length <$> getFormField channels
  unless (y >= maxIdx) $ do
    modifyForm (set currentChannel y)
    vScrollToEnd (viewportScroll MessageBox)
    markChannelAsRead

incrementSelectedChannel :: EventM Name AppForm ()
incrementSelectedChannel = do
  maxIdx <- length <$> getFormField channels
  current <- getFormField currentChannel
  modifyForm (set currentChannel ((current + 1) `mod` maxIdx))
  vScrollToEnd (viewportScroll MessageBox)
  markChannelAsRead

decrementSelectedChannel :: EventM Name AppForm ()
decrementSelectedChannel = do
  maxIdx <- length <$> getFormField channels
  current <- getFormField currentChannel
  modifyForm (set currentChannel ((current - 1) `mod` maxIdx))
  vScrollToEnd (viewportScroll MessageBox)
  markChannelAsRead

joinChannel :: UiConfig -> EventM Name AppForm ()
joinChannel config = do
  newChannel <- suspendAndResume' runJoinDialog
  -- Reset Mouse mode
  vty <- getVtyHandle
  liftIO $ V.setMode (V.outputIface vty) V.Mouse True
  -- Do nothing if input was empty
  unless (null newChannel) $ do
    -- Add channel to list if it doesn't exist
    chans <- getFormField messages
    unless (newChannel `Map.member` chans) $ do
      modifyForm (over channels (++ [newChannel]))
      modifyForm (over messages $ Map.insert newChannel [])
      -- Join channel if it is not a DM
      when (head newChannel `elem` ("& #+!" :: String)) $ do
        let msg = pack $ "JOIN " ++ newChannel ++ "\r\n"
        liftIO $ writeBChan (sendChannel config) msg
        addNewMessages config [encodeUtf8 msg]
      -- Change current channel and scroll
      maxIdx <- length <$> getFormField channels
      modifyForm (set currentChannel (maxIdx - 1))
      vScrollToEnd (viewportScroll MessageBox)
      vScrollToEnd (viewportScroll ChannelBox)

exitApp :: UiConfig -> EventM Name AppForm ()
exitApp config = do
  -- Reset Mouse mode
  vty <- getVtyHandle
  liftIO $ V.setMode (V.outputIface vty) V.Mouse False
  liftIO (writeBChan (sendChannel config) "QUIT")
  halt

-- | Handle an event on the UI
-- The events can be:
-- * Scrolling up or down on a viewport -> Scroll the viewport in question
-- * Clicking on a channel -> Select that channel
-- * Clicking on the "Join/DM" -> Open the join channel prompt
-- * Up or Down arrows are pressed -> Scroll the chat history
-- * Up or Down arrows are pressed with the control key -> Change selected channel
-- * Up arrow is pressed WITH the shift key-> Set the input prompt to the last sent message
-- * Down arrow is pressed WITH the shift key -> Clear the input promp
-- * Enter is pressed -> Send a message (pass it to the client module)
-- * A message is present in the received channel -> Display it in the chat history
-- * Another key was pressed -> Write the input to the prompt
appEvent :: UiConfig -> BrickEvent Name ByteString -> EventM Name AppForm ()
appEvent _ (MouseDown box V.BScrollUp _ _) = vScrollBy (viewportScroll box) (-1)
appEvent _ (MouseDown box V.BScrollDown _ _) = vScrollBy (viewportScroll box) 1
appEvent _ (MouseDown ChannelBox _ _ l) = changeSelectedChannel l
appEvent config (MouseDown JoinButton _ _ _) = joinChannel config
appEvent _ (VtyEvent (V.EvKey V.KDown [])) = vScrollBy (viewportScroll MessageBox) 1
appEvent _ (VtyEvent (V.EvKey V.KUp [])) = vScrollBy (viewportScroll MessageBox) (-1)
appEvent _ (VtyEvent (V.EvKey V.KDown [V.MCtrl, V.MShift])) = vScrollBy (viewportScroll ChannelBox) 1
appEvent _ (VtyEvent (V.EvKey V.KUp [V.MCtrl, V.MShift])) = vScrollBy (viewportScroll ChannelBox) (-1)
appEvent _ (VtyEvent (V.EvKey V.KDown [V.MShift])) = forwardHistory
appEvent _ (VtyEvent (V.EvKey V.KUp [V.MShift])) = rewindHistory
appEvent _ (VtyEvent (V.EvKey V.KDown [V.MCtrl])) = incrementSelectedChannel
appEvent _ (VtyEvent (V.EvKey V.KUp [V.MCtrl])) = decrementSelectedChannel
appEvent config (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = exitApp config
appEvent config (VtyEvent (V.EvKey V.KEnter [])) = sendMessage config
appEvent config (AppEvent event) = receiveMessage config event
appEvent _ ev = handleFormEvent ev -- Basically making it so the keyboard keyboards
