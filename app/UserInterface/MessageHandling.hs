{-# LANGUAGE OverloadedStrings #-}

module UserInterface.MessageHandling (addNewMessages) where

import Brick.Main
import Brick.Types
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import IrcParsing.Parsing
import IrcParsing.Types
import Lens.Micro (over)
import UserInterface.Types
import UserInterface.Widgets

messageSender :: UiConfig -> Message -> String
messageSender config msg = case prefix msg of
  ServerName sn -> sn
  UserName un _ _ -> if null un then displayName config else un

getChannel :: Message -> String
getChannel msg
  | null (params msg) = ""
  | otherwise = head (params msg)

addContentToChannel :: a -> (a -> [ByteString] -> [ByteString]) -> [ByteString] -> [ByteString]
addContentToChannel content adder = adder content

addParsedMessage :: UiConfig -> Message -> Map.Map String [ByteString] -> Map.Map String [ByteString]
addParsedMessage config msg
  | isPrivateMessage msg = Map.alter (addToOrCreateChannel content) channel
  | otherwise = id
  where
    channel
      | getChannel msg == displayName config = messageSender config msg
      | otherwise = getChannel msg
    content
      | null (params msg) = ""
      | otherwise = fromString $ (messageSender config msg ++ ": ") ++ last (params msg)
    addToOrCreateChannel c = return . maybe [c] (addContentToChannel c (:))

isPrivateMessage :: Message -> Bool
isPrivateMessage = (`elem` ["NOTICE", "PRIVMSG"]) . command

getNewDmSender :: UiConfig -> [String] -> [Message] -> [String]
getNewDmSender config chans =
  nub
    . filter (`notElem` chans)
    . map (messageSender config)
    . filter ((== displayName config) . getChannel)
    . filter isPrivateMessage

-- | Add the messages to the right channels in the AppState
addNewMessages :: UiConfig -> [ByteString] -> EventM Name AppForm ()
addNewMessages config msgs = do
  let parsed = map parseMessage msgs
  modifyForm (over messages addRaw)

  -- Add new dms to the channels list
  chans <- getFormField channels
  let dmSenders = getNewDmSender config chans parsed
  modifyForm (over channels (++ dmSenders))

  -- Add the messages in the right channels
  modifyForm (over messages $ flip (foldl addParsed) parsed)

  -- Scroll only for messages in the current channel
  let changedChannels = map getChannel $ filter isPrivateMessage parsed
  curChannelName <- (!!) <$> getFormField channels <*> getFormField currentChannel
  when (curChannelName `elem` changedChannels) $ do
    vScrollBy (viewportScroll MessageBox) (length . filter (== curChannelName) $ changedChannels)
  when (curChannelName == "- Raw") $ do
    vScrollBy (viewportScroll MessageBox) (length msgs)
  -- Mark channels with new messages as unread if they're not selected
  let unreadChans = filter (/= curChannelName) $ "- Raw" : changedChannels
  modifyForm (over unreadChannels $ flip (foldr Set.insert) unreadChans)
  where
    addRaw = Map.adjust (addContentToChannel msgs (++)) "- Raw"
    addParsed = flip $ addParsedMessage config
