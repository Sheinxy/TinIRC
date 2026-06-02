{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module UserInterface.PartDialog (runPartDialog) where

import Brick.AttrMap (attrMap)
import Brick.Main
import Brick.Types
import Brick.Util (bg, on)
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Dialog
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

data Choice = Yes | No
  deriving (Show)

data Name
  = YesButton
  | NoButton
  deriving (Show, Eq, Ord)

drawUI :: String -> Dialog Choice Name -> [Widget Name]
drawUI channel d = [ui]
  where
    ui = renderDialog d $ hCenter $ padAll 1 $ str ("Do you want to leave " ++ channel ++ "?")

appEvent :: BrickEvent Name e -> EventM Name (Dialog Choice Name) ()
appEvent (VtyEvent (V.EvKey V.KEsc _)) = modify (setDialogFocus NoButton) >> halt
appEvent (VtyEvent (V.EvKey V.KEnter _)) = halt
appEvent (VtyEvent ev) = handleDialogEvent ev
appEvent (MouseDown button _ _ _) = modify (setDialogFocus button) >> halt
appEvent _ = return ()

initialState :: Dialog Choice Name
initialState = dialog (Just $ str "") (Just (YesButton, choices)) 50
  where
    choices = [("Yes", YesButton, Yes), ("No", NoButton, No)]

app :: String -> App (Dialog Choice Name) e Name
app channel =
  App
    { appDraw = drawUI channel,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = do
        vty <- getVtyHandle
        liftIO $ V.setMode (V.outputIface vty) V.Mouse True,
      appAttrMap = const $ attrMap V.defAttr [(buttonAttr, V.black `on` V.white), (buttonSelectedAttr, bg V.red)]
    }

runPartDialog :: String -> IO String
runPartDialog channel = do
  d <- defaultMain (app channel) initialState
  case dialogSelection d of
    Nothing -> return "No"
    Just (_, a) -> return (show a)
