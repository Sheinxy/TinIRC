{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UserInterface.JoinDialog (runJoinDialog) where

import Brick.AttrMap (attrMap)
import Brick.Forms
import Brick.Main
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Data.Text (Text, unpack)
import qualified Graphics.Vty as V
import Lens.Micro (set)
import Lens.Micro.TH

data Name = InputBox deriving (Ord, Show, Eq)

newtype Input = Input {_input :: Text}

makeLenses ''Input

type JoinDialog = Form Input () Name

draw :: JoinDialog -> [Widget Name]
draw f = [box prompt]
  where
    prompt = center . hLimitPercent 40 . border $ renderForm f
    box = center . vLimitPercent 50 . hLimitPercent 50 . borderWithLabel (str "Enter which channel/DM you want to join:")

appEvent :: BrickEvent Name () -> EventM Name JoinDialog ()
appEvent (VtyEvent (V.EvKey V.KEnter [])) = halt
appEvent (VtyEvent (V.EvKey V.KEsc [])) = gets formState >>= modify . updateFormState . set input "" >> halt
appEvent ev = handleFormEvent ev

app :: App JoinDialog () Name
app =
  App
    { appDraw = draw,
      appStartEvent = return (),
      appHandleEvent = appEvent,
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = showFirstCursor
    }

initialState :: JoinDialog
initialState = newForm [editTextField input InputBox (Just 1)] $ Input ""

runJoinDialog :: IO String
runJoinDialog =
  unpack
    . _input
    . formState
    <$> defaultMain app initialState
