module UserInterface.Widgets where

import Brick.Forms
import Brick.Types
import Brick.Widgets.Core
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Lens.Micro (Getting, (^.))
import UserInterface.Types (AppState, input)

-- | The widgets present in the UI:
-- * MessageBox is the widget where the chat is displayed
-- * PromptBox is the widget where the user types messages to be sent
-- * ChannelBox is the widget where the user can select the current channel
data Name = MessageBox | PromptBox | ChannelBox | JoinButton deriving (Ord, Show, Eq)

-- | The Form representing the UI
type AppForm = Form AppState ByteString Name

-- | Construct the MessageBox widget
messageBox ::
  -- | The messages to be displayed
  [ByteString] ->
  Widget Name
messageBox msgs = withVScrollBars OnRight . viewport MessageBox Vertical $ vBox $ map (txtWrap . decodeUtf8) . reverse $ msgs

-- | Create the form representing the UI.
makeForm :: AppState -> AppForm
makeForm = newForm [editTextField input PromptBox (Just 1)]

-- | Construct the channelBox widget
channelBox :: [String] -> Int -> Widget Name
channelBox channels selected = withVScrollBars OnLeft . viewport ChannelBox Vertical $ vBox $ zipWith makeChannel [0 ..] channels
  where
    makeChannel idx = padLeft (Pad 1) . str . ((if idx == selected then "* " else "  ") ++)

-- | Modify the form's inner state
modifyForm :: (AppState -> AppState) -> EventM Name AppForm ()
modifyForm f = do
  st <- gets formState
  modify . updateFormState $ f st

-- | Get a field from the form's inner state
getFormField :: Getting a AppState a -> EventM Name AppForm a
getFormField field = do
  st <- gets formState
  return (st ^. field)
