module Main where

import Brick
import Brick.AttrMap
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.Table
import Graphics.Vty.Input.Events

import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as DNE
import Graphics.Vty
import System.Exit
import Data.Char

-- Main
main :: IO ()
main = do
    initState <- buildInitState
    endState <- defaultMain app initState
    print endState

-- My own state for the program
data MyState = MyState {
    evalString :: String
    }
    deriving (Show, Eq)

-- Resource name is simply typedef of String
type ResourceName = String

-- Creating the app
app :: App MyState e ResourceName
app = App {
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = pure,
    appAttrMap = const $ attrMap mempty [(attrName "selected", fg red)]
}

-- Takes a state and returns a list of widgets of
-- the appropriate resource name
drawUI :: MyState -> [Widget ResourceName]
drawUI s = [title, evalText]
        where
              title = hCenter $ str $ "Haskellator v" ++ show 0.1
              evalText = center $ hCenter $ border $ str $ evalString s

-- buildInitState: bui
buildInitState :: IO MyState
buildInitState = pure MyState { evalString = "" }

-- Handling vty events
handleEvent :: MyState -> BrickEvent n e -> EventM n (Next MyState)
handleEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey KBS [] -> continue s'
                    where s' = if null $ evalString s then s
                               else MyState { evalString = init $ evalString s }
                EvKey (KChar c) [] -> continue MyState {evalString = evalString s ++ [c] }
                _ -> continue s
        _ -> continue s
