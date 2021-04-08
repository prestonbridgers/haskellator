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
    evalString :: String,
    cursor :: NonEmptyCursor String
    }
    deriving (Show, Eq)

-- Resource name is simply typedef of String
type ResourceName = String

-- Creating the app
app :: App MyState e ResourceName
app = App {
    appDraw = drawUI,
    appChooseCursor = showFirstCursor,
    appHandleEvent = handleEvent,
    appStartEvent = pure,
    appAttrMap = const $ attrMap mempty [(attrName "selected", fg red)]
}

-- Takes a state and returns a list of widgets of
-- the appropriate resource name
drawUI :: MyState -> [Widget ResourceName]
drawUI s = [title, evalText, renderedNumpad]
        where
              title = hCenter $ str "Haskellator v0.1"
              evalText = padTop (Pad 2) $hCenter $ border $ str $ evalString s
              renderedNumpad = let nec = cursor s
                               in center $ hBox $ concat [
                                   map (padTop (Pad 1) . str) (reverse $ nonEmptyCursorPrev nec),
                                   [border $ str $ nonEmptyCursorCurrent nec],
                                   map (padTop (Pad 1) . str) $ nonEmptyCursorNext nec
                                  ]

-- buildInitState: bui
buildInitState :: IO MyState
buildInitState = do
    let numpad = ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]
    pure MyState { evalString = "", cursor = case DNE.nonEmpty numpad of
                                                      Just np -> makeNonEmptyCursor np }

-- Handling vty events
handleEvent :: MyState -> BrickEvent n e -> EventM n (Next MyState)
handleEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey KRight [] -> continue s'
                    where s' = case nonEmptyCursorSelectNext (cursor s) of
                                Nothing -> s
                                Just newCursor -> MyState { evalString = evalString s, cursor = newCursor }
                EvKey KLeft [] -> continue s'
                    where s' = case nonEmptyCursorSelectPrev (cursor s) of
                                Nothing -> s
                                Just newCursor -> MyState { evalString = evalString s, cursor = newCursor }
                EvKey KEnter [] -> continue s'
                    where s' = MyState { evalString = evalString s ++ [head $ nonEmptyCursorCurrent $ cursor s], cursor = cursor s }
                EvKey KBS [] -> continue s'
                    where s' = if null $ evalString s then s
                               else MyState { evalString = init $ evalString s, cursor = cursor s }
                EvKey (KChar '+') [] -> continue MyState {evalString = evalString s ++ ['+'], cursor = cursor s }
                EvKey (KChar '-') [] -> continue MyState {evalString = evalString s ++ ['-'], cursor = cursor s }
                EvKey (KChar '*') [] -> continue MyState {evalString = evalString s ++ ['*'], cursor = cursor s }
                EvKey (KChar '/') [] -> continue MyState {evalString = evalString s ++ ['/'], cursor = cursor s }
                EvKey (KChar c) [] -> if isDigit c then continue MyState {evalString = evalString s ++ [c], cursor = cursor s } else continue s
                _ -> continue s
        _ -> continue s
