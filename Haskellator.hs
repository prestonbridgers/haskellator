module Main where

import Lang

import Brick
import Brick.AttrMap
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.Table
import Graphics.Vty.Input.Events

-- our own created .hs files for parsing and executing input from the user  
import Parse
----

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
    env        :: [(String, Double)]
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
drawUI s = [
           (hCenter $ str $ "Haskellator v0.1") <=> (padTop (Pad 1) $ hCenter $ str $ "when giving variables values to use later you must put everything after = in parenthesis.")
            <=> (padTop (Pad 1) $ hCenter $ str $ "example: x = (3+1), y = (4)") <=> (padTop (Pad 1) $ hCenter $ str $ "you can used saved values instead of rewriting the numbers they represent") 
            <=> (padTop (Pad 2) $ hCenter $ border $ vBox $ map str $ envCon $ env s)
           ,center $ border $ str $ evalString s 
           ]

envCon :: [(String, Double)] -> [String] 
envCon [] = []
envCon (x:xs) = (fst x ++ " = " ++ (show $ snd x)) : envCon xs

-- buildInitState: bui
buildInitState :: IO MyState
buildInitState = pure MyState { evalString = "", env = [] }

-- Handling vty events
handleEvent :: MyState -> BrickEvent n e -> EventM n (Next MyState)
handleEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
            EvKey (KChar 'q') [] -> halt s
            EvKey KBS [] -> continue s'
                where s' = if null $ evalString s then s
                           else MyState { evalString = init $ evalString s, env = env s }
            EvKey (KChar c) [] -> continue MyState {evalString = evalString s ++ [c], env = env s }
            EvKey KEnter [] -> continue s'
                where s' = case exec (evalString s) (env s) of
                           (Left val)     -> MyState {evalString = show val, env = env s}
                           (Right newEnv) -> MyState {evalString = "", env = newEnv}
            _ -> continue s
        _ -> continue s
