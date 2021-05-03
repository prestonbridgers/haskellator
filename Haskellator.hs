{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

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

--  ____        _          _____                      
-- |  _ \  __ _| |_ __ _  |_   _|   _ _ __   ___  ___ 
-- | | | |/ _` | __/ _` |   | || | | | '_ \ / _ \/ __|
-- | |_| | (_| | || (_| |   | || |_| | |_) |  __/\__ \
-- |____/ \__,_|\__\__,_|   |_| \__, | .__/ \___||___/
--                              |___/|_|              
                      
data MyState = MyState {
    _evalString :: String,
    _env        :: [(String, Double)]
    } deriving (Show, Eq)
makeLenses ''MyState

type ResourceName = String

app :: App MyState e ResourceName
app = App {
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = pure,
    appAttrMap = const $ attrMap mempty [(attrName "selected", fg red)]
    }

--  __  __       _       
-- |  \/  | __ _(_)_ __  
-- | |\/| |/ _` | | '_ \ 
-- | |  | | (_| | | | | |
-- |_|  |_|\__,_|_|_| |_|
main :: IO ()
main = do
    initState <- pure $ MyState "" []
    endState <- defaultMain app initState
    print endState

--  _                _      
-- | |    ___   __ _(_) ___ 
-- | |   / _ \ / _` | |/ __|
-- | |__| (_) | (_| | | (__ 
-- |_____\___/ \__, |_|\___|
--             |___/        

-- Takes a state and returns a list of widgets of
-- the appropriate resource name
drawUI :: MyState -> [Widget ResourceName]
drawUI s = [
           (hCenter $ str $ "Haskellator v0.1") <=> (padTop (Pad 1) $ hCenter $ str $ "when giving variables values to use later you must put everything after = in parenthesis.")
            <=> (padTop (Pad 1) $ hCenter $ str $ "example: x = (3+1), y = (4) NOT: x = 3+1 , y = 4") <=> (padTop (Pad 1) $ hCenter $ str $ "you can used saved values instead of rewriting the numbers they represent") 
            <=> (padTop (Pad 2) $ hCenter $ border $ vBox $ map str $ envCon $ s^.env)
           ,center $ border $ str $ s^.evalString
           ]

envCon :: [(String, Double)] -> [String] 
envCon [] = []
envCon (x:xs) = (fst x ++ " = " ++ (show $ snd x)) : envCon xs

-- Handling vty events
handleEvent :: MyState -> BrickEvent n e -> EventM n (Next MyState)
handleEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
            EvKey (KChar 'q') [] -> halt s
            EvKey KBS [] -> continue s'
                where s' = if null $ s^.evalString then s
                           else s & evalString .~ (init $ s^.evalString)
            EvKey (KChar c) [] -> continue $ s & evalString .~ s^.evalString ++ [c]
            EvKey KEnter [] -> continue s'
                where s' = case exec (s^.evalString) (s^.env) of
                           (Left newEnv) -> s & (evalString.~"") . (env.~newEnv)
                           (Right val)   -> s & evalString.~(show val)
            _ -> continue s
        _ -> continue s
