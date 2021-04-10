module Prepro where

import Eval
import Data.Char


data BOps = AddOp | SubOp | MulOp | DivOp | ExpOp
          deriving (Show,Enum,Eq,Ord)

data UOps = CosOp | SinOp 
          deriving Show 

data Token = Tcon N | TId Id
           | UOp UOps | BOp BOps 
           | LPar | RPar | Err 
           | OpAss
           | PE Expr | PAss Id Expr 
           deriving Show 

classify :: String -> Token
classify "+" = BOp AddOp
classify "-" = BOp SubOp
classify "*" = BOp MulOp
classify "=" = OpAss
classify "^" = BOp ExpOp
--classify "==" = BOp EqlOp
classify "cos" = UOp CosOp 
classify "sin" = UOp SinOp
classify "(" = LPar
classify ")" = RPar
classify x | isVSym x = TId x
           | isCSym x = Tcon (read x :: Double)
           | otherwise = Err
--classify _ = Err 

preproc :: String -> String 
preproc [] = []
preproc ('(':xs) = " ( " ++ preproc xs
preproc (')':xs) = " ) " ++ preproc xs
preproc ('+':xs) = " + " ++ preproc xs
preproc ('-':xs) = " - " ++ preproc xs
preproc ('*':xs) = " * " ++ preproc xs
preproc ('^':xs) = " ^ " ++ preproc xs
preproc ('=':'=':xs) = " == " ++ preproc xs
preproc ('=':xs) = " = " ++ preproc xs
preproc ('c':'o':'s':xs) = " cos " ++ preproc xs
preproc ('s':'i':'n':xs) = " sin " ++ preproc xs
preproc (x:xs) = x : preproc xs 

lexer :: String -> [Token]
lexer s = map classify $ words $ preproc s

-- checking for correct number value 123 decimal followed by 12312 no more decimals 
isCSym:: String -> Bool 
isCSym"" = False
isCSym(x:xs) = isDigit x && q1 xs 
    where q1 "" = True 
    -- check the state after a decimal is invloved 
          q1 ('.':ys) = q2 ys
            where q2 "" = True
                  q2 (z:zs) = isDigit z && q2 zs
    -- if before a decimal then keep checking isDigit
          q1 (y:ys) = isDigit y && q1 ys 

isVSym :: String -> Bool 
isVSym "" = False
isVSym (x:xs) = (isUpper x || isLower x) && q1 xs 
    where q1 "" = True
          q1 (y:ys) = (isUpper y || isLower y ) && q1 ys 
