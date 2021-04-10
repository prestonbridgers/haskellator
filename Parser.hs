module Parser where

import Eval
import Prepro


-- data BOps = AddOp | SubOp | MulOp | DivOp | ExpOp
--        
--           deriving (Show,Enum,Eq,Ord)

-- data UOps = CosOp | SinOp 
--           deriving Show 

-- data Token = Tcon N | TId Id
--            | UOp UOps | BOp BOps 
--            | LPar | RPar | Err 
--            | PE Expr  | PAss Id Expr 
--            deriving Show  

-- here we are goin to do all the parsing in this file 
parse :: [Token] -> [Token] -> [Token]
parse (Err : stack) input = error "somthing wacky on the stacky yo!"
parse (Tcon v : stack) input = parse (PE (Const v): stack) input

parse s@(PE ee : BOp op1 : PE e : stack) (BOp op2 : input) | op1 < op2 = parse (BOp op2 : s) input -- taking care of presidence in an Expr 

parse (RPar: PE e : LPar : stack)        input = parse (PE e : stack) input -- taking care of parens we just get rid of them 

parse (PE ee : BOp AddOp : PE e : stack) input = parse (PE (Add e ee) : stack) input -- covers add
parse (PE ee : BOp SubOp : PE e : stack) input = parse (PE (Sub e ee) : stack) input -- covers Sub
parse (PE ee : BOp MulOp : PE e : stack) input = parse (PE (Mul e ee) : stack) input -- covers mul
parse (PE ee : BOp DivOp : PE e : stack) input = parse (PE (Add e ee) : stack) input -- covers div 
parse (PE ee : BOp ExpOp : PE e : stack) input = parse (PE (Expo e ee) : stack) input

parse (PE e : UOp CosOp : stack)         input = parse (PE(Cos e):stack) input -- taking care of cos
parse (PE e : UOp SinOp : stack)         input = parse (PE(Sin e):stack) input -- taking care of sin

parse (PE ee: OpAss : TId e : stack)     input = parse (PAss e ee : stack ) input -- taking care of an assignmnet 

tokToIn :: [Token] -> Instr
tokToIn [] = error "Nothing to parse to instruction"
tokToIn [(PE e)] = AExpr e 
tokToIn [(PAss id e)] = Assign id e 
tokToIn _ = error "Parse Error"

exec :: String -> Either Double (Id, N)
exec s = eval $ tokToIn $ parse [] $ lexer s