module Parse where 

import Data.Char

type Id = String 
type N = Double 

data Instr = Assign Id Expr
           | AExpr Expr 
           deriving Show 

data Expr = Var Id | Const N
          |Add Expr Expr 
          |Sub Expr Expr
          |Mul Expr Expr
          |Div Expr Expr
          |Expo Expr Expr
          |Cos Expr
          |Sin Expr
          deriving Show 

data Token = Tcon N 
           | TId Id
           | AddOp 
           | SubOp
           | MulOp 
           | DivOp
           | ExpOp 
           | CosOp
           | SinOp
           | OpAss
           | LPar | RPar
           | TExpr Expr 
           | TAss Id Expr 
           deriving Show 

--type of environment for variables to be assigned in
type Env = [(Id,N)]

--to find values that are associated with given variables
lookUp :: String -> Env -> Double
lookUp a [] = error "there is no pair associated with this String"
lookUp a ((x,y):zs) = if (a==x) then y else lookUp a zs

--this function adds a var to a env list 
addVar :: (String, Double) -> Env -> Env 
addVar x [] = [x]
addVar x env = x : env 

--when calling update we will need to call remove to replace the env 
removeVar :: String -> Env -> Env 
removeVar a [] = []
removeVar a ((x,y):zs) = if a == x then zs else (x,y) : removeVar a zs

--update will set value of given parameter to new value and keep everything else the same
update :: String -> Double -> Env -> Env 
update s d [] = [(s,d)]
update s d env = addVar (s,d) (removeVar s env)

eval :: Instr -> Env -> Either Env Double 
eval (Assign v e) env = Left (update (v) (exprEval env e) env) 
eval (AExpr e) env = Right ((exprEval env e))

--handling evaluating our exp 
exprEval ::Env -> Expr -> Double
exprEval env (Var x) = lookUp x env
exprEval env (Const x) = x
exprEval env (Add x xx) = (exprEval env x) + (exprEval env xx)
exprEval env (Sub x xx) = (exprEval env x) - (exprEval env xx)
exprEval env (Mul x xx) = (exprEval env x) * (exprEval env xx)
exprEval env (Div x xx) = (exprEval env x) / (exprEval env xx)
exprEval env (Expo x xx) = ((exprEval env x) ** (exprEval env xx))
exprEval env (Cos x ) = cos(exprEval env x) 
exprEval env (Sin x ) = sin(exprEval env x) 

------------------------- Now we are in preproccessing land here 
tokenize :: String -> [Token]
tokenize s = map classify $ words $ prep s

prep :: String -> String 
prep [] = []
prep ('(':xs) = " ( " ++ prep xs
prep (')':xs) = " ) " ++ prep xs
prep ('+':xs) = " + " ++ prep xs
prep ('-':xs) = " - " ++ prep xs
prep ('=':xs) = " = " ++ prep xs
prep ('*':xs) = " * " ++ prep xs
prep ('/':xs) = " / " ++ prep xs
prep ('^':xs) = " ^ " ++ prep xs
prep ('c':'o':'s':xs) = " cos " ++ prep xs
prep ('s':'i':'n':xs) = " sin " ++ prep xs
prep (x:xs) = x : prep xs

-- checking for correct number value 123 decimal followed by 12312 no more decimals 
isCSym:: String -> Bool 
isCSym [] = True
isCSym (x:xs) | x `elem` ['0' .. '9'] = isCSym xs 
              | x == '.' = isCSym' xs 
              | otherwise = False 
              where isCSym' [] = True
                    isCSym' (y:ys) = if y `elem` ['0' .. '9'] then isCSym' ys else False 

isVSym :: String -> Bool 
isVSym [] = False
isVSym (x:xs) =  if isLower x then isVSym' xs else False 

isVSym' :: String -> Bool
isVSym' [] = True
isVSym' (x:xs) = if (isLower x && isDigit x) then isVSym' xs else False

classify :: String -> Token
classify "(" = LPar
classify ")" = RPar
classify "+" = AddOp
classify "-" = SubOp
classify "=" = OpAss
classify "*" = MulOp
classify "/" = DivOp
classify "^" = ExpOp
classify "cos" = CosOp
classify "sin" = SinOp
classify x | isVSym x = TId x
           | isCSym x = Tcon (read x :: Double)
           | otherwise = error "classify error: Not a token"

 

-- data Token = Tcon N 
--            | TId Id
--            | AddOp 
--            | SubOp
--            | MulOp 
--            | DivOp
--            | ExpOp 
--            | CosOp
--            | SinOp
--            | OpAss
--            | LPar | RPar
--            | TExpr Expr 
--            | TAss Id Expr 
--            deriving Show 

-- here we are goin to do all the parsing in this file 
parse :: [Token] -> [Token] -> [Token]
parse (Tcon n : stack) input = parse (TExpr (Const n): stack) input

parse (RPar: TExpr e : LPar : stack)        input = parse (TExpr e : stack) input -- taking care of parens we just get rid of them 

parse (TExpr e2 : AddOp : TExpr e1 : stack) input = parse (TExpr (Add e1 e2) : stack) input -- covers add
parse (TExpr e2 : SubOp : TExpr e1: stack) input = parse (TExpr(Sub e1 e2) : stack) input -- covers Sub
parse (TExpr e2 : MulOp : TExpr e1 : stack) input = parse (TExpr (Mul e1 e2) : stack) input 
parse (TExpr e2 : DivOp : TExpr e1: stack) input = parse (TExpr(Div e1 e2) : stack) input 
parse (TExpr e2 : ExpOp : TExpr e1 : stack) input = parse (TExpr (Expo e1 e2) : stack) input 

parse (TExpr e : CosOp : stack)         input = parse (TExpr(Cos e):stack) input -- taking care of cos
parse (TExpr e : SinOp : stack)         input = parse (TExpr(Sin e):stack) input -- taking care of sin

parse (TExpr e2: OpAss : TId v : stack)     input = parse (TAss v e2 : stack ) input -- taking care of an assignmnet 

parse stack (i:input) = parse (i:stack) input
parse stack [] = stack


tokToIn :: [Token] -> Instr
tokToIn [] = error "Nothing to parse to instruction"
tokToIn [(TExpr e1)] = AExpr e1 
tokToIn [(TAss id e1)] = Assign id e1 
tokToIn _ = error "Parse Error"

tt :: String -> [Token]
tt s = tokenize s 

pp :: [Token] -> [Token]
pp s = parse [] s

tok :: [Token] -> Instr
tok s = tokToIn s

exec :: String -> Either Env Double
exec s = eval (tok (pp (tt s))) ([])