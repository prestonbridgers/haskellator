module Parse (exec) where 

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

data BOps = OpAss | SubOp | AddOp | DivOp | MulOp | ExpOp
           deriving (Show,Enum,Eq,Ord)

data UOps =  CosOp | SinOp
           deriving Show

data Token = CSym Double | VSym String
           | UOp UOps | BOp BOps 
           | LPar | RPar
           | TExpr Expr 
           | TAss Instr 
           deriving Show 

--type of environment for variables to be assigned in
type Env = [(Id,N)]

--to find values that are associated with given variables
lookUp :: String -> Env -> Double
lookUp a [] = error "there is no pair associated with this String"
lookUp a ((x,y):zs) = if (a==x) then y else lookUp a zs

--this function adds a var to a env list 
addVar :: (String, Double) -> Env -> Env 
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
prep ('=':xs) = " = ( " ++ prep xs ++ " )"
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
                 where isVSym' [] = True
                       isVSym' (x:xs) = if x `elem` ['a'..'z']++['A'..'Z']++['0'..'9']++"-_" then isVSym' xs else False

classify :: String -> Token
classify "(" = LPar
classify ")" = RPar
classify "+" = BOp AddOp
classify "-" = BOp SubOp
classify "=" = BOp OpAss
classify "*" = BOp MulOp
classify "/" = BOp DivOp
classify "^" = BOp ExpOp
classify "cos" = UOp CosOp
classify "sin" = UOp SinOp
classify x | isVSym x = VSym x
           | isCSym x = CSym (read x :: Double)
           | otherwise = error "classify error: Not a token"

-- here we are goin to do all the parsing in this file 
parse :: [Token] -> [Token] -> [Token]
parse (CSym n : stack)                    input = parse (TExpr (Const n): stack) input
parse (VSym v : stack)                   input = parse (TExpr (Var v): stack) input
parse (RPar: TExpr e : LPar : stack)      input = parse (TExpr e : stack) input -- taking care of parens we just get rid of them 

parse (TExpr e2 : BOp AddOp : TExpr e1 : stack) input = parse (TExpr (Add e1 e2) : stack) input -- covers add
parse (TExpr e2 : BOp SubOp : TExpr e1: stack)  input = parse (TExpr(Sub e1 e2) : stack) input -- covers Sub
parse (TExpr e2 : BOp MulOp : TExpr e1 : stack) input = parse (TExpr (Mul e1 e2) : stack) input 
parse (TExpr e2 : BOp DivOp : TExpr e1: stack)  input = parse (TExpr(Div e1 e2) : stack) input 
parse (TExpr e2 : BOp ExpOp : TExpr e1 : stack) input = parse (TExpr (Expo e1 e2) : stack) input 

parse (TExpr e : UOp CosOp : stack)             input = parse (TExpr(Cos e):stack) input -- taking care of cos
parse (TExpr e : UOp SinOp : stack)             input = parse (TExpr(Sin e):stack) input -- taking care of sin

parse (TExpr e2: BOp OpAss : TExpr (Var v) : stack)     input = parse (TAss (Assign v e2 ): stack ) input -- taking care of an assignmnet 

parse stack (i:input) = parse (i:stack) input
parse stack [] = stack


tokToIn :: [Token] -> Instr
tokToIn [] = error "Nothing to parse to instruction"
tokToIn [(TExpr e1)] = AExpr e1 
tokToIn [(TAss (Assign id e1))] = Assign id e1 
tokToIn _ = error "Parse Error"

-- addParensAfterEq :: String -> String
-- addParensAfterEq [] = []
-- addParensAfterEq s | '=' `elem` s = addParensAfterEq' [] s
--              | otherwise    = s
--              where addParensAfterEq' stack []     = []
--                    addParensAfterEq' stack (x:xs) | x == '='  = stack ++ "=(" ++ xs ++ ")"
--                                                   | otherwise = addParensAfterEq' (stack ++ [x]) xs

exec :: String -> Env -> Either Env Double
exec s env = eval (run s) env
    where run = tokToIn . parse [] . tokenize

