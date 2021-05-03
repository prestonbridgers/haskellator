module Parse (exec) where 

import Data.Char


-- Data types declared here

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

data BOps = SubOp | AddOp | DivOp | MulOp | ExpOp | OpAss 
           deriving (Show,Enum,Eq,Ord)

data UOps =  CosOp | SinOp
           deriving Show

data Token = CSym Double | VSym String
           | UOp UOps | BOp BOps 
           | LPar | RPar
           | TExpr Expr 
           | TAss Instr 
           deriving Show 

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

-- Evaluates an instruction with respect to a given environment
-- Returns either an updated Environment in the case of an assignment
--             or The result of the computation in the form of a double
--                in the case of an arithmetic expression
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

-- Helper function determining valid variable names (IDs)
isVSym :: String -> Bool 
isVSym [] = False
isVSym (x:xs) =  if isLower x then isVSym' xs else False 
                 where isVSym' [] = True
                       isVSym' (x:xs) = if x `elem` ['a'..'z']++['A'..'Z']++['0'..'9']++"-_" then isVSym' xs else False

-- Converts string representation of tokens to the internal representation
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

-- Core parsing function returns singleton list of a token encapsulating
-- the instruction to be run (can contain either an Assign or AExpr
-- instruction)
parse :: [Token] -> [Token] -> [Token]
parse (CSym n : stack)                    input = parse (TExpr (Const n): stack) input
parse (VSym v : stack)                    input = parse (TExpr (Var v): stack) input
parse (RPar: TExpr e : LPar : stack)      input = parse (TExpr e : stack) input -- taking care of parens we just get rid of them 
parse s@(TExpr e2 : BOp op1 : TExpr e1 : stack) (BOp op2 : input) | op1 < op2 = parse (BOp op2 : s) input
parse (TExpr e2 : BOp AddOp : TExpr e1 : stack) input = parse (TExpr (Add e1 e2) : stack) input -- covers add
parse (TExpr e2 : BOp SubOp : TExpr e1: stack)  input = parse (TExpr(Sub e1 e2) : stack) input -- covers Sub
parse (TExpr e2 : BOp MulOp : TExpr e1 : stack) input = parse (TExpr (Mul e1 e2) : stack) input 
parse (TExpr e2 : BOp DivOp : TExpr e1: stack)  input = parse (TExpr(Div e1 e2) : stack) input 
parse (TExpr e2 : BOp ExpOp : TExpr e1 : stack) input = parse (TExpr (Expo e1 e2) : stack) input 
parse (TExpr e : UOp CosOp : stack)             input = parse (TExpr(Cos e):stack) input -- taking care of cos
parse (TExpr e : UOp SinOp : stack)             input = parse (TExpr(Sin e):stack) input -- taking care of sin
parse (TExpr e2: BOp OpAss : TExpr (Var v) : stack)     input = parse (TAss (Assign v e2 ): stack ) input -- taking care of an assignmnet 
parse stack (i:input) = parse (i:stack) input
parse stack []        = stack

-- This takes the output of the parse function and destructs the token
-- into the corresponding Instr type
tokToIn :: [Token] -> Instr
tokToIn [] = error "Nothing to parse to instruction"
tokToIn [(TExpr e1)] = AExpr e1 
tokToIn [(TAss (Assign id e1))] = Assign id e1 
tokToIn _ = error "Parse Error"

-- Core function that is exposed, takes raw string input and parses/evaluates
-- it returning:
-- Either a new environment reflecting an assignment operation
--     or a double reflecting the result of an arithmetic expression
exec :: String -> Env -> Either Env Double
exec s env = eval (run s) env
    where run = tokToIn . parse [] . tokenize

