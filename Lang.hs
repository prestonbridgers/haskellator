module Lang (exec) where

type Id  = String
type Val = Double

type Env = [(Id, Val)]

data Instr = IAssign Id Expr
           | IExpr Expr
	       deriving (Show, Eq)

data Expr = Const Val
     | Var Id
     | Add Expr Expr
	 | Sub Expr Expr
	 | Mul Expr Expr
	 | Div Expr Expr
	 deriving (Show, Eq)

data Token = TId Id | TConst Val
           | TEq | TAdd | TSub | TMul
		   | TDiv | TLPar | TRPar
		   | TExpr Expr
		   | TAssign Id Expr
		   deriving (Show, Eq)

-- Test environment
testEnv :: Env
testEnv = [("pi", 3.1415), ("x", 2.2)]

envLookup :: Id -> Env -> Val
envLookup _ [] = error "Variable not in environment."
envLookup k ((id, val):xs) | k == id   = val
                           | otherwise = envLookup k xs

envAdd :: (Id, Val) -> Env -> Env
envAdd var e = var : e

envRemove :: Id -> Env -> Env
envRemove _ [] = []
envRemove k (v@(id, _):xs) | k == id   =     envRemove k xs
                           | otherwise = v : envRemove k xs

envUpdate :: (Id, Val) -> Env -> Env
envUpdate x e = envAdd x $ envRemove (fst x) e

evalInstr :: Instr -> Env -> Either Val Env
evalInstr (IAssign id expr) env = Right (envUpdate (id, evalExpr expr env) env)
evalInstr (IExpr expr) env = Left (evalExpr expr env)

evalExpr :: Expr -> Env -> Val
evalExpr (Const v) e = v
evalExpr (Var id) e = envLookup id e
evalExpr (Add e1 e2) e = evalExpr e1 e + (evalExpr e2 e)
evalExpr (Sub e1 e2) e = evalExpr e1 e - (evalExpr e2 e)
evalExpr (Mul e1 e2) e = evalExpr e1 e * (evalExpr e2 e)
evalExpr (Div e1 e2) e = evalExpr e1 e / (evalExpr e2 e)



isCSym :: String -> Bool
isCSym [] = True
isCSym (x:xs) | x `elem` ['0' .. '9'] = isCSym xs
              | x == '.' = isCSym' xs
			  | otherwise = False
			  where isCSym' [] = True
			        isCSym' (y:ys) = if y `elem` ['0' .. '9'] then isCSym' ys else False

isVSym :: String -> Bool
isVSym [] = False
isVSym (x:xs) = if x `elem` ['a' .. 'z'] then isVSym' xs else False

isVSym' :: String -> Bool
isVSym' [] = True
isVSym' (x:xs) = if x `elem` ['a' .. 'z'] ++ ['1' .. '9'] then isVSym' xs else False

classify :: String -> Token
classify s | s == "+" = TAdd
           | s == "-" = TSub
           | s == "*" = TMul
           | s == "/" = TDiv
           | s == "=" = TEq
           | s == "(" = TLPar
           | s == ")" = TRPar
		   | isCSym s = TConst (read s :: Val)
		   | isVSym s = TId s

prep :: String -> String
prep [] = []
prep ('+':xs) = " + " ++ prep xs
prep ('-':xs) = " - " ++ prep xs
prep ('*':xs) = " * " ++ prep xs
prep ('/':xs) = " / " ++ prep xs
prep ('=':xs) = " = " ++ prep xs
prep ('(':xs) = " ( " ++ prep xs
prep (')':xs) = " ) " ++ prep xs
prep (x:xs) = x : prep xs 

tokenize :: String -> [Token]
tokenize s = map classify $ words $ prep s





-- data Token = TId Id | TConst Val
--            | TEq | TAdd | TSub | TMul
-- 		   | TDiv | TLPar | TRPar
-- 		   | TExpr Expr
-- 		   | TAssign Id Expr
-- 		   deriving Show

-- Parse time baby
parse :: [Token] -> [Token] -> [Token]
parse ((TConst c):stack) input = parse (TExpr (Const c) : stack) input

-- Var
parse ((TId id):stack) input = parse (TExpr (Var id) : stack) input

-- Add, Sub, Mul, Div
parse ((TExpr e2):TDiv:(TExpr e1):stack) input = parse (TExpr (Div e1 e2):stack) input
parse ((TExpr e2):TMul:(TExpr e1):stack) input = parse (TExpr (Mul e1 e2):stack) input
parse ((TExpr e2):TAdd:(TExpr e1):stack) input = parse (TExpr (Add e1 e2):stack) input
parse ((TExpr e2):TSub:(TExpr e1):stack) input = parse (TExpr (Sub e1 e2):stack) input

-- Assignment
parse ((TExpr e):TEq:(TExpr (Var id)):stack) input = parse (TAssign id e : stack) input

-- Parens
parse (TRPar : expr@(TExpr e) : TLPar : stack) input = parse (expr:stack) input

-- Defaults
parse stack (i:input) = parse (i:stack) input
parse stack [] = stack



tti :: Env -> [Token] -> Either Val Env
tti env [(TExpr e)]      = evalInstr (IExpr e) env
tti env [(TAssign id e)] = evalInstr (IAssign id e) env
tti _ _                  = error "tti error: too many instructions?" 

exec :: String -> Env -> Either Val Env
exec s e = tti e $ parse [] $ tokenize s
