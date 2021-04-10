module Eval where 
-- below we are going to add fucntions for handling parsing/evaluating from user input
-- these are instructions to handle with parsing

type Id = String 
type N = Double 

data Instr = Assign Id Expr
    | AExpr Expr 
    deriving Show 

data Expr = Const N
    |Add Expr Expr | Sub Expr Expr
    |Mul Expr Expr | Div Expr Expr
    |Expo Expr Expr -- | Mod Expr Expr
    |Sin Expr | Cos Expr
    deriving Show 

-- type of environment for variables to be assigned in
--type Env = [(Id,N)]

-- to find values that are associated with given variables
--lookUp :: String -> Env -> Double
--lookUp a [] = error "there is no pair associated with this String"
--lookUp a ((x,y):zs) = if (a==x) then y else lookUp a zs

--this function adds a var to a env list 
--addVar :: (String, Double) -> Env -> Env 
--addVar x [] = [x]
--addVar x env = x : env 

-- when calling update we will need to call remove to replace the env 
--removeVar :: String -> Env -> Env 
--removeVar a [] = []
--removeVar a ((x,y):zs) = if a == x then zs else (x,y) : removeVar a zs

-- update will set value of given parameter to new value and keep everything else the same
--update :: String -> Double -> Env -> Env 
--update s d [] = [(s,d)]
--update s d env = addVar (s,d) (removeVar s env)

eval :: Instr -> Either Double (Id, N)
eval (Assign x xx) = Right (x, evalExp xx)
eval (AExpr x) = Left (evalExp x)

--handling evaluating our exp 
evalExp :: Expr -> Double
evalExp (Const x) = x
evalExp (Add x xx) = (evalExp x) + (evalExp xx)
evalExp (Sub x xx) = (evalExp x) - (evalExp xx)
evalExp (Mul x xx) = (evalExp x) * (evalExp xx) 
evalExp (Div x xx) = (evalExp x) / (evalExp xx)
evalExp (Expo x xx) = (evalExp x) ** (evalExp xx)
evalExp (Cos x) = cos(evalExp x)
evalExp (Sin x) = sin(evalExp x)
--come back to mod because it does not really have a double value way could cast using tointegral but who knows 
--evA env (Mod x xx) = (evA env x) % (evA env xx) 
