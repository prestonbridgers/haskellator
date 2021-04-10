module Eval where 
-- below we are going to add fucntions for handling parsing/evaluating from user input
-- these are instructions to handle with parsing
data Instr = Assign String AExpr
    | Exp AExpr 
    deriving Show 

data AExpr = Var String |Const Double
    |Add AExpr AExpr | Sub AExpr AExpr
    |Mul AExpr AExpr | Div AExpr AExpr
    |Expo AExpr AExpr -- | Mod AExpr AExpr
    |Sin AExpr | Cos AExpr
    deriving Show 

-- type of environment for variables to be assigned in
type Env = [(String,Double)]

-- to find values that are associated with given variables
lookUp :: String -> Env -> Double
lookUp a [] = error "there is no pair associated with this String"
lookUp a ((x,y):zs) = if (a==x) then y else lookUp a zs

--this function adds a var to a env list 
addVar :: (String, Double) -> Env -> Env 
addVar x [] = [x]
addVar x env = x : env 

-- when calling update we will need to call remove to replace the env 
removeVar :: String -> Env -> Env 
removeVar a [] = []
removeVar a ((x,y):zs) = if a == x then zs else (x,y) : removeVar a zs

-- update will set value of given parameter to new value and keep everything else the same
update :: String -> Double -> Env -> Env 
update s d [] = [(s,d)]
update s d env = addVar (s,d) (removeVar s env)

--handling evaluating our Aeprs 
evalA :: Env -> AExpr -> Double
evalA env (Var x) = lookUp x env
evalA env (Const x) = x
evalA env (Add x xx) = (evalA env x) + (evalA env xx)
evalA env (Sub x xx) = (evalA env x) - (evalA env xx)
evalA env (Mul x xx) = (evalA env x) * (evalA env xx) 
evalA env (Div x xx) = (evalA env x) / (evalA env xx)
evalA env (Expo x xx) = (evalA env x) ** (evalA env xx)
evalA env (Cos x) = cos(evalA env x)
evalA env (Sin x) = sin(evalA env x)
--come back to mod because it does not really have a double value way could cast using tointegral but who knows 
--evA env (Mod x xx) = (evA env x) % (evA env xx) 

--ex function is going to do an insturction on what it is supposed to do 
ex :: Instr -> Env -> Env
ex (Assign var aE) env = update (var) (evalA env aE) env 
--ex (Exp aEx) env = evalA env aEx