module Eval where 
-- below we are going to add fucntions for handling parsing/evaluating from user input
-- these are instructions to handle with parsing
data Instr = Assign String AExpr
    | Do [Instr]

-- 
data AExpr = Var String |Const Double
    |Add AExpr AExpr | Sub AExpr AExpr
    |Mul AExpr AExpr | Div AExpr AExpr
    |Expo AExpr AExpr | Mod AExpr AExpr

-- type of environment for variables to be assigned in
type Env = [(String,Double)]

-- to find values that are associated with given variables
lookUp :: String -> Env -> Double
lookUp a [] = error "there is no pair associated with this String"
lookUp a ((x,y):zs) = if (a==x) then y else lookUp a zs

-- when calling update we will need to call remove to replace the env 
removeL :: String -> Env -> Env 
removeL a [] = []
removeL a ((x,y):zs) = if a == x then zs else (x,y) : removeL a zs

-- update will set value of given parameter to new value and keep everything else the same
update :: String -> Double -> Env -> Env 
update s d [] = [(s,d)]
update s d env = (s,d) : removeL s env

--handling evaluating our Aeprs 
evA :: Env -> AExpr -> Double
evA env (Var x) = lookUp x env
evA env (Const x) = x
evA env (Add x xx) = (evA env x) + (evA env xx)
evA env (Sub x xx) = (evA env x) - (evA env xx)
evA env (Mul x xx) = (evA env x) * (evA env xx) 
--evA env (Div x xx) = (evA x env) / (evA xx env)
--evA env (Expo x xx) = (evA x env) ** (evA xx env) 
--evA env (Mod x xx) = (evA x env) `mod` (evA xx env) 