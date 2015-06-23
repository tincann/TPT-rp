module Beta where

import Context
import Data.Maybe
import Debug.Trace


type Env = [(Name, Lambda)]

exBeta :: Expression
exBeta = App (Lam "y" $ App (Lam "x" (Var "x")) (Var "y")) (Lam "r" (Var "r"))

exBeta2 = App (Lam "x" (Var "x")) (Lam "y" (Var "y"))

exBeta3 = App (Lam "x" (Lam "y"  (App (Var "x") (Var "y")))) (Lam "z" (Var "z"))

data Lambda = Lambda Name Expression Env

instance Show Lambda where
    show (Lambda decl body _) = "(\\" ++ decl ++ " -> " ++ show body ++ ")"


-- https://gist.github.com/andrusha/2713435
reduce :: Expression -> Expression
reduce (Var v)       = Var v
reduce (Lam a t)  = Lam a (reduce t)
reduce (App t1 t2) = apply (reduce t1) (reduce t2)
 
apply :: Expression -> Expression -> Expression
apply (Lam param t1) t2 = reduce $ rename param t2 t1
apply t1 t2 = App t1 t2
 
fmap' :: (Expression -> Expression) -> Expression -> Expression 
fmap' f (Var v) = f (Var v)
fmap' f (Lam v t) = Lam v (fmap' f t)
fmap' f (App t1 t2) = App (fmap' f t1) (fmap' f t2)
 
rename :: Name -> Expression -> Expression -> Expression
rename a t1 = fmap' replace
    where replace = \(Var x) -> if x == a then t1 else (Var x)



--eval :: Env -> Expression -> Lambda
--eval env (Lam name body) = trace ("lam - env:" ++ show env ++ " | lam:" ++ name ++ " | body:" ++ show body) $ Lambda name body env
--eval env (Var name) = trace "var" (fromJust $ lookup name env)
--eval env (App f arg) =
--    let 
--        earg = eval env arg
--        (Lambda eName eE pEnv) = eval env f
--        newEnv = (eName, earg) : pEnv
--    in trace ("app - env:" ++ show newEnv ++ " | eval body of f:" ++ show eE ++ " | eval arg:" ++ show earg) $ eval newEnv eE

--main = do
--    --print exBeta3
--    print $ eval [] exBeta3

{-eval env (Lam name body)   = eval ((name, body) : env) body
eval env (App expr1 expr2) = eval env expr1
eval env (Var name)        = fromJust $ lookup name env
-}

{-
eval env (Lam name body)   = eval ((name, body) : env) body
eval env (App (Lam name body) expr2) = eval ((name, eval env expr2) : env) body
eval env (App (Var name) expr2) = eval env expr2
eval env (Var name)        = fromJust $ lookup name env
-}