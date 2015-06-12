module Beta where

import Context
import Data.Maybe


type Env = [(Name, Lambda)]

exBeta :: Expression
exBeta = App (Lam "y" $ App (Lam "x" (Var "x")) (Var "y")) (Lam "r" (Var "r"))

exBeta2 = App (Lam "x" (Var "x")) (Lam "y" (Var "y"))

exBeta3 = App (Lam "x" (Lam "y"  (App (Var "x") (Var "y")))) (Lam "y" (Var "y"))

data Lambda = Lambda Name Expression Env

instance Show Lambda where
	show (Lambda decl body _) = "(\\" ++ decl ++ " -> " ++ show body ++ ")"

eval :: Env -> Expression -> Lambda
eval env (Lam name body) = Lambda name body env
eval env (Var name) = fromJust $ lookup name env
eval env (App f arg) =
	let 
		earg = eval env arg
		(Lambda eName eE pEnv) = eval env f
		newEnv = (eName, earg) : pEnv
	in eval newEnv eE

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