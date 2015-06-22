module Context where

type Name = String

data Expression = Lam Name Expression
                | App Expression Expression
                | Var Name

instance Show Expression where
    show (Lam decl body) = "(\\" ++ decl ++ " -> " ++ show body ++ ")"
    show (App t1 t2) = (show t1) ++ " $ " ++ (show t2)
    show (Var name) = name

ex :: Expression
ex = Lam "x" (App (Var "x") (Lam "y" (Var "y")))

exLam :: Expression
exLam = Lam "z" $ App (Lam "y" $ App (Var "y") (Lam "x" (Var "x"))) (Lam "x" $ App (Var "z") (Var "x"))

exLam2 :: Expression
exLam2 = Lam "zz" $ App (Lam "yy" $ App (Var "yy") (Lam "xx" (Var "xx"))) (Lam "xx" $ App (Var "zz") (Var "xx"))