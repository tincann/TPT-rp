module DeBruijn where

import Context
import Data.Maybe


instance Show BExpression where
    show (BLam body) = "(L -> " ++ show body ++ ")"
    show (BApp t1 t2) = (show t1) ++ " $ " ++ (show t2)
    show (BVar index) = show index


exLamBruijn :: BExpression
exLamBruijn = BLam (BApp (BLam (BApp (BVar 1) (BLam (BVar 1)))) (BLam (BApp (BVar 2) (BVar 1))))




type Depth = Int
type DepthMap = [(Name, Depth)]

data BExpression = 
    BLam BExpression
    | BApp BExpression BExpression
    | BVar Depth
    deriving (Eq)

instance Eq Expression where
    a == b = (normalize a) == (normalize b)

normalize :: Expression -> BExpression
normalize ex = transform ex [] 1 where
    transform :: Expression -> DepthMap -> Depth -> BExpression
    transform (Lam name body) dm d = BLam (transform body newDepthMap (d + 1))
        where
            newDepthMap = ((name, d) : dm)
    transform (App b1 b2) dm d = BApp (transform b1 dm d) (transform b2 dm d)
    transform (Var name) dm d = BVar (d - declDepth) 
        where
            declDepth = fromJust $ lookup name dm



