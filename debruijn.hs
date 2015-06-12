module DeBruijn where

import Context
import Data.Maybe

type Depth = Int
type DepthMap = [(Name, Depth)]

data BExpression = 
	BLam BExpression
	| BApp BExpression BExpression
	| BVar Depth
	deriving (Eq)

instance Show BExpression where
	show (BLam body) = "(L -> " ++ show body ++ ")"
	show (BApp t1 t2) = (show t1) ++ " $ " ++ (show t2)
	show (BVar index) = show index

instance Eq Expression where
	a == b = (normalize a) == (normalize b)

exLamBruijn :: BExpression
exLamBruijn = BLam (BApp (BLam (BApp (BVar 1) (BLam (BVar 1)))) (BLam (BApp (BVar 2) (BVar 1))))


normalize :: Expression -> BExpression
normalize ex = transform ex [] 1
	where
		transform :: Expression -> DepthMap -> Depth -> BExpression
		transform (Lam name body) dm d = BLam (transform body ((name, d) : dm) (d + 1))
		transform (App b1 b2) dm d = BApp (transform b1 dm d) (transform b2 dm d)
		transform (Var name) dm d = BVar (d - declDepth)
			where
				declDepth = fromJust $ lookup name dm


