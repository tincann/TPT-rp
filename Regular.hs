
{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts #-}

import Context
import Generics.Regular

--data Expression Expression = 
--    Lam Name Expression
--    | App Expression Expression
--    | Var Name


type instance PF Expression = (K Name :*: I) :+: (I :*: I) :+: (K Name)




type instance PF Expression = (Bind Name :*: I) :+: (I :*: I) :+: (Use Name)

-- ----------------------------------
-- Defining generic alpha equivalence
-- ----------------------------------
-- Equal to the way that you can describe a data structure in Regular. Our idea
-- was to make something similar, but introduces two types: Bind and Use.
-- This is a way for 'tagging' the bind and use constructs in your datatype.
-- Now a generic equality function must be specified, that accounts for alpha-
-- equivalence. (Bind a with a Use a, should be the same as Bind b with a Use b).
-- 
-- Next we define a normalize function that replaces the names of the Bind 
-- and Use occurences with a deterministic value that depends on how in the
-- expression the Bind and Use occur relative to each other. This number can be
-- relative depth perhaps (In lambda expressions this would be like converting
-- to De Bruijn representation).
-- We tried to implement this, but failed to do so as we had trouble making
-- this construction compile (due to our limited Haskell knowledge).


newtype Bind a b = Bind { unBind :: a }
newtype Use a b = Use { unUse :: a }

--instance Regular Expression where
--    from (Lam name exp) = L (K name :*: I exp)
--    from (App exp1 exp2) = R (L (I exp1 :*: I exp2))
--    from (Var name) = R (R (K name))
--    to (L (K name :*: I exp)) = Lam name exp
--    to (R (L (I exp1 :*: I exp2))) = App exp1 exp2
--    to (R (R (K name))) = Var name

instance Regular Expression where
    from (Lam name exp) = L (Bind name :*: I exp)
    from (App exp1 exp2) = R (L (I exp1 :*: I exp2))
    from (Var name) = R (R (Use name))
    to (L (Bind name :*: I exp)) = Lam name exp
    to (R (L (I exp1 :*: I exp2))) = App exp1 exp2
    to (R (R (Use name))) = Var name



normalize :: PF Expression String -> PF Expression Int
normalize = _

main = do
    print $ normalize exLam
    print exLam2