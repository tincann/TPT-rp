
import Context


newtype FixBind f bv = In { out :: f (FixBind f) }

data BindUse = None | Bind | Use

data ExpressionF r where 
    AppF :: r    -> r -> ExpressionF r
    LamF :: Name -> r -> ExpressionF r
    VarF :: Name ->      ExpressionF r

type Expression' = FixBind ExpressionF
