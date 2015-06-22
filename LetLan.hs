import Context



data LetLan = LetLan [Assignment] Exp
data Assignment = Bind String Integer
data Exp = Plus Var Var
         | Var String
         | Const Integer


-- How to define equality on this language?
let x = 5
in x + x

let y = 5
in y + y

-- This would be equivalent using alpha reduction, but what about:

let x = 5, z = 1
in x + x

let y = 5
in y + y

-- The variable 'z' here is unused in the expression, so evaluating both
-- expressions should give us the same answer, but do we consider them
-- equivalent?

-- Two possible ways to look at this. 
-- 1. We only consider equality on the body of the expression (after 'in'). So
--    we only look at bindings that are used.
-- 2. Technically the datatypes are not equal (even after using alpha-reduction),
--    because the list of assignments contains an extra variable.
--    


function (x){
    return x * 2;
}

function (x) {
    return x << 2;
}