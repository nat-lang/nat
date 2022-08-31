module Mean.Core.Encoding where

import Mean.Core.Syntax

import Prelude hiding (and, exp, id, not, or, succ, (&&), (*), (**), (+), (++), (-), (||))

app :: CoreExpr -> CoreExpr -> CoreExpr
app = mkCApp

fOfX :: CoreExpr
fOfX = mkCVar "f" * mkCVar "x"

f :: CoreExpr
f = mkCVar "f"

x :: CoreExpr
x = mkCVar "x"

y :: CoreExpr
y = mkCVar "y"

m :: CoreExpr
m = mkCVar "m"

n :: CoreExpr
n = mkCVar "n"

-- λx.x
id :: CoreExpr
id = "x" ~> mkCVar "x"

-- λnλfλx . f(n f x)
succ :: CoreExpr
succ = "n" ~> ("f" ~> ("x" ~> (f * (n * f * x))))

-- λfλx . x
zero :: CoreExpr
zero = "f" ~> ("x" ~> x)

-- λfλx . f x
one :: CoreExpr
one = "f" ~> ("x" ~> (f * x))

-- λfλx . f(f x)
two :: CoreExpr
two = "f" ~> ("x" ~> (f * (f * x)))

-- λfλx . f(f(f x))
three :: CoreExpr
three = "f" ~> ("x" ~> (f * (f * (f * x))))

-- λmλn . m succ n
add :: CoreExpr
add = "m" ~> ("n" ~> (m * succ * n))

(+) :: CoreExpr -> CoreExpr -> CoreExpr
m + n = add * m * n

-- λmλnλfλx . m f (n f x)
add' :: CoreExpr
add' = "m" ~> ("n" ~> ("f" ~> ("x" ~> (m * f * (n * f * x)))))

(++) :: CoreExpr -> CoreExpr -> CoreExpr
m ++ n = add' * m * n

-- λmλn . m(add' n)0
mul :: CoreExpr
mul = "m" ~> ("n" ~> (m * (add * n) * zero))

-- λmλn . m(mul n)1
exp :: CoreExpr
exp = "m" ~> ("n" ~> (m * (mul * n) * one))

(**) :: CoreExpr -> CoreExpr -> CoreExpr
m ** n = exp * m * n

-- λxλy . x
true :: CoreExpr
true = "x" ~> ("y" ~> x)

-- λxλy . y
false :: CoreExpr
false = "x" ~> ("y" ~> y)

-- λfλxλy . f(x)(y)
if' :: CoreExpr
if' = "f" ~> ("x" ~> ("y" ~> (f * x * y)))

-- λxλy . if(x)(y)(false)
and :: CoreExpr
and = "x" ~> ("y" ~> (if' * x * y * false))

(&&) :: CoreExpr -> CoreExpr -> CoreExpr
x && y = and * x * y

-- λxλy . if(x)(true)(y)
or :: CoreExpr
or = "x" ~> ("y" ~> (if' * x * true * y))

(||) :: CoreExpr -> CoreExpr -> CoreExpr
x || y = or * x * y

-- λxλy . if(x)(true)(false)
not :: CoreExpr
not = "x" ~> ("y" ~> (if' * x * true * false))

(-) :: CoreExpr -> CoreExpr
(-) = app not
