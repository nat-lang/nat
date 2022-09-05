module Mean.Core.Encoding where

import Mean.Core.Syntax
import Prelude hiding (and, exp, id, not, or, succ, (&&), (*), (**), (+), (++), (-), (>), (||))

lOne :: Lit
lOne = LInt 1

lZero :: Lit
lZero = LInt 0

lTrue :: Lit
lTrue = LBool True

lFalse :: Lit
lFalse = LBool False

true :: CoreExpr
true = CLit lTrue

false :: CoreExpr
false = CLit lFalse

f :: CoreExpr
f = mkCVar "f"

x :: CoreExpr
x = mkCVar "x"

y :: CoreExpr
y = mkCVar "y"

z :: CoreExpr
z = mkCVar "z"

m :: CoreExpr
m = mkCVar "m"

n :: CoreExpr
n = mkCVar "n"

l :: CoreExpr
l = mkCVar "l"

r :: CoreExpr
r = mkCVar "r"

p :: CoreExpr
p = mkCVar "p"

q :: CoreExpr
q = mkCVar "q"

-- λx.x
id :: CoreExpr
id = x ~> x

(?) :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
(?) = CTernOp Cond

(>) :: (CoreExpr -> CoreExpr) -> CoreExpr -> CoreExpr
e > e' = e e'

(&&) :: CoreExpr -> CoreExpr -> CoreExpr
p && q = CBinOp And p q

(||) :: CoreExpr -> CoreExpr -> CoreExpr
p || q = CBinOp Or p q

eq :: CoreExpr -> CoreExpr -> CoreExpr
eq = CBinOp Eq

(===) :: CoreExpr -> CoreExpr -> CoreExpr
(===) = eq

nEq :: CoreExpr -> CoreExpr -> CoreExpr
nEq = CBinOp NEq

(!==) :: CoreExpr -> CoreExpr -> CoreExpr
(!==) = nEq

not' :: CoreExpr -> CoreExpr
not' = CUnOp Neg