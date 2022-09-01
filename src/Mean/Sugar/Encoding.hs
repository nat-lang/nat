module Mean.Sugar.Encoding where

import Mean.Sugar.Syntax
import Prelude hiding ((*), (~>))

f :: SugarExpr
f = mkSVar "f"
x :: SugarExpr
x = mkSVar "x"
y :: SugarExpr
y = mkSVar "y"
z :: SugarExpr
z = mkSVar "z"
e :: SugarExpr
e = mkSVar "e"
b :: SugarExpr
b = mkSVar "b"
l :: SugarExpr
l = mkSVar "l"
r :: SugarExpr
r = mkSVar "r"
-- church encoding of binary tree with data in nodes

-- λeλb . e
leaf :: SugarExpr
leaf = e ~> (b ~> e)

-- λxλlλrλeλb . b(x)(l e b)(r e b)
node :: SugarExpr
node = x ~> (l ~> (r ~> (e ~> (b ~> (b * x * (l * e * b) * (r * e * b))))))