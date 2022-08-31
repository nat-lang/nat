module Mean.Sugar.Encoding where

import Mean.Core.Syntax
import Mean.Sugar.Syntax
import Prelude hiding ((*))

e :: CoreExpr
e = mkCVar "e"
b :: CoreExpr
b = mkCVar "b"
x :: CoreExpr
x = mkCVar "x"
l :: CoreExpr
l = mkCVar "l"
r :: CoreExpr
r = mkCVar "r"

-- λx.x
id :: SugarExpr
id = "x" ~> mkSVar "x"

-- church encoding of binary tree with data in nodes

-- λeλb . e
leaf :: CoreExpr
leaf = "e" ~> ("b" ~> e)

-- λxλlλrλeλb . b(x)(l e b)(r e b)
node :: CoreExpr
node = "x" ~> ("l" ~> ("r" ~> ("e" ~> ("b" ~> (b * x) * (l * e * b) * (r * e * b)))))