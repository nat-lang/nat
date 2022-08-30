module Mean.Sugar.Factory where

import Mean.Core.Syntax
import Mean.Sugar.Syntax
import Prelude hiding (id)

fn x = mkSLam (Binder (mkVar x) TyNil)

id = fn "x" (mkSVar "x")
