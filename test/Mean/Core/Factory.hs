module Mean.Core.Factory where

import Mean.Core.Syntax
import Prelude hiding (succ, exp, and, id, not, or, (&&), (+), (++), (-), (||), (*), (**))

app = mkCApp
fn x = mkCLam (Binder (mkVar x) TyNil)

idFn x = fn x (mkCVar x)
fOfX = mkCApp (mkCVar "f") (mkCVar "x")

f = mkCVar "f"
x = mkCVar "x"
y = mkCVar "y"
m = mkCVar "m"
n = mkCVar "n"

-- λx.x
id = idFn "x"

-- λnλfλx . f(n f x)
succ = fn "n" (fn "f" (fn "x" (app f (app (app n f) x))))

-- λfλx . x
zero = fn "f" (fn "x" x)

-- λfλx . f x
one = fn "f" (fn "x" (app f x))

-- λfλx . f(f x)
two = fn "f" (fn "x" (app f (app f x)))

-- λfλx . f(f(f x))
three = fn "f" (fn "x" (app f (app f (app f x))))

-- λmλn . m succ n
add = fn "m" (fn "n" (app (app m succ) n))
m + n = app (app add m) n

-- λmλnλfλx . m f (n f x)
add' = fn "m" (fn "n" (fn "f" (fn "x" (app (app m f) (app (app n f) x)))))
m ++ n = app (app add' m) n

-- λmλn . m(add' n)0
mul = fn "m" (fn "n" (app (app m (app add n)) zero))
m * n = app (app mul m) n

-- λmλn . m(mul n)1
exp = fn "m" (fn "n" (app (app n (app mul m)) one))
m ** n = app (app exp m) n

-- λxλy . x
true = fn "x" (fn "y" x)

-- λxλy . y
false = fn "x" (fn "y" y)

-- λfλxλy . f(x)(y)
if' = fn "f" (fn "x" (fn "y" (app (app f x) y)))
x ~> y = app (app if' x) y

-- λxλy . if(x)(y)(false)
and = fn "x" (fn "y" (app (app (app if' x) y) false))
x && y = app (app and x) y

-- λxλy . if(x)(true)(y)
or = fn "x" (fn "y" (app (app (app if' x) true) y))
x || y = app (app or x) y

-- λxλy . if(x)(true)(false)
not = fn "x" (fn "y" (app (app (app if' x) true) false))
not' = app not
(-) = not'
