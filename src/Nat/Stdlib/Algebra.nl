/*
  Set-theoretic predicates and axioms.
*/

let isLeftId(a,d,f)  = forall b in d. f(a,b) == b
let isRightId(a,d,f) = forall b in d. f(b,a) == b
let isId(a,d,f)      = isLeftId(a,d,f) && isRightId(a,d,f)
let idOf(f,d)        = the a in d. isId(a,d,f)
let hasId(f,d)       = do
  let id = idOf(f,d)
  bool(id)

let isLeftInverse(a,b,f,id)  = f(b,a) == id
let isRightInverse(a,b,f,id) = f(a,b) == id
let isInverse(a,b,f,id)      = isLeftInverse(a,b,f,id) && isRightInverse(a,b,f,id)
let inverseOf(b,d,f,id)      = the a in d. isInverse(a,b,f,id)
let hasInverse(b,d,f,id)     = do
  let inverse = inverseOf(b,d,f,id)
  bool(inverse)

// Does each element of d have an f-inverse?
let invertible(f,d) = do
  let id = idOf(f,d)
  forall a in d. hasInverse(a,d,f,id)

let closedUnder(f,d) = forall a in d. !!d[f(a)]
let commutative(f,d) = forall a in d, b in d. f(a,b) == f(b,a)
let idempotent(f,d)  = forall a in d. f(e) == e
let associative(f,d) = forall a,b,c in d. f(f(a,b),c) == f(a,f(b,c))

let algebra(fs,d)  = forall f in fs. closedUnder(f,d)
let semigroup(f,d) = closedUnder(f,d) && associative(f,d)
let monoid(f,d)    = semigroup(f,d) && hasId(f,d)
let group(f,d)     = monoid(f,d) && invertible(f,d)

let abelianMonoid(f,d) = monoid(f,d) && commutative(f,d)
let abelianGroup(f,d)  = group(f,d) && commutative(f,d)

let cancels(f,d)       = forall a,b,c in d. ((c != 0) && (f(c,a) == f(c,b))) => (a == b)
let distributes(f,g,d) = forall a,b,c in d. f(a, g(b,c)) == g(f(a,b), f(a,c))

let integral(f,g,d) =
  algebra([f,g], d) &&
  abelianGroup(a,d) &&
  abelianMonoid(m,d) &&
  cancels(m,d) &&
  distributes(a,m,d)

let reflexive(f,d) = forall a in d. f(a,a)