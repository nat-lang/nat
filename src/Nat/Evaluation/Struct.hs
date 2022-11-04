{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Nat.Evaluation.Struct where

import Control.Monad (foldM, replicateM)
import Data.List (foldl', permutations)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Nat.Control (allM, anyM, findM)
import Nat.Evaluation.Surface
import Nat.Reduction
import Nat.Syntax.Surface hiding ((>))
import qualified Nat.Syntax.Surface as S
import Prelude hiding ((*))

type ExprSet = Set.Set Expr

-- | A `Struct` is a domain with n operations.
data Struct = Struct Expr [Expr] deriving (Show)

mkI = ELit . LInt

arity e = case e of
  EFun {} -> arity' e
  ELam {} -> arity' e
  e -> error ("expected a function but got: " ++ show e)
  where
    arity' = \case
      EFun bndrs _ -> length bndrs
      ELam b e -> 1 + arity' e
      _ -> 0

binary f = arity f == 2

-- | k-combination of a set
pDom k s = replicateM k (Set.toList s)

-- | k-combination of a domain where k = arity(f)
aDom f = pDom (arity f)

ap :: Expr -> [Expr] -> Reduction Expr ExprEvalError ExprReductionEnv
ap f@EFun {} args = reduce (EInv f args)
ap f@ELam {} args = reduce (foldl' EApp f args)

bReduce e = bool <$> reduce e

eq e0 e1 = bReduce (e0 === e1)

-- | Map a monadic 1-ary predicate over a set
allM1 s p = allM p (Set.toList s)

findM1 s p = findM p (Set.toList s)

isLeftId :: Expr -> ExprSet -> Expr -> ExprReductionT Bool
isLeftId e d f = allM1 d (\a -> eq (f * e * a) a)

isRightId :: Expr -> ExprSet -> Expr -> ExprReductionT Bool
isRightId e d f = allM1 d (\a -> eq (f * a * e) a)

isId :: Expr -> ExprSet -> Expr -> ExprReductionT Bool
isId e d f = do
  l <- isLeftId e d f
  r <- isRightId e d f
  return (l && r)

-- | Find the first id of `f` in `d`
idOf f d = findM1 d (\e -> isId e d f)

-- | Has `f` an id in `d`?
hasId f d = isJust <$> idOf f d

-- | Is `b` an `f`-left inverse of `a` for `id`?
isLeftInverse f id a b = (f * b * a) `eq` id

-- | Is `b` an `f`-right inverse of `a` for `id` in `d`?
isRightInverse f id a b = (f * a * b) `eq` id

isInverse f id a b = do
  l <- isLeftInverse f id a b
  r <- isRightInverse f id a b
  return (l && r)

inverseOf f id d a = findM1 d (isInverse f id a)

hasInverse f id d a = isJust <$> inverseOf f id d a

-- | Does each element of `d` have an `f`-inverse?
invertible f _ | not (binary f) = return False
invertible f d = do
  mId <- idOf f d
  case mId of
    Nothing -> return False
    Just id -> allM1 d (hasInverse f id d)

commutative f d = allM sym (pDom 2 d)
  where
    sym [a, b] = (f * a * b) `eq` (f * b * a)

idempotent f d = allM1 d (\e -> (f * e) `eq` e)

associative f _ | not (binary f) = return False
associative f d = allM sym (pDom 3 d)
  where
    sym [a, b, c] = (f * (f * a * b) * c) `eq` (f * a * (f * b * c))

closedUnder :: Expr -> ExprSet -> ExprReductionT Bool
closedUnder f s = allM into (aDom f s)
  where
    into :: [Expr] -> ExprReductionT Bool
    into as = Set.member <$> ap f as <*> pure s

eachM ms = and <$> sequenceA ms

is :: (Struct -> ExprReductionT Bool) -> Struct -> Either ExprEvalError Bool
is p s = runReduction (p s) mkReductionEnv

algebra :: Struct -> ExprReductionT Bool
algebra (Struct (ESet d) fs) = allM (`closedUnder` d) fs

semigroup :: Struct -> ExprReductionT Bool
semigroup (Struct _ fs) | length fs /= 1 = return False
semigroup s@(Struct (ESet d) [f]) = eachM [algebra s, associative f d]

monoid :: Struct -> ExprReductionT Bool
monoid s@(Struct (ESet d) [f]) = eachM [semigroup s, hasId f d]

group :: Struct -> ExprReductionT Bool
group s@(Struct (ESet d) [f]) = eachM [monoid s, invertible f d]
group s = error ("unexpected struct: " ++ show s)

abelianMonoid s@(Struct (ESet d) [f]) = eachM [monoid s, commutative f d]

abelianGroup s@(Struct (ESet d) [f]) = eachM [group s, commutative f d]

cancellation f d = allM p (pDom 3 d)
  where
    z = mkI 0
    p [a, b, c] = bReduce ((c !== z &&& ((f * c * a) === (f * c * b))) ==> (a === b))

distribution f0 f1 d = allM p (pDom 3 d)
  where
    p [a, b, c] = (f0 * a * (f1 * b * c)) `eq` (f1 * (f0 * a * b) * (f0 * a * c))

integral str@(Struct d@(ESet s) [a, m]) =
  eachM
    [ algebra str,
      abelianGroup (Struct d [a]),
      abelianMonoid (Struct d [m]),
      cancellation m s,
      distribution a m s
    ]

reflexive f _ | not (binary f) = return False
reflexive f d = allM1 d (\e -> bReduce (f * e * e))

symmetric f d = allM sym (pDom 2 d)
  where
    sym x y = bReduce ((f * x === y) ==> (f * y === x))

-- antiSymmetric f d

transitive f d = allM tran (pDom 3 d)
  where
    tran x y z = bReduce (f * x ===)

partiallyOrdered f d = allM p (pDom 2 d)
  where
    p = 
-- chain

-- continuous