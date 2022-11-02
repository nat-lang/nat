module Nat.Evaluation.Struct where

import Control.Monad
import Data.List (permutations)
import qualified Data.Set as Set
import Nat.Evaluation.Surface
import Nat.Reduction
import Nat.Syntax.Surface hiding ((*))
import qualified Nat.Syntax.Surface as S

-- | A `Struct` is a domain and n operations.
data Struct = Struct (Domain Expr) [Expr]

arity (EFun bndrs _) = length bndrs
arity _ = error "expected a function"

binary f = arity f == 2

pDom n (Dom _ es) = replicateM n (Set.toList es)

-- | All the possible arguments of the function `f` in the domain.
aDom f d = pDom (arity f) d

ap f args = reduce (foldl' (S.*) f args)

eq e0 e1 = bool <$> reduce (e0 === e1)

leftIdentity f e d = allM  (pDom 1 d)
rightIdentity
identity

{-
commutative
idempotent

leftInverse
rightInverse
inverse
-}

associative f d | binary f = allM (sym f) (pDom d 3)
  where
    sym [a, b, c] = (f * (f * a * b) * c) `eq` (f * a * (f * b * c))
associative _ _ = return False

closedUnder f d@(Dom _ es) = allM into (aDom f d)
  where
    into as = Set.member <$> ap f as <*> es

algebra (Struct d fs) = allM (`closedUnder` d) fs

group s@(Struct d [f]) = and [algebra s, associative f d, ]
group _ _ = False