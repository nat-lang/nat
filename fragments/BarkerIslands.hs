{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module BarkerIslands
  ( islands,
  )
where

import Control.Monad.Memo (MonadMemo, for4, liftM2, memo, startEvalMemo)
import Data.Functor ((<&>))
import Data.List (nub, sort)
import Debug.Trace (trace)
import Text.PrettyPrint
import Prelude hiding ((<>))

abstractionBudget = 2

-- Data structures: Formulas, semantic Terms, syntactic Structures, Proofs:
data Formula
  = DP
  | Sentence
  | Noun
  | T
  | F
  | FS Int Formula Formula
  | BS Int Formula Formula
  deriving (Eq, Show, Ord)

data Term = Operator String | Var Int | Lambda Int Term | Application Term Term deriving (Eq, Show, Ord)

data Structure = Leaf Term Formula | Node Int Structure Structure | I | B | C deriving (Eq, Show, Ord)

data Proof = Proof String Structure Formula [Proof] Term deriving (Show)

instance Eq Proof where
  (Proof _ structA formA _ termA) == (Proof _ structB formB _ termB) =
    structA == structB
      && formA == formB
      && etaReduce termA == etaReduce termB

focus :: Structure -> [Structure]
focus struct = case struct of
  Node int structL structR ->
    [Node 0 struct I] -- Top
      ++ [Node j f (Node int c (Node 0 structR B)) | Node j f c <- focus structL] -- LeftR
      ++ [Node j f (Node int structL (Node 0 c C)) | Node j f c <- focus structR] -- RightR
  _ -> [Node 0 struct I] -- Base

plug :: Structure -> Structure
plug s = case s of
  Node _ f I -> f
  Node m f (Node n c (Node _ r B)) -> Node n (plug (Node m f c)) r
  Node m f (Node n l (Node _ c C)) -> Node n l (plug (Node m f c))
  _ -> s

reducible :: Structure -> Bool -- tests for scope island violation
reducible s = case s of
  Node _ _ I -> True
  Node m f (Node n c (Node _ _ B)) -> m >= n && reducible (Node m f c)
  Node m f (Node n _ (Node _ c C)) -> m >= n && reducible (Node m f c)
  _ -> True

isContext :: Structure -> Bool
isContext (Node _ _ (Node _ _ s)) = s == B || s == C
isContext s = s == I

-- not necessary, but provides significant speedup
polarityBalanced :: Structure -> Formula -> Bool
polarityBalanced l r =
  let pairs = (vals l ++ valf False r)
   in sort [at | (at, True) <- pairs] == sort [at | (at, False) <- pairs]

vals :: Structure -> [(Formula, Bool)]
vals (Node _ l r) = vals l ++ vals r
vals (Leaf _ f) = valf True f
vals _ = []

valf :: Bool -> Formula -> [(Formula, Bool)]
valf b (FS _ l r) = valf b l ++ valf (not b) r
valf b (BS _ l r) = valf (not b) l ++ valf b r
valf b atom = [(atom, b)]

term (Proof s l r ps t) = t

pack :: (Monad m) => [m [Proof]] -> m [Proof]
pack = foldl (liftM2 (++)) (return [])

-- memoized for significant gain in efficiency
-- n is the number of nested abstractions, i is the next unused variable index
prove ::
  (MonadMemo (Structure, Formula, Int, Int) [Proof] m) =>
  Structure ->
  Formula ->
  Int ->
  Int ->
  m [Proof]
prove l r n i =
  let agenda = focus l
   in -- trace (show (prettySeq l r)) $
      if not (polarityBalanced l r)
        then return []
        else
          pack
            [ pack [return [Proof "Ax" l r [] t] | Leaf t r' <- [l], r' == r],
              pack
                [ do
                    xs <- for4 memo prove (plug l) r (n -1) i
                    return [Proof "Red" l r [x] (term x) | x <- xs]
                  | Node m p c <- [l],
                    m > 0,
                    isContext c,
                    reducible l
                ],
              pack
                [ do
                    xs <- for4 memo prove (Node 0 f c) r (n + 1) i
                    return [Proof "EXP" l r [x] (term x) | x <- xs]
                  | Node _ f c <- agenda,
                    n < abstractionBudget
                ],
              pack
                [ do
                    xs <- for4 memo prove (Node m l (Leaf (Var i) b)) c n (i + 1)
                    return [Proof "/R" l r [x] (Lambda i (term x)) | x <- xs]
                  | FS m c b <- [r]
                ],
              pack
                [ do
                    xs <- for4 memo prove (Node m (Leaf (Var i) a) l) c n (i + 1)
                    return [Proof "\\R" l r [x] (Lambda i (term x)) | x <- xs]
                  | BS m a c <- [r]
                ],
              pack
                [ do
                    xs <- for4 memo prove gam a n i
                    pack
                      [ do
                          ys <-
                            for4
                              memo
                              prove
                              (plug (Node m (Leaf (Application tl (term x)) b) c))
                              r
                              n
                              i
                          return [Proof "/L" l r [x, y] (term y) | y <- ys]
                        | x <- xs
                      ]
                  | Node _ (Node m (Leaf tl (FS m' b a)) gam) c <- agenda,
                    m == 0 || m >= m'
                ],
              pack
                [ do
                    xs <- for4 memo prove gam a n i
                    pack
                      [ do
                          ys <-
                            for4
                              memo
                              prove
                              (plug (Node m (Leaf (Application tl (term x)) b) c))
                              r
                              n
                              i
                          return [Proof "\\L" l r [x, y] (term y) | y <- ys]
                        | x <- xs
                      ]
                  | Node _ (Node m gam (Leaf tl (BS m' a b))) c <- agenda,
                    m == 0 || m >= m'
                ]
            ]
            <&> nub

etaReduce :: Term -> Term
etaReduce (Lambda i (Application t (Var i'))) =
  if (i == i') then etaReduce t else (Lambda i (Application (etaReduce t) (Var i')))
etaReduce (Application t1 t2) = Application (etaReduce t1) (etaReduce t2)
etaReduce t = t

-- =============================================================================

prettyProof :: Proof -> Doc
prettyProof (Proof "Ax" l r ps _) = text " " <> prettySeq l r
prettyProof (Proof rule l r [x] _) =
  text "  " <> (prettyProof x $+$ prettySeq l r <> text (" " ++ rule))
prettyProof (Proof rule l r [x, y] _) =
  text "  "
    <> ( prettyProof x $+$ prettyProof y $+$ prettySeq l r
           <> text (" _" ++ rule)
       )

prettySeq :: Structure -> Formula -> Doc
prettySeq s f = prettyStructure s <+> text "|-" <+> prettyFormula f

vars = "xyz" ++ ['a' .. 'w']

prettyStructure :: Structure -> Doc
prettyStructure (Leaf (Var i) Sentence) = char (vars !! i)
prettyStructure (Leaf t@(Operator word) _) = prettyTerm t
prettyStructure (Leaf t f) = prettyFormula f
prettyStructure s@(Node m l r) = prettyNode 0 s
prettyStructure s = text (show s)

prettyNode, prettyNode' :: Int -> Structure -> Doc
prettyNode v s@(Node m l r) =
  if isContext s
    then
      text "\\" <> (char (vars !! v))
        <> prettyNode' (v + 1) (plug (Node m (Leaf (Var v) Sentence) s))
    else
      prettyNode' v l
        <+> text (show m)
        <+> prettyNode' v r
prettyNode' v s@(Node _ _ _) = parens (prettyNode v s)
prettyNode' v s = prettyStructure s

prettyFormula, prettyFor' :: Formula -> Doc
prettyFormula (BS m l r) =
  prettyFor' l <> text "\\" <> text (show m) <> prettyFor' r
prettyFormula (FS m l r) = prettyFor' l <> text "/" <> text (show m) <> prettyFor' r
prettyFormula a = text (show a)
prettyFor' a@(FS _ _ _) = parens (prettyFormula a)
prettyFor' a@(BS _ _ _) = parens (prettyFormula a)
prettyFor' a = prettyFormula a

prettyTerm :: Term -> Doc
prettyTerm (Operator s) = text s
prettyTerm (Var i) = char (vars !! i)
prettyTerm (Lambda i t) = parens (char '\\' <> char (vars !! i) <+> prettyTerm t)
prettyTerm (Application t1 t2) = parens ((prettyTerm t1) <+> (prettyTerm t2))

-- =============================================================================

ann = Leaf (Operator "ann") DP

bill = Leaf (Operator "bill") DP

dog = Leaf (Operator "dog") Noun

left = Leaf (Operator "left") (BS 0 DP Sentence)

the = Leaf (Operator "the") (FS 0 DP Noun)

-- (DP \ Sentence) / DP
saw = Leaf (Operator "saw") (FS 0 (BS 0 DP Sentence) DP)

noone = Leaf (Operator "no one") (FS 0 Sentence (BS 0 DP Sentence))

-- Sentence / (DP \ Sentence)
everyone = Leaf (Operator "everyone") (FS 0 Sentence (BS 1 DP Sentence))

anyone = Leaf (Operator "anyone") (FS 0 Sentence (BS 2 DP Sentence))

-- Sentence / (DP \ Sentence)
someone = Leaf (Operator "someone") (FS 0 Sentence (BS 3 DP Sentence))

ensured = Leaf (Operator "ensured") (FS 1 (BS 0 DP Sentence) Sentence)

thought = Leaf (Operator "thought") (FS 2 (BS 0 DP Sentence) Sentence)

doubts = Leaf (Operator "doubts") (FS 3 (BS 0 DP Sentence) Sentence)

only = Leaf (Operator "only") (FS 4 (BS 0 DP Sentence) F)

foc = Leaf (Operator "foc") (FS 0 (FS 0 F (BS 4 DP (BS 0 DP Sentence))) DP)

-- T /
damn = Leaf (Operator "damn") (FS 0 T (BS 4 (FS 0 Noun Noun) Sentence))

same = Leaf (Operator "same") (FS 0 (BS 1 DP Sentence) (BS 1 (FS 0 Noun Noun) (BS 1 DP Sentence)))

s1 = (Node 0 everyone (Node 0 saw (Node 0 the (Node 0 same dog))), Sentence)

s2 = (Node 0 (Node 0 the (Node 0 same dog)) (Node 0 saw everyone), Sentence)

s3 =
  ( Node
      0
      ann
      ( Node
          2
          thought
          ( Node
              0
              (Node 0 the (Node 0 same dog))
              (Node 0 saw everyone)
          )
      ),
    Sentence
  )

s4 = (Node 0 (Node 0 the (Node 0 same dog)) (Node 0 saw ann), Sentence)

s5 = (Node 0 ann (Node 0 saw everyone), Sentence)

ex81 = (Node 0 someone (Node 1 ensured (Node 0 noone left)), Sentence)

ex82 = (Node 0 someone (Node 1 ensured (Node 0 everyone left)), Sentence)

ex83 = (Node 0 ann (Node 2 thought (Node 0 everyone left)), Sentence)

ex84 = (Node 0 ann (Node 2 thought (Node 0 someone left)), Sentence)

ex85 = (Node 0 ann (Node 3 doubts (Node 0 anyone left)), Sentence)

ex86 = (Node 0 ann (Node 3 doubts (Node 0 someone left)), Sentence)

ex87 =
  ( Node
      0
      ann
      ( Node
          4
          only
          ( Node
              2
              thought
              (Node 0 someone (Node 0 saw (Node 0 foc bill)))
          )
      ),
    Sentence
  )

ex88 =
  ( Node
      0
      ann
      ( Node
          4
          only
          ( Node
              2
              thought
              ( Node
                  0
                  (Node 0 the (Node 0 damn dog))
                  (Node 0 saw (Node 0 foc bill))
              )
          )
      ),
    T
  )

-- =============================================================================

try :: (Structure, Formula) -> [Doc]
try (s, f) =
  map
    ( \p ->
        text "\n" <> prettyTerm (term p)
          $+$ prettyTerm (etaReduce (term p))
          $+$ prettyProof p
          --  $+$ text "\n" -- uncomment to see derivation
    )
    (startEvalMemo (prove s f 0 0))

islands = print $ map try [ex81, ex82, ex83, ex84, ex85, ex86, ex87, ex88]

-- =============================================================================

prettyFocus :: (Structure, Formula) -> [Doc]
prettyFocus (ss, f) = fmap (\s' -> text "\n" <> prettyStructure s') (focus ss)

focused = print $ map prettyFocus [s1, s2, s3, s4]