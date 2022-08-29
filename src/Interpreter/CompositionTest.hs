module Interpreter.CompositionTest where

import Compiler.Core.Inference
import Compiler.Core.Parser
import Compiler.Core.Pretty
import Compiler.Core.Syntax
import qualified Compiler.Core.TypeEnv as TE
import Compiler.Tree.Parser (parseConstituencyTree, parseUnPosConstituencyTree)
import Compiler.Tree.Syntax
import Control.Monad (void)
import Data.Foldable (toList)
import Interpreter.Composition
import Interpreter.Fragment

main :: IO ()
main = do
  -- let fragE = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Duration:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Duration e) subs t) & (p e); [t] = T:<i>"
  -- let fragE = parseFragS "[bindt] = \\t:<i>; \n [V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); \n [NP] = NP:<e>; \n Runtime:<v,i>; \n [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); \n [t] = T:<i>; \n [id] = \\x:<A> . x"
  let fragParse = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Runtime:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); [z] = z; [id] = \\x . x; [bindt] = \\t:<i>; [t] = t; [bindt'] = \\t':<i>; [t'] = t'"
  let treeParse = parseConstituencyTree "[S bindt [TP id [t' [t [X PAST C] t][X bindt' [AspP id [Asp' [Asp PF t'][VP [NP Brutus] [V' [V stab] [NP Caesar]]]]]]]]]"
  -- let treeParse = parseConstituencyTree "(VP (NP Brutus) (V' (V stab) (NP Caesar)))"
  -- let treeParse = parseConstituencyTree "(S (bindt) (TP (id) (T' (T (X (PAST) (C)) (t)) (X (PP (ontheidesofMarch44BC)) (X (bindt') (AspP (id) (Asp' (Asp (PF) (t')) (VP (NP (Caesar)) (V' (V (stab)) (NP (Brutus)))))))))))"
  -- let treeParse = parseConstituencyTree "(S (TP (id) (bindt (T (X (PAST) (C) (Asp (PF) (t'))) (t)) (T' (X (bindt') (AspP (id) (Asp' (VP (NP (Brutus)) (V' (V (stab)) (NP (Caesar)))))))))))"
  -- [s bindt [tp id [t' [t [x PAST C ]t ][x [pp ontheidesofMarch44BC ][x bindt' [aspp id [asp' [asp PF t' ][vp [np Caesar ][v' [v stab ][np Brutus ]]]]]]]]]]
  case fragParse of
    Left err -> print err
    Right decls -> case treeParse of
      Left err -> print err
      Right tree -> do
        -- printTree tree
        let cTree = compose (mkFragment decls) tree
        printTree cTree
        print $ collectCompositionErrors cTree