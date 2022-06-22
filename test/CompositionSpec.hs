module Interpreter.CompositionSpec where
import Data.Foldable (toList)
import Control.Monad (void)
-- import Test.Hspec
import Interpreter.Compose
import Interpreter.Fragment
import Compiler.Core.Syntax
import Compiler.Core.Parser
import Compiler.Core.Inference
import Compiler.Core.Pretty
import qualified Compiler.Core.TypeEnv as TE
import Compiler.Tree.Syntax
import Compiler.Tree.Parser (parseConstituencyTree, parseUnPosConstituencyTree)

main :: IO ()
main = do

  -- let fragE = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Duration:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Duration e) subs t) & (p e); [t] = T:<i>"
  -- let fragE = parseFragS "[bindt] = \\t:<i>; \n [V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); \n [NP] = NP:<e>; \n Runtime:<v,i>; \n [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); \n [t] = T:<i>; \n [id] = \\x:<A> . x"
  let fragParse = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Runtime:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); [z] = z; [id] = \\x:<A> . x; [bindt] = \\z:<i>"
  let treeParse = parseConstituencyTree "(S (bindt) (TP (id) (T' (T (X (PAST) (C)) (t)) (X (PP (ontheidesofMarch44BC)) (X (bindt') (AspP (id) (Asp' (Asp (PF) (t')) (VP (NP (Caesar)) (V' (V (stab)) (NP (Brutus)))))))))))"

  case fragParse of
    Left err -> print err
    Right decls -> case treeParse of
      Left err -> print err
      Right tree -> do
        printTree tree
        printTree $ compose (mkFragment decls) tree