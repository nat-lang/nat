{-# LANGUAGE FlexibleContexts #-}

module FOL where

import Control.Monad.Error
import Data.Map (Map, (!))
import Data.Maybe (catMaybes)
import Data.Unique
import System.IO.Unsafe
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr

import qualified Data.List as L
import qualified Data.Map as M

---------------
-- Utilities --
---------------

-- |Join a list of strings, separating them with commas.
commaSep :: [String] -> String
commaSep xs = concat $ L.intersperse "," xs

-- |Return 'True' if the list is not null.
notNull :: [a] -> Bool
notNull = not . null

-- |Return all subsets of a list.
subsets :: [a] -> [[a]]
subsets = filterM $ const [True,False]

----------------
-- Data Types --
----------------

type ThrowsError   = Either LogicError

data LogicError    = ParseError
                   | InvalidExpression
                   | UnknownCommand
                   | DefaultError deriving (Show)

instance Error LogicError where noMsg = DefaultError

-----------------
-- Expressions --
-----------------

-- |Class for logical expressions, supporting only a single method 'parseExpr'
--  which parses a 'String' into an expression.
class (Show p, Eq p) => Expr p where
    -- |Parse a 'String' as a logical expression.
    parseExpr :: String -> ThrowsError p

--------------------
-- Knowledge Base --
--------------------

-- |A class for knowledge bases, supporting operations 'tell' (to tell a new
--  fact), 'ask' (to query the knowledge base) and 'retract' (to un-tell a
--  fact). Instances of 'KB' are used in routines in the @Logic.Interative@
--  package.
class (Expr p, Show t) => KB k p t where
    -- |Returns an empty knowledge base.
    empty :: k p t

    -- |Store a new fact in the knowledge base.
    tell :: k p t -> p -> k p t

    -- |Ask whether a particular statement is entailed by the knowledge base.
    ask :: k p t -> p -> Bool

    -- |Given a statement containing variables, return an assignment of
    --  variables to terms that satisfies the statement.
    askVars :: k p t -> p -> [Map String t]

    -- |Retract a fact from the knowledge base.
    retract :: k p t -> p -> k p t

    -- |List the propositions stored in the knowledge base.
    axioms :: k p t -> [p]

    -- |Ask if the knowledge base contains a particular fact.
    contains :: k p t -> p -> Bool
    contains kb p = p `elem` axioms kb

----------------
-- Data Types --
----------------

data Term = Var String
          | Sym String
          | Func String [Term]
          deriving (Eq,Ord)

data FOLExpr = Val Bool
             | Pred String [Term]
             | Not FOLExpr
             | And [FOLExpr]
             | Or  [FOLExpr]
             | Implies FOLExpr FOLExpr
             | ForAll String FOLExpr
             | Exists String FOLExpr
             deriving (Eq,Ord)

instance Show Term where
    show (Var x)     = x
    show (Sym x)     = x
    show (Func f xs) = f ++ "(" ++ commaSep (map show xs) ++ ")"

instance Show FOLExpr where
    show (Val True)    = "T"
    show (Val False)   = "F"
    show (Not p)       = "~" ++ show p
    show (And ps)      = "(" ++ (concat $ L.intersperse " & " $ map show ps) ++ ")"
    show (Or ps)       = "(" ++ (concat $ L.intersperse " | " $ map show ps) ++ ")"
    show (Implies p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
    show (ForAll x p)  = "forall " ++ x ++ ". " ++ show p
    show (Exists x p)  = "exists " ++ x ++ ". " ++ show p
    show (Pred p xs)   = p ++ "(" ++ commaSep (map show xs) ++  ")"

instance Expr FOLExpr where
    parseExpr = parseFOL

instance Expr DefiniteClause where
    parseExpr str = parseFOL str >>= toDefiniteClause

data FCKB p t = FCKB [DefiniteClause]

instance KB FCKB DefiniteClause Term where
    empty               = FCKB []
    tell    (FCKB cs) c = FCKB (cs ++ [c])
    ask     (FCKB cs) c = not . null $ fc cs (conclusion c)
    askVars (FCKB cs) c = fc cs (conclusion c)
    retract (FCKB cs) c = FCKB (L.delete c cs)
    axioms  (FCKB cs)   = cs

conjuncts :: FOLExpr -> [FOLExpr]
conjuncts (And ps) = ps
conjuncts e        = [e]

isPred :: FOLExpr -> Bool
isPred (Pred _ _) = True
isPred _          = False

termVars :: Term -> [String]
termVars (Var x) = [x]
termVars (Sym _) = [ ]
termVars (Func _ args) = L.nub (concatMap termVars args)

-----------------
-- Unification --
-----------------

data UnificationExpr = UVar String
                     | UTerm Term
                     | UExpr FOLExpr
                     | UList [UnificationExpr]
                     deriving (Eq)

-- |Unify expressions @x@, @y@ with subsitution @theta@. Return either a
--  substitution that would make @x@, @y@ equal, or 'Nothing' if @x@ and @y@
--  cannot unify.
unify :: FOLExpr -> FOLExpr -> Maybe (Map String Term)
unify x y = unify' (UExpr x) (UExpr y) (Just M.empty)

unify' :: UnificationExpr -> UnificationExpr -> Maybe (Map String Term) -> Maybe (Map String Term)
unify' x y  Nothing = Nothing
unify' x y (Just theta)
    | x == y  = Just theta
    | isVar x = unifyVar (getVar x) y theta
    | isVar y = unifyVar (getVar y) x theta
    | isExpr x && isExpr y =
        unify' (getArgs x) (getArgs y) (unify' (getOp x) (getOp y) (Just theta))
    | isList x && isList y && (getLength x == getLength y) = 
        unify' (getRest x) (getRest y) (unify' (getHd x) (getHd y) (Just theta))
    | otherwise = Nothing

unifyVar :: String -> UnificationExpr -> Map String Term -> Maybe (Map String Term)
unifyVar var x theta
    | M.member var theta = unify' (UTerm $ theta ! var) x (Just theta)
    | isVar x && M.member (getVar x) theta =
        unify' (UVar var) (UTerm $ theta ! getVar x) (Just theta)
    | occurCheck var x   = Nothing
    | otherwise          = Just (M.insert var (getTerm x) theta)

-- |Return 'True' if @var@ occurs anywhere in @x@.
occurCheck :: String -> UnificationExpr -> Bool
occurCheck var x
    | isVar x   = getVar x == var
    | isExpr x  = occurCheck var (getArgs x)
    | isList x  = any (occurCheck var) (getList x)
    | otherwise = False

----------------------
-- Definite Clauses --
----------------------

data Statement = Statement { symbol :: String
                           , args :: [Term] } deriving (Eq)

data DefiniteClause = DC { premises :: [Statement]
                         , conclusion :: Statement } deriving (Eq)

instance Show Statement where
    show (Statement sym []  ) = sym
    show (Statement sym args) = sym ++ "(" ++ commaSep (map show args) ++  ")"

instance Show DefiniteClause where
    show (DC []    conc) = show conc
    show (DC prems conc) =
        concat (L.intersperse " & " $ map show prems) ++ " => " ++ show conc

isFact :: DefiniteClause -> Bool
isFact = null . premises

toFact :: Statement -> DefiniteClause
toFact s = DC [] s

facts :: [DefiniteClause] -> [Statement]
facts = map conclusion . filter isFact

rules :: [DefiniteClause] -> [DefiniteClause]
rules = filter (not . isFact)

toStatement :: FOLExpr -> Statement
toStatement (Pred sym args) = Statement sym args

toDefiniteClause :: FOLExpr -> ThrowsError DefiniteClause
toDefiniteClause (Pred sym args) = return $ DC [] (Statement sym args)
toDefiniteClause (Implies p q)   = if all isPred xs && isPred q
    then return $ DC (map toStatement xs) (toStatement q)
    else throwError InvalidExpression
    where
        xs = conjuncts p

toExpr :: DefiniteClause -> FOLExpr
toExpr (DC [] c) = statementToExpr c
toExpr (DC ps c) = And (map statementToExpr ps) `Implies` statementToExpr c

statementToExpr :: Statement -> FOLExpr
statementToExpr s = Pred (symbol s) (args s)

clauseVars :: DefiniteClause -> [String]
clauseVars (DC prems conc) =
    L.nub $ concatMap statementVars prems ++ statementVars conc

statementVars :: Statement -> [String]
statementVars (Statement _ args) = L.nub $ concatMap termVars args

subst :: DefiniteClause -> Map String Term -> DefiniteClause
subst (DC ps c) m = DC (map renameS ps) (renameS c)
    where
        renameS (Statement sym args) = Statement sym (map renameT args)

        renameT (Var x) = case M.lookup x m of
                            Nothing -> Var x
                            Just t  -> t
        renameT (Sym x) = Sym x
        renameT (Func x args) = Func x (map renameT args)

stUnify :: [Statement] -> [Statement] -> Maybe (Map String Term)
stUnify s1 s2 = unify (expr s1) (expr s2)
    where
        expr s = And $ L.sort $ map (toExpr . toFact) s

----------------------
-- Forward Chaining --
----------------------

fc :: [DefiniteClause] -> Statement -> [Map String Term]
fc kb a = unsafePerformIO $ go (facts kb) (rules kb)
    where
        go known rules = do
            (result, new) <- applyRules known rules
            if null new
                then return result
                else do
                    (result', new') <- applyRules (known ++ new) rules
                    return (result' ++ result)

        applyRules known []          = return ([],[])
        applyRules known (rule:rest) = do
            DC ps q <- standardizeApart rule

            let subs         = getSubstitutions ps known
                (res1, new1) = makeInferences q known [] [] subs

            (res2, new2) <- applyRules known rest
            return (res1++res2, new1++new2)

        makeInferences _ known new result []     = (result, new)
        makeInferences q known new result (t:ts) = if isRenaming q' (known ++ new)
            then makeInferences q known new result ts
            else makeInferences q known new' result' ts
            where
                q'      = conclusion (subst (toFact q) t)
                new'    = q' : new
                result' = case stUnify [q'] [a] of
                    Nothing -> result
                    Just r  -> r:result

isRenaming :: Statement -> [Statement] -> Bool
isRenaming s kb = notNull $ catMaybes $ map (stUnify [s]) (map return kb)

getSubstitutions :: [Statement] -> [Statement] -> [Map String Term]
getSubstitutions ps kb = catMaybes $ map (stUnify ps) (subsets kb)
          
----------------------
-- Rename Variables --
----------------------

standardizeApart :: DefiniteClause -> IO DefiniteClause
standardizeApart dc = uniqify (clauseVars dc) >>= return . subst dc

uniqify :: [String] -> IO (Map String Term)
uniqify = foldM func M.empty
    where func m s = do s' <- mkUnique s
                        return $ M.insert s (Var s') m

mkUnique :: String -> IO String
mkUnique s = newUnique >>= \n -> return (':' : s ++ show (hashUnique n))

----------------------------------
-- Unification Helper Functions --
----------------------------------

isVar :: UnificationExpr -> Bool
isVar (UVar _) = True
isVar _        = False

getVar :: UnificationExpr -> String
getVar (UVar x) = x
getVar _        = error "Not a variable -- AI.Logic.FOL.getVar"

isTerm :: UnificationExpr -> Bool
isTerm (UTerm _) = True
isTerm _         = False

getTerm :: UnificationExpr -> Term
getTerm (UTerm t) = t
getTerm _         = error "Not a term -- AI.Logic.FOL.getTerm"

isExpr :: UnificationExpr -> Bool
isExpr (UExpr _) = True
isExpr _         = False

getArgs :: UnificationExpr -> UnificationExpr
getArgs (UExpr x) = UList (go x)
    where go (Not p)       = map UExpr [p]
          go (And ps)      = map UExpr ps
          go (Or ps)       = map UExpr ps
          go (Implies p q) = map UExpr [p,q]
          go (Pred p ts)   = map toU ts
            where toU (Var x) = UVar x
                  toU term    = UTerm term
getArgs _ = error "Not an expression -- AI.Logic.FOL.getArgs"

getOp :: UnificationExpr -> UnificationExpr
getOp (UExpr x) = UTerm $ Sym (go x)
    where go (Not p)       = "Not"
          go (And ps)      = "And"
          go (Or ps)       = "Or"
          go (Implies p q) = "Implies"
          go (Pred p ts)   = p
getOp _ = error "Not an expression -- AI.Logic.FOL.getOp"

isList :: UnificationExpr -> Bool
isList (UList _) = True
isList _         = False

getList :: UnificationExpr -> [UnificationExpr]
getList (UList xs) = xs
getList _          = error "Not a list -- AI.Logic.FOL.getList"

getLength :: UnificationExpr -> Int
getLength (UList xs) = length xs
getLength _          = error "Not a list -- AI.Logic.FOL.getLength"

getHd :: UnificationExpr -> UnificationExpr
getHd (UList xs) = head xs
getHd _          = error "Not a list -- AI.Logic.FOL.getHd"

getRest :: UnificationExpr -> UnificationExpr
getRest (UList xs) = UList (tail xs)
getRest _          = error "Not a list -- AI.Logic.FOL.getRest"

---------------------
-- Associate Exprs --
---------------------

-- |Exploit the associative and commutative rules for conjunction and
--  disjunction to simplify statements in first-order logic. This function is
--  called after parsing a statement. The purpose is to simplify the unification
--  algorithm by ensuring that all arguments to an 'And' or 'Or' statement
--  appear in a consistent order, and avoid having to write a more complicated
--  AC- or ACU-unification routine.
associate :: FOLExpr -> FOLExpr
associate (And ps) = And $ L.sort $ foldr f [] (map associate ps)
    where f (And xs) ys = xs ++ ys
          f expr ys     = expr : ys
associate (Or ps)  = Or  $ L.sort $ foldr f [] (map associate ps)
    where f (Or xs) ys = xs ++ ys
          f expr ys    = expr : ys
associate (Not p)       = Not (associate p)
associate (Implies p q) = Implies (associate p) (associate q)
associate (ForAll x p)  = ForAll x (associate p)
associate (Exists x p)  = Exists x (associate p)
associate expr          = expr

------------------------------
-- First-Order Logic Parser --
------------------------------

parseFOL :: String -> ThrowsError FOLExpr
parseFOL input = case parse expr "" input of
    Left _  -> Left ParseError
    Right x -> return (associate x)

expr :: Parser FOLExpr
expr = buildExpressionParser table term <?> "expression"

term :: Parser FOLExpr
term = parens expr
   <|> try parseForAll
   <|> try parseExists
   <|> try parsePred
   <|> parseVal
   <?> "T, F, Predicate, ForAll or Exists"

parseTerm :: Parser Term
parseTerm = try parseFunc
        <|> parseVar
        <|> parseSym
        <?> "Variable, Symbol or Function"

parseVar :: Parser Term
parseVar = do
    x <- name
    return $ Var x

parseSym :: Parser Term
parseSym = do
    x <- capsName
    return $ Sym x

parseFunc :: Parser Term
parseFunc = do
    x  <- capsName
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Func x ts

parseVal :: Parser FOLExpr
parseVal = (char 'T' >> return (Val True))
       <|> (char 'F' >> return (Val False))
       <?> "T or F"

parsePred :: Parser FOLExpr
parsePred = do
    x  <- capsName
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Pred x ts

parseForAll :: Parser FOLExpr
parseForAll = do
    string "forall" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ ForAll x e

parseExists :: Parser FOLExpr
parseExists = do
    string "exists" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ Exists x e

capsName :: Parser String
capsName = do
    c  <- upper
    cs <- many alphaNum
    return (c:cs)

name :: Parser String
name = do
    c  <- lower
    cs <- many alphaNum
    return (c:cs)

parens :: Parser a -> Parser a
parens p = do
    char '(' >> spaces
    x <- p
    spaces >> char ')'
    return x

table = 
    [ [prefix "~" Not]
    , [binary "&" (\x y -> And [x,y]) AssocLeft]
    , [binary "|" (\x y -> Or [x,y]) AssocLeft]
    , [binary "=>" Implies AssocRight] ]

binary name fun assoc = Infix (do{ string name; return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })
