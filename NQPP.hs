module NQPP where

import Data.Function
import Data.List hiding (lookup)
import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Data.String
import Prelude hiding (lookup)


--
-- * Abstract syntax
--

type Var = String

-- Abstract syntax of expressions
--
--     exp  ::=   value
--            |   exp >= exp
--            |   exp <= exp
--            |   exp + exp
--            |   exp - exp
--            |   exp * exp
--            |   exp / exp

-- Operator Precedence (higher number is greater precedence)
-- infixl 5 :>=:, :<=:
-- infixl 6 :+:, :-:
-- infixl 7 :*:, :/:

-- data Exp
--   = CI Int
-- 	| CB Bool
-- 	| CF Float
--   | CS String
-- 	|	V Var        -- variable
--   | Exp :>=: Exp   -- Greater than or Equal t
--   | Exp :<=: Exp   -- Less than or Equal to
--   | Exp :+: Exp    -- addition
--   | Exp :-: Exp    -- subtraction
--   | Exp :*: Exp    -- multiplication
--   | Exp :/: Exp    -- division
-- 	deriving (Eq,Show)


-- | 2 * (3 + 4) ==> 14
-- ex1 :: [Exp]
-- ex1 = [CI 3 :+: CI 4, (CI 2 :*:) ]
--
-- -- | 2 * (3 + 4) ==> 10
-- ex2 :: Exp
-- ex2 = CI 2 :*: (CI 3 :+: CI 4)

-- -- | 1 + 1 ==> 2
-- ex3 :: Type
-- ex3 = smt(CI 1 :+: CI 1)
--
-- -- | 1 - 1 ==> 0
-- ex4 :: Type
-- ex4 = smt(CI 1 :-: CI 1)
--
-- -- | 2.5 * 2.5 ==> 6.25
-- ex5 :: Type
-- ex5 = smt(CF 2.5 :*: CF 2.5)
--
-- -- | 2 / 2 ==> 1
-- ex6 :: Type
-- ex6 = smt(CI 2 :/: CI 2)
--
-- -- | 6 / 2.5 ==> 2.4
-- ex7 :: Type
-- ex7 = smt(CI 6 :/: CF 2.5)
--
-- -- | "Hello " + "World" ==> "Hello World"
-- ex8 :: Type
-- ex8 = smt (CS "Hello " :+: CS "World")



-- Abstract syntax of values
--
--     value  ::=   int
--            |     bool
--            |     float
--            |     string
--
-- data Type
--    = TInt Int
--    | TBool Bool
--    | TFloat Float
--    | TString String
--    | TError
--   deriving (Eq,Show)

-- type Decl = (Var,Type)


infix 1 :=

data Stmt
  = Var := Exp      -- assignment
  -- | While Exp Stmt  -- loop
	-- | Eval Exp
  -- | Seq [Stmt]      -- sequence

-- type Prog = Stmt
-- type Val = Int
-- type Assign = [(Var, Exp)]





data Value
   = Int
   | Bool
   | Float
   | String
  deriving (Eq,Show)

data Exp
   = Lit Value
   | Add Exp Exp
   | Mul Exp Exp
   | Equ Exp Exp
   | If  Exp Exp Exp
  deriving (Eq,Show)

  --
  -- * Type system
  --

  -- | Variable environments. An environment maps variable names to the
  --   things those variable names are bound to. During typing, each name
  --   will be bound to a type, while during evaluation (semantics), each
  --   name will be bound to a value.
type Env a = Map Var a

data Type = TBool | TInt | TFloat | TString
  deriving (Eq,Show)

-- ex1 = [
--       a = 8
--       b = 2.1
--       while b < a
--         b = b :+: 1
--         a = a :-: 1
--      print a
-- ]

-- 8 :+: 2.1
-- (Add (I 8) (F 2.1))

-- 2. Define the typing relation.
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _)   _ = Just TInt
typeExpr (Add l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         (Just TFloat, TFloat) -> Just TFloat
                         (Just TBool, Just TBool) -> Just TBool
                         (Just TString, TString) -> Just TString
                         _                      -> Nothing
typeExpr (Mul l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         (Just TFloat, Just TFloat) -> Just TFloat
                         (Just TInt, Just TFloat) -> Just TFloat
                         (Just TFloat, Just TInt) -> Just TFloat
                         _                      -> Nothing
typeExpr (Not e)   m = case typeExpr e m of
                         Just TBool -> Just TBool
                         _          -> Nothing
typeExpr (Ref v)   m = lookup v m

typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e)   m = case (lookup v m, typeExpr e m) of
                            (Just tv, Just te) -> tv == te
                            _ -> False
typeStmt (If c st se) m = case typeExpr c m of
                            Just TBool -> typeStmt st m && typeStmt se m
                            _ -> False
typeStmt (While c sb) m = case typeExpr c m of
                            Just TBool -> typeStmt sb m
                            _ -> False
typeStmt (Block ss)   m = all (\s -> typeStmt s m) ss

typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)




-- | The basic values in our language.
type Val = Either (Either Int Float) (Either Bool String)

-- | Semantics of type-correct expressions. Note that since we assume the
--   expression is statically type correct (otherwise it would have failed
--   type checking and we never try to evaluate it), we do not need to
--   explicitly represent the error case with a Maybe type. Also note that
--   our environment now contains the *value* that each name is bound to.
--   Since expressions can refer to variables but not change them, the
--   environment is read-only (i.e. it's an input but not an output of the
--   function).


evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i)   _ = Left i
evalExpr (Add l r) m = Left (evalInt l m + evalInt r m)
evalExpr (Add l r) m = Left (evalInt l m + evalInt r m)

evalExpr (LTE l r) m = Right (evalInt l m <= evalInt r m)
evalExpr (Not e)   m = Right (not (evalBool e m))
evalExpr (Ref x)   m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"

-- | Helper function to evaluate an expression to an integer. Note that
--   in all cases, we should only get an "internal error" if we try to
--   evaluate an expression that didn't pass the static type checker we
--   wrote above.
evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                Left i  -> i
                Right _ -> error "internal error: expected Int got Bool"

-- | Helper function to evaluate an expression to a Boolean.
evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"

-- | Semantics of statements. Statements update the bindings in the
--   environment, so the semantic domain is 'Env Val -> Env Val'. The
--   bind case is the case that actually changes the environment. The
--   other cases should look similar to other examples you've seen.
evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e)   m = insert x (evalExpr e m) m
evalStmt (If c st se) m = if evalBool c m
                          then evalStmt st m
                          else evalStmt se m
evalStmt (While c sb) m = if evalBool c m
                          then evalStmt (While c sb) (evalStmt sb m)
                          else m
evalStmt (Block ss)   m = evalStmts ss m

-- | Helper function to evaluate a list of statements. We could also
--   have used 'foldl' here.
evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)

-- | Semantics of programs. This runs a program with an initial
--   environment where all integer variables are initialized to 0, and
--   all Boolean variables are initialized to false.
evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (map (\(x,t) -> (x, init t)) ds)
    init TInt  = Left 0
    init TBool = Right False

-- | Type check and then run a program.
runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)
                          else Nothing






--
-- List of statements
-- Each statement can be a declaration, an if statement or a while loop
--   Here the statement is type checked and run through semantics stuffs
--   Ensures type correct stuff goes through and runs in the right add/sub/mult/div statements
--
-- Program goes through each statement and runs it
-- Saves state in an environment
-- Continues thru the envs until it reaches the end and returns a value or whatever
