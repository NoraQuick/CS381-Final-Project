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
infixl 5 :>=:, :<=:
infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp
  = CI Int
	| CB Bool
	| CF Float
  | CS String
	|	V Var        -- variable
  | Exp :>=: Exp   -- Greater than or Equal t
  | Exp :<=: Exp   -- Less than or Equal to
  | Exp :+: Exp    -- addition
  | Exp :-: Exp    -- subtraction
  | Exp :*: Exp    -- multiplication
  | Exp :/: Exp    -- division
	deriving (Eq,Show)


-- | 2 * (3 + 4) ==> 14
-- ex1 :: [Exp]
-- ex1 = [CI 3 :+: CI 4, (CI 2 :*:) ]
--
-- -- | 2 * (3 + 4) ==> 10
-- ex2 :: Exp
-- ex2 = CI 2 :*: (CI 3 :+: CI 4)

-- | 1 + 1 ==> 2
ex3 :: Type
ex3 = smt(CI 1 :+: CI 1)

-- | 1 - 1 ==> 0
ex4 :: Type
ex4 = smt(CI 1 :-: CI 1)

-- | 2.5 * 2.5 ==> 6.25
ex5 :: Type
ex5 = smt(CF 2.5 :*: CF 2.5)

-- | 2 / 2 ==> 1
ex6 :: Type
ex6 = smt(CI 2 :/: CI 2)

-- | 6 / 2.5 ==> 2.4
ex7 :: Type
ex7 = smt(CI 6 :/: CF 2.5)

-- | "Hello " + "World" ==> "Hello World"
ex8 :: Type
ex8 = smt (CS "Hello " :+: CS "World")



-- Abstract syntax of values
--
--     value  ::=   int
--            |     bool
--            |     float
--            |     string

data Type
   = TInt Int
   | TBool Bool
   | TFloat Float
   | TString String
   | TError
  deriving (Eq,Show)

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


--
-- Type checking expressions (coming soon)
--

-- type Env a = Map Type a
-- typeExpr :: Exp -> Env Type -> Maybe Type
-- typeExpr (CI _)   _ = Just TInt
-- typeExpr (CB _)   _ = Just TBool
-- typeExpr (CF _)   _ = Just TFloat
-- typeExpr (V _)   _ = Just TString
-- --
-- -- typeExpr (Exp l r) m = case (typeExpr l m, typeExpr r m) of
-- --                          (Just TInt, Just TInt) -> Just TInt
-- --                          _                      -> Nothing
-- -- typeExpr (LTE l r) m = case (typeExpr l m, typeExpr r m) of
-- --                          (Just TInt, Just TInt) -> Just TBool
-- --                          _                      -> Nothing
-- -- typeExpr (Not e)   m = case typeExpr e m of
-- --                          Just TBool -> Just TBool
-- --                          _          -> Nothing
-- -- typeExpr (Ref v)   m = lookup v m


smt :: Exp -> Type
smt (CI n) = TInt n
smt (CB n) = TBool n
smt (CF n) = TFloat n
smt (CS n) = TString n
--Ints
smt (CI n :+: CI n2) = TInt (n + n2)
smt (CI n :-: CI n2) = TInt (n - n2)
smt (CI n :*: CI n2) = TInt (n * n2)
smt (CI n :/: CI n2) = TInt (n `div` n2)
--Floats
smt (CF n :+: CF n2) = TFloat (n + n2)
smt (CF n :-: CF n2) = TFloat (n - n2)
smt (CF n :*: CF n2) = TFloat (n * n2)
smt (CF n :/: CF n2) = TFloat (n / n2)
--Strings
smt (CS n :+: CS n2) = TString (n ++ n2)
--Combo of Ints and Floats
smt (CI n :+: CF n2) = TFloat (fromIntegral n + n2)
smt (CI n :-: CF n2) = TFloat (fromIntegral n - n2)
smt (CI n :*: CF n2) = TFloat (fromIntegral n * n2)
smt (CI n :/: CF n2) = TFloat (fromIntegral n / n2)

smt (CF n :+: CI n2) = TFloat (n + fromIntegral n2)
smt (CF n :-: CI n2) = TFloat (n - fromIntegral n2)
smt (CF n :*: CI n2) = TFloat (n * fromIntegral n2)
smt (CF n :/: CI n2) = TFloat (n / fromIntegral n2)
