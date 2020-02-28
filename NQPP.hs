-- | Thiem Nam - namt
-- | Zachary Parsons - parsonsz
-- | Luke Puppo - puppol
-- | Nora quick - quickn


module NQPP where

import Data.Function
import Data.List


type StngVar = String
type IntVar = Int
type BoolVar = Bool

infixl 1 :
infixl 2 :+:, :-:
infixl 3 :*:, :/:

data Expr = Const Int
	  | Expr :+: Expr
	  | Expr :-: Expr
	  | Expr :*: Expr
	  | Expr :/: Expr

data Cmd = 
	 -- | While Expr Cmd

cmd :: Cmd -> State -> (State, Maybe Line)
cmd = undefined

prog :: Prog -> State -> (State, [Line])
prog = undefined



-- | ??
DoTheAdd = undefined


DoTheSub = undefined


DoTheMult = undefined


DoTheDiv = undefined



-- | ??
cmd = undefined

prog = undefined
