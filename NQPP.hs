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

infixl 1 .=.
infixl 2 .+., .-.
infixl 3 .*., ./.

data Expr = Const Int
	  | Var Var
	  | Expr .+. Expr
	  | Expr .-. Expr
	  | Expr .*. Expr
	  | Expr ./. Expr

data Cmd = Var := Expr
	 | While Expr Cmd

type Prog = [Cmd]


-- | ??
DoTheAdd
DoTheAdd

DoTheSub
DoTheSub

DoTheMult
DoTheMult

DoTheDiv
DoTheDiv


-- | ??
cmd
cmd

prog
prog