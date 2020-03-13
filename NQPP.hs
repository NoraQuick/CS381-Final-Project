module NQPP where

import Data.Function
import Data.List hiding (lookup, insert)
import Data.Map (Map, fromList, lookup, insert, update, adjust, alter, empty)
import Data.Maybe (fromJust)
import Data.String
import Prelude hiding (lookup, LT, GT)

-- ***********************
-- Abstract Syntax
-- ***********************

-- | Current State
type State = Map Name Var
-- | List of Commands
type Prog = [Cmd]
-- | Variables
type Name = String

-- | Abstract syntax of Expressions
--
--    expr ::= math operation expr expr
--           | var
--           | 'get' var
data Expr = WeirdMathStuff Op Expr Expr
          | Lit Var
          | Get Name
        deriving(Eq,Show)


-- | Abstract syntax of Variables
--
--    var ::= int
--          | float
--          | bool
--          | string
data Var = I Int
         | F Float
         | B Bool
         | S String
        deriving(Eq,Show)


-- | Abstract syntax of Commands

--    cmd ::= newvar := expr
--          | `while` expr prog
--          | `if` expr prof `else` prog
--          | var := expr
data Cmd = Declare Name Expr
          | While Expr Prog
          | If Expr Prog Prog
          | Set Name Expr


-- | Abstract syntax of Operations
--    op ::= add
--         | subtract
--         | multiply
--         | divide
--         | equal
--         | lte
--         | gte
--         | lt
--         | gt
data Op = Add | Sub | Mult | Div | Equ | LTE | GTE | LT | GT
            deriving(Eq,Show)


-- ***********************
-- * Type Checking
-- ***********************

-- | Type checking Expressions
typeExpr :: Expr -> State -> Maybe Var
typeExpr (Lit (I _))   _ = Just (I 0)
typeExpr (Lit (F _))   _ = Just (F 0.0)
typeExpr (Lit (S _))   _ = Just (S "")
typeExpr (Lit (B _))   _ = Just (B True)
typeExpr (WeirdMathStuff Add l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (I 0)
                        (Just (F _), Just (F _)) -> Just (F 0.0)
                        (Just (B _), Just (B _)) -> Just (B True)
                        (Just (S _), Just (S _)) -> Just (S "")
                        _                      -> Nothing
typeExpr (WeirdMathStuff Sub l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (I 0)
                        (Just (F _), Just (F _)) -> Just (F 0.0)
                        _                      -> Nothing
typeExpr (WeirdMathStuff Mult l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (I 0)
                        (Just (I _), Just (F _)) -> Just (F 0.0)
                        (Just (F _), Just (I _)) -> Just (F 0.0)
                        _                      -> Nothing
typeExpr (WeirdMathStuff Div l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (I 0)
                        (Just (F _), Just (F _)) -> Just (F 0.0)
                        _                      -> Nothing
typeExpr (WeirdMathStuff LTE l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (B True)
                        (Just (F _), Just (F _)) -> Just (B True)
                        _                      -> Nothing
typeExpr (WeirdMathStuff GTE l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (B True)
                        (Just (F _), Just (F _)) -> Just (B True)
                        _                      -> Nothing
typeExpr (WeirdMathStuff LT l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (B True)
                        (Just (F _), Just (F _)) -> Just (B True)
                        _                      -> Nothing
typeExpr (WeirdMathStuff GT l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (B True)
                        (Just (F _), Just (F _)) -> Just (B True)
                        _                      -> Nothing
typeExpr (WeirdMathStuff Equ l r) m = case (typeExpr l m, typeExpr r m) of
                        (Just (I _), Just (I _)) -> Just (B True)
                        (Just (F _), Just (F _)) -> Just (B True)
                        (Just (B _), Just (B _)) -> Just (B True)
                        (Just (S _), Just (S _)) -> Just (B True)
                        _                    -> Nothing

-- | Type checking Commands
typeCmd :: Cmd -> State -> Bool
typeCmd (Declare n e) s = case (lookup n s, typeExpr e s) of
                           (Just tn, Just te) -> tn == te
                           _ -> False
typeCmd (While e p) s = case typeExpr e s of
                              Just (B _) -> typeCmdList p s
                              _ -> False
typeCmd (If e p1 p2) s = case typeExpr e s of
                          Just (B _)-> typeCmdList p1 s && typeCmdList p2 s
                          _ -> False
typeCmd (Set n e) s = True

typeCmdList :: [Cmd] -> State -> Bool
typeCmdList (p:ps) s = typeCmd p s && typeCmdList ps s


-- ***********************
-- Semantics
-- ***********************

-- This actually runs the program and updates the state.
run :: Prog -> State -> State
run [] s = s
run (c:cs) s = run cs (cmd c s)

-- This calls the type check for the commands in order to verify the running of the program
runProg :: Prog -> State -> Maybe State
runProg p s = if typeCmdList p s then Just (run p s)
                                 else Nothing

-- This runs all of the commands, such as the while loops, if statements, and setting of variables. These update the variables, which therefore updates the state.
cmd :: Cmd -> State -> State
cmd (Declare n e) s = insert n (expr e s) s
cmd (While e p) s = case expr e s of
            (B b) -> if b then cmd (While e p) (run p s) else s
cmd (If e p1 p2) s = case expr e s of
            (B b) -> if b then run p1 s else run p2 s
cmd (Set n e) s = set n e s

-- This runs the expressions and turns them into usable variables.
expr :: Expr -> State -> Var
expr (Lit v) s = v
expr (Get n) s = get n s
expr (WeirdMathStuff o e1 e2) s = weirdMathStuff o (expr e1 s) (expr e2 s)

-- This runs all the mathematics possible, including addition, subtraction, multiplication, and division of applicable variables.
weirdMathStuff :: Op -> Var -> Var -> Var
-- | Ints
weirdMathStuff Add (I i1) (I i2) = I (i1 + i2)
weirdMathStuff Sub (I i1) (I i2) =  I (i1 - i2)
weirdMathStuff Mult (I i1) (I i2) = I (i1 * i2)
weirdMathStuff Div (I i1) (I i2) =  I (i1 `div` i2)
weirdMathStuff Equ (I i1) (I i2) = B (i1 == i2)
weirdMathStuff LTE (I i1) (I i2) = B (i1 <= i2)
weirdMathStuff GTE (I i1) (I i2) = B (i1 >= i2)
weirdMathStuff LT (I i1) (I i2) = B (i1 < i2)
weirdMathStuff GT (I i1) (I i2) = B (i1 > i2)
-- | Floats
weirdMathStuff Add (F f1) (F f2) = F (f1 + f2)
weirdMathStuff Sub (F f1) (F f2) = F (f1 - f2)
weirdMathStuff Mult (F f1) (F f2) = F (f1 * f2)
weirdMathStuff Div (F f1) (F f2) = F (f1 / f2)
weirdMathStuff Equ (F f1) (F f2) = B (f1 == f2)
weirdMathStuff LTE (F f1) (F f2) = B (f1 <= f2)
weirdMathStuff GTE (F f1) (F f2) = B (f1 >= f2)
weirdMathStuff LT (F f1) (F f2) = B (f1 < f2)
weirdMathStuff GT (F f1) (F f2) = B (f1 > f2)
-- | Strings
weirdMathStuff Add (S s1) (S s2) = S (s1 ++ s2)
-- | Error
weirdMathStuff _ _ _ = error "internal error: Types are incorrect after statically type checking"

-- This gets the variable value from the current state.
get :: Name -> State -> Var
get name s = case lookup name s of
              (Just v) -> v
              Nothing -> error "Internal Error: Attempted to get a value that has not been declared"

-- This updates the variable's value in the current state.
set :: Name -> Expr -> State -> State
set name e s = adjust (\x -> expr e s) name s


-- ***********************
-- Good Example Functions
-- ***********************

-- Example Program
exProg = run [Declare "var" (Lit (I 2)), Declare "var2" (Lit (I 8)),
             Declare "result" (WeirdMathStuff Add (Get "var") (Get "var2"))
             ] empty

-- If Loop Example
ifProg = run [Declare "true" (Lit (B True)),
             If (Get "true")
              [Declare "True" (Lit (I 1))]
              [Declare "False" (Lit (I 0))]
            ] empty

-- While Loop Example
whileProg = run [Declare "a" (Lit (I 0)),
                Declare "b" (Lit (I 10)),
                While (WeirdMathStuff LT (Get "a") (Get "b"))
                [Set "a" (WeirdMathStuff Add (Get "a") (Lit(I 1)))],
                Declare "finalVal" (Get "a")
               ] empty

-- String Addition Example
stringProg = run [
             Declare "s1" (Lit (S "Hello")),
             Declare "s2" (Lit (S " World")),
             Declare "result" (WeirdMathStuff Add (Get "s1") (Get "s2"))
             ] empty

-- ***********************
-- Bad Example Functions
-- ***********************

-- Fails static type checking
fail1 = run [Declare "a" (Lit (I 0)),
                Declare "b" (Lit (S "hello")),
                While (WeirdMathStuff LT (Get "a") (Get "b"))
                [Set "a" (WeirdMathStuff Add (Get "a") (Lit(I 1)))],
                Declare "finalVal(Should be 10)" (Get "a")
               ] empty

-- Fails to get a variable
fail2 = run [
          Declare "var" (Lit (I 2)),
          If (Get "var2")
           [Declare "Success" (Lit (I 1))]
           [Declare "Fail" (Lit (I 0))]
        ] empty
