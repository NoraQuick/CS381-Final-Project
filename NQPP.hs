module NQPP where

import Data.Function
import Data.List hiding (lookup, insert)
import Data.Map (Map, fromList, lookup, insert, update, adjust, alter, empty)
import Data.Maybe (fromJust)
import Data.String
import Prelude hiding (lookup, LT, GT)

--
-- * Abstract Syntax
--

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
--
--    cmd ::= newvar := expr
--          | `while` expr prog
--          | `if` expr prof `else` prog
--          | var := expr
data Cmd = Declare Name Expr
          | While Expr Prog
          | If Expr Prog Prog
          | Set Name Expr

-- | Abstract syntax of Operations
--
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

-- Define the typing relation. (type checking)
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

typeCmd :: Cmd -> State -> Bool
typeCmd (Declare n e) m = case (lookup v m, typeExpr e m) of
                           (Just tv, Just te) -> tv == te
                           _ -> False
typeCmd (If c st se) m = case typeExpr c m of
                           Just (B _) -> typeCmd st m && typeCmd se m
                           _ -> False
typeCmd (While c sb) m = case typeExpr c m of
                           Just (B _) -> typeCmd sb m
                           _ -> False



run :: Prog -> State -> State
run [] s = s
run (c:cs) s = run cs (cmd c s)

cmd :: Cmd -> State -> State
cmd (Declare n e) s = insert n (expr e s) s
cmd (While e p) s = case expr e s of
            (B b) -> if b then cmd (While e p) (run p s) else s
cmd (If e p1 p2) s = case expr e s of
            (B b) -> if b then run p1 s else run p2 s
cmd (Set n e) s = set n e s


expr :: Expr -> State -> Var
expr (Lit v) s = v
expr (Get n) s = get n s
expr (WeirdMathStuff o e1 e2) s = weirdMathStuff o (expr e1 s) (expr e2 s)



weirdMathStuff :: Op -> Var -> Var -> Var
-- Ints
weirdMathStuff Add (I i1) (I i2) = I (i1 + i2)
weirdMathStuff Sub (I i1) (I i2) =  I (i1 - i2)
weirdMathStuff Mult (I i1) (I i2) = I (i1 * i2)
weirdMathStuff Div (I i1) (I i2) =  I (i1 `div` i2)
weirdMathStuff Equ (I i1) (I i2) = B (i1 == i2)
weirdMathStuff LTE (I i1) (I i2) = B (i1 <= i2)
weirdMathStuff GTE (I i1) (I i2) = B (i1 >= i2)
weirdMathStuff LT (I i1) (I i2) = B (i1 < i2)
weirdMathStuff GT (I i1) (I i2) = B (i1 > i2)
-- Floats
weirdMathStuff Add (F f1) (F f2) = F (f1 + f2)
weirdMathStuff Sub (F f1) (F f2) = F (f1 - f2)
weirdMathStuff Mult (F f1) (F f2) = F (f1 * f2)
weirdMathStuff Div (F f1) (F f2) = F (f1 / f2)
weirdMathStuff Equ (F f1) (F f2) = B (f1 == f2)
weirdMathStuff LTE (F f1) (F f2) = B (f1 <= f2)
weirdMathStuff GTE (F f1) (F f2) = B (f1 >= f2)
weirdMathStuff LT (F f1) (F f2) = B (f1 < f2)
weirdMathStuff GT (F f1) (F f2) = B (f1 > f2)
-- Strings
weirdMathStuff Add (S s1) (S s2) = S (s1 ++ s2)
-- Error
weirdMathStuff _ _ _ = error "internal error: unable to perform operation on the inputs"

get :: Name -> State -> Var
get name s = case lookup name s of
              (Just v) -> v
              Nothing -> error "Something in the get"

set :: Name -> Expr -> State -> State
set name e s = adjust (\x -> expr e s) name s

exprog = run [Declare "var" (Lit (I 2)), Declare "var2" (Lit (I 8)),
             Declare "result" (WeirdMathStuff Add (Get "var") (Get "var2"))
             ] empty

ifprog = run [Declare "true" (Lit (B True)),
              If (Get "true")
               [Declare "True" (Lit (I 1))]
               [Declare "False" (Lit (I 0))]
             ] empty

whileprog = run [Declare "a" (Lit (I 0)),
                 Declare "b" (Lit (I 10)),
                 While (WeirdMathStuff LT (Get "a") (Get "b"))
                 [Set "a" (WeirdMathStuff Add (Get "a") (Lit(I 1)))],
                 Declare "finalVal" (Get "a")
                ] empty


-- Good Example Functions
-- ifprog = run [Declare "true" (Lit (B True)),
--              If (Get "true")
--               [Declare "True" (Lit (I 1))]
--               [Declare "False" (Lit (I 0))]
--             ] empty

-- whileprog = run [Declare "a" (Lit (I 0)),
--                 Declare "b" (Lit (I 10)),
--                 While (WeirdMathStuff LT (Get "a") (Get "b"))
--                 [Set "a" (WeirdMathStuff Add (Get "a") (Lit(I 1)))],
--                 Declare "finalVal" (Get "a")
--                ] empty


-- Bad Example Functions



--
-- * Abstract syntax
--

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

-- type Prog = Stmt
-- type Val = Int
-- type Assign = [(Var, Exp)]

-- data Value
--    = Int
--    | Bool
--    | Float
--    | String
--    deriving (Eq,Show)
--
-- data Exp
--    = I Int
--    | F Float
--    | S String
--    | Add Exp Exp -- addition
--    | Sub Exp Exp -- subtraction
--    | Mul Exp Exp -- multiplication
--    | Div Exp Exp -- division
--    | Equ Exp Exp -- checking if two expressions are equal
--    | LTE Exp Exp -- less than or equal to
--    | GTE Exp Exp -- greater than or equal to
--    | LT Exp Exp  -- less than
--    | GT Exp Exp  -- greater than
--    | Not Exp     -- opposite of bool
--    | Ref Var     -- referencing an environment variable
--   deriving (Eq,Show)
--
-- data Stmt
--   = Bind Var Exp      -- assignment
--   | While Exp Stmt  -- loop
--   | If Expr Stmt Stmt
--   | Seq [Stmt]      -- sequence
--  deriving (Eq, Show)
--
-- data Type = TBool | TInt | TFloat | TString
--   deriving (Eq,Show)
--
-- type Decl = (Var,Type)
--
-- data Prog = P [Decl] Stmt
--   deriving (Eq,Show)
--
-- type Env a = Map Var a
--
-- -- ex1 = [
-- --       a = 8
-- --       b = 2.1
-- --       while b < a
-- --         b = b :+: 1
-- --         a = a :-: 1
-- --      print a
-- -- ]
--
-- -- 2. Define the typing relation. (type checking)
-- typeExpr :: Expr -> Env Type -> Maybe Type
-- typeExpr (I _)   _ = Just TInt
-- typeExpr (F _)   _ = Just TFloat
-- typeExpr (S _)   _ = Just TString
-- typeExpr (B _)   _ = Just TBool
-- typeExpr (Add l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TInt
--                         (Just TInt, Just TFloat) -> Just TFloat
--                         (Just TFloat, Just TFloat) -> Just TFloat
--                         (Just TFloat, Just TInt) -> Just TFloat
--                         (Just TBool, Just TBool) -> Just TBool
--                         (Just TString, Just TString) -> Just TString
--                         _                      -> Nothing
-- typeExpr (Sub l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TInt
--                         (Just TInt, Just TFloat) -> Just TFloat
--                         (Just TFloat, Just TFloat) -> Just TFloat
--                         (Just TFloat, Just TInt) -> Just TFloat
--                         (Just TBool, Just TBool) -> Just TBool
--                         _                      -> Nothing
-- typeExpr (Mul l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TInt
--                         (Just TFloat, Just TFloat) -> Just TFloat
--                         (Just TInt, Just TFloat) -> Just TFloat
--                         (Just TFloat, Just TInt) -> Just TFloat
--                         _                      -> Nothing
-- typeExpr (Div l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TFloat
--                         (Just TFloat, Just TFloat) -> Just TFloat
--                         (Just TInt, Just TFloat) -> Just TFloat
--                         (Just TFloat, Just TInt) -> Just TFloat
--                         _                      -> Nothing
-- typeExpr (LTE l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TBool
--                         (Just TFloat, Just TFloat) -> Just TBool
--                         (Just TInt, Just TFloat) -> Just TBool
--                         (Just TFloat, Just TInt) -> Just TBool
--                         _                      -> Nothing
-- typeExpr (GTE l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TBool
--                         (Just TFloat, Just TFloat) -> Just TBool
--                         (Just TInt, Just TFloat) -> Just TBool
--                         (Just TFloat, Just TInt) -> Just TBool
--                         _                      -> Nothing
-- typeExpr (LT l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TBool
--                         (Just TFloat, Just TFloat) -> Just TBool
--                         (Just TInt, Just TFloat) -> Just TBool
--                         (Just TFloat, Just TInt) -> Just TBool
--                         _                      -> Nothing
-- typeExpr (GT l r) m = case (typeExpr l m, typeExpr r m) of
--                         (Just TInt, Just TInt) -> Just TBool
--                         (Just TFloat, Just TFloat) -> Just TBool
--                         (Just TInt, Just TFloat) -> Just TBool
--                         (Just TFloat, Just TInt) -> Just TBool
--                         _                      -> Nothing
-- typeExpr (Not l) m = case (typeExpr l m) of
--                         (Just TBool) -> Just TBool
--                         _                      -> Nothing
-- typeExpr (Equ l r) m = case (typeExpr l m, typeExpr r m) of
--                          (Just TInt, Just TInt) -> Just TBool
--                          (Just TFloat, Just TFloat) -> Just TBool
--                          (Just TBool, Just TBool) -> Just TBool
--                          (Just TString, Just TString) -> Just TBool
--                          _          -> Nothing
-- typeExpr (Ref v)   m = lookup v m
--
-- -- Type checking of statements (not all of these statements are actually returning bools,
-- -- but rather checking if the statement itself has type correct elements)
-- typeStmt :: Stmt -> Env Type -> Bool
-- typeStmt (Bind v e)   m = case (lookup v m, typeExpr e m) of
--                             (Just tv, Just te) -> tv == te
--                             _ -> False
-- typeStmt (If c st se) m = case typeExpr c m of
--                             Just TBool -> typeStmt st m && typeStmt se m
--                             _ -> False
-- typeStmt (While c sb) m = case typeExpr c m of
--                             Just TBool -> typeStmt sb m
--                             _ -> False
-- typeStmt (Seq ss)   m = all (\s -> typeStmt s m) ss
--
-- typeProg :: Prog -> Bool
-- typeProg (P ds s) = typeStmt s (fromList ds)
--
--
--
-- type Val = Either (Either Int Float) (Either Bool String)
--
-- -- calls the expressions after using helper functions to unwrap each type, then rewraps the result in the correct type
-- evalExpr :: Expr -> Env Val -> Val
-- evalExpr (I i)   _ = Left (Left i)
-- evalExpr (F f)   _ = Left (Right f)
-- evalExpr (S s)   _ = Right (Right s)
-- evalExpr (Add l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Left (evalInt l m + evalInt r m)
--                         (F a, F b) -> Left (evalFloat l m + evalFloat r m)
--                         (I a, F b) -> Left (evalInt l m + evalFloat r m)
--                         (F a, I b) -> Left (evalFloat l m + evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (Sub l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Left (evalInt l m - evalInt r m)
--                         (F a, F b) -> Left (evalFloat l m - evalFloat r m)
--                         (I a, F b) -> Left (evalInt l m - evalFloat r m)
--                         (F a, I b) -> Left (evalFloat l m - evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (Mul l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Left (evalInt l m * evalInt r m)
--                         (F a, F b) -> Left (evalFloat l m * evalFloat r m)
--                         (I a, F b) -> Left (evalInt l m * evalFloat r m)
--                         (F a, I b) -> Left (evalFloat l m * evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (Div l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I 0) -> error "cannot divide by 0"
--                         (F a, F 0.0) -> error "cannot divide by 0"
--                         (I a, F 0.0) -> error "cannot divide by 0"
--                         (F a, I 0) -> error "cannot divide by 0"
--                         (I a, I b) -> Left (evalInt l m 'div' evalInt r m)
--                         (F a, F b) -> Left (evalFloat l m 'div' evalFloat r m)
--                         (I a, F b) -> Left (evalInt l m 'div' evalFloat r m)
--                         (F a, I b) -> Left (evalFloat l m 'div' evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (LTE l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Right (evalInt l m <= evalInt r m)
--                         (F a, F b) -> Right (evalFloat l m <= evalFloat r m)
--                         (I a, F b) -> Right (evalInt l m <= evalFloat r m)
--                         (F a, I b) -> Right (evalFloat l m <= evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (GTE l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Right (evalInt l m >= evalInt r m)
--                         (F a, F b) -> Right (evalFloat l m >= evalFloat r m)
--                         (I a, F b) -> Right (evalInt l m >= evalFloat r m)
--                         (F a, I b) -> Right (evalFloat l m >= evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (LT l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Right (evalInt l m < evalInt r m)
--                         (F a, F b) -> Right (evalFloat l m < evalFloat r m)
--                         (I a, F b) -> Right (evalInt l m < evalFloat r m)
--                         (F a, I b) -> Right (evalFloat l m < evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (GT l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Right (evalInt l m > evalInt r m)
--                         (F a, F b) -> Right (evalFloat l m > evalFloat r m)
--                         (I a, F b) -> Right (evalInt l m > evalFloat r m)
--                         (F a, I b) -> Right (evalFloat l m > evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (Equ l r) m = case (evalExpr l m, evalExpr r m) of
--                         (I a, I b) -> Right (evalInt l m == evalInt r m)
--                         (F a, F b) -> Right (evalFloat l m == evalFloat r m)
--                         (I a, F b) -> Right (evalInt l m == evalFloat r m)
--                         (F a, I b) -> Right (evalFloat l m == evalInt r m)
--                         _ -> error "internal error: unable to calculate types"
-- evalExpr (Not e)   m = Right (not (evalBool e m))
-- evalExpr (Ref x)   m = case lookup x m of
--                          Just v  -> v
--                          Nothing -> error "internal error: undefined variable"
--
-- -- | Helper function to evaluate an expression to an integer. Note that
-- --   in all cases, we should only get an "internal error" if we try to
-- --   evaluate an expression that didn't pass the static type checker we
-- --   wrote above.
-- evalInt :: Expr -> Env Val -> Int
-- evalInt e m = case evalExpr e m of
--                 Left (Left i)  -> i
--                 _ -> error "internal error: expected Int"
--
-- -- | Helper function to evaluate an expression to a Boolean.
-- evalFloat :: Expr -> Env Val -> Float
-- evalFloat e m = case evalExpr e m of
--                 Left (Right f) -> f
--                 _  -> error "internal error: expected Float"
--
-- -- | Helper function to evaluate an expression to a Boolean.
-- evalBool :: Expr -> Env Val -> Bool
-- evalBool e m = case evalExpr e m of
--                  Right (Left b) -> b
--                  _  -> error "internal error: expected Bool"
--
-- -- | Helper function to evaluate an expression to a Boolean.
-- evalString :: Expr -> Env Val -> String
-- evalString e m = case evalExpr e m of
--                 Right (Right s) -> s
--                 _  -> error "internal error: expected String"
--
--
-- evalStmt :: Stmt -> Env Val -> Env Val
-- evalStmt (Bind x e)   m = insert x (evalExpr e m) m
-- evalStmt (If c st se) m = if evalBool c m
--                           then evalStmt st m
--                           else evalStmt se m
-- evalStmt (While c sb) m = if evalBool c m
--                           then evalStmt (While c sb) (evalStmt sb m)
--                           else m
-- evalStmt (Block ss)   m = evalStmts ss m
--
-- evalStmts :: [Stmt] -> Env Val -> Env Val
-- evalStmts []     m = m
-- evalStmts (s:ss) m = evalStmts ss (evalStmt s m)
--
-- evalProg :: Prog -> Env Val
-- evalProg (P ds s) = evalStmt s m
--   where
--     m = fromList (map (\(x,t) -> (x, init t)) ds)
--     init TInt  = Left 0
--     init TBool = Right False
--     -- (NEED TO ADD OTHERS HERE)
--
-- -- | Type check and then run a program.
-- runProg :: Prog -> Maybe (Env Val)
-- runProg p = if typeProg p then Just (evalProg p)
--                           else Nothing
--
--
