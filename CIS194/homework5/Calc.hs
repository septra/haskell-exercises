module Calc where

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- End Exercise 1


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr exp = case parseExp Lit Add Mul exp of 
                Just x  -> Just (eval x)
                Nothing -> Nothing 

-- evalStr = fmap eval . parseExp Lit Add Mul

-- End Exercise 2


-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a 
    mul :: a -> a -> a 


instance Expr ExprT where
    lit x   = Lit x
    add x y = Add x y
    mul x y = Mul x y

-- End Exercise 3


-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x   = MinMax x
    add (MinMax x) (MinMax y) = MinMax (if x > y then x else y)
    mul (MinMax x) (MinMax y) = MinMax (if x < y then y else x)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x   = Mod7 $ if elem x [0..6] then x else error "Value outside range"
    add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7

-- End Exercise 3

