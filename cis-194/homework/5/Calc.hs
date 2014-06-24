{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit int) = int
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp Lit Add Mul s)

reify :: ExprT -> ExprT
reify = id

class Expr a where
      lit :: Integer -> a
      add :: a -> a -> a
      mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x = x > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax n) (MinMax m) = MinMax $ min n m
  mul (MinMax n) (MinMax m) = MinMax $ max n m

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

