{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import qualified Data.Map                      as M
import           Data.Maybe
import           Parser
import qualified StackVM                       as VM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (Lit x    ) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) (parseExp Lit Add Mul s)

---

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x | x < 0     = False
        | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

---

instance Expr VM.Program where
  lit n = [VM.PushI n]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

---

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String

instance HasVars VarExprT where
  var = VVar

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit i = \_ -> Just i
  add f1 f2 = \varMap -> if isJust (f1 varMap) && isJust (f2 varMap)
    then Just $ fromJust (f1 varMap) + fromJust (f2 varMap)
    else Nothing
  mul f1 f2 = \varMap -> if isJust (f1 varMap) && isJust (f2 varMap)
    then Just $ fromJust (f1 varMap) * fromJust (f2 varMap)
    else Nothing

withVars
  :: [(String, Integer)]
  -> (M.Map String Integer -> Maybe Integer)
  -> Maybe Integer
withVars vs e = e $ M.fromList vs
