{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

import           Control.Applicative

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

nextFibs :: Integer -> Integer -> [Integer]
nextFibs x y = z : nextFibs y z where z = x + y

fibs2 :: [Integer]
fibs2 = nextFibs 0 1

---

data Stream a = SCons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (SCons h t) = h : streamToList t

streamRepeat :: a -> Stream a
streamRepeat a = SCons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (SCons h t) = SCons (fn h) (streamMap fn t)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn initial = SCons initial $ streamFromSeed fn (fn initial)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (SCons h1 t1) xs = SCons h1 $ interleaveStreams xs t1

---

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = genRuler 0
  where genRuler y = interleaveStreams (streamRepeat y) (genRuler (y + 1))

--

x :: Stream Integer
x = SCons 0 $ SCons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = SCons n $ streamRepeat 0
  negate = streamMap negate
  (SCons x xs) + (SCons y ys) = SCons (x + y) (xs + ys)
  (SCons x xs) * b@(SCons y ys) = SCons (x * y) $ streamMap (* x) ys + (xs * b)

instance Fractional (Stream Integer) where
  a@(SCons x xs) / b@(SCons y ys) =
    SCons (x `div` y) $ streamMap (`div` y) (xs - ((a / b) * ys))

---

-- | 2x2 Matrix:
--
-- | a b |
-- | c d |

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  fromInteger a = Matrix a 0 0 a
  (Matrix a b c d) * (Matrix a' b' c' d') = Matrix (a * a' + b * c')
                                                   (a * b' + b * d')
                                                   (c * a' + d * c')
                                                   (c * b' + d * d')

fib4 :: Integer -> Integer
fib4 n = case f ^ n of
  (Matrix _ x _ _) -> x
  where f = Matrix 1 1 1 0
