module HW4 where

import           Data.List

-- Ex 1

---- 1.1.

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

---- 1.2

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate f
 where
  f n | even n    = n `div` 2
      | otherwise = 3 * n + 1

-- Ex 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr (addToTree 0) Leaf

treeDepth :: Tree a -> Integer
treeDepth Leaf                      = 0
treeDepth (Node depth Leaf _ Leaf ) = depth
treeDepth (Node depth left _ right) = max (treeDepth left) (treeDepth right)

addToTree :: Ord a => Integer -> a -> Tree a -> Tree a
addToTree currentDepth value Leaf = Node currentDepth Leaf value Leaf
addToTree _ value (Node depth left nodeValue right)
  | treeDepth left < treeDepth right = Node depth
                                            (addToTree (depth + 1) value left)
                                            nodeValue
                                            right
  | otherwise = Node depth left nodeValue (addToTree (depth + 1) value right)

-- Ex 3

---- 3.1

xor :: [Bool] -> Bool
xor = odd . foldr (\val acc -> acc + if val then 1 else 0) 0

---- 3.2

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\val acc -> f val : acc) []

---- 3.3

-- TODO
-- myFoldl :: (a -> b -> a) -> a - [b] -> a
-- myFoldl f base xs = foldr

---- 3.4

-- sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map
  ((+ 1) . (* 2))
  (  [1 .. n]
  \\ [ i + j + 2 * i * j
     | i <- [1 .. n]
     , j <- [1 .. i]
     , i + j + 2 * i * j <= n
     ]
  )
