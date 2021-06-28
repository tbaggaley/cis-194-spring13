module Party where

import           Data.List                      ( sort )
import           Data.Tree
import           Employee

-- ex 1.

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emps ++ [emp]) (fun + empFun emp)

-- ex 1.2.

instance Semigroup GuestList where
  (GL empsA funA) <> (GL empsB funB) = GL (empsA ++ empsB) (funA + funB)

instance Monoid GuestList where
  mempty = GL [] 0

-- ex 1.3.

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- ex 2.

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f x (Node val []) = val `f` x
treeFold f x (Node val sf) = val `f` foldr (flip (treeFold f)) x sf

treeFold' :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold' f x (Node label forest) = label `f` map (treeFold' f x) forest

-- ex 3.

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp lst =
  (safeMaximum (map (glCons emp) withoutBossList), safeMaximum withBossList)
 where
  withBossList    = map fst lst
  withoutBossList = map snd lst

safeMaximum :: (Monoid a, Ord a) => [a] -> a
safeMaximum [] = mempty
safeMaximum xs = maximum xs

-- ex 4.

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun a b where (a, b) = treeFold' nextLevel mempty tree

-- ex 5.

main :: IO ()
main = readFile "company.txt" >>= \contents ->
  (putStrLn . showGuestList . maxFun) (read contents :: Tree Employee)

showGuestList (GL emps fun) =
  unlines $ ("Total fun: " ++ show fun) : sort (map empName emps)
