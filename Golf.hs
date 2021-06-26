module Golf where

-- Code golf exercises per HW 3, CIS 194, Spring '13

import           Data.List

-- Ex 1. Hopscotch

-- | skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- First elmement: input list
-- Second element: every 2nd element in initial list
-- nth element: every nth element in initial list
-- impl: use zip to allow index-aware processing, filter based on index

skips :: [a] -> [[a]]
skips lst = map (`takeEveryNth` lst) [1 ..]
  where takeEveryNth n = map snd . filter ((0 ==) . mod 0 . fst) . zip [1 ..]

-- Ex 2. Local maxima

-- | localMaxima: get array of local maxima in input array
-- e.g. localMaxima [2,9,5,6,1] == [9,6]
-- local maxima exist where preceeding + following elements are less than a given element
localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : d) = [ b | b > a, b > c ] ++ localMaxima (b : c : d)
localMaxima _               = []

-- Ex 3. Histogram

-- | histogram :: [Integer] -> String
-- Generate textual representation of a histogram given input list of
-- repeated numbers in range 1 .. 9
-- impl: generate list of counts per number in range 1 to 9 using elemIndices
--       starting at highest count, render rows by checking if each number is present that amount of times
--       or greater - if so, we render the "*", otherwise " "
-- Example:
-- λ> putStr $ histogram [1,2,2,9,2,2,3,4,5,1,9,9,9,9,9]
--         *
--         *
--  *      *
--  *      *
-- **      *
-- *****   *
-- =========
-- 123456789

histogram arr = unlines $ concat
  [ map (\count -> [ if x < count then ' ' else '*' | x <- counts ])
    $ reverse [1 .. foldr max 0 counts]
  , [replicate 9 '=']
  , [['1' .. '9']]
  ]
  where counts = map (length . (`elemIndices` arr)) [1 .. 9]
