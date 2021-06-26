-- module Main where

import           Buffer
import           Editor
import           JoinList

jlEditor = editor :: Editor ScrabbleIndexList ()

main =
  runEditor jlEditor $ fromString $ unlines ["Hello worldy", "Trouble so hard"]
