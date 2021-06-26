module Spring13HW01 where

-- Luhn exercises omitted as completed as part of Spring 15

type Peg = String
type Move = (Peg, Peg)
type Disc = Integer

-- | moveDiscs: move n pegs from target to destination via spare stacks
moveDiscs :: Integer -> Peg -> Peg -> Peg -> [Move]
moveDiscs n source target spare
  | n <= 0 = []
  | n == 1 = [(source, target)]
  | otherwise = concat
    [ moveDiscs (n - 1) source spare  target
    , moveDiscs 1       source target spare
    , moveDiscs (n - 1) spare  target source
    ]
