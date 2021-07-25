{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Risk where

import           Control.Monad.Random
import           Data.List                      ( sort )

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

type DieRolls = [DieValue]

dice :: Int -> Rand StdGen DieRolls
dice = flip replicateM die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield
  { attackers :: Army
  , defenders :: Army
  }
  deriving Show

-- Ex 1

clampToZero :: Int -> Int
clampToZero x | x < 0     = 0
              | otherwise = x

-- | Returns pair of (player a's losses, player b's losses)
rollsToLosses :: [DieValue] -> [DieValue] -> (Int, Int)
rollsToLosses rollsA rollsB =
  (sum (map bWins outcomes), sum (map aWins outcomes))
 where
  outcomes = zipWith (>) (reverse . sort $ rollsA) (reverse . sort $ rollsB)
  aWins True = 1
  aWins _    = 0
  bWins False = 1
  bWins _     = 0

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attackers defenders)
  | noAttacking == 0 = pure bf
  | noDefending == 0 = pure bf
  | otherwise = liftM2 Battlefield
                       ((attackers -) . fst <$> losses)
                       ((defenders -) . snd <$> losses)
 where
  noAttacking = clampToZero $ min (attackers - 1) 3
  noDefending = clampToZero $ min defenders 2
  losses      = liftM2 rollsToLosses (dice noAttacking) (dice noDefending)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a b)
  | a < 2 || b <= 0 = pure bf
  | otherwise       = battle bf >>= \outcome -> invade outcome

defendersLost :: Battlefield -> Bool
defendersLost (Battlefield _ 0) = True
defendersLost _                 = False

successProb :: Battlefield -> Rand StdGen Double
successProb bf = sum <$> invasions
 where
  invasionValue bf | defendersLost bf = 0.0
                   | otherwise        = 0.001
  invasions :: Rand StdGen [Double]
  invasions = replicateM 1000 (invasionValue <$> invade bf)
