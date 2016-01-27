{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns, ScopedTypeVariables, Rank2Types #-}

module Crossover where

--------------------------------------------------------------------

import Data.Vector          (Vector(..), fromList)
import Data.List            (foldl', genericLength, splitAt, sortBy)
import Data.Ord             (comparing)
import Control.Monad.Random (MonadRandom(), getRandomR)
import Data.Word            (Word8(..))

--------------------------------------------------------------------

import Player (ascendingPieChart, getUniquePlayers, Player(..), newPlayer)
--------------------------------------------------------------------

-- | roulette crossover with variable tactic
--
-- * onePointCrossover
-- * twoPointCrossover
-- * uniformCrossover 
-- 
-- Creates a pie chart based on the overall wins / games played ratio
-- TODO think about the logic, the players get picked without removing them from the list

rouletteCrossover :: MonadRandom r => CrossoverTactic -> [Player] -> r [Player]
rouletteCrossover tactic players = go [] (length players) ascPieChart
  where
        ascPieChart = ascendingPieChart players

        go :: MonadRandom m => [Player] -> Int -> [(Player, Double)] -> m [Player]
        go acc     0 l = return acc
        go acc     1 l = return acc -- TODO
        go acc count l = do
            (p1, p2) <- getUniquePlayers l
            (c1, c2) <- tactic p1 p2
            go (c1:c2:acc) (count-2) l


type CrossoverTactic = forall m. MonadRandom m => Player -> Player -> m (Player, Player)

-- | Example of onePointCrossover
--
-- p1 =  AA AA AA AA AA
-- p2 =  BB BB BB BB BB

-- random point generated = 3

-- c1 =  AA AB BB BB BB
-- c2 =  BB BA AA AA AA

onePointCrossover :: MonadRandom m => Player -> Player -> m (Player, Player)
onePointCrossover p1 p2 = do
    percent <- getRandomR (0.0, 1.0)
    let len  = genericLength (moves p1) :: Double

        (p1A , p1B)  = splitAt (round (percent * len)) (moves p1)
        (p2A , p2B)  = splitAt (round (percent * len)) (moves p2)

        !p3 = newPlayer (p1A ++ p2B)
        !p4 = newPlayer (p2A ++ p1B)

    return (p3, p4)

-- | Example of twoPointCrossover
--
-- p1 =  AA AA AA AA AA
-- p2 =  BB BB BB BB BB
--
-- random point generated = 3
--
-- length of the rest = 7
-- second random point generated = 5
--
-- splitting at 3 and (3 + 5)
--
-- c1 =  AA AB BB BB AA
-- c2 =  BB BA AA AA BB

twoPointCrossover :: MonadRandom m => Player -> Player -> m (Player, Player)
twoPointCrossover p1 p2 = do
    point1 <- getRandomR (0.0, 1.0)
    point2 <- getRandomR (0.0, 1.0)

    let len           = genericLength (moves p1) :: Double

        (p1A, p1Rest) = splitAt (round (point1 * len)) (moves p1)
        (p2A, p2Rest) = splitAt (round (point1 * len)) (moves p2)

        sublen        = genericLength (p1Rest) :: Double

        (p1B, p1C) = splitAt (round (point2 * sublen)) p1Rest
        (p2B, p2C) = splitAt (round (point2 * sublen)) p2Rest

        !p3 = newPlayer (p1A ++ p2B ++ p1C)
        !p4 = newPlayer (p2A ++ p1B ++ p2C)

    return (p3, p4)

-- | If the players have strings with different lengths, the children will have both the string with the lesser length
--
-- Example:
--
-- p1 = AA AA AA AA AA
-- p2 = BB BB BB BB BB
--
-- random distribution ->
--
-- c1 = AA BA BB AB BA
-- c2 = BB AB AA BA AB

uniformCrossover :: MonadRandom m => Player -> Player -> m (Player, Player)
uniformCrossover p1 p2 = go ([],[]) (zip (moves p1) (moves p2))

  where go :: MonadRandom m => ([Word8], [Word8]) -> [(Word8, Word8)] -> m (Player, Player)
        go (!p3moves, !p4moves)            [] = return (newPlayer (reverse p3moves), newPlayer (reverse p4moves))
        go (!p3moves, !p4moves) ((c1, c2):xs) = do
            (v :: Double) <- getRandomR (0.0, 1.0)
            case v >= 0.1 of
                True -> go (c1:p3moves, c2:p4moves) xs
                _    -> go (c2:p3moves, c1:p4moves) xs