{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns, ScopedTypeVariables, Rank2Types #-}

module Crossover where

import Data.Vector (Vector(..), fromList)
import Data.List   (foldl', genericLength, splitAt, sortBy)
import Data.Ord    (comparing)
import System.Random

import Control.Monad.Random

import Player

-- | roulette crossover with variable tactic
--
-- * onePointCrossover
-- * twoPointCrossover
-- * uniformCrossover 
-- 
-- Creates a pie chart based on the overall wins / games played ratio
-- TODO think about the logic, the players get picked without removing them from the list

rouletteCrossover :: MonadRandom m => CrossoverTactic -> [Player] -> m [Player]
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


type CrossoverTactic = MonadRandom m => Player -> Player -> m (Player, Player)

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
    let len  = genericLength (str p1) :: Double

        (p1A , p1B)  = splitAt (round (percent * len)) (str p1)
        (p2A , p2B)  = splitAt (round (percent * len)) (str p2)

        p3 = Player (p1A ++ p2B) 0 1
        p4 = Player (p2A ++ p1B) 0 1

    return (p3, p4)

--onePointCrossover :: CrossoverTactic
--onePointCrossover g p1 p2 = (p3, p4, g')
--    where (percent , g' ) = randomR (0.0, 1.0) g

--          len          = genericLength (str p1) :: Double

--          (p1A , p1B)  = splitAt (round (percent * len)) (str p1)
--          (p2A , p2B)  = splitAt (round (percent * len)) (str p2)

--          p3 = Player (p1A ++ p2B) 0 1
--          p4 = Player (p2A ++ p1B) 0 1


-- | Example of twoPointCrossover
--
-- p1 =  AA AA AA AA AA
-- p2 =  BB BB BB BB BB

-- random point generated = 3

-- length of the rest = 7
-- second random point generated = 5

-- splitting at 3 and (3 + 5)

-- c1 =  AA AB BB BB AA
-- c2 =  BB BA AA AA BB

twoPointCrossover :: MonadRandom m => Player -> Player -> m (Player, Player)
twoPointCrossover p1 p2 = do
    point1 <- getRandomR (0.0, 1.0)
    point2 <- getRandomR (0.0, 1.0)

    let len           = genericLength (str p1) :: Double

        (p1A, p1Rest) = splitAt (round (point1 * len)) (str p1)
        (p2A, p2Rest) = splitAt (round (point1 * len)) (str p2)

        sublen        = genericLength (p1Rest) :: Double

        (p1B, p1C) = splitAt (round (point2 * sublen)) p1Rest
        (p2B, p2C) = splitAt (round (point2 * sublen)) p2Rest

        p3 = Player (p1A ++ p2B ++ p1C) 0 1
        p4 = Player (p2A ++ p1B ++ p2C) 0 1

    return (p3, p4)

--twoPointCrossover :: CrossoverTactic
--twoPointCrossover g p1 p2 = (p3, p4, g'')
--    where (point1, g' ) = randomR (0.0, 1.0) g
--          (point2, g'') = randomR (0.0, 1.0) g'

--          len           = genericLength (str p1) :: Double

--          (p1A, p1Rest) = splitAt (round (point1 * len)) (str p1)
--          (p2A, p2Rest) = splitAt (round (point1 * len)) (str p2)

--          sublen        = genericLength (p1Rest) :: Double

--          (p1B, p1C) = splitAt (round (point2 * sublen)) p1Rest
--          (p2B, p2C) = splitAt (round (point2 * sublen)) p2Rest

--          p3 = Player (p1A ++ p2B ++ p1C) 0 1
--          p4 = Player (p2A ++ p1B ++ p2C) 0 1


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
uniformCrossover p1 p2 = go ([],[]) (zip (str p1) (str p2))

  where go :: MonadRandom m => (String, String) -> [(Char, Char)] -> m (Player, Player)
        go (p3str, p4str)            [] = return (Player (reverse p3str) 0 1, Player (reverse p4str) 0 1)
        go (p3str, p4str) ((c1, c2):xs) = do
            (v :: Double) <- getRandomR (0.0, 1.0)
            case v >= 0.5 of
                True -> go (c1:p3str, c2:p4str) xs
                _    -> go (c2:p3str, c1:p4str) xs

-- uniformCrossover :: CrossoverTactic
-- uniformCrossover g p1 p2 = (p3, p4, g')
--   where (p3, p4, g') = go ([],[]) g (zip (str p1) (str p2))
-- 
--         go :: (String, String) -> StdGen -> [(Char, Char)] -> (Player, Player, StdGen)
--         go (p3str, p4str) g            [] = (Player (reverse p3str) 0 1, Player (reverse p4str) 0 1, g)
--         go (p3str, p4str) g ((c1, c2):xs)
--               | v >= 0.5  = go (c1:p3str, c2:p4str) g' xs
--               | otherwise = go (c2:p3str, c1:p4str) g' xs
--             where (v, g') = randomR (0.0, 1.0) g :: (Double, StdGen)


