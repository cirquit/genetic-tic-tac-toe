{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Crossover where

import Data.Vector (Vector(..), fromList)
import Data.List   (foldl', genericLength, splitAt, sortBy)
import Data.Ord    (comparing)
import System.Random

import Player

rouletteCrossover :: CrossoverTactic -> StdGen -> [Player] -> ([Player], StdGen)
rouletteCrossover tactic g players = go [] (length players) g ascPieChart
  where 
        ascPieChart = ascendingPieChart players

        go :: [Player] -> Int -> StdGen -> [(Player, Double)] -> ([Player], StdGen)
        go acc     0 g l = (acc, g)
        go acc     1 g l = (acc, g) -- TODO
        go acc count g l = go (c1:c2:acc) (count-2) g'' l
            where 
                  (p1, p2, g')  = getUniquePlayers g l
                  (c1, c2, g'') = tactic g' p1 p2


type CrossoverTactic = StdGen -> Player -> Player -> (Player, Player, StdGen)

onePointCrossover :: CrossoverTactic
onePointCrossover g p1 p2 = (p3, p4, g')
    where (percent , g' ) = randomR (0.0, 1.0) g

          len          = genericLength (str p1) :: Double

          (p1A , p1B)  = splitAt (round (percent * len)) (str p1)
          (p2A , p2B)  = splitAt (round (percent * len)) (str p2)

          p3 = Player (p1A ++ p2B) 0 1
          p4 = Player (p2A ++ p1B) 0 1


twoPointCrossover :: CrossoverTactic
twoPointCrossover g p1 p2 = (p3, p4, g'')
    where (point1, g' ) = randomR (0.0, 1.0) g
          (point2, g'') = randomR (0.0, 1.0) g'

          len           = genericLength (str p1) :: Double

          (p1A, p1Rest) = splitAt (round (point1 * len)) (str p1)
          (p2A, p2Rest) = splitAt (round (point1 * len)) (str p2)

          sublen        = genericLength (p1Rest) :: Double

          (p1B, p1C) = splitAt (round (point2 * sublen)) p1Rest
          (p2B, p2C) = splitAt (round (point2 * sublen)) p2Rest

          p3 = Player (p1A ++ p2B ++ p1C) 0 1
          p4 = Player (p2A ++ p1B ++ p2C) 0 1


-- | If the players have strings with different lengths, the children will have the both a string with the lesser length

uniformCrossover :: CrossoverTactic
uniformCrossover g p1 p2 = (p3, p4, g')
  where (p3, p4, g') = go ([],[]) g (zip (str p1) (str p2))

        go :: (String, String) -> StdGen -> [(Char, Char)] -> (Player, Player, StdGen)
        go (p3str, p4str) g            [] = (Player (reverse p3str) 0 1, Player (reverse p4str) 0 1, g)
        go (p3str, p4str) g ((c1, c2):xs)
              | v >= 0.5  = go (c1:p3str, c2:p4str) g' xs
              | otherwise = go (c2:p3str, c1:p4str) g' xs
            where (v, g') = randomR (0.0, 1.0) g :: (Double, StdGen)




-- #### DEPRICATED ####

-- |  α = percent to be crossbred, creates (α * length) - 1 new population
-- 
--                α         
alphaCrossover :: Double -> StdGen -> [Player] -> ([Player], StdGen)
alphaCrossover alpha g players = (sortByDscRatio (rest ++ children), g')
  where best                   = round (alpha * genericLength players) :: Int
        (parents, rest)        = splitAt best players
        (children, g')         = foldl' go ([], g) (zip parents (tail parents))
            where go (acc, g) (p1, p2)  = (child : acc, g')
                      where (child, g') = fitnessCrossoverP g p1 p2


-- | Creates a crossover between two individuals by comparing win chances
--
-- β = percent of the string to mutate
-- 
-- chooses from the father-string or the mother-string based on the fitness ratio for every char
--
fitnessCrossoverP :: StdGen -> Player -> Player -> (Player, StdGen)
fitnessCrossoverP g (Player str1 fit1 games1) (Player str2 fit2 games2) = (Player str3 fit3 games3, g')

    where (str3, g') = go [] beta g (zip str1 str2)
          fit3       = (fit1 + fit2)     `div` 2
          games3     = (games1 + games2) `div` 2
          beta       = (((toD fit1) / (toD games1)) + ((toD fit2) / (toD games2))) / 2
          toD        = fromIntegral
--                           β
          go :: String -> Double -> StdGen -> [(Char , Char)] -> (String, StdGen)
          go acc    _ g [] = (reverse acc, g)
          go acc beta g ((x,y):xs)
                | beta >= v = go (x:acc) beta g' xs
                | otherwise = go (y:acc) beta g' xs
              where (v, g') = randomR (0.0, 1.0) g