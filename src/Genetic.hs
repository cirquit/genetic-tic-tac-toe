{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Genetic where

import Data.Vector (Vector(..), fromList)
import Data.List   (foldl', genericLength, splitAt)
import System.Random

import Player     (Player(..), sortByDescFitness)
import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib   (toBoardVector)

-- |             Size 
genIndividual :: Int -> StdGen -> (Player, StdGen)
genIndividual len g = let player  = Player (take len (randomRs ('A', 'I') g)) 0 1
                          (g', _) = split g
                      in (player, g')


-- |              Size -> Stringlength
genIndividuals :: Int  -> Int          -> StdGen -> ([Player], StdGen)
genIndividuals = go []
    where go :: [Player] -> Int -> Int -> StdGen -> ([Player], StdGen)
          go acc    0   _ g = (acc, g)
          go acc size len g = let (res, g') = genIndividual len g
                              in go (res : acc) (size - 1) len g'

-- | mutates every player with δ chance, mutation β percent of the string
--   no fitness adjusting
--
-- δ = chance to mutate
-- β = percent of the string to mutate
--
--          δ         β
mutate :: Double -> Double -> StdGen -> [Player] -> ([Player], StdGen)
mutate = go []
    where go :: [Player] -> Double -> Double -> StdGen -> [Player] -> ([Player], StdGen)
          go acc     _    _ g     [] = ((reverse acc), g) 
          go acc delta beta g (p:ps)
              | delta >= v = go (p':acc) delta beta g'' ps
              | otherwise  = go (p :acc) delta beta g'  ps
            where (v , g' ) = randomR (0.0, 1.0) g
                  (p', g'') = mutateP p g' beta
--
--                                         β
          mutateP :: Player -> StdGen -> Double -> (Player, StdGen)
          mutateP (Player l fit games) = go ([], fit, games) l
            where go :: (String, Int, Int) -> String -> StdGen -> Double -> (Player, StdGen)
                  go (acc, fit, games)     [] g    _ = ((Player (reverse acc) fit games), g)
                  go (acc, fit, games) (x:xs) g beta 
                      | beta >= v = go ((c:acc), fit, games) xs g'' beta
                      | otherwise = go ((x:acc), fit, games) xs g'  beta
                    where (v, g' ) = randomR (0.0, 1.0) g
                          (c, g'') = randomR ('A', 'I') g'


-- | fills up the player list with new individuals up to the population size
--
--                        Population Size    String Length
repopulate :: [Player] -> Int             -> Int           -> StdGen -> ([Player], StdGen)
repopulate players size len g = (players ++ pop, g')
    where tocreate  = size - length players
          (pop, g') = genIndividuals tocreate len g


-- | θ = percent to be removed by natural selection
-- 
--                    θ
naturalselection :: Double -> [Player] -> [Player]
naturalselection tetha players = take best (sortByDescFitness players)
    where best = length players - round (tetha * genericLength players)


-- |  α = percent to be crossbred, creates (α * length) - 1 new population
-- 
--                α         
alphaCrossover :: Double -> StdGen -> [Player] -> ([Player], StdGen)
alphaCrossover alpha g players     = (sortByDescFitness (rest ++ children), g')
  where best                  = round (alpha * genericLength players) :: Int
        (parents, rest)       = splitAt best players
        (children, g')        = foldl' go ([], g) (zip parents (tail parents))
            where go (acc, g) (p1, p2)  = (child : acc, g')
                      where (child, g') = fitnessCrossoverP g p1 p2


-- roulettecrossover :: Double -> StdGen -> [Player] -> ([Player], StdGen)
-- roulettecrossover alpha g players = 



-- | 

onePointCrossoverP :: StdGen -> Player -> Player -> (Player, Player, StdGen)
onePointCrossoverP g p1 p2 = (p3, p4, g')
    where (percent , g' ) = randomR (0.0, 1.0) g

          len          = genericLength (str p1) :: Double

          (p1A , p1B)  = splitAt (round (percent * len)) (str p1)
          (p2A , p2B)  = splitAt (round (percent * len)) (str p2)

          p3 = Player (p1A ++ p2B) 0 1
          p4 = Player (p2A ++ p1B) 0 1


twoPointCrossoverP :: StdGen -> Player -> Player -> (Player, Player, StdGen)
twoPointCrossoverP g p1 p2 = (p3, p4, g'')
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


-- | If the players have a strings with different lengths, the children will have the both a string with the lesser length

uniformCrossover :: StdGen -> Player -> Player -> (Player, Player, StdGen)
uniformCrossover g p1 p2 = (p3, p4, g')
  where (p3, p4, g') = go ([],[]) g (zip (str p1) (str p2))

        go :: (String, String) -> StdGen -> [(Char, Char)] -> (Player, Player, StdGen)
        go (p3str, p4str) g            [] = (Player (reverse p3str) 0 1, Player (reverse p4str) 0 1, g)
        go (p3str, p4str) g ((c1, c2):xs)
              | v >= 0.5  = go (c1:p3str, c2:p4str) g' xs
              | otherwise = go (c2:p3str, c1:p4str) g' xs
            where (v, g') = randomR (0.0, 1.0) g :: (Double, StdGen)



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