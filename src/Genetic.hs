{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Genetic where

import Data.Vector (Vector(..), fromList)
import Data.List   (foldl', genericLength, splitAt, sortBy)
import Data.Ord    (comparing)
import System.Random

import Player     (Player(..), sortByDscRatio)
import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib   (toBoardVector)
import Crossover

-- |        Chromosome length
--          \_______________/
--                |
genIndividual :: Int -> StdGen -> (Player, StdGen)
genIndividual len g = let player  = Player (take len (randomRs ('A', 'I') g)) 0 1
                          (g', _) = split g
                      in (player, g')


-- |             List size  Chromosome length
--               \_______/  \_______________/
--                 |         /
--                 |        /
genIndividuals :: Int  -> Int -> StdGen -> ([Player], StdGen)
genIndividuals = go []
    where go :: [Player] -> Int -> Int -> StdGen -> ([Player], StdGen)
          go acc    0   _ g = (acc, g)
          go acc size len g = let (res, g') = genIndividual len g
                              in go (res : acc) (size - 1) len g'


-- | mutates every player with δ chance, mutation β percent of the Chromosome
--   no fitness adjusting
--
-- δ = chance to mutate
-- β = percent of the Chromosome to mutate
--
--          δ         β
--          |         |
mutate :: Double -> Double -> StdGen -> [Player] -> ([Player], StdGen)
mutate = go []
    where go :: [Player] -> Double -> Double -> StdGen -> [Player] -> ([Player], StdGen)
          go !acc     _    _ g     [] = ((reverse acc), g) 
          go !acc delta beta g (p:ps)
              | delta >= v = go (p':acc) delta beta g'' ps
              | otherwise  = go (p :acc) delta beta g'  ps
            where (v , g' ) = randomR (0.0, 1.0) g
                  (p', g'') = mutateP p g' beta
--
--                                         β
--                                         |
          mutateP :: Player -> StdGen -> Double -> (Player, StdGen)
          mutateP (Player l fit games) = go ([], fit, games) l
            where go :: (String, Int, Int) -> String -> StdGen -> Double -> (Player, StdGen)
                  go (!acc, fit, games)     [] g    _ = ((Player (reverse acc) fit games), g)
                  go (!acc, fit, games) (x:xs) g beta 
                      | beta >= v = go ((c:acc), fit, games) xs g'' beta
                      | otherwise = go ((x:acc), fit, games) xs g'  beta
                    where (v, g' ) = randomR (0.0, 1.0) g
                          (c, g'') = randomR ('A', 'I') g'


-- | fills up the player list with new individuals up to the population size
--
--            Population size    Chromosome length
--             \___________/     \_______________/
--                        \       |
repopulate :: [Player] -> Int -> Int -> StdGen -> ([Player], StdGen)
repopulate players size len g = (players ++ pop, g')
    where tocreate  = size - length players
          (pop, g') = genIndividuals tocreate len g


-- | θ = percent to be removed by natural selection
-- 
--                    θ
--                    |
naturalselection :: Double -> [Player] -> [Player]
naturalselection tetha players = take best (sortByDscRatio players)
    where best = length players - round (tetha * genericLength players)