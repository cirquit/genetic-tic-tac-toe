{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Genetic where

import Data.Vector (Vector(..), fromList)
import Data.List   (foldl', genericLength, splitAt)
import System.Random

import Player     (Player(..), sortByDescFitness)
import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib   (toBoardVector)

-- Let gensize (100) Players play vs each other  -> ([Player], StdGen)  <=> length players = 100
-- crossover 10% best percent          -> ([Player], StdGen)  <=> length players = 105
-- mutation                            --  same as above
-- natural selection 20% worst percent -> length players = 84 (105 - (105 * 0.2))
-- fill up with randoms until gensize (100) -> length players = 100

-- |             Size 
genIndividual :: Int -> StdGen -> (Player, StdGen)
genIndividual len g = let player  = Player (take len (randomRs ('A', 'I') g)) 1
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
--   currently no fitness adjusting
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
          mutateP (Player l fit) = go ([], fit) l
            where go :: (String, Int) -> String -> StdGen -> Double -> (Player, StdGen)
                  go (acc, fit)     [] g    _ = ((Player (reverse acc) fit), g)
                  go (acc, fit) (x:xs) g beta 
                      | beta >= v = go ((c:acc), fit) xs g'' beta
                      | otherwise = go ((x:acc), fit) xs g'  beta
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
naturalselection tetha players = take best players
    where best = length players - round (tetha * genericLength players)


-- |  α = percent to be crossbred, creates α/2 new population
-- 
--                α         
crossover :: Double -> StdGen -> [Player] -> ([Player], StdGen)
crossover alpha g players     = (sortByDescFitness (rest ++ children), g')
  where best                  = round (alpha * genericLength players) :: Int
        (parents, rest)       = splitAt best players
        (children, g')        = foldl' go ([], g) (zip parents (tail parents))
            where go (acc, g) (p1, p2)  = (child : acc, g')
                      where (child, g') = crossoverP g p1 p2

-- | Creates a crossover between two individuals by comparing win chances
--
-- β = percent of the string to mutate
-- 
-- chooses from the father-string or the mother-string based on the fitness ratio for every char
--
crossoverP :: StdGen -> Player -> Player -> (Player, StdGen)
crossoverP g (Player str1 fit1) (Player str2 fit2) = (Player str3 fit3, g')

    where (str3, g') = go [] (fromIntegral fit1 / fromIntegral (fit1 + fit2)) g (zip str1 str2)
          fit3       = (fit1 + fit2) `div` 2

--                           β
          go :: String -> Double -> StdGen -> [(Char , Char)] -> (String, StdGen)
          go acc    _ g [] = (reverse acc, g)
          go acc beta g ((x,y):xs)
                | beta >= v = go (x:acc) beta g' xs
                | otherwise = go (y:acc) beta g' xs
              where (v, g') = randomR (0.0, 1.0) g