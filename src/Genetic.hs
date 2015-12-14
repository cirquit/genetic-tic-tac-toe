{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Genetic where

import Data.Vector (Vector(..), fromList)
import System.Random

import Player     (Player(..))
import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib   (toBoardVector)

-- |            Size 
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


-- | Creates a crossover between two individuals by comparing win chances
--
-- β = percent of the string to mutate
-- 
-- chooses the father-string or the mother-string based on the fitness ratio for every char
-- 
--
crossover :: StdGen -> Player -> Player -> (Player, StdGen)
crossover g (Player str1 fit1) (Player str2 fit2) = ((Player str3 ((fit1 + fit2) `div` 2)), g')

    where (str3, g') = go [] (fromIntegral fit1 / fromIntegral (fit1 + fit2)) g (zip str1 str2)

--                           β
          go :: String -> Double -> StdGen -> [(Char , Char)] -> (String, StdGen)
          go acc    _ g [] = (reverse acc, g)
          go acc beta g ((x,y):xs)
                | beta >= v = go (x:acc) beta g' xs
                | otherwise = go (y:acc) beta g' xs
              where (v, g') = randomR (0.0, 1.0) g


















