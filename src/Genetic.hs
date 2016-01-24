{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Genetic where

import Data.Vector (Vector(..), fromList)
import Data.List   (foldl', genericLength, splitAt, sortBy)
import Data.Ord    (comparing)
import System.Random
import Control.Monad.Random (MonadRandom(), getRandomR, getRandomRs)

import Player     (Player(..), sortByDscRatio)
import Board.Types (Board(..), Move(..), Value(..), Result(..))
import Crossover

-- |                         Chromosome length
--                           \_______________/
--                                 |
genIndividual :: MonadRandom m => Int -> m Player
genIndividual len = do
  stream <- getRandomRs ('A', 'I')
  let chrom = take len stream
  return $ Player chrom 0 0 0 0 1


-- |                        Population size  Chromosome length
--                          \_____________/  \_______________/
--                                  |         /
--                                  |        /
genIndividuals :: MonadRandom m => Int  -> Int -> m [Player]
genIndividuals = go []
    where go :: MonadRandom m => [Player] -> Int -> Int -> m [Player]
          go acc    0   _ = return acc
          go acc size len = do
              res <- genIndividual len
              go (res : acc) (size - 1) len


-- | mutates every player with δ chance, mutation β percent of the Chromosome
--   no fitness adjusting
--
-- δ = chance to mutate
-- β = percent of the Chromosome to mutate
--
--                           δ         β
--                           |         |
mutate :: MonadRandom m => Double -> Double -> [Player] -> m [Player]
mutate = go []
    where go :: MonadRandom m => [Player] -> Double -> Double -> [Player] -> m [Player]
          go !acc     _    _     [] = return $ reverse acc
          go !acc delta beta (p:ps) = do
              v  <- getRandomR (0.0, 1.0)
              p' <- mutateP p beta
              case delta >= v of
                  True  -> go (p':acc) delta beta ps
                  False -> go (p :acc) delta beta ps

--
--                                                β
--                                                |
          mutateP :: MonadRandom m => Player -> Double -> m Player
          mutateP (Player l turns wins ties losses games) = go ([], turns, wins, ties, losses, games) l
            where go :: MonadRandom m => (String, Int, Int, Int, Int, Int) -> String -> Double -> m Player
                  go (!acc, turns, wins, ties, losses, games)     []    _ = return $ Player (reverse acc) turns wins ties losses games
                  go (!acc, turns, wins, ties, losses, games) (x:xs) beta = do
                      v <- getRandomR (0.0, 1.0)
                      c <- getRandomR ('A', 'I')
                      case beta >= v of
                          True  -> go ((c:acc), turns, wins, ties, losses, games) xs beta
                          False -> go ((x:acc), turns, wins, ties, losses, games) xs beta

-- | fills up the player list with new individuals up to the population size
--
--                             Population size    Chromosome length
--                              \___________/     \_______________/
--                                         \       |
repopulate :: MonadRandom m => [Player] -> Int -> Int -> m [Player]
repopulate players size chromlen = do
    let popsize = size - length players
    pop <- genIndividuals popsize chromlen
    return $ players ++ pop

-- | θ = percent to be removed by natural selection
-- 
--                    θ
--                    |
naturalselection :: Double -> [Player] -> [Player]
naturalselection tetha players = take best (sortByDscRatio players)
    where best = length players - round (tetha * genericLength players)