{-# LANGUAGE RecordWildCards, PatternSynonyms #-}
module Main where

import Control.Monad
import System.Random (mkStdGen)
import Data.Vector
import Genetic  -- (genIndividual, genIndividuals)
import Player --   (play,playIO)
import BoardLib -- (createBoardPositions)
import BoardTypes

main :: IO ()
main = do
    boardV <- createBoardPositions "lib/tictactoeCombos827.txt"
    let g = mkStdGen 27
        ([p1, p2], _) = genIndividuals 2 850 g
    -- r <- return $ play boardV p1 p2
    r <- playIO boardV p1 p2
    print r