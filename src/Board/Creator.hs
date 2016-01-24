{-# LANGUAGE RecordWildCards #-}

module Board.Creator where

import Data.Hashable
import Data.List
import Data.Map as M (insert, Map(..), empty)

import Board.Utils
import Board.Types

createMinHashed :: [(Board, Rotation)] -> String
createMinHashed bs = encodeBoard . snd $ minHash
    where hashed  = map (\(x,_) -> (hashBoard x, x)) bs
          minHash = minimumBy (\(x,_) (y,_)-> compare x y) hashed

saveBestHashes :: IO ()
saveBestHashes = do
    l <- createBoardStatesFrom "lib/tictactoeCombos827.txt"
    let l' = map (createMinHashed . createRotations) l
    writeFile "lib/testinCombos827.txt" (unlines l')