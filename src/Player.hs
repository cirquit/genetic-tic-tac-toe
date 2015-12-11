{-# LANGUAGE RecordWildCards, PatternSynonyms, ImplicitParams #-}

module Player where

import Data.Vector (Vector(..), elemIndex)

import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib (playMove, emptyBoard, rotate, toMove, gameState, isValidOn)

newtype Player = Player String

getMove :: Player -> Int -> Move
getMove (Player str) i = toMove (str !! i)

nextMove :: Vector Board -> Player -> Board -> (Bool, Board)
nextMove v player board = (move `isValidOn` board, playMove board move)
  where 
        move  = getMove player index
        index = tryRotations (rotate board) v

        tryRotations :: [Board] -> Vector Board -> Int
        tryRotations []     v = error $ "No rotation was found for \n" ++ show board
        tryRotations (b:bs) v =
           case elemIndex b v of
              (Just i) -> i
              Nothing  -> tryRotations bs v

play :: (?boardV :: Vector Board) => Player -> Player -> Result Value
play p1 p2 = playGame ?boardV p1 p2 emptyBoard
  where playGame :: Vector Board -> Player -> Player -> Board -> Result Value
        playGame v p1 p2 b@Board{..} = 
            case nextMove v p1 b of
                (True, b') ->
                    case gameState b' of
                        Ongoing -> playGame v p2 p1 b'
                        Tie     -> Tie
                        (Win w) -> Win w
                (False, _) -> Win (prev turn)

        prev = succ

