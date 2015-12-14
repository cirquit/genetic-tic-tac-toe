{-# LANGUAGE RecordWildCards, PatternSynonyms, ImplicitParams #-}

module Player where

import Data.Vector (Vector(..), elemIndex)
import Debug.Trace (trace)

import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib (playOn, emptyBoard, rotate, toMove, gameState, isValidOn)

data Player = Player { str :: String, fitness :: Int }
  deriving Show

getMove :: Player -> Int -> Move
getMove (Player str _) i = toMove (str !! i)

nextMove :: Vector Board -> Player -> Board -> (Bool, Move,  Board)
nextMove v player board = (move `isValidOn` board, move, move `playOn` board)
  where 
        move  = getMove player index
        index = tryRotations (rotate board) v

tryRotations :: [Board] -> Vector Board -> Int
tryRotations []     v = error $ "No rotation was found"
tryRotations (b:bs) v =
   case elemIndex b v of
      (Just i) -> i -- trace ("Index: " ++ show i) (i)
      Nothing  -> tryRotations bs v

play :: Vector Board -> Player -> Player -> Result Value
play boardV p1 p2 = playGame boardV p1 p2 emptyBoard
  where playGame :: Vector Board -> Player -> Player -> Board -> Result Value
        playGame v p1 p2 b@Board{..} = 
            case nextMove v p1 b of
                (True, _, b') ->
                    case gameState b' of
                        Ongoing -> playGame v p2 p1 b'
                        Tie     -> Tie
                        (Win w) -> Win w
                (False, _, _) -> Win (prev turn)

        prev = succ

playIO :: Vector Board -> Player -> Player -> IO (Result Value)
playIO boardV p1 p2 = playGame boardV p1 p2 emptyBoard
  where playGame :: Vector Board -> Player -> Player -> Board -> IO (Result Value)
        playGame v p1 p2 b@Board{..} = 
            case nextMove v p1 b of
                (True, _, b') ->
                    case gameState b' of
                        Ongoing -> print b' >> playGame v p2 p1 b'
                        Tie     -> print b' >> return Tie
                        (Win w) -> print b' >> return (Win w)
                (False, move, b') -> putStrLn (show move ++ " is invalid") >> return (Win (prev turn))

        prev = succ

