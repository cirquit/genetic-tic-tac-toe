{-# LANGUAGE RecordWildCards, PatternSynonyms #-}

module Player where

import Data.Vector (Vector(..), elemIndex)
import Debug.Trace (trace)
import Data.List   (foldl', genericLength, sortBy)
import Data.Ord    (comparing)
import Data.Function (on)


import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib (playOn, emptyBoard, rotate, toMove, gameState, isValidOn)

data Player = Player { str :: String, fitness :: Int }


instance Show Player where
  show (Player str fitness) = "Player # " ++ take 12 str ++ " # Fitness: " ++ show fitness

getMove :: Player -> Int -> Move
getMove (Player str _) i = toMove (str !! i)

nextMove :: Vector Board -> Player -> Board -> (Bool, Move, Board)
nextMove v player board = (move `isValidOn` board, move, move `playOn` board)
  where 
        move  = getMove player index
        index = tryRotations (rotate board) v

tryRotations :: [Board] -> Vector Board -> Int
tryRotations []     v = error "No rotation was found"
tryRotations (b:bs) v =
   case elemIndex b v of
      (Just i) -> i -- trace ("Index: " ++ show (i + 1)) (i)
      Nothing  -> tryRotations bs v

play :: Vector Board -> Player -> Player -> (Player, Player, Result Value)
play boardV p1 p2 = 
    case playGame boardV p1 p2 emptyBoard of
        (Win X) -> (p1 { fitness = fitness p1 + 1 }, p2                             , (Win X))
        (Win O) -> (p1                             , p2 { fitness = fitness p2 + 1 }, (Win O))
        Tie     -> (p1 { fitness = fitness p1 + 1 }, p2 { fitness = fitness p2 + 1 },  Tie   )
        Ongoing -> error "Game is not finished but was evaluated by play"

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

playIO :: Vector Board -> Player -> Player -> IO (Player, Player, Result Value)
playIO boardV p1 p2 = playGame boardV p1 p2 emptyBoard >>= \res -> 
    case res of
        (Win X) -> return (p1 { fitness = fitness p1 + 1 }, p2                             , (Win X))
        (Win O) -> return (p1                             , p2 { fitness = fitness p2 + 1 }, (Win O))
        Tie     -> return (p1 { fitness = fitness p1 + 1 }, p2 { fitness = fitness p2 + 1 },  Tie   )
        Ongoing -> error "Game is not finished but was evaluated by playIO"


  where playGame :: Vector Board -> Player -> Player -> Board -> IO (Result Value)
        playGame v p1 p2 b@Board{..} = 
            case nextMove v p1 b of
                (True, _, b') ->
                    case gameState b' of
                        Ongoing -> print b' >> playGame v p2 p1 b'
                        Tie     -> print b' >> putStrLn (show ((Tie) :: Result Value)) >> return Tie
                        (Win w) -> print b' >> putStrLn (show p1 ++ " won!") >> return (Win w)
                (False, move, b') -> putStrLn (show move ++ " is invalid") >> putStrLn (show p1 ++ " won!") >> return (Win (prev turn))

        prev = succ

populationPlay :: Vector Board -> [Player] -> [Player]
populationPlay v      [] = []
populationPlay v (p1:ps) = p1' : populationPlay v ps'
    where
          (p1', ps') = go v p1 ps []

          go :: Vector Board -> Player -> [Player] -> [Player] -> (Player, [Player])
          go v p1     []  acc = (p1, reverse acc)
          go v p1 (p2:ps) acc = go v p1' ps (p2':acc)
              where (p1', p2', _) = play v p1 p2

populationPlayIO :: Vector Board -> [Player] -> IO [Player]
populationPlayIO v      [] = return []
populationPlayIO v (p1:ps) = do
        (p1', ps') <- go v p1 ps []
        rest       <- populationPlayIO v ps'
        return $ p1' : rest 
    where
          go :: Vector Board -> Player -> [Player] -> [Player] -> IO (Player, [Player])
          go v p1     []  acc = return (p1, reverse acc)
          go v p1 (p2:ps) acc = do
              (p1', p2', _) <- playIO v p1 p2
              go v p1' ps (p2':acc)

sortByDescFitness :: [Player] -> [Player]
sortByDescFitness = sortBy (flip (compare `on` fitness))