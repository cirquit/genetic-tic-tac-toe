{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Player where

--------------------------------------------------------------------

import Data.Vector as V (Vector(..), elemIndex, (!), length)
import Debug.Trace      (trace)
import Data.List        (foldl', genericLength, sortBy)
import Data.Ord         (comparing)
import Data.Function    (on)
import Control.Monad.Random (MonadRandom(), getRandomR)
--------------------------------------------------------------------

--------------------------------------------------------------------

import BoardTypes (Board(..), Move(..), Value(..), Result(..))
import BoardLib   (playOn, emptyBoard, rotate, toMove, gameState, isValidOn)
--------------------------------------------------------------------

--------------------------------------------------------------------
-- | Player definition & instances
--
-- 
data Player = Player { str        :: String  -- moves encoded as Chars for every possible boardstate
                     , turnsLived :: Int     -- turns lived
                     , games      :: Int }   -- amount of games played
  deriving Eq


instance Show Player where
    show (Player str turnsLived games) = 
        unwords [ "# Player"               , take 20 str
                , "# Avg turns lived in %:", percent
                , "# Games:"               , show games
                ]
      where percent = take 6 $ show ((fromIntegral turnsLived) / (fromIntegral games))
--------------------------------------------------------------------


--------------------------------------------------------------------
-- | shortcut to increment turnsLived count
--
addTurn :: Player -> Player
addTurn p = p { turnsLived = turnsLived p + 1 }

--------------------------------------------------------------------
-- | shortcut to increment game count
--
addGame :: Player -> Player
addGame p = p { games = games p + 1 }


--------------------------------------------------------------------
-- | select the move from the chromosome based on the index
--
getMove :: Player -> Int -> Move
getMove (Player str _ _) i = toMove (str !! i)


--------------------------------------------------------------------
-- | calculates the next move for a single player based on the current board state
--  
--   returns the validity of the move, the calculated move and the new current board state
--

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


---------------------------------------------------------------------
-- | loop for two players to play a game
--
--   returns both players and the game result
--
play :: Vector Board -> Player -> Player -> Board -> (Player, Player, Result Value)
play boardV p1 p2 board = 
    case playGame boardV p1 p2 board of
        (p1', p2', (Win X)) -> (addGame p1', addGame p2', (Win X))
        (p1', p2', (Win O)) -> (addGame p1', addGame p2', (Win O))
        (p1', p2', Tie    ) -> (addGame p1', addGame p2',  Tie   )

  where playGame :: Vector Board -> Player -> Player -> Board -> (Player, Player, Result Value)
        playGame v p1 p2 b@Board{..} = 
            case nextMove v p1 b of
                (True, _, b') ->
                    case gameState b' of
                        Ongoing -> playGame v p2 (addTurn p1) b'
                        Tie     -> (addTurn p1, p2, Tie)
                        (Win w) -> (addTurn p1, p2, Win w)
                (False, _, _) -> (p1, p2, Win (prev turn))

        prev = succ

{- 
playIO :: Vector Board -> Player -> Player -> Board ->  IO (Player, Player, Result Value)
playIO boardV p1 p2 board = playGame boardV p1 p2 board >>= \res -> 
    case res of
        (Win X) -> return (p1 { fitness = fitness p1 + 1 , games = games p1 + 1 }, p2                            { games = games p2 + 1 }, (Win X))
        (Win O) -> return (p1                            { games = games p1 + 1 }, p2 { fitness = fitness p2 + 1 , games = games p2 + 1 }, (Win O))
        Tie     -> return (p1 { fitness = fitness p1 + 1 , games = games p1 + 1 }, p2 { fitness = fitness p2 + 1 , games = games p2 + 1 },  Tie   )
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

-}

populationPlay :: Vector Board -> [Player] -> [Player]
populationPlay v      [] = []
populationPlay v (p1:ps) = p1' : populationPlay v ps'
    where
          (p1', ps') = go v p1 ps []

          go :: Vector Board -> Player -> [Player] -> [Player] -> (Player, [Player])
          go v p1     []  acc  = (p1, reverse acc)
          go v p1 (p2:ps) acc  = go v p1' ps (p2':acc)
              where
                   (p1', p2') = playAllBoards (v, V.length v - 1) p1 p2

          playAllBoards :: (Vector Board, Int) -> Player -> Player -> (Player, Player)
          playAllBoards (v, -1) p1 p2 = (p1, p2)
          playAllBoards (v,  n) p1 p2 = playAllBoards (v, n - 1) p1'' p2''
              where
                    (p1' , p2' , _) = play v p1  p2  (v ! n)
                    (p2'', p1'', _) = play v p2' p1' (v ! n)

populationPlayEmptyBoard :: Vector Board -> [Player] -> [Player]
populationPlayEmptyBoard v [] = []
populationPlayEmptyBoard v (p1:ps) = p1' : populationPlayEmptyBoard v ps'
    where 
          (p1', ps') = go v p1 ps []

          go :: Vector Board -> Player -> [Player] -> [Player] -> (Player, [Player])
          go v p1     []  acc  = (p1, reverse acc)
          go v p1 (p2:ps) acc  = go v p1' ps (p2':acc)
              where
                   (p1', p2') = playEmptyBoard v p1 p2

          playEmptyBoard :: Vector Board -> Player -> Player -> (Player, Player)
          playEmptyBoard v p1 p2 = (p1'', p2'')
              where
                   (p1',  p2' , _) = play v p1  p2  emptyBoard
                   (p2'', p1'', _) = play v p2' p1' emptyBoard
{-
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
              (p1', p2') <- playAllBoardsIO (v, V.length v - 1) p1 p2
              go v p1' ps (p2':acc)

          playAllBoardsIO :: (Vector Board, Int) -> Player -> Player -> IO (Player, Player)
          playAllBoardsIO (v, -1) p1 p2 = return (p1, p2)
          playAllBoardsIO (v,  n) p1 p2 = do
              (p1' , p2' , _) <- playIO v p1  p2  (v ! n)
              (p2'', p1'', _) <- playIO v p2' p1' (v ! n)
              playAllBoardsIO (v, n - 1) p1'' p2''
-}


sortByAscRatio :: [Player] -> [Player]
sortByAscRatio = sortBy (comparing turnsRatio)

sortByDscRatio :: [Player] -> [Player]
sortByDscRatio = sortBy (flip (comparing turnsRatio))


turnsRatio :: Player -> Double
turnsRatio x = toD (turnsLived x) / toD (games x)
    where toD = fromIntegral


sumRatios :: [Player] -> Double
sumRatios = foldl (\acc x -> acc + turnsRatio x) 0.0


ascendingPieChart :: [Player] -> [(Player, Double)]
ascendingPieChart players = foldl (\acc x -> (x, turnsRatio x / sumR) : acc) [] descPlayers
    where sumR        = sumRatios players
          descPlayers = sortByDscRatio players

-- | gets an ascending pieChart and returns two semi-random unique players
--
getUniquePlayers :: MonadRandom m => [(Player, Double)] -> m (Player, Player)
getUniquePlayers l = getRandomR (0.0, 1.0) >>= findPartner l . getPlayer l
  where
      findPartner :: MonadRandom m => [(Player, Double)] -> Player -> m (Player, Player)
      findPartner l p1 = do
          v <- getRandomR (0.0, 1.0)
          let p2 = getPlayer l v
          case p1 == p2 of
              True  -> findPartner l p1
              False -> return (p1, p2)

      getPlayer :: [(Player, Double)] -> Double -> Player
      getPlayer ((player, r):ps) v
            | v' <= 0.0 = player
            | otherwise = getPlayer ps v'
          where v' = v - r


-- | returns the amount of chars which are not the same 
--   if the Chromosomelength is not the same, the rest will be ignored
--
comparePlayers :: String -> String -> Int
comparePlayers (x:xs) (y:ys)
  | x == y    = comparePlayers xs ys
  | otherwise = 1 +  comparePlayers xs ys
comparePlayers    xs     ys = 0