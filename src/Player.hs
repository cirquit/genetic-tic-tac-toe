{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns, ViewPatterns #-}

module Player where

--------------------------------------------------------------------

import Data.Vector as V (Vector(..), elemIndex, (!), length)
import Debug.Trace      (trace)
import Data.List        (foldl', genericLength, sortBy)
import Data.Ord         (comparing)
import Data.Function    (on)
import System.Random.Shuffle (shuffleM)
import Control.Monad.Random (MonadRandom(), getRandomR)
import Data.Map as M (insert, Map(..), empty, lookup)
import Data.Word (Word8(..))

import Debug.Trace

--------------------------------------------------------------------

--------------------------------------------------------------------

import Board.Types (Board(..), Move(..), Value(..), Result(..), Rotation(..))
import Board.Utils -- (playOn, emptyBoard, toMoveRot, gameState, isValidOn, hashBoard)
--------------------------------------------------------------------

--------------------------------------------------------------------
-- | Player definition & instances
--
-- 
data Player = Player { moves       :: [Word8]     -- moves encoded as Chars for every possible boardstate
                     , turnsLived  :: Int        -- turns lived
                     , wins        :: Int        -- total wins
                     , ties        :: Int        -- total ties
                     , losses      :: Int
                     , played      :: Bool
                     , games       :: Int }      -- amount of games played

instance Eq Player where
  p1 == p2 = (moves p1) == (moves p2)


instance Show Player where
    show p@(Player moves turnsLived wins ties losses played games) = 
        unwords [ "# Player"           , show $ take 10 moves
              --  , "# Avg turns lived:" , percent
                , "# Wins:"            , show wins
                , "# Ties:"            , show ties
                , "# Loss:"             , show losses
                , "# Games:"           , show games
                , "# Fits:"            , show (fitnessRatio p)
                ]
      where percent = take 6 $ show ((fromIntegral turnsLived) / (fromIntegral games))
--------------------------------------------------------------------

--------------------------------------------------------------------
-- | shortcut to increment turnsLived count
--
newPlayer :: [Word8] -> Player
newPlayer moves = Player moves 0 0 0 0 False 1


newSPlayer :: String -> Player
newSPlayer moves = Player (map readW8 moves) 0 0 0 0 False 1
    where
        readW8 :: Char -> Word8
        readW8 = read . (:[])

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
-- | shortcut to decrement game count
--
rmGame :: Player -> Player
rmGame p = p { games = games p - 1 }


--------------------------------------------------------------------
-- | shortcut to increment tie count
--
addTie :: Player -> Player
addTie p = p { ties = ties p + 1 }

--------------------------------------------------------------------
-- | shortcut to increment win count
--
addWin :: Player -> Player
addWin p = p { wins = wins p + 1 }


--------------------------------------------------------------------
-- | shortcut to increment win count
--
addLoss :: Player -> Player
addLoss p = p { losses = losses p + 1 }



{- DEPRECATED -}
--------------------------------------------------------------------
-- | select the move from the chromosome based on the index
--
{-
getMove :: Player -> Int -> Move
getMove (Player str _ _) i = toMove (str !! i)
-}

getMoveRot :: Player -> Int -> Rotation -> Move
getMoveRot (Player moves _ _ _ _ _ _) i rot = toMoveRot (moves !! i) rot

--------------------------------------------------------------------
-- | calculates the next move for a single player based on the current board state
--  
--   returns the validity of the move, the calculated move and the new current board state
--

nextMove ::  Map Int (Int, Rotation) -> Player -> Board -> (Bool, Move, Board)
nextMove hmap player board = (move `isValidOn` board, move, move `playOn` board)
  where 
        move  = getMoveRot player i rot

        hashedBoard     = hashBoard board
        (Just (i, rot)) = M.lookup hashedBoard hmap

---------------------------------------------------------------------
-- | one player plays all permutations from emptyboard using a stack-queue
--
--   
playSingle :: Map Int (Int, Rotation) -> Map Int [Board] -> Player -> Player
playSingle _      _        p@Player{ played = True } = p
playSingle rotMap nextMMap p                         = p'' { played = True }
    where 

        (Just round1Boards) = M.lookup (hashBoard emptyBoard) nextMMap

        p'  = go [emptyBoard] rotMap nextMMap p
        p'' = go round1Boards rotMap nextMMap p'

        go :: [Board] -> Map Int (Int, Rotation) -> Map Int [Board] -> Player -> Player
        go []     rotMap nextMMap p = p
        go (x:xs) rotMap nextMMap p =
            case playGame p x of
                Left p'       -> go xs rotMap nextMMap (addGame p')
                Right (p', b) -> let (Just nextBs) = M.lookup (hashBoard b) nextMMap
                                     !futureBoards  = nextBs ++ xs
                                 in go futureBoards rotMap nextMMap p'
            where 

                playGame :: Player -> Board -> Either Player (Player, Board)
                playGame p board
                  | Tie     <- gameState board = Left (addTie  $ p)-- Left (addFutureLosses board . addTie  $ p)
                  | (Win _) <- gameState board = Left (addLoss $ p)-- Left (addFutureLosses board . addLoss $ p)
                  | Ongoing <- gameState board =
                      case nextMove rotMap p board of
                          (True, _, board') -> 
                              case gameState board' of
                                  Ongoing     -> Right (p, board')
                                  (Win _)     -> Left (addWin p)-- Left (addFutureLosses board' . addWin $ p)
                                  Tie         -> Left (addTie p)-- Left (addFutureLosses board' . addTie $ p)
                          (False, _, _) -> Left (rmGame p) -- Left (addLoss $ p) -- Left (addFutureLosses board . addLoss $ p)

{-
                addFutureLosses :: Board -> Player -> Player
                addFutureLosses b p = p { losses = futureLosses + losses p, games = futureLosses + games p }
                    where

                          emptyFields = foldl' countEmpty 0 (encodeBoard b)  -- XOOO___OXX

                          countEmpty xs '_' = 1 + xs
                          countEmpty xs _   = xs

                          futureLosses = go emptyFields 1
                              where go n m
                                       | 2 < (n - m) = (n - m) * (go n (m + 2))
                                       | otherwise   = 2
-}

---------------------------------------------------------------------
-- | loop for two players to play a game
--
--   returns both players and the game result
--
play ::  Map Int (Int, Rotation) -> Player -> Player -> Board -> (Player, Player, Result Value)
play hmap p1 p2 board = 
    case playGame hmap p1 p2 board of
        (p1', p2', (Win X)) -> (addGame p1', addGame p2', (Win X))
        (p1', p2', (Win O)) -> (addGame p1', addGame p2', (Win O))
        (p1', p2', Tie    ) -> (addGame p1', addGame p2',  Tie   )

  where playGame ::  Map Int (Int, Rotation) -> Player -> Player -> Board -> (Player, Player, Result Value)
        playGame hmap p1 p2 b@Board{..} = 
            case nextMove hmap p1 b of
                (True, _, b') ->
                    case gameState b' of
                        Ongoing -> playGame hmap p2 p1 b'
                        Tie     -> (addTie p1, addTie  p2, Tie  )
                        (Win X) -> (addWin p1, addLoss p2, Win X)     -- to ensure that all p1 and p2 are the "starting" p1 and p2
                        (Win O) -> (addWin p2, addLoss p1, Win O)     -- 
                (False, _, b')  -> 
                    case turn of
                        X -> (        p1,  addLoss p2, Win O)     -- to ensure that all p1 and p2 are the "starting" p1 and p2
                        O -> (addLoss p2,          p1, Win X)     -- 

---------------------------------------------------------------------
-- | loop for two players to play a game with IO
--
--   returns both players and the game result
--
--   TODO update the use with HashMap
{-
playIO :: Vector Board -> Player -> Player -> Board ->  IO (Player, Player, Result Value)
playIO boardV p1 p2 board = playGame boardV p1 p2 board >>= \res -> 
    case res of
        (p1', p2', (Win X)) -> return (addGame p1', addGame p2', (Win X))
        (p1', p2', (Win O)) -> return (addGame p1', addGame p2', (Win O))
        (p1', p2', Tie    ) -> return (addGame p1', addGame p2',  Tie   )


  where playGame :: Vector Board -> Player -> Player -> Board -> IO (Player, Player, Result Value)
        playGame v p1 p2 b@Board{..} = 
            case nextMove v p1 b of
                (True, _, b') ->
                    case gameState b' of
                        Ongoing  -> print b' >> playGame v p2 (addTurn p1) b'
                        Tie      -> print b' >> putStrLn (show ((Tie) :: Result Value)) >> return (addTurn p1, p2, Tie)
                        (Win X)  -> print b' >> putStrLn (show p1 ++ " won!")           >> return (addTurn p1, p2, (Win X))                     -- to ensure that all p1 and p2 are the "starting" p1 and p2
                        (Win O)  -> print b' >> putStrLn (show p1 ++ " won!")           >> return (addTurn p2, p1, (Win O))                     -- 
                (False, move, _) ->
                    case turn of
                        X -> putStrLn (show move ++ " is invalid") >> putStrLn (show p1 ++ " won!") >> return (p1, p2, Win O) -- to ensure that all p1 and p2 are the "starting" p1 and p2
                        O -> putStrLn (show move ++ " is invalid") >> putStrLn (show p1 ++ " won!") >> return (p2, p1, Win X) --
-}

---------------------------------------------------------------------
-- | Lets the population play on every board possible
-- 
populationPlay :: Vector Board -> Map Int (Int, Rotation) -> [Player] -> [Player]
populationPlay v m      [] = []
populationPlay v m (p1:ps) = p1' : populationPlay v m ps'
    where
          (p1', ps') = go v m p1 ps []

          go :: Vector Board -> Map Int (Int, Rotation) -> Player -> [Player] -> [Player] -> (Player, [Player])
          go v m p1     []  acc  = (p1, reverse acc)
          go v m p1 (p2:ps) !acc  = go v m p1' ps (p2':acc)
              where
                   (p1', p2') = playAllBoards (v, V.length v - 1) m p1 p2

          playAllBoards :: (Vector Board, Int) -> Map Int (Int, Rotation) -> Player -> Player -> (Player, Player)
          playAllBoards (v, -1) m p1 p2 = (p1, p2)
          playAllBoards (v,  n) m p1 p2 = playAllBoards (v, n - 1) m p1'' p2''
              where
                    (p1' , p2' , _) = play m p1  p2  (v ! n)
                    (p2'', p1'', _) = play m p2' p1' (v ! n)

---------------------------------------------------------------------
-- | Lets the population play on an empty board
--
populationPlayEmptyBoard :: Map Int (Int, Rotation) -> [Player] -> [Player]
populationPlayEmptyBoard m [] = []
populationPlayEmptyBoard m (p1:ps) = p1' : populationPlayEmptyBoard m ps'
    where 
          (p1', ps') = go m p1 ps []

          go :: Map Int (Int, Rotation) -> Player -> [Player] -> [Player] -> (Player, [Player])
          go m p1     []  acc  = (p1, reverse acc)
          go m p1 (p2:ps) acc  = go m p1' ps (p2':acc)
              where
                   (p1', p2') = playEmptyBoard m p1 p2

          playEmptyBoard :: Map Int (Int, Rotation) -> Player -> Player -> (Player, Player)
          playEmptyBoard m p1 p2 = (p1'', p2'')
              where
                   (p1',  p2' , _) = play m p1  p2  emptyBoard
                   (p2'', p1'', _) = play m p2' p1' emptyBoard
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
sortByAscRatio = sortBy (comparing fitnessRatio)

sortByDscRatio :: [Player] -> [Player]
sortByDscRatio = sortBy (flip (comparing fitnessRatio))


fitnessRatio :: Player -> Double
fitnessRatio p =  gamesD -- (gamesD - toD (losses p)) / gamesD 

-- gamesD * toD ( wins p) * toD (ties p) / 10000000

--      | gamesD < 827 = gamesD + toD (wins p) + toD (ties p)
--      | otherwise    = gamesD + toD (wins p) * 4 + toD (ties p) * 2
    where
          -- (gamesD - toD (losses p)) / gamesD -- winRatio -- * turnsRatio
       --   winRatio = toD (3 * wins p + 2 * ties p + losses p) / gamesD

          -- winRatio   = (toD (wins x) +  0.5 * toD (ties x)) / gamesD  -- [0..1] (bounds)

          -- turnsRatio = toD (turnsLived x)  / gamesD                   -- [0..4,5]         (bounds)

          gamesD = toD (games p)

          toD = fromIntegral



sumRatios :: [Player] -> Double
sumRatios = foldl (\acc x -> acc + fitnessRatio x) 0.0


ascendingPieChart :: [Player] -> [(Player, Double)]
ascendingPieChart players = foldl (\acc x -> (x, fitnessRatio x / sumR) : acc) [] descPlayers
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
comparePlayers :: [Word8] -> [Word8] -> Int
comparePlayers (x:xs) (y:ys)
  | x == y    = comparePlayers xs ys
  | otherwise = 1 +  comparePlayers xs ys
comparePlayers    xs     ys = 0
