{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module Board.Utils where

import Data.List (foldl')
import Data.Vector (Vector(..), fromList)
import Data.Hashable (hash)
import Data.Map as M (insert, Map(..), empty)


import Board.Types (Board(..), Move(..), Value(..), Result(..), Rotation(..))

---------------------------------------------------------------------
-- | does not check if the move is valid
-- 
playOn ::  Move -> Board -> Board
playOn A1 b@Board{..} = b { a1 = Just turn, turn = next turn }
playOn A2 b@Board{..} = b { a2 = Just turn, turn = next turn }
playOn A3 b@Board{..} = b { a3 = Just turn, turn = next turn }
playOn B1 b@Board{..} = b { b1 = Just turn, turn = next turn }
playOn B2 b@Board{..} = b { b2 = Just turn, turn = next turn }
playOn B3 b@Board{..} = b { b3 = Just turn, turn = next turn }
playOn C1 b@Board{..} = b { c1 = Just turn, turn = next turn }
playOn C2 b@Board{..} = b { c2 = Just turn, turn = next turn }
playOn C3 b@Board{..} = b { c3 = Just turn, turn = next turn }

next = succ

---------------------------------------------------------------------
-- | returns Tie, Ongoing or Win [X | O]
--
gameState :: Board -> Result Value
gameState b@Board{..}
    | same a1 a2 a3 = Win (succ turn)
    | same b1 b2 b3 = Win (succ turn)
    | same c1 c2 c3 = Win (succ turn)
    | same a1 b1 c1 = Win (succ turn)
    | same a2 b2 c2 = Win (succ turn)
    | same a3 b3 c3 = Win (succ turn)
    | same a1 b2 c3 = Win (succ turn)
    | same a3 b2 c1 = Win (succ turn)
  where same (Just a) (Just b) (Just c) = a == b && b == c
        same       _        _        _  = False
gameState (Board (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) = Tie
gameState _                                                                                          = Ongoing

---------------------------------------------------------------------
-- | checks validity by the changes on the board after the move
--
isValidOn :: Move -> Board -> Bool
isValidOn move b = changes b /= changes (move `playOn` b)
    where
        changes b@Board{..} = foldl' countEmpty 0 [a1,a2,a3,b1,b2,b3,c1,c2,c3]

        countEmpty acc Nothing = acc + 1
        countEmpty acc _       = acc

---------------------------------------------------------------------
-- | starting board shortcut
--
emptyBoard :: Board
emptyBoard = Board e e e  -- row 1
                   e e e  -- row 2
                   e e e  -- row 3
                   X      -- start move
  where e = Nothing

{- DEPRECATED -}
{- 
-- | creates all possible rotations for one boardstate
--   TODO: memoisation
--
rotate :: Board -> [Board]
rotate b0 = [b0, b1, b2, b3, b4, b5, b6, b7]
  where b1 = cw b0
        b2 = cw b1
        b3 = cw b2
        b4 = mirror b0
        b5 = cw b4
        b6 = cw b5
        b7 = cw b6

        cw :: Board -> Board
        cw b@Board{..} = b { a1 = c1, a2 = b1, a3 = a1
                           , b1 = c2         , b3 = a2
                           , c1 = c3, c2 = b3, c3 = a3 }
        
        mirror :: Board -> Board
        mirror b@Board{..} = b { a1 = a3         , a3 = a1
                               , b1 = b3         , b3 = b1
                               , c1 = c3         , c3 = c1 }

-}

{- DEPRECATED -}
{-
-- | Encoding for moves
--
toMove :: Char -> Move
toMove 'A' = A1
toMove 'B' = A2
toMove 'C' = A3
toMove 'D' = B1
toMove 'E' = B2
toMove 'F' = B3
toMove 'G' = C1
toMove 'H' = C2
toMove 'I' = C3
toMove  x  = error ("BoardLib.toMove: could not find the move: <" ++ x:">")
-}


---------------------------------------------------------------------
-- | Encoding for moves with rotation
--
toMoveRot :: Char -> Rotation -> Move
toMoveRot 'A' rot = applyRot A1 rot
toMoveRot 'B' rot = applyRot A2 rot
toMoveRot 'C' rot = applyRot A3 rot
toMoveRot 'D' rot = applyRot B1 rot
toMoveRot 'E' rot = applyRot B2 rot
toMoveRot 'F' rot = applyRot B3 rot
toMoveRot 'G' rot = applyRot C1 rot
toMoveRot 'H' rot = applyRot C2 rot
toMoveRot 'I' rot = applyRot C3 rot
toMoveRot  x  _   = error ("BoardLib.toMoveRot: could not find the move: <" ++ x:">")


---------------------------------------------------------------------
-- | Hardcoded rotation for performance purposes
--
applyRot :: Move -> Rotation -> Move
applyRot m (Rotation mir cw) = turn cw . mirror mir $ m

    where mirror True A1 = A3
          mirror True B1 = B3
          mirror True C1 = C3

          mirror True A3 = A1
          mirror True B3 = B1
          mirror True C3 = C1

          mirror _ x = x

          turn 0 move = move
          turn n A1 = turn (n-1) C1
          turn n B1 = turn (n-1) C2
          turn n C1 = turn (n-1) C3

          turn n A2 = turn (n-1) B1
          turn n C2 = turn (n-1) B3

          turn n A3 = turn (n-1) A1
          turn n B3 = turn (n-1) A2
          turn n C3 = turn (n-1) A3

          turn _ m  = m


---------------------------------------------------------------------
-- | board parser for every possible gamestate encoded in a single .txt-file
-- 
toBoardList :: String -> [Board]
toBoardList input = go (lines input) []
  where 
        go :: [String] -> [Board] -> [Board]
        go []      acc = reverse acc
        go (x:xs) !acc = go xs (toBoard x : acc)

        toBoard :: String -> Board
        toBoard l@[a1, a2, a3, b1, b2, b3, c1, c2, c3] =
                Board { a1 = toV a1, a2 = toV a2, a3 = toV a3
                      , b1 = toV b1, b2 = toV b2, b3 = toV b3
                      , c1 = toV c1, c2 = toV c2, c3 = toV c3
                      , turn = turn' }

            where turn' 
                      | even underscores = X
                      | otherwise        = O
                    where underscores = foldl' (\acc e -> if e == '_' then acc - 1 else acc) 9 l

        toV :: Char -> Maybe Value
        toV 'X' = Just X
        toV 'O' = Just O
        toV '_' = Nothing

---------------------------------------------------------------------
-- | shortcut for creating all possible board states from a .txt
-- 
createBoardStatesFrom :: FilePath -> IO [Board]
createBoardStatesFrom fp = readFile fp >>= (return . toBoardList)


---------------------------------------------------------------------
-- | creates the (hopefully) fastests implementation for lookups
--   
--   * Every possible unique boardstate (827) has a different hash
--   * For every unique boardstate there are 8 possible permutations
--   * We compute the hash for the 8 possible permutations and save the lowest one to the .txt file
--
--
--   * The key for this map is every possible boardstate as hash
--   * The value is the index in the chromosome and the rotation to the "base case"
--
--
--                             Hashed Board  Index for the Chromosome     Rotation for the current board
--                             \__________/   /                              /
--                                    |      /         _____________________/
--                                    |     /         /
createHashMap :: FilePath -> IO (Map Int (Int, Rotation))
createHashMap fp = do

        l <- createBoardStatesFrom fp

        let boardRots      = map createRotations l
            enumBoardRots  = zip [0..] boardRots

        return $ go enumBoardRots M.empty

    where
        go :: [(Int, [(Board, Rotation)])] -> Map Int (Int, Rotation) -> Map Int (Int, Rotation)
        go []            m = m
        go ((i, brs):xs) m = go xs m'
            where m' = foldl' insertBoardRotations m brs

                  insertBoardRotations :: Map Int (Int, Rotation) -> (Board, Rotation) -> Map Int (Int, Rotation)
                  insertBoardRotations m (board, rot) = M.insert bhash (i, rot) m
                      where bhash = hashBoard board

---------------------------------------------------------------------
-- | creates all possible rotations for a single board, whilst encoding how to rotation is to the "base"-case
--
createRotations :: Board -> [(Board, Rotation)]
createRotations b = [b0, b1, b2, b3, b4, b5, b6, b7]
    where
        b0 = (          b,     Rotation False 0)
        b1 = (cw (fst b0),     Rotation False 1)
        b2 = (cw (fst b1),     Rotation False 2)
        b3 = (cw (fst b2),     Rotation False 3)
        b4 = (mirror (fst b0), Rotation True  0)
        b5 = (cw (fst b4),     Rotation True  1)
        b6 = (cw (fst b5),     Rotation True  2)
        b7 = (cw (fst b6),     Rotation True  3)

        cw :: Board -> Board
        cw b@Board{..} = b { a1 = c1, a2 = b1, a3 = a1
                           , b1 = c2         , b3 = a2
                           , c1 = c3, c2 = b3, c3 = a3 }
        
        mirror :: Board -> Board
        mirror b@Board{..} = b { a1 = a3         , a3 = a1
                               , b1 = b3         , b3 = b1
                               , c1 = c3         , c3 = c1 }

---------------------------------------------------------------------
-- | hashes a board
--
hashBoard :: Board -> Int
hashBoard b@Board{..} = (hash . encodeBoard) b

---------------------------------------------------------------------
-- | sample encoding for a board (compares to the .txt-file)
--
encodeBoard :: Board -> String
encodeBoard Board{..} = boardString
    where
        boardString = map showTurn [a1, a2, a3, b1, b2, b3, c1, c2, c3]

        showTurn (Just X) = 'X'
        showTurn (Just O) = 'O'
        showTurn Nothing  = '_'