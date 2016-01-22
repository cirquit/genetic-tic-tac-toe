{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}

module BoardLib where

import Data.List (foldl')
import Data.Vector (Vector(..), fromList)

import BoardTypes (Board(..), Move(..), Value(..), Result(..))

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

-- | returns Tie, Ongoing or Win [X | O]
--
gameState :: Board -> Result Value
gameState (Board (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) = Tie
gameState b@Board{..}
    | same a1 a2 a3 = Win (succ turn)
    | same b1 b2 b3 = Win (succ turn)
    | same c1 c2 c3 = Win (succ turn)
    | same a1 b1 c1 = Win (succ turn)
    | same a2 b2 c2 = Win (succ turn)
    | same a3 b3 c3 = Win (succ turn)
    | same a1 b2 c3 = Win (succ turn)
    | same a3 b2 c1 = Win (succ turn)
    | otherwise     = Ongoing
  where same (Just a) (Just b) (Just c) = a == b && b == c
        same       _        _        _  = False

-- | checks validity by the changes on the on the board after the move
--
isValidOn :: Move -> Board -> Bool
isValidOn move b = changes b /= changes (move `playOn` b)
    where
        changes b@Board{..} = foldl' countEmpty 0 [a1,a2,a3,b1,b2,b3,c1,c2,c3]

        countEmpty acc Nothing = acc + 1
        countEmpty acc _       = acc

-- | starting board shortcut
--
emptyBoard :: Board
emptyBoard = Board e e e  -- row 1
                   e e e  -- row 2
                   e e e  -- row 3
                   X      -- start move
  where e = Nothing


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


-- | board parser for every possible gamestate encoded in a single .txt-file
-- 
toBoardVector :: String -> Vector Board
toBoardVector input = go (lines input) []
  where 
        go :: [String] -> [Board] -> Vector Board
        go []      acc = fromList (reverse acc)
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

-- | shortcut for creating all possible board states from a .txt
-- 
createBoardStatesFrom :: FilePath -> IO (Vector Board)
createBoardStatesFrom fp = readFile fp >>= (return . toBoardVector)