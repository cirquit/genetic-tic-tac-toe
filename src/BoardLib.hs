{-# LANGUAGE RecordWildCards, PatternSynonyms #-}
module BoardLib where

import Data.List (foldl')

import BoardTypes (Board(..), Move(..), Value(..), Result(..))

-- | does not check if the move is valid
-- 
playMove :: Board -> Move -> Board
playMove b@Board{..} A1  = b { a1 = Just turn, turn = next turn }
playMove b@Board{..} A2  = b { a2 = Just turn, turn = next turn }
playMove b@Board{..} A3  = b { a3 = Just turn, turn = next turn }
playMove b@Board{..} B1  = b { b1 = Just turn, turn = next turn }
playMove b@Board{..} B2  = b { b2 = Just turn, turn = next turn }
playMove b@Board{..} B3  = b { b3 = Just turn, turn = next turn }
playMove b@Board{..} C1  = b { c1 = Just turn, turn = next turn }
playMove b@Board{..} C2  = b { c2 = Just turn, turn = next turn }
playMove b@Board{..} C3  = b { c3 = Just turn, turn = next turn }

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
isValidOn move b@Board{..} = changes b /= changes (playMove b move)
  where changes b = foldl' go 0 [a1,a2,a3,b1,b2,b3,c1,c2,c3]

        go acc Nothing = acc + 1
        go acc _       = acc

-- | starting board shortcut
--
emptyBoard :: Board
emptyBoard = Board e e e  e e e  e e e  X
  where e = Nothing

-- | later used by gen. alg.
--
decodeMove :: Char -> Move
decodeMove 'A' = A1
decodeMove 'B' = A2
decodeMove 'C' = A3
decodeMove 'D' = B1
decodeMove 'E' = B2
decodeMove 'F' = B3
decodeMove 'G' = C1
decodeMove 'H' = C2
decodeMove 'I' = C3


