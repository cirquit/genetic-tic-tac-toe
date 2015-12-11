{-# LANGUAGE RecordWildCards, PatternSynonyms, ImplicitParams #-}

module BoardLib where

import Data.List (foldl')
import Data.Vector.Generic (Vector(..))

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


-- | 

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

-- | later used by gen. alg.
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
toMove  x  = error ("could not find the move: <" ++ x:">")