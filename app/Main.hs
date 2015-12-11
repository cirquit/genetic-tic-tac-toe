{-# LANGUAGE RecordWildCards, PatternSynonyms #-}
module Main where

import Control.Monad
import Assets


decodeMove 'A' = A1 
decodeMove 'B' = A2 
decodeMove 'C' = A3 
decodeMove 'D' = B1 
decodeMove 'E' = B2 
decodeMove 'F' = B3 
decodeMove 'G' = C1 
decodeMove 'H' = C2 
decodeMove 'I' = C3 

next = succ

playMove :: Board -> Move -> Board
playMove b@Board{..} A1  = b { a1 = Some turn, turn = next turn }
playMove b@Board{..} A2  = b { a2 = Some turn, turn = next turn }
playMove b@Board{..} A3  = b { a3 = Some turn, turn = next turn }
playMove b@Board{..} B1  = b { b1 = Some turn, turn = next turn }
playMove b@Board{..} B2  = b { b2 = Some turn, turn = next turn }
playMove b@Board{..} B3  = b { b3 = Some turn, turn = next turn }
playMove b@Board{..} C1  = b { c1 = Some turn, turn = next turn }
playMove b@Board{..} C2  = b { c2 = Some turn, turn = next turn }
playMove b@Board{..} C3  = b { c3 = Some turn, turn = next turn }

isValid :: Board -> String -> Bool
isValid board move = countNones board /= (countNones $ playMove board (read move))
  where countNones b = foldl (\x y -> if y == None then x + 1 else x) 0 [a1 b,a2 b,a3 b,b1 b,b2 b,b3 b,c1 b,c2 b,c3 b]

playGame :: Board -> IO ()
playGame b@Board{..} = do
    let (fin, winner) = finished b
    when fin $ putStrLn $ show winner ++ " won!"

    putStr $ "Turn " ++ show turn ++ ": "
    move <- getLine
    case isValid b move of
        False -> putStrLn "Not a valid input, try again!" >> playGame b
        _     -> let b' = (playMove b (read move)) in
                 print b' >> playGame b'


finished :: Board -> (Bool, Option String)
finished (Board (Some _) (Some _) (Some _) (Some _) (Some _) (Some _) (Some _) (Some _) (Some _) _) = (True, Some "Nobody")
finished b@Board{..}
    | same a1 a2 a3 = (True, Some (show (succ turn)))
    | same b1 b2 b3 = (True, Some (show (succ turn)))
    | same c1 c2 c3 = (True, Some (show (succ turn)))
    | same a1 b1 c1 = (True, Some (show (succ turn)))
    | same a2 b2 c2 = (True, Some (show (succ turn)))
    | same a3 b3 c3 = (True, Some (show (succ turn)))
    | same a1 b2 c3 = (True, Some (show (succ turn)))
    | same a3 b2 c1 = (True, Some (show (succ turn)))
    | otherwise     = (False, None)
  where same (Some a) (Some b) (Some c) = a == b && b == c
        same       _        _        _  = False



-- main :: IO ()
-- main = playGame emptyBoard
