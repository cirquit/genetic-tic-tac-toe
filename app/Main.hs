{-# LANGUAGE RecordWildCards, PatternSynonyms #-}
module Main where

import Control.Monad
import BoardTypes (Board(..), Move(..), Value(..))
import BoardLib

main :: IO ()
main = putStrLn "Hello World"

-- playGame :: Board -> IO ()
-- playGame b@Board{..} = do
--     let (fin, winner) = finished b
--     when fin $ putStrLn $ show winner ++ " won!"
-- 
--     putStr $ "Turn " ++ show turn ++ ": "
--     move <- getLine
--     case isValid b move of
--         False -> putStrLn "Not a valid input, try again!" >> playGame b
--         _     -> let b' = (playMove b (read move)) in
--                  print b' >> playGame b'



-- main :: IO ()
-- main = playGame emptyBoard
