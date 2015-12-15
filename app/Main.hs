{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns #-}
module Main where

import Control.Monad
import System.Random
import qualified Data.Vector as V
import System.IO          (hSetBuffering, stdout, BufferMode(..))

import Genetic  -- (genIndividual, genIndividuals)
import Crossover
import Player --   (play,playIO)
import BoardLib -- (createBoardPositions)
import BoardTypes

-- Let gensize (100) Players play vs each other
-- crossover according to alpha
-- mutation  according to delta + beta

-- CO + MU creates new children population
-- Children population is being evaluated

-- Sort the children population and parent population by fitness
-- Natural selection removed individuals according to tetha

-- Fill up the remaining spots

popsize      = 50
stringlength = 827
delta        = 0.01    -- chance to mutate
beta         = 0.05    -- percent of the string to mutate
tetha        = 0.7     -- percent to be removed by natural selection
alpha        = 1.0     -- percent to be crossbred -> equals alpha/2 children


main :: IO ()
main = do
        boardV <- createBoardPositions "lib/tictactoeCombos827.txt"
        let g0           = mkStdGen 123456
            (pop, g1)    = genIndividuals popsize stringlength g0
        loop' boardV pop g1 390

playMe :: String -> IO ()
playMe str = do
    let p = Player str 1 1
    boardV <- createBoardPositions "lib/tictactoeCombos827.txt"
    playAI boardV emptyBoard p 0

loop' :: V.Vector Board -> [Player] -> StdGen -> Int -> IO ()
loop' v pop g1 0 = mapM_ (putStrLn . str) (take 10 pop)
loop' v pop g1 n = do
            let !playedpop            = populationPlay v pop
                (!children,g2)        = rouletteCrossover uniformCrossover g1 playedpop
                (!mutantchildren, g3) = mutate delta beta g2 children
                !playedchildren       = populationPlay v mutantchildren
                !natpop               = naturalselection tetha (playedpop ++ playedchildren)
                (pop', g4)            = repopulate natpop popsize stringlength g3
            mapM_ print pop'
            putStrLn "\n New Generation \n"
            loop' v pop' g4 (n-1)



loop :: V.Vector Board -> [Player] -> StdGen -> IO ()
loop v pop g1 = do
    hSetBuffering stdout NoBuffering
    putStr "Another round? (y/n) or (p)rint "
    input <- getLine
    case input of
        "y" -> do
            let !playedpop            = populationPlay v pop
                (!children,g2)        = rouletteCrossover uniformCrossover g1 playedpop
                (!mutantchildren, g3) = mutate delta beta g2 children
                !playedchildren       = populationPlay v mutantchildren
                !natpop               = naturalselection tetha (playedpop ++ playedchildren)
                (pop', g4)            = repopulate natpop popsize stringlength g3
            mapM_ print pop'
            putStrLn "\n New Generation \n"
            loop v pop' g4
        "p" -> mapM_ (putStrLn . str) (take 10 pop)
        _  -> putStrLn "Exiting..." >> mapM_ print pop


playAI :: V.Vector Board -> Board -> Player -> Int -> IO ()
playAI v board player n = do
    print board
    case (gameState board, n) of
        (Ongoing, 0) -> do
                   putStr "My move: "
                   input <- getLine
                   case input of
                       "1" -> playAI v (A1 `playOn` board) player 1
                       "2" -> playAI v (A2 `playOn` board) player 1
                       "3" -> playAI v (A3 `playOn` board) player 1
                       "4" -> playAI v (B1 `playOn` board) player 1
                       "5" -> playAI v (B2 `playOn` board) player 1
                       "6" -> playAI v (B3 `playOn` board) player 1
                       "7" -> playAI v (C1 `playOn` board) player 1
                       "8" -> playAI v (C2 `playOn` board) player 1
                       "9" -> playAI v (C3 `playOn` board) player 1
                       _   -> putStrLn "Noob l2p" >> playAI v board player 0
        (Ongoing, 1) -> case nextMove v player board of
                             (False, move, _)      -> putStrLn ("Master wins, I tried to play " ++ show move)
                             (True , _   , board') -> playAI v board' player 0
        ((Win r), _) -> putStrLn (show r ++ " won!")
        (Tie    , _) -> putStrLn "It's a tie"


crossoverTest :: IO ()
crossoverTest = do
    let p1 = Player "AAAAAAAAAA" 1    3
        p2 = Player "BBBBBBBBBB" 2    5
        p3 = Player "CCCCCCCCCC" 3    7
        p4 = Player "DDDDDDDDDD" 10  11
        p5 = Player "EEEEEEEEEE" 2  127
        p6 = Player "FFFFFFFFFF" 6   67
        g  = mkStdGen 311

        (l,_) = rouletteCrossover uniformCrossover g [p1,p2,p3,p4,p5,p6]
    mapM_ print l
