{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns, ViewPatterns #-}
module Main where

-----------------------------------------------------------------------------------

import Control.Monad
import System.Random
import qualified Data.Vector as V
import System.IO                  (hSetBuffering, stdout, BufferMode(..))
import System.Posix.Signals
import System.Exit
import Control.Concurrent
import Control.Monad.Random

-----------------------------------------------------------------------------------

import Genetic  -- (genIndividual, genIndividuals)
import Crossover
import Player   -- (play,playIO)
import BoardLib -- (createBoardPositions)
import BoardTypes
import SimpleLogger
-----------------------------------------------------------------------------------


-- Ideas:
--
--  * Coevolution - two different populations play vs each other (no crossover between the two of them)
--  * fitness is based on "turns lived"
--  * fitness counts real wins / invalidmove wins / losses from invalid moves
--  * only play two games from empty board only
--  * invalid moves are not permitted (predefined in boardVector)

-- real win = 1.0 count + 1
-- win through invalid move = 0 count stays same
-- turn = turns / 5  count + 5 for every game played (new count)
-- lose                       = 0.2 count + 1
-- loose through invalid move = 0   count + 1


-- Let gensize (100) Players play vs each other
-- crossover according to alpha
-- mutation  according to delta + beta

-- CO + MU creates new children population
-- Children population is being evaluated

-- Sort the children population and parent population by fitness
-- Natural selection removed individuals according to tetha

-- Fill up the remaining spots

popsize      = 200    -- populationsize
stringlength = 827     -- possible boardstates
delta        = 0.15    -- chance to mutate
beta         = 0.10    -- percent of the string to mutate
tetha        = 0.7     -- percent to be removed by natural selection
generations  = 500

-------------------------------------------------------------------------
-- | Main entry point
--
main :: IO ()
main = do

--  create board positions
    vec <- createBoardStatesFrom "lib/tictactoeCombos827.txt"
    
-- set up logger
    fLog   <- createFileLog "log/" ""
    stdLog <- createStdoutLog
    let logger = mergeLogs [fLog, stdLog]
    
-- create initial population
    let g      = mkStdGen 13457398164723746
        (g',_) = split g
        population = flip evalRand g $ genIndividuals popsize stringlength

-- start the evolution
    evolution vec population generations g' logger

-------------------------------------------------------------------------
-- | Automatic evolution dependent on 'generations'
--
evolution :: V.Vector Board -> [Player] -> Int -> StdGen -> Logger -> IO ()
evolution vec population 0 _ log = mapM_ ((log <!!>) . str) (take 10 population) >> closeLog log
evolution vec population n g log = do

    log <!!> unwords ["Generation(s) left to live: ", show n, "\n"]

    let newpop = flip evalRand g $ do
            let parents = populationPlayEmptyBoard vec population
            children  <- rouletteCrossover uniformCrossover parents
            mutants   <- mutate delta beta children
            let children' = populationPlayEmptyBoard vec children
                selected  = naturalselection tetha (parents ++ children')
            repopulate selected popsize stringlength

    let (g',_) = split g
    mapM_ (log <!>) newpop
    evolution vec newpop (n-1) g' log

-----------------------------------------------------------------------------------
-- | User input for communication
-- 
customEvolution :: V.Vector Board -> [Player] -> StdGen -> Logger -> IO ()
customEvolution vec population g log = do
    log <!!> "Another round? (y/n) or (p)rint "
    input <- getLine
    log <!!> input
    case input of
        "y" -> do
           let newpop = flip evalRand g $ do
                    let parents = populationPlay vec population
                    children  <- rouletteCrossover uniformCrossover parents
                    mutants   <- mutate delta beta children
                    let children' = populationPlay vec children
                        selected  = naturalselection tetha (parents ++ children')
                    repopulate selected popsize stringlength
           let (g',_) = split g
           log <!> newpop
           customEvolution vec newpop g' log
        "p" -> mapM_ ((log <!>) . str) (take 10 population) >> customEvolution vec population g log
        _   -> mapM_ ( log <!>)        (take 10 population)

{-
safeExit :: [Player] -> V.Vector Board -> ThreadId -> IO ()
safeExit population vec tid = do
    putStrLn ""
    mapM_ print population
    putStrLn $ "\n * <int> - from the range 0 - " ++ show (length population) ++ " prints the indiviual"
    putStrLn    " * exit/quit/q/:q  - exits the program"
    putStrLn    " * play <int> [me|ki] - let's you play vs the individual at that index, second arg is who starts first"
    putStr "Command: "
    input <- getLine
    loop population vec input
  where 
    loop :: [Player] -> V.Vector Board -> String -> IO ()
    loop population vec "exit" = killThread tid >> exitSuccess
    loop population vec "quit" = killThread tid >> exitSuccess
    loop population vec "q"    = killThread tid >> exitSuccess
    loop population vec ":q"   = killThread tid >> exitSuccess
    loop population vec ('p':'l':'a':'y':' ':xs)
      | [(n, " me")] <- reads xs, n >= 0, n < length population = do
            playAI vec (population !! n) 0
            putStr "Command: "
            input <- getLine
            loop population vec input
      | [(n, " ki")] <- reads xs, n >= 0, n < length population = do
            playAI vec (population !! n) 1
            putStr "Command: "
            input <- getLine
            loop population vec input
    loop population vec (reads -> [(n, "")])
      | n >= 0, n < length population = do
           putStrLn ("Individual Nr." ++ show n)
           let individual = population !! n
           putStrLn ("Fitness in %, Wins + Ties, Total Games: " ++ show (ratio individual) ++ ", " ++ show (fitness individual) ++ ", " ++ show (games individual))
           putStrLn ("Chromosome: " ++ str individual)
           putStr "Command: "
           input <- getLine
           loop population vec input
    loop population vec _      = putStrLn "Sorry, something didn't work...try again" >> putStr "Command: " >> getLine >>= \input -> loop population vec input
-}
-----------------------------------------------------------------------------------
-- ## Tests ##

-- | Play vs a custom Chromosome for testing purposes
--
--   0 - You start
--   1 - AI starts
--
playAI :: V.Vector Board -> Player -> Int -> IO ()
playAI vec player n = playAI' vec emptyBoard player n
  where
      playAI' :: V.Vector Board -> Board -> Player -> Int -> IO ()
      playAI' vec board player n = do
          print board
          case (gameState board, n) of
              (Ongoing, 0) -> do
                         putStr "My move: "
                         input <- getLine
                         case input of
                             "1" -> playAI' vec (A1 `playOn` board) player 1
                             "2" -> playAI' vec (A2 `playOn` board) player 1
                             "3" -> playAI' vec (A3 `playOn` board) player 1
                             "4" -> playAI' vec (B1 `playOn` board) player 1
                             "5" -> playAI' vec (B2 `playOn` board) player 1
                             "6" -> playAI' vec (B3 `playOn` board) player 1
                             "7" -> playAI' vec (C1 `playOn` board) player 1
                             "8" -> playAI' vec (C2 `playOn` board) player 1
                             "9" -> playAI' vec (C3 `playOn` board) player 1
                             _   -> putStrLn "Noob l2p" >> playAI' vec board player 0
              (Ongoing, 1) -> case nextMove vec player board of
                                   (False, move, _)      -> putStrLn ("Master wins, I tried to play " ++ show move)
                                   (True , _   , board') -> playAI' vec board' player 0
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

        l = flip evalRand g $ rouletteCrossover onePointCrossover [p1,p2,p3,p4,p5,p6]
    mapM_ print l

naturalselectionTest :: IO ()
naturalselectionTest = do
    let p1 = Player "AAAAAAAAAA"  31   7   -- 4.428
        p2 = Player "BBBBBBBBBB"  30   8   -- 3.75
        p3 = Player "CCCCCCCCCC"  35   9   -- 3.88
        p4 = Player "DDDDDDDDDD"  10  11   -- 0.9
        p5 = Player "EEEEEEEEEE" 114  10   -- 11.4
        p6 = Player "FFFFFFFFFF"  60  67   -- 0.89

        popsize = 6
        tetha = 0.7
        stringlength = 10

        l = naturalselection tetha [p1,p2,p3,p4,p5,p6]
        g = mkStdGen 12345
        l' = flip evalRand g $ repopulate l popsize stringlength
    mapM_ print l'

testPlay :: String -> Int -> IO ()
testPlay s i = do
    let p = Player s 0 1
    vec <- createBoardStatesFrom "lib/tictactoeCombos827.txt"
    playAI vec p i


