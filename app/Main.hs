
{-# LANGUAGE RecordWildCards, PatternSynonyms, BangPatterns, ViewPatterns #-}

module Main where

-----------------------------------------------------------------------------------

import Control.Monad
import System.Random
import Data.Vector as V (Vector(..), fromList)
import System.IO                  (hSetBuffering, stdout, BufferMode(..))
import System.Posix.Signals
import System.Exit
import Control.Concurrent
import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import Data.Map as M (Map(..))
import Data.Word (Word8(..))

import Control.Parallel.Strategies

-----------------------------------------------------------------------------------

import Genetic  -- (genIndividual, genIndividuals)
import Crossover
import Player      -- (play,playIO)
import Board.Utils -- (createBoardPositions)
import Board.Types
import SimpleLogger
-----------------------------------------------------------------------------------


-- Ideas:
--
--  * Coevolution - two different populations play vs each other (no crossover between the two of them)      Island separated multi population scheme
--  * fitness is based on "turns lived"                                                                      Done
--  * fitness counts real wins / invalidmove wins / losses from invalid moves                                Doesn't matter
--  * only play two games from empty board only                                                              Done
--  * invalid moves are not permitted (predefined in boardVector)                                            (last resort)

-- Current issues:

-- * Inbreeding...
-- * TODO: Hash 827 board states - Hash all rotations and store all 9 in a (Map Hash [Hash])


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

popsize      = 1000    -- populationsize
stringlength = 827     -- possible boardstates
delta        = 0.10    -- chance to mutate
beta         = 0.05    -- percent of the string to mutate
tetha        = 0.8     -- percent to be removed by natural selection
generations  = 3900

-------------------------------------------------------------------------
-- | Main entry point
--
main :: IO ()
main = do

--  create board positions
    rotMap <- createRotHashMap "lib/sortedCombos827.txt"
    
    nextMMap <- createNextBoardStateMap "lib/sortedCombos827.txt"

    vec <- V.fromList <$> createBoardStatesFrom "lib/sortedCombos827.txt"

-- set up logger
    time   <- getTime
    fLog   <- createFileLog ("log/" ++ time ++ "/") ""
    stdLog <- createStdoutLog
    let logger = mergeLogs [fLog, stdLog]

-- create initial population
    let g      = mkStdGen 13457398364872342
        (g',_) = split g
        population = flip evalRand g $ genIndividuals popsize stringlength

-- start the evolution
    evolution rotMap nextMMap vec population generations g logger

-------------------------------------------------------------------------
-- | Automatic evolution dependent on 'generations'
--
evolution :: Map Int (Int, Rotation) -> Map Int [Board] -> Vector Board -> [Player] -> Int -> StdGen -> Logger -> IO ()
evolution rotMap nextMMap vec pop 0 _ log = mapM_ ((log <!>) . moves) (take 10 pop) >> closeLog log
evolution rotMap nextMMap vec pop n g log = do

    log <!!> unwords ["Generation(s) left to live: ", show n, "\n"]

    let strategy = parList rseq
        npop = flip evalRand g $ do

            let !parents = map (playSingle rotMap nextMMap) pop `using` strategy
          
            --let !parents = populationPlay vec  rotMap pop `using` strategy

            children  <- randomTacticCrossover uniformCrossover parents
          
            mutants   <- mutate delta beta children
          
            let !children' = map (playSingle rotMap nextMMap) mutants `using` strategy

            -- let !children' = populationPlay vec  rotMap mutants `using` strategy
                selected  = naturalselection tetha (parents ++ children')
          
            repopulate selected popsize stringlength

    let (g',_) = split g

    mapM_ (log <!>) (take 10 npop)
    mapM_ ((log <!!>) . (>>= show) . moves) (take 3 npop)
    
    evolution rotMap nextMMap vec npop (n-1) g' log

-----------------------------------------------------------------------------------
-- ## Tests ##

-- | Play vs a custom Chromosome for testing purposes
--
--   0 - You start
--   1 - AI starts
--

playAI :: Map Int (Int, Rotation) -> Player -> Int -> IO ()
playAI hmap player n = playAI' hmap emptyBoard player n
  where
      playAI' :: Map Int (Int, Rotation) -> Board -> Player -> Int -> IO ()
      playAI' hmap board player n = do
          print board
          case (gameState board, n) of
              (Ongoing, 0) -> do
                         putStr "My move: "
                         input <- getLine
                         case input of
                             "1" -> playAI' hmap (A1 `playOn` board) player 1
                             "2" -> playAI' hmap (A2 `playOn` board) player 1
                             "3" -> playAI' hmap (A3 `playOn` board) player 1
                             "4" -> playAI' hmap (B1 `playOn` board) player 1
                             "5" -> playAI' hmap (B2 `playOn` board) player 1
                             "6" -> playAI' hmap (B3 `playOn` board) player 1
                             "7" -> playAI' hmap (C1 `playOn` board) player 1
                             "8" -> playAI' hmap (C2 `playOn` board) player 1
                             "9" -> playAI' hmap (C3 `playOn` board) player 1
                             _   -> putStrLn "Noob l2p" >> playAI' hmap board player 0
              (Ongoing, 1) -> case nextMove hmap player board of
                                   (False, move, _)      -> putStrLn ("Master wins, I tried to play " ++ show move)
                                   (True , _   , board') -> playAI' hmap board' player 0
              ((Win r), _) -> putStrLn (show r ++ " won!")
              (Tie    , _) -> putStrLn "It's a tie"


crossoverTest :: IO ()
crossoverTest = do
    let p1 = Player [0,0,0,0,0,0,0,0,0,0] 1  0 0 0 False   3
        p2 = Player [1,1,1,1,1,1,1,1,1,1] 2  0 0 0 False   5
        p3 = Player [2,2,2,2,2,2,2,2,2,2] 3  0 0 0 False   7
        p4 = Player [3,3,3,3,3,3,3,3,3,3] 10 0 0 0 False  11
        p5 = Player [4,4,4,4,4,4,4,4,4,4] 2  0 0 0 False 127
        p6 = Player [5,5,5,5,5,5,5,5,5,5] 6  0 0 0 False  67
        g  = mkStdGen 311

        l = flip evalRand g $ randomTacticCrossover uniformCrossover [p1,p2,p3,p4,p5,p6]
    mapM_ print l

naturalselectionTest :: IO ()
naturalselectionTest = do
    let p1 = Player [0,0,0,0,0,0,0,0,0,0]  31  0 0 0 False  7   -- 4.428
        p2 = Player [1,1,1,1,1,1,1,1,1,1]  30  0 0 0 False  8   -- 3.75
        p3 = Player [2,2,2,2,2,2,2,2,2,2]  35  0 0 0 False  9   -- 3.88
        p4 = Player [3,3,3,3,3,3,3,3,3,3]  10  0 0 0 False 11   -- 0.9
        p5 = Player [4,4,4,4,4,4,4,4,4,4] 114  0 0 0 False 10   -- 11.4
        p6 = Player [5,5,5,5,5,5,5,5,5,5]  60  0 0 0 False 67   -- 0.89

        popsize = 6
        tetha = 0.7
        stringlength = 10

        l = naturalselection tetha [p1,p2,p3,p4,p5,p6]
        g = mkStdGen 12345
        l' = flip evalRand g $ repopulate l popsize stringlength
    mapM_ print l'


shuffleTest :: IO ()
shuffleTest = do
    let p1 = Player [0,0,0,0,0,0,0,0,0,0]  31  0 0 0 False  7   -- 4.428
        p2 = Player [1,1,1,1,1,1,1,1,1,1]  30  0 0 0 False  8   -- 3.75
        p3 = Player [2,2,2,2,2,2,2,2,2,2]  35  0 0 0 False  9   -- 3.88
        p4 = Player [3,3,3,3,3,3,3,3,3,3]  10  0 0 0 False 11   -- 0.9
        p5 = Player [4,4,4,4,4,4,4,4,4,4] 114  0 0 0 False 10   -- 11.4
        p6 = Player [5,5,5,5,5,5,5,5,5,5]  60  0 0 0 False 67   -- 0.89

        g = mkStdGen 123545
        l' = flip evalRand g $ shuffleM [p1, p2, p3, p4, p5, p6]
    mapM_ print l'

singlePlayerTest :: IO Player
singlePlayerTest = do
    rotMap <- createRotHashMap "lib/sortedCombos827.txt"
    
    nextMMap <- createNextBoardStateMap "lib/sortedCombos827.txt"
    let g = mkStdGen 123123123
        p1 = flip evalRand g $ genIndividual 827

    return $ playSingle rotMap nextMMap p1


-- testPlay :: String -> Int -> IO ()
-- testPlay s i = do
--     let p = Player s 0 1
--     vec <- createBoardStatesFrom "lib/tictactoeCombos827.txt"
--     playAI vec p i



-----------------------------------------------------------------------------------
-- | User input for communication
--

{- 
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
        "p" -> mapM_ ((log <!>) . moves) (take 10 population) >> customEvolution vec population g log
        _   -> mapM_ ( log <!>)        (take 10 population)
-}


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
           putStrLn ("Chromosome: " ++ moves individual)
           putStr "Command: "
           input <- getLine
           loop population vec input
    loop population vec _      = putStrLn "Sorry, something didn't work...try again" >> putStr "Command: " >> getLine >>= \input -> loop population vec input
-}