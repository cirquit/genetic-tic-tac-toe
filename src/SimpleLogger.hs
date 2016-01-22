module SimpleLogger where

import System.IO
import Data.Time
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

data Logger = Logger Handle [Logger]

-------------------------------------------------------------------------
-- | Creating a log file with a dir, that's being created if missing
--   If no arguments are provided:
--
--  > createFile "" ""
--  This creates a directory "log/" and a file "log-UTC-current-timestamp.txt"
--
createFileLog :: FilePath -> FilePath -> IO Logger
createFileLog dir fp = do
  let directory
       | null dir  = "log/"
       | otherwise =  dir
  createDirectoryIfMissing True dir
  
  time <- getTime
  let filename
        | null fp   = "log-" ++ time ++ ".txt"
        | otherwise = fp

  handle <- openFile (directory </> filename) AppendMode
  hSetBuffering handle NoBuffering
  createLog handle

-------------------------------------------------------------------------
-- | Creating a log that pipes everything to stdout
--
createStdoutLog :: IO Logger
createStdoutLog = createLog stdout


mergeLogs :: [Logger] -> Logger
mergeLogs [] = error "SimpleLogger.mergeLogs: logger list is empty"
mergeLogs ((Logger l ls):lss) = Logger l (ls ++ lss)

-------------------------------------------------------------------------
-- | Filling the log with creation information
--
createLog :: Handle -> IO Logger
createLog handle = do
  time <- getTime
  hPutStrLn handle $ unlines [ "-------------------------------------------------------------------------"
                             , unwords ["Starting log at ", time]
                             , "-------------------------------------------------------------------------"
                             ]
  return (Logger handle [])

-------------------------------------------------------------------------
-- | Closing the log if it's not stdout
--
closeLog :: Logger -> IO ()
closeLog (Logger handle hs) = do
  time <- getTime
  hPutStrLn handle $ unlines [ "-------------------------------------------------------------------------"
                             , unwords ["Stopping log at ", time]
                             , "-------------------------------------------------------------------------"
                             ]
  case show handle of
      "{handle: <stdout>}" -> mapM_ closeLog hs >> return ()
      _                    -> mapM_ closeLog hs >> hClose handle



-------------------------------------------------------------------------
-- | Send something showable to be logged
--
(<!>) :: Show a => Logger -> a -> IO ()
logger <!> x = logger <!!> (show x)

infixr 1 <!>

-------------------------------------------------------------------------
-- | Send a String to be logged
--
(<!!>) :: Logger -> String -> IO ()
(Logger handle logs) <!!> str = do
  getTime >>= \time -> hPutStrLn handle $ unwords [time++":", str]
  mapM_ (<!!> str) logs

infixr 1 <!!>

-------------------------------------------------------------------------
-- | Pretty UTC time
--
getTime :: IO String
getTime = (take 19 . concatMap (++ "-") . words . show) <$> getCurrentTime