module Main where

import Entry
import System.Environment
import System.Exit
import System.IO as IO

main :: IO ()
main = do
  -- get arguments. Assumes files are sorted by user. 
  -- Algorithm runs fastest if smallest file is supplied first.
  -- Output file goes last.
  [inFile, outFile] <- getArgs

  -- contents :: String
  contents <- readFile inFile
  let entries = map entryFromString1 (lines contents)

  -- result: [String]
  let result = map (\user -> unwords [show . movie $ entry | entry <- user])
                $ groupByUser entries

  -- Write the result to a file
  outHdl <- openFile outFile WriteMode
  mapM_ (hPutStrLn outHdl) result

  -- Clean up and exit.
  hClose outHdl
  exitSuccess