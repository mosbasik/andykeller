module Main where

import Entry
import Data.List
import System.Environment
import System.Exit
import System.IO as IO

main :: IO ()
main = do
  -- get arguments
  [inFile, outFile] <- getArgs
  entries <- (readFile inFile) >>= 
                (\contents -> return $ map entryFromString (lines contents))
  let resorted  = foldl1 mergeByUser (groupByMovie entries)

  -- write output file
  outHdl <- openFile outFile WriteMode
  mapM_ ((hPutStrLn outHdl) . entryToString) resorted

  -- clean up by closing the out handle
  hClose outHdl
  exitSuccess