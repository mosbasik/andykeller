module Main where

import Entry
import System.Environment
import System.Exit
import System.IO as IO
import Data.List

main :: IO ()
main = do
    -- Get dem arguments.
    [inFile, outFile] <- getArgs

    -- Read the infile.
    contents <- readFile inFile

    -- Do the work.
    let entries = map movieRatingFromString (lines contents)
        result  = map (show . mean . (map $ fromIntegral . getRating)) $ groupByMovie entries

    -- Write the result to a file
    outHdl <- openFile outFile WriteMode
    mapM_ (hPutStrLn outHdl) result

    -- Clean up and exit.
    hClose outHdl
    exitSuccess

    where
      getRating :: Entry -> Int
      getRating [_, r] = r

      movieRatingFromString :: String -> Entry
      movieRatingFromString s = let [_, m, _, r] = words s
                               in [read m, read r]

      mean :: [Float] -> Float
      mean lst = (sum lst) / (fromIntegral $ length lst)

      groupByMovie :: [Entry] -> [[Entry]]
      groupByMovie = groupBy (\(m1:_) (m2:_) -> m1 == m2)