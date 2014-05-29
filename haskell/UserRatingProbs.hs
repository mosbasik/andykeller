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
    let userRatings    = map (userRatingFromString) $ lines contents
        ratingsPerUser = map (map getRating) 
                         $ groupBy (\e1 e2 -> (user e1 == (user e2))) userRatings
        result = map (unwords . (map show . ratingProbs)) ratingsPerUser

    -- Write the result to a file
    outHdl <- openFile outFile WriteMode
    mapM_ (hPutStrLn outHdl) result

    -- Clean up and exit.
    hClose outHdl
    exitSuccess

    where
      getRating :: Entry -> Rating
      getRating [_, r] = r

      userRatingFromString :: String -> Entry
      userRatingFromString s = let [u, _, _, r] = words s
                               in [read u, read r]

      count :: Int -> [Int] -> Float
      count n lst = fromIntegral $ (length . (filter (== n))) lst

      ratingProbs :: [Rating] -> [Float]
      ratingProbs ratings = let occurrences = [count 1 ratings, count 2 ratings,
                                                count 3 ratings, count 4 ratings,
                                                count 5 ratings]
                                probabilities    = map (/ (sum occurrences)) occurrences
                            in  probabilities