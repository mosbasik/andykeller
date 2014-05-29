module ChangeSort where

import Data.List
import System.Environment
import System.Exit
import System.IO as IO

type User = Int
type Movie = Int
type Time = Int
type Rating = Int
type Entry = [Int]

-- split the Netflix data into arrays based on movie number and return an 
-- array of arrays.
groupByMovie :: [Entry] -> [[Entry]]
groupByMovie = groupBy (\(_:m1:_) (_:m2:_) -> m1 == m2)

groupByUser :: [Entry] -> [[Entry]]
groupByUser = groupBy (\(u1:_) (u2:_) -> u1 == u2)

mov :: Entry -> Movie
mov (_:m:_) = m

user :: Entry -> User
user (u:_) = u

{-
groupByMovie entries = iter 1 entries where 
  iter _ [] = []
  iter movie_no entries = 
    let (current, rest) = span (\[_, m, _, _] -> m == movie_no)
                          $ entries
    in
        current ++ (iter $ movie_no + 1 
                         $ rest)
-}

-- Take two lists of [user, movie, time, rating], each sorted by user,
-- such that movie1 < movie2,
-- and merge them so that the result is sorted by user -> movie
mergeByUser :: [Entry] -> [Entry] -> [Entry]
mergeByUser lst1@(x@(user1:_):xs) lst2@(y@(user2:_):ys)
  | user1 <= user2 = x:(mergeByUser xs lst2)
  | otherwise = y:(mergeByUser lst1 ys)
mergeByUser [] lst2 = lst2
mergeByUser lst1 [] = lst1

mergeByMovie :: [Entry] -> [Entry] ->[Entry]
mergeByMovie lst1@(x@(_:movie1:_):xs) lst2@(y@(_:movie2:_):ys)
  | movie1 <= movie2 = x:(mergeByMovie xs lst2)
  | otherwise = y:(mergeByMovie lst1 ys)
mergeByMovie [] lst2 = lst2
mergeByMovie lst1 [] = lst1

entryFromString :: String -> Entry
entryFromString s = map read $ words s

entryToString :: Entry -> String
entryToString e = unwords $ map show e


readEntriesFromDta :: IO.FilePath -> IO [Entry]
readEntriesFromDta f = do
  contents <- readFile f -- contents :: String
  return $ map entryFromString 
         $ lines contents -- lines contents :: [String]

-- Read entries from a .dta file, including only the user and movie.
readEntriesFromDta1 :: IO.FilePath -> IO [Entry]
readEntriesFromDta1 f = do
  contents <- readFile f
  return $ [[u, m] | [u, m, _, _] <- map entryFromString (lines contents)]

main :: IO ()
main = do
  -- get arguments
  [inFile, outFile] <- getArgs
  entries <- readEntriesFromDta inFile
  let resorted  = foldl1 mergeByUser (groupByMovie entries)

  -- write output file
  outHdl <- openFile outFile WriteMode
  mapM_ ((hPutStrLn outHdl) . entryToString) resorted

  -- clean up by closing the out handle
  hClose outHdl
  exitSuccess