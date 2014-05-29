module Entry where

import Data.List
import qualified System.IO as IO

type User = Int
type Movie = Int
type Time = Int
type Rating = Int
type Entry = [Int]

movie :: Entry -> Movie
movie (_:m:_) = m

user :: Entry -> User
user (u:_) = u

rating :: Entry -> Rating
rating [_, _, _, r] = r

time :: Entry -> Time
time (_:_:t:_) = t

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

entryFromString1 :: String -> Entry
entryFromString1 s = let u:m:_ = map read (words s)
                     in  [u, m]

entryToString :: Entry -> String
entryToString e = unwords $ map show e

-- Read entries from a .dta file, including only the user and movie.
readEntriesFromDta1 :: IO.FilePath -> IO [Entry]
readEntriesFromDta1 f = do
  contents <- readFile f
  return $ [[u, m] | [u, m, _, _] <- map entryFromString (lines contents)]