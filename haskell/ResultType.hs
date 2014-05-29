module ResultType where

import Entry
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO as IO

type ResultType = Float

type Matrix = [Vector ResultType]

getRow :: [Vector a] -> Int -> Vector a
getRow m i = m !! i

getCol :: [Vector a] -> Int -> Vector a
getCol m i = V.fromList $ map (V.! i) m

-- Create a numeric matrix from a matrix market file
readMatFromMM :: IO.FilePath -> IO Matrix
readMatFromMM f = do
  contents <- readFile f -- contents :: String
  -- goodLines :: [String]
  let goodLines = tail  $ dropWhile ((== '%') . (!! 0))
                        $ lines contents
      mat       = map ((V.map read) . V.fromList . words) 
                        $ goodLines
  return mat

vectorProd :: (Num a) => Vector a -> Vector a -> a
vectorProd v w
  | (V.length v == V.length w) = vectorProd_ v w
  | otherwise = error "Vector dimensions don't match up!"

vectorProd_ :: (Num a) => Vector a -> Vector a -> a
vectorProd_ v w = V.sum $ V.zipWith (*) v w

vectorSum :: (Num a) => Vector a -> Vector a -> Vector a
vectorSum v w | V.length v == (V.length w) = vectorSum_ v w
              | otherwise = error "Vector dimensions don't match up!"

vectorSum_ :: (Num a) => Vector a -> Vector a -> Vector a
vectorSum_ v w = V.zipWith (+) v w