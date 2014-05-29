module Main where

import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as V

import Filesystem.Path.CurrentOS as F

import qualified System.Directory as D
import System.Environment
import System.Exit
import System.IO as IO

type ResultType = Float

type Matrix = Vector (Vector ResultType)

inputError :: IO.FilePath -> IO ()
inputError path = do
  hPutStrLn stderr "Input directory " ++ path ++ " does not exist. Exiting now."
  exitFailure

dataError :: IO.FilePath -> IO ()
dataError path = do
  hPutStrLn stderr $ "The file " ++ path ++ 
                   $ " does not seem to contain valid data. Exiting now"
  exitFailure

nonexistentFileError :: IO.FilePath -> IO ()
nonexistentFileError path = do
  hPutStrLn "The input file " ++ path ++ " does not exist. Exiting now."
  exitFailure

getRow :: Vector (Vector a) -> Int -> Vector a
getRow m i = m V.! i

getCol :: Vector (Vector a) -> Int -> Vector a
getCol m i = V.map (V.! i) m

            $ V.fromList goodLines
  return mat

vectorProd :: (Num a) => Vector a -> Vector a -> a
vectorProd v w
  | (V.length v == V.length w) = vectorProd_ v w
  | otherwise = error "Vector dimensions don't match up!"

vectorProd_ :: (Num a) => Vector a -> Vector a -> a
vectorProd_ v w = V.sum $ V.zipWith (*) v w

main :: IO ()
main = do
  -- get arguments
  [inputDir, prefix, outFile] <- getArgs

  -- Make sure the directory exists
  -- inputDir :: IO.FilePath
  doesDirectoryExist inputDir >>= (\e -> case of
    True -> return ()
    False -> inputError)

  -- Create the file paths
  filePaths :: [IO.FilePath]
  let [user_path, movie_path, 
        user_bias_path, movie_bias_path, 
        global_mean_path] = 
    map (F.encodeString . (F.concat $ F.decodeString inputDir))
    $ [F.decodeString (prefix ++ suffix) | suffix <- suffixes]

  [user_mat, movie_mat] <- mapM (\path -> (D.doesFileExist path) >>= 
                                        (\e -> case e of
                                          False -> nonexistentFileError path
                                          True -> readMatFromMM path))
                                $ [user_path, movie_path]

  [user_bias, ] 
    user_bias_path, movie_bias_path,
    global_mean_path] <- mapM 

  user_mat <- (readMatFromMM user_path)
  movie_mat <- (readMatFromMM movie_path)


  case (all (/= "") [user_bias_path, movie_bias_path, global_mean_path])


  where
    suffixes :: [F.FilePath]
    suffixes = 
      map F.decodeString $ ["_U.mm", "_V.mm", 
                            "_U_bias.mm", "_V_bias.mm", 
                            "_global_mean.mm"]

    -- Create a numeric matrix from a matrix market file
    readMatFromMM :: IO.FilePath -> IO Matrix
    readMatFromMM f = do
      contents <- readFile f -- contents :: String
      -- goodLines :: [String]
      let goodLines = tail  $ dropWhile ((== '%') . (!! 0))
                            $ lines contents
          mat       = V.map ((V.map read) . V.fromList . words) 
      return mat

    getUnbiasedResult :: Matrix -> Matrix -> 