module Main where

import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as V

import Filesystem.Path.CurrentOS as F
import ScientificNum as S

import System.Directory
import System.Environment
import System.Exit
import System.IO as IO



type SciMat = Vector (Vector SciNum)

numRows :: Vector (Vector a) -> Int
numRows m = V.length m

numCols :: Vector (Vector a) -> Int
numCols m = V.length (getRow m 0)

getRow :: Vector (Vector a) -> Int -> Vector a
getRow m i = m V.! i

getCol :: Vector (Vector a) -> Int -> Vector a
getCol m i = V.map (V.! i) m

transpose :: Vector (Vector a) -> Vector (Vector a)
transpose m = V.generate (numRows m) (getCol m)

vectorProd :: (Num a) => Vector a -> Vector a -> a
vectorProd v w = V.sum $ V.zipWith (*) v w

fileError :: IO.FilePath -> IO ()
fileError f = hPutStrLn stderr $ "Error: The file \"" 
                               ++ f ++ "\" does not exist. Exiting now."

-- Create a zero vector of length n
zeros :: (Num a) => Int -> Vector a
zeros n = V.replicate n 0

-- Attampt to multiply a vector by a matrix. If the dimensions are wrong,
-- throw an error.
vecByMat :: (Num a) => Vector a -> Vector (Vector a) -> Vector a
vecByMat v m
    | V.length v /= (numRows m) 
        = error "vecByMat: The dimensions of the vector don't \
        \match those of the matrix."
    | otherwise = vecByMat_ v m

-- Multiply a vector by a matrix without checking bounds
vecByMat_ :: (Num a) => Vector a -> Vector (Vector a) -> Vector a
vecByMat_ v m = 
  let init = zeros (V.length v) in
    V.foldl (V.zipWith (+)) init -- :: Num a => Vector (Vector a) -> Vector a
    $ V.zipWith (V.map) (fmap (*) v) -- :: Num a => Vector (Vector a) -> Vector (Vector a)
    $ m -- Vector (Vector a)


-- Attempt to multiply two matrices. If the bounds don't work, throw an error.
matProd :: (Num a) => Vector (Vector a) -> Vector (Vector a)
                   -> Vector (Vector a)
matProd m1 m2
  | (numRows m1) /= (numRows m2)
    = error "matProd: The dimensions of the matrices are incompatible."
  | otherwise = matProd_ m1 m2

-- Multiply two matrices without checking bounds.
matProd_ :: (Num a) => Vector (Vector a) -> Vector (Vector a) 
                    -> Vector (Vector a)
matProd_ m1 m2 = V.map (`vecByMat_` m2) m1

-- Create a SciMat from a matrix market file
readSciMatFromMM :: IO.FilePath -> IO SciMat
readSciMatFromMM f = do
  contents <- readFile f -- contents :: String
  -- goodLines :: [String]
  let goodLines = tail $ dropWhile ((== '%') . (!! 0)) 
                       $ lines contents
      mat = V.map ((V.map S.fromString) . V.fromList . words) 
              $ V.fromList goodLines
  return mat

-- Write a matrix of SciNums to a file in matrix market format. Convert 
-- from SciNum to Float
writeSciMatToMM :: IO.FilePath -> SciMat -> IO ()
writeSciMatToMM f m = do
  -- Check if the file exists. It should if this was called from main.
  doesFileExist f >>= (\e -> case e of
                        True -> do return ()
                        False -> do
                          fileError f
                          exitFailure)

  -- Write the dimension header (not a comment)
  appendFile f $ unwords (map show [numRows m, numCols m])

  -- Write values
  V.mapM_ (\line -> do
            appendFile f line
            appendFile f "\n") -- Sequence of appendFiles
    $ V.map (unwords . V.toList . (V.map (show . S.sciToFloat))) -- :: Vector (Vector SciNum) -> Vector String
    $ m  

-- Writes an array of comments to a file. This should be the first thing.
addComment :: IO.FilePath -> String -> IO ()
addComment f c = do
  appendFile f ("% " ++ c)
  appendFile f "\n"


main :: IO ()
main = do
  -- get arguments
  userFile:itemFile:outFile:comments <- getArgs

  -- Make sure the input files are valid. If not, quit.
  mapM_ (\f -> doesFileExist f >>= (\e -> case e of
          True -> return ()
          False -> do fileError f
                      exitFailure)) [userFile, itemFile]


  -- Check if the output directory exists. If not, create it.
  let outDir = F.encodeString $ parent (F.decodeString outFile)
  doesDirectoryExist outDir >>= (\e -> case e of
      True -> return ()
      False -> do
        putStrLn $ "Creating directory \"" ++ outDir ++ "\"..."
        createDirectory outDir)
  
  -- If the output file already exists, prompt for overwrite.
  doesFileExist outFile >>= (\e -> case e of
      False -> return ()
      True -> do
        putStrLn $ "File \"" ++ outFile ++ "\" already exists."
        putStrLn $ "Overwrite? (yes / no))"
        getLine >>= (\resp -> case resp of
                      'y':_ -> return ()
                      _ -> do
                        putStrLn "Choose another destination for the \
                        \output file and try again."
                        exitFailure))

  -- Create or overwrite the output file.
  writeFile outFile ""

  -- Write comment header
  mapM_ (addComment outFile) comments

  -- Get start time
  startTime <- getCurrentTime

  -- Compute the result and write it to file.
  userMat <- readSciMatFromMM userFile
  itemMat <- readSciMatFromMM itemFile
  let result = matProd userMat (transpose itemMat)
  writeSciMatToMM outFile result

  -- Get finish time
  finishTime <- getCurrentTime

  -- Return runtime info.
  putStrLn "Runtime: \n"
  putStrLn (show (diffUTCTime finishTime startTime))

  exitSuccess