module Main where

import Data.List
import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as V

import Filesystem.Path.CurrentOS as F

import System.Directory
import System.Environment
import System.Exit
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

main :: IO ()
main = do
  -- get arguments
  userFile:itemFile:singularValueFile:qualFile:outFile:comments <- getArgs

  -- Make sure the input files are valid. If not, quit.
  mapM_ (\f -> doesFileExist f >>= (\e -> case e of
          True -> return ()
          False -> do fileError f
                      exitFailure)) [userFile, itemFile, singularValueFile, qualFile]


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
        putStrLn $ "Overwrite? (yes / no)"
        getLine >>= (\resp -> case resp of
                      'y':_ -> return ()
                      _ -> do
                        putStrLn "Choose another destination for the \
                        \output file and try again."
                        exitSuccess))

  -- Create blank output file and grab its handle.
  outHdl <- openFile outFile WriteMode
  mapM_ (writeComment outHdl) comments -- Write comments

  -- Get start time
  startTime <- getCurrentTime

  -- Compute the result and write it to file.
  userMat <- readMatFromMM userFile
  itemMat <- readMatFromMM itemFile
  singularValues <- readSingularValues singularValueFile

  -- DEBUG:
  -- putStrLn $ "First line of user matrix: " ++ (show $ getRow userMat 0)
  -- putStrLn $ "First line of item matrix: " ++ (show $ getRow itemMat 0)
  qualContents <- readFile qualFile
  let coords = V.fromList [(user - 1, movie - 1) | user:movie:_ <- (map (map (read :: String -> Int) . words) (lines qualContents))]
      results = V.map (getResultAt userMat itemMat singularValues) coords

  -- Write results to file.
  V.mapM_ (writeResult outHdl) results

  -- Add the runtime info to the list of comments.
  runTime <- (getCurrentTime >>= (\t -> return $ diffUTCTime t startTime))
  
  -- write comments.
  putStrLn $ "Finished. Runtime: " ++ (show runTime)

  -- Clean up and exit.
  hClose outHdl
  exitSuccess

  where

    readSingularValues :: IO.FilePath -> IO (Vector ResultType)
    readSingularValues f = (readFile f >>= (\contents -> do
      let goodLines = tail  $ dropWhile ((== '%') . (!! 0))
                            $ lines contents
      return $ V.fromList $ map read goodLines))

    getResultAt :: Num a => [Vector a] -> [Vector a] -> Vector a
                         -> (Int, Int) -> a
    getResultAt userMat itemMat singularValues (i, j) = vectorProd_ (V.zipWith (*) (getRow userMat i) (singularValues)) (getRow itemMat j)

    writeResult :: Handle -> ResultType -> IO ()
    writeResult hdl r = hPutStrLn hdl (show r)

    -- Writes an array of comments to a file. This should be the first thing.
    writeComment :: Handle -> String -> IO ()
    writeComment hdl c = hPutStrLn hdl ('%':' ':c)

    fileError :: IO.FilePath -> IO ()
    fileError f = hPutStrLn stderr $ "Error: The file \"" 
                                   ++ f ++ "\" does not exist. Exiting now."