module Main where

import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as V

import Filesystem.Path.CurrentOS as F

import System.Directory
import System.Environment
import System.Exit
import System.IO as IO

import ResultType

main :: IO ()
main = do
  -- get arguments
  userFile:itemFile:userBiasFile:itemBiasFile:globalMeanFile:qualFile:outFile:comments <- getArgs

  -- Make sure the input files are valid. If not, quit.
  mapM_ (\f -> doesFileExist f >>= (\e -> case e of
          True -> return ()
          False -> do fileError f
                      exitFailure)) [userFile, itemFile, qualFile]


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
  userBiases <- readBiases userBiasFile
  itemBiases <- readBiases itemBiasFile
  globalMean <- readGlobalMean globalMeanFile

  -- DEBUG:
  -- putStrLn $ "First line of user matrix: " ++ (show $ getRow userMat 0)
  -- putStrLn $ "First line of item matrix: " ++ (show $ getRow itemMat 0)
  qualContents <- readFile qualFile
  let coords = V.fromList [(user - 1, movie - 1) | user:movie:_ <- (map (map (read :: String -> Int) . words) (lines qualContents))]
      results = V.map (getResultAt userMat itemMat userBiases itemBiases globalMean) coords

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
    getResultAt :: Num a => [Vector a] -> [Vector a] -> Vector a -> Vector a
                         -> a -> (Int, Int) -> a
    getResultAt userMat itemMat 
                userBiases itemBiases 
                globalMean (u, i) = let u_vec      = getRow userMat u
                                        (p_u, w_u) = V.splitAt (truncate ((fromIntegral $ V.length u_vec) / 2)) $ u_vec
                                        b_u        = userBiases V.!u
                                        b_i        = itemBiases V.!i
                                        bias       = globalMean + b_u + b_i
                                    in (vectorProd_ (vectorSum p_u w_u) (getRow itemMat i)) + bias

    writeResult :: Handle -> ResultType -> IO ()
    writeResult hdl r = hPutStrLn hdl (show r)

    readBiases :: IO.FilePath -> IO (Vector ResultType)
    readBiases f = do
      contents <- readFile f -- contents :: String
      -- goodLines :: [String]
      let goodLines = tail  $ dropWhile ((== '%') . (!! 0))
                            $ lines contents
          res       = V.map read $ V.fromList goodLines
      return res

    readGlobalMean :: IO.FilePath -> IO Float
    readGlobalMean f = do
      contents <- readFile f
      return $ read $ last $ lines contents


    -- Writes an array of comments to a file. This should be the first thing.
    writeComment :: Handle -> String -> IO ()
    writeComment hdl c = hPutStrLn hdl ('%':' ':c)

    fileError :: IO.FilePath -> IO ()
    fileError f = hPutStrLn stderr $ "Error: The file \"" 
                                   ++ f ++ "\" does not exist. Exiting now."