{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as BL
import Data.HDF5.Direct.Internal (discoverDatasets, dsiName, dsiDimensions)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  let testFiles = 
        [ "test-data/simple.h5"
        , "test-data/group.h5"
        , "test-data/dataset.h5"
        , "test-data/be_data.h5"
        , "test-data/le_data.h5"
        ]
  
  putStrLn "Testing dataset discovery on HDF5 files:"
  mapM_ testFile testFiles

testFile :: FilePath -> IO ()
testFile path = do
  exists <- doesFileExist path
  if not exists
    then putStrLn $ "  " ++ path ++ " - NOT FOUND"
    else do
      contents <- BL.readFile path
      let datasets = discoverDatasets contents
      putStrLn $ "\n  " ++ path ++ ":"
      if null datasets
        then putStrLn "    (no datasets found in root group)"
        else do
          putStrLn $ "    Found " ++ show (length datasets) ++ " dataset(s):"
          mapM_ showDataset datasets

showDataset :: Show a => a -> IO ()
showDataset ds = putStrLn $ "      - " ++ show ds
