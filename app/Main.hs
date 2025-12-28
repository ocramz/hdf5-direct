{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.HDF5.Direct.Internal
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: hdf5-direct <file.hdf5>"
      exitFailure
    (filepath:_) -> do
      catch (testReadHDF5 filepath) (\(e :: SomeException) -> do
        putStrLn $ "Error: " ++ show e
        exitFailure)

testReadHDF5 :: FilePath -> IO ()
testReadHDF5 filepath = do
  putStrLn $ "Opening: " ++ filepath
  result <- introspectHDF5File filepath
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right introspection -> do
      putStrLn $ "Valid HDF5 signature: " ++ show (intro_validSignature introspection)
      putStrLn $ "Summary: " ++ intro_summary introspection
  
  -- Test dataset discovery using the unified implementation
  putStrLn "\nDataset Discovery (unified pure + IO version):"
  bs <- BL.readFile filepath
  let datasets = discoverDatasets bs
  putStrLn $ "Discovered " ++ show (length datasets) ++ " datasets"
  mapM_ (\d -> putStrLn $ "  - " ++ dsiName d ++ ": " ++ show (dsDimensions (dsiDataspace d))) datasets
