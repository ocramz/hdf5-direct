{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.BenchPress
import qualified Data.ByteString.Lazy as BL
import qualified Data.Massiv.Array as M
import Data.Word (Word32, Word64)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Exception (catch, try, SomeException)
import Control.Monad (filterM)

import Data.HDF5.Direct.Internal
  ( parseSuperblockFromFile
  , discoverDatasets
  , mmapFileRegion
  )

import Data.HDF5.Direct.Massiv
  ( withArray1DFromFile
  , withArray2DFromFile
  , withArray3DFromFile
  )

-- | Benchmark configuration
main :: IO ()
main = do
  putStrLn "HDF5-Direct Benchmark Suite"
  putStrLn "==========================="
  putStrLn ""
  
  -- Check which test files are available
  testFiles <- filterAvailableFiles
    [ "test-data/le_data.h5"
    , "test-data/be_data.h5"
    , "test-data/simple.h5"
    , "test-data/dataset.h5"
    , "test-data/group.h5"
    , "test-data/kosarak-jaccard.hdf5"
    ]
  
  if null testFiles
    then putStrLn "No test files found in test-data/. Please run download-test-files.sh first."
    else do
      putStrLn $ "Found " ++ show (length testFiles) ++ " test file(s)"
      putStrLn ""
      
      -- Run benchmarks for each available file
      mapM_ benchmarkFile testFiles

-- | Filter list to only files that exist
filterAvailableFiles :: [FilePath] -> IO [FilePath]
filterAvailableFiles = filterM doesFileExist

-- | Benchmark a single HDF5 file
benchmarkFile :: FilePath -> IO ()
benchmarkFile filePath = do
  putStrLn $ "Benchmarking: " ++ filePath
  putStrLn $ replicate (14 + length filePath) '-'
  putStrLn ""
  
  -- Benchmark parseSuperblockFromFile
  putStrLn "  parseSuperblockFromFile:"
  bench 100 $ do
    _ <- parseSuperblockFromFile filePath `catch` \(e :: SomeException) -> 
      return (Left $ error $ show e)
    return ()
  
  putStrLn ""
  
  -- Benchmark discoverDatasets (includes superblock parsing + dataset discovery)
  putStrLn "  discoverDatasets:"
  bench 100 $ do
    bs <- mmapFileRegion filePath Nothing `catch` \(e :: SomeException) ->
      error $ "Failed to mmap: " ++ show e
    let datasets = discoverDatasets bs
    return $! length datasets
  
  putStrLn ""
  
  -- Benchmark just the mmap operation for comparison
  putStrLn "  mmapFileRegion:"
  bench 100 $ do
    bs <- mmapFileRegion filePath Nothing `catch` \(e :: SomeException) ->
      error $ "Failed to mmap: " ++ show e
    return $! BL.length bs
  
  putStrLn ""
  
  -- Benchmark new loading combinators (single-mmap approach)
  putStrLn "  withArray1DFromFile (Word32):"
  result1D <- try $ withArray1DFromFile "" filePath $ \(arr :: M.Array M.U M.Ix1 Word32) -> do
    let M.Sz (M.Ix1 size) = M.size arr
    return size
  case result1D of
    Right size -> do
      bench 50 $ withArray1DFromFile "" filePath $ \(arr :: M.Array M.U M.Ix1 Word32) -> do
        let M.Sz (M.Ix1 s) = M.size arr
        return $! s
      putStrLn $ "    (loaded " ++ show size ++ " elements)"
    Left (e :: SomeException) -> putStrLn $ "    (skipped: " ++ show e ++ ")"
  
  putStrLn ""
  
  putStrLn "  withArray2DFromFile (Word32):"
  result2D <- try $ withArray2DFromFile "" filePath $ \(arr :: M.Array M.U M.Ix2 Word32) -> do
    let M.Sz (M.Ix2 rows cols) = M.size arr
    return (rows, cols)
  case result2D of
    Right (rows, cols) -> do
      bench 50 $ withArray2DFromFile "" filePath $ \(arr :: M.Array M.U M.Ix2 Word32) -> do
        let M.Sz (M.Ix2 r c) = M.size arr
        return $! (r, c)
      putStrLn $ "    (loaded " ++ show rows ++ " x " ++ show cols ++ " elements)"
    Left (e :: SomeException) -> putStrLn $ "    (skipped: " ++ show e ++ ")"
  
  putStrLn ""
  
  putStrLn "  withArray1DFromFile (Word64):"
  result1D64 <- try $ withArray1DFromFile "" filePath $ \(arr :: M.Array M.U M.Ix1 Word64) -> do
    let M.Sz (M.Ix1 size) = M.size arr
    return size
  case result1D64 of
    Right size -> do
      bench 50 $ withArray1DFromFile "" filePath $ \(arr :: M.Array M.U M.Ix1 Word64) -> do
        let M.Sz (M.Ix1 s) = M.size arr
        return $! s
      putStrLn $ "    (loaded " ++ show size ++ " elements)"
    Left (e :: SomeException) -> putStrLn $ "    (skipped: " ++ show e ++ ")"
  
  putStrLn ""
  putStrLn ""
