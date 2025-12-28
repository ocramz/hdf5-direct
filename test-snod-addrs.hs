{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as BL
import Data.HDF5.Direct.Internal
import System.IO.MMap
import Control.Exception

main :: IO ()
main = do
  let testPath = "test-data/kosarak-jaccard.hdf5"
  contents <- mmapFileByteStringLazy testPath Nothing
  
  case parseSuperblockMetadata contents of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right (rootAddr, offsetSz, lenSz) -> do
      putStrLn $ "Root: " ++ show rootAddr
      putStrLn $ "Sizes: offset=" ++ show offsetSz ++ ", length=" ++ show lenSz
      
      case parseExtendedSuperblocV0V1 contents offsetSz lenSz of
        Left err -> putStrLn $ "Extended parse error: " ++ show err
        Right (rootObjAddr, btreeAddr, heapAddr) -> do
          putStrLn $ "B-tree addr: " ++ show btreeAddr
          putStrLn $ "Heap addr: " ++ show heapAddr
          
          let entries = parseSymbolTableForDatasets contents btreeAddr heapAddr
          putStrLn $ "\nFound " ++ show (length entries) ++ " entries:"
          mapM_ (\e -> putStrLn $ "  - " ++ steName e ++ " @ 0x" ++ showHex (steObjectHeaderAddr e) "") entries
          
          putStrLn "\nTrying to parse each object header:"
          mapM_ (tryParse contents offsetSz lenSz) entries

tryParse :: BL.ByteString -> Int -> Int -> SymbolTableEntry -> IO ()
tryParse fileData offsetSz lenSz entry = do
  let name = steName entry
      addr = steObjectHeaderAddr entry
  putStrLn $ "\nParsing: " ++ name ++ " @ 0x" ++ showHex addr ""
  result <- try $ evaluate $ parseDatasetFromObjectHeader fileData offsetSz lenSz name addr
  case result of
    Left (e :: SomeException) -> putStrLn $ "  ERROR: " ++ show e
    Right Nothing -> putStrLn $ "  Failed to parse"
    Right (Just info) -> putStrLn $ "  Success: " ++ show (dsDimensions $ dsDataspace info)

showHex :: Word64 -> String -> String
showHex n _ = "0x" ++ go n
  where
    go 0 = "0"
    go x = reverse $ take 16 $ reverse $ map toHex $ takeWhile (> 0) $ iterate (`div` 16) x
    toHex d | d < 10 = toEnum (fromEnum '0' + fromIntegral d)
            | otherwise = toEnum (fromEnum 'a' + fromIntegral (d - 10))
