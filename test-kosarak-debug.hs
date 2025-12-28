import qualified Data.ByteString.Lazy as BL
import Data.HDF5.Direct.Internal

main :: IO ()
main = do
  bs <- BL.readFile "test-data/kosarak-jaccard.hdf5"
  let datasets = discoverDatasets bs
  putStrLn $ "Found " ++ show (length datasets) ++ " datasets"
  mapM_ (\ds -> putStrLn $ "  - " ++ dsiName ds ++ ": " ++ show (dsDimensions (dsiDataspace ds))) datasets
  
  -- Also try parseSuperblockMetadata
  case parseSuperblockMetadata bs of
    Left err -> putStrLn $ "Superblock error: " ++ show err
    Right (rootAddr, offsetSz, lenSz) -> do
      putStrLn $ "Superblock: rootAddr=" ++ show rootAddr ++ ", offsetSz=" ++ show offsetSz ++ ", lenSz=" ++ show lenSz
      
      -- Try extended superblock
      if rootAddr == -1
        then case parseExtendedSuperblocV0V1 bs offsetSz lenSz of
          Left err -> putStrLn $ "Extended superblock error: " ++ show err
          Right (roa, btree, heap) -> do
            putStrLn $ "Extended: rootObjAddr=" ++ show roa ++ ", btreeAddr=" ++ show btree ++ ", heapAddr=" ++ show heap
            
            -- Try parsing symbol table
            let entries = parseSymbolTableForDatasets bs btree heap
            putStrLn $ "Symbol table entries: " ++ show (length entries)
            mapM_ (\e -> putStrLn $ "  - " ++ steName e ++ " @ " ++ show (steObjectHeaderAddr e)) entries
        else putStrLn "Not extended addressing"
