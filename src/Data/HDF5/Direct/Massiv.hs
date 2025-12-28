{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.HDF5.Direct.Massiv
  ( -- * Error types
    MassivError(..)
  , DimensionError(..)
  , ConversionError(..)
    -- * Dimension support
  , Rank(..)
  , SomeDimensions(..)
  , extractDimensions
  , getDimensionsAtRank
    -- * Massiv Array Integration (Bracket-based API)
    -- | All functions use bracket pattern to guarantee MmapFile lifetime
  , HDF5Dataset
  , getDatasetDimensions
  , getArrayMetadata
    -- * Reading from files (bracket-based)
  , withArray1DFromFile
  , withArray2DFromFile
  , withArray3DFromFile
    -- * Internal helpers for testing
    -- | These are primarily for unit testing. Production code should use bracket-based API.
  , createDatasetND
  , createDataset1D
  , createDataset2D
  , createDataset3D
  , toMassivArray1D
  , toMassivArray2D
  , toMassivArray3D
    -- * Creating from arrays (pure operations)
  , fromMassivArray1D
  , fromMassivArray2D
  , fromMassivArray3D
    -- * Writing to files
  , writeArrayAsDataset1D
  , writeArrayAsDataset2D
  , writeArrayAsDataset3D
    -- * Array metadata
  , ArrayMetadata(..)
    -- * Element type support
  , SupportedElement(..)
  , ElementReadError(..)
    -- * HDF5 Metadata discovery
  , HDF5Superblock(..)
  , HDF5DatasetInfo(..)
  , discoverDatasets
  , discoverDatasetsFromFile
    -- * Re-exported Massiv types
  , module Data.Massiv.Array
  ) where

import qualified Data.HDF5.Direct.Internal as I
import Data.HDF5.Direct.Internal
  ( HDF5Exception(..)
  , HDF5Datatype(..)
  , DatatypeClass(..)
  , FixedPointType(..)
  , FloatingPointType(..)
  , TimeType(..)
  , StringType(..)
  , BitfieldType(..)
  , OpaqueType(..)
  , ByteOrder(..)
  , HDF5DataLayout(..)
  , MmapFile(..)
  , withMmapFile
  , bytesToWord16
  , bytesToWord32
  , bytesToWord64
  , word16ToBytes
  , word32ToBytes
  , word64ToBytes
  , writeHDF5File
  , HDF5Superblock(..)
  , HDF5DatasetInfo(..)
  , discoverDatasets
  , discoverDatasetsFromFile
  )

import qualified Data.Massiv.Array as M
import Data.Massiv.Array (Array, Ix1, Ix2, Ix3, U)
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.ByteString.Lazy (ByteString)
import Foreign.Ptr (castPtr)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Control.Exception (catch, throwIO, SomeException)
import Data.List (find)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.Generics (Generic)

-- ============================================================================
-- Error Types
-- ============================================================================

-- | Errors that can occur during dimension extraction and validation
data DimensionError
  = DimensionEmpty                      -- ^ Array has no dimensions
  | DimensionNonPositive [Int]          -- ^ One or more dimensions are non-positive
  | DimensionRankMismatch               -- ^ Dimension rank doesn't match expected rank
  deriving (Show, Eq)

-- | Errors that can occur during array conversion
data ConversionError
  = NotOneDimensional                   -- ^ Expected 1D dataset
  | NotTwoDimensional                   -- ^ Expected 2D dataset
  | NotThreeDimensional                 -- ^ Expected 3D dataset
  | IncompleteElementData               -- ^ Byte data is incomplete for element
  | ElementCountMismatch Int Int         -- ^ Expected elements vs actual elements
  | ElementReadFailure ElementReadError  -- ^ Failed to read an element from bytes
  deriving (Show, Eq)

-- | Comprehensive error type for Massiv array operations
data MassivError
  = DimensionErr DimensionError
  | ConversionErr ConversionError
  | EmptyArrayError                     -- ^ Cannot create dataset from empty array
  | DatasetNotFound String              -- ^ Dataset name not found in file
  | UnsupportedLayout String            -- ^ Layout type not yet supported
  | MetadataParseError String           -- ^ Failed to parse HDF5 metadata
  deriving (Show, Eq)

-- ============================================================================
-- Dimension Support
-- ============================================================================

-- | Runtime rank indicator
data Rank = Rank1D | Rank2D | Rank3D | RankHigher Int
  deriving (Show, Eq, Ord)

-- | Represents dimensions at unknown rank
--   Extracted from HDF5 format, allowing safe inspection before loading
data SomeDimensions
  = Dims1D !Ix1
  | Dims2D !Ix2
  | Dims3D !Ix3
  | DimsHigherRank ![Int]  -- ^ Fallback for rank > 3, stored as list
  deriving (Show, Eq)



-- | Extract dimensions from HDF5 format without loading data
--   This allows safe inspection of dataset shape before materialization
extractDimensions :: [Int] -> Either MassivError SomeDimensions
extractDimensions dims
  | null dims = Left (DimensionErr DimensionEmpty)
  | any (<= 0) dims = Left (DimensionErr (DimensionNonPositive dims))
  | otherwise = case dims of
      [n] -> Right $ Dims1D (M.Ix1 n)
      [n1, n2] -> Right $ Dims2D (M.Ix2 n1 n2)
      [n1, n2, n3] -> Right $ Dims3D (M.Ix3 n1 n2 n3)
      _ -> Right $ DimsHigherRank dims

-- | Get dimensions at a specific rank, or fail if rank doesn't match
getDimensionsAtRank :: Rank -> SomeDimensions -> Either MassivError [Int]
getDimensionsAtRank Rank1D (Dims1D (M.Ix1 n)) = Right [n]
getDimensionsAtRank Rank2D (Dims2D (M.Ix2 n1 n2)) = Right [n1, n2]
getDimensionsAtRank Rank3D (Dims3D (M.Ix3 n1 n2 n3)) = Right [n1, n2, n3]
getDimensionsAtRank (RankHigher r) (DimsHigherRank dims)
  | length dims == r = Right dims
  | otherwise = Left (DimensionErr DimensionRankMismatch)
getDimensionsAtRank _ _ = Left (DimensionErr DimensionRankMismatch)

-- ============================================================================
-- HDF5 Array Types
-- ============================================================================

-- | Metadata describing an HDF5 dataset
data ArrayMetadata = ArrayMetadata
  { amDimensions :: !SomeDimensions  -- ^ Shape of the array (type-safe)
  , amTotalElements :: !Int          -- ^ Product of all dimensions
  , amDatatype :: !HDF5Datatype      -- ^ Element datatype
  , amByteOrder :: !ByteOrder        -- ^ Byte order (little/big endian)
  , amElementSize :: !Int            -- ^ Size of each element in bytes
  } deriving (Show, Eq, Generic)

-- | Represents an HDF5 dataset with multidimensional array data
--   The array can be 1D, 2D, 3D, or higher dimensional
--   Uses lazy ByteString backed by mmap for efficient memory usage
--   
--   Type parameter 'a' is the element type
data HDF5Dataset a = HDF5Dataset
  { dsMetadata :: !ArrayMetadata
  , dsDataLazy :: !ByteString        -- ^ Lazy bytes from mmap (defers page faults)
  , dsDimensions :: !SomeDimensions  -- ^ Type-safe dimensions
  } deriving (Generic, Show)

-- ============================================================================
-- Element Reading Errors
-- ============================================================================

-- | Error that can occur when reading an element from bytes
data ElementReadError
  = ByteLengthMismatch Int Int  -- ^ Expected bytes, actual bytes
  | InvalidByteOrder
  | UnexpectedEndOfData
  deriving (Show, Eq)

-- ============================================================================
-- Element Type Support
-- ============================================================================

-- | Typeclass for elements that can be stored in HDF5 arrays
class (VU.Unbox a, Eq a, Show a) => SupportedElement a where
  -- | Get the HDF5 datatype for this element type
  toHDF5Type :: a -> HDF5Datatype
  
  -- | Read an element from raw bytes with specified byte order
  readElement :: ByteOrder -> [Word8] -> Either ElementReadError a
  
  -- | Write an element to raw bytes with specified byte order
  writeElement :: ByteOrder -> a -> [Word8]

-- | Fixed-width 8-bit integer
instance SupportedElement Int8 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedInt8)
    where fixedInt8 = FixedPointType LittleEndian True 0 8 1
  readElement _ bs = case bs of
    [b] -> Right (fromIntegral b :: Int8)
    _ -> Left (ByteLengthMismatch 1 (length bs))
  writeElement _ i = [fromIntegral i]

-- | Unsigned 8-bit integer
instance SupportedElement Word8 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedWord8)
    where fixedWord8 = FixedPointType LittleEndian False 0 8 1
  readElement _ bs = case bs of
    [b] -> Right b
    _ -> Left (ByteLengthMismatch 1 (length bs))
  writeElement _ w = [w]

-- | Fixed-width 16-bit signed integer
instance SupportedElement Int16 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedInt16)
    where fixedInt16 = FixedPointType LittleEndian True 0 16 2
  readElement order bs = case bytesToWord16 order bs of
    Right w -> Right (fromIntegral w :: Int16)
    Left _ -> Left (ByteLengthMismatch 2 (length bs))
  writeElement order i =
    word16ToBytes order (fromIntegral i)

-- | Unsigned 16-bit integer
instance SupportedElement Word16 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedWord16)
    where fixedWord16 = FixedPointType LittleEndian False 0 16 2
  readElement order bs = case bytesToWord16 order bs of
    Right w -> Right w
    Left _ -> Left (ByteLengthMismatch 2 (length bs))
  writeElement order w = word16ToBytes order w

-- | Fixed-width 32-bit signed integer
instance SupportedElement Int32 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedInt32)
    where fixedInt32 = FixedPointType LittleEndian True 0 32 4
  readElement order bs = case bytesToWord32 order bs of
    Right w -> Right (fromIntegral w :: Int32)
    Left _ -> Left (ByteLengthMismatch 4 (length bs))
  writeElement order i =
    word32ToBytes order (fromIntegral i)

-- | Unsigned 32-bit integer
instance SupportedElement Word32 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedWord32)
    where fixedWord32 = FixedPointType LittleEndian False 0 32 4
  readElement order bs = case bytesToWord32 order bs of
    Right w -> Right w
    Left _ -> Left (ByteLengthMismatch 4 (length bs))
  writeElement order w = word32ToBytes order w

-- | Fixed-width 64-bit signed integer
instance SupportedElement Int64 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedInt64)
    where fixedInt64 = FixedPointType LittleEndian True 0 64 8
  readElement order bs = case bytesToWord64 order bs of
    Right w -> Right (fromIntegral w :: Int64)
    Left _ -> Left (ByteLengthMismatch 8 (length bs))
  writeElement order i =
    word64ToBytes order (fromIntegral i)

-- | Unsigned 64-bit integer
instance SupportedElement Word64 where
  toHDF5Type _ = HDF5Datatype 0 (ClassFixedPoint fixedWord64)
    where fixedWord64 = FixedPointType LittleEndian False 0 64 8
  readElement order bs = case bytesToWord64 order bs of
    Right w -> Right w
    Left _ -> Left (ByteLengthMismatch 8 (length bs))
  writeElement order w = word64ToBytes order w

-- ============================================================================
-- Helper Functions for Metadata Extraction
-- ============================================================================

-- | Find a dataset by name in the discovered dataset list
--   Returns Nothing if dataset not found
findDataset :: String -> [HDF5DatasetInfo] -> Maybe HDF5DatasetInfo
findDataset name datasets
  | null name = case datasets of
      []    -> Nothing
      (d:_) -> Just d  -- Return first dataset if name is empty
  | otherwise = find (\d -> dsiName d == name) datasets

-- | Extract byte order from HDF5Datatype
extractByteOrder :: HDF5Datatype -> ByteOrder
extractByteOrder (HDF5Datatype _ (ClassFixedPoint fp)) = fpByteOrder fp
extractByteOrder (HDF5Datatype _ (ClassFloatingPoint flt)) = fltByteOrder flt
extractByteOrder (HDF5Datatype _ (ClassTime tm)) = timeByteOrder tm
extractByteOrder (HDF5Datatype _ (ClassBitfield bf)) = bfByteOrder bf
extractByteOrder _ = LittleEndian  -- Default for types without byte order

-- | Extract element size in bytes from HDF5Datatype
extractElementSize :: HDF5Datatype -> Int
extractElementSize (HDF5Datatype _ (ClassFixedPoint fp)) = fpSize fp
extractElementSize (HDF5Datatype _ (ClassFloatingPoint flt)) = fltSize flt
extractElementSize (HDF5Datatype _ (ClassTime tm)) = timeSize tm
extractElementSize (HDF5Datatype _ (ClassString str)) = strSize str
extractElementSize (HDF5Datatype _ (ClassBitfield bf)) = bfSize bf
extractElementSize (HDF5Datatype _ (ClassOpaque op)) = opaqueSize op
extractElementSize _ = 4  -- Default fallback for complex types

-- | Validate that a dataset has the expected rank
validateDatasetRank :: Int -> HDF5DatasetInfo -> Either MassivError ()
validateDatasetRank expectedRank info =
  let spaceDims = I.dsDimensions (dsiDataspace info)
      actualRank = length spaceDims
  in if actualRank == expectedRank
     then Right ()
     else Left $ case expectedRank of
       1 -> ConversionErr NotOneDimensional
       2 -> ConversionErr NotTwoDimensional
       3 -> ConversionErr NotThreeDimensional
       _ -> DimensionErr DimensionRankMismatch

-- | Create ArrayMetadata from parsed HDF5DatasetInfo
createMetadataFromInfo :: HDF5DatasetInfo -> Either MassivError ArrayMetadata
createMetadataFromInfo info = do
  let spaceDims = I.dsDimensions (dsiDataspace info)
      dims = map fromIntegral spaceDims
  dimStruct <- extractDimensions dims
  let totalElems = product dims
      byteOrder = extractByteOrder (dsiDatatype info)
      elemSize = extractElementSize (dsiDatatype info)
  return $ ArrayMetadata dimStruct totalElems (dsiDatatype info) byteOrder elemSize

-- | Convert MmapFile to lazy ByteString for parsing
--   Uses the existing mmap without creating a new one
mmapToByteString :: MmapFile -> ByteString
mmapToByteString (MmapFile _ fptr offset size) =
  BL.fromStrict $ unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    let actualPtr = ptr `plusPtr` offset
    BSU.unsafePackCStringLen (castPtr actualPtr, size)

-- | Extract raw data from existing MmapFile based on layout type
--   Avoids creating a new mmap by using the existing file mapping
extractDataFromMmap :: MmapFile -> HDF5DataLayout -> IO ByteString
extractDataFromMmap mmapFile layout = case layout of
  CompactLayout _ _ rawData ->
    -- Data is embedded directly in the object header
    return rawData
  
  ContiguousLayout _ addr size -> do
    -- Extract data region from existing mmap using pointer arithmetic
    let MmapFile _ fptr _ _ = mmapFile
        offset = fromIntegral addr
        dataSize = fromIntegral size
    return $ BL.fromStrict $ unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
      let dataPtr = ptr `plusPtr` offset
      BSU.unsafePackCStringLen (castPtr dataPtr, dataSize)
  
  ChunkedLayout _ _ btreeAddr chunkDims elemSize ->
    -- Chunked layout requires B-tree parsing (not yet implemented)
    throwIO $ massivErrorToException $ UnsupportedLayout $
      "Chunked layout not yet supported. Dataset uses B-tree at address " ++
      show btreeAddr ++ " with chunk dimensions " ++ show chunkDims ++
      " and element size " ++ show elemSize

-- | Extract raw data from file based on layout type
-- ============================================================================
-- Core Operations
-- ============================================================================

-- | Get the dimensions of a dataset
getDatasetDimensions :: HDF5Dataset a -> SomeDimensions
getDatasetDimensions (HDF5Dataset _ _ dims) = dims

-- | Get metadata about an array
getArrayMetadata :: HDF5Dataset a -> ArrayMetadata
getArrayMetadata = dsMetadata

-- | Internal: Create a new HDF5 dataset with arbitrary dimensions
--   (For testing purposes - production code should use bracket-based API)
createDatasetND
  :: SupportedElement a
  => [Int]                    -- ^ Dimension sizes
  -> a                        -- ^ Sample element (for type inference)
  -> Either MassivError (HDF5Dataset a)
createDatasetND dims sample = do
  dimStruct <- extractDimensions dims
  let elementSize = length (writeElement LittleEndian sample)
      totalElems = product dims
      metadata = ArrayMetadata
        { amDimensions = dimStruct
        , amTotalElements = totalElems
        , amDatatype = toHDF5Type sample
        , amByteOrder = LittleEndian
        , amElementSize = elementSize
        }
      rawData = BL.pack $ concat $ replicate totalElems (writeElement LittleEndian sample)
  Right $ HDF5Dataset metadata rawData dimStruct

-- | Internal: Create a new 1D HDF5 dataset with specified size
createDataset1D
  :: SupportedElement a
  => Int                      -- ^ Dimension size
  -> a                        -- ^ Sample element (for type inference)
  -> Either MassivError (HDF5Dataset a)
createDataset1D n sample = createDatasetND [n] sample

-- | Internal: Create a new 2D HDF5 dataset with specified dimensions
createDataset2D
  :: SupportedElement a
  => Int                      -- ^ Dimension 1 size
  -> Int                      -- ^ Dimension 2 size
  -> a                        -- ^ Sample element (for type inference)
  -> Either MassivError (HDF5Dataset a)
createDataset2D n1 n2 sample = createDatasetND [n1, n2] sample

-- | Internal: Create a new 3D HDF5 dataset with specified dimensions
createDataset3D
  :: SupportedElement a
  => Int                      -- ^ Dimension 1 size
  -> Int                      -- ^ Dimension 2 size
  -> Int                      -- ^ Dimension 3 size
  -> a                        -- ^ Sample element (for type inference)
  -> Either MassivError (HDF5Dataset a)
createDataset3D n1 n2 n3 sample = createDatasetND [n1, n2, n3] sample

-- | Convert 1D HDF5 dataset to a materialized 1D Massiv array
toMassivArray1D
  :: SupportedElement a
  => HDF5Dataset a
  -> Either MassivError (Array U Ix1 a)
toMassivArray1D (HDF5Dataset metadata bytes dims) = case dims of
  Dims1D (M.Ix1 n) -> byteDataToArray1D n bytes (amByteOrder metadata) (amElementSize metadata)
  _ -> Left (ConversionErr NotOneDimensional)

-- | Convert 2D HDF5 dataset to a materialized 2D Massiv array
toMassivArray2D
  :: SupportedElement a
  => HDF5Dataset a
  -> Either MassivError (Array U Ix2 a)
toMassivArray2D (HDF5Dataset metadata bytes dims) = case dims of
  Dims2D (M.Ix2 n1 n2) -> do
    arr1d <- byteDataToArray1D (n1 * n2) bytes (amByteOrder metadata) (amElementSize metadata)
    let indexFn (M.Ix2 i j) = arr1d M.! M.Ix1 (i * n2 + j)
    Right $ M.makeArray M.Par (M.Sz2 n1 n2) indexFn
  _ -> Left (ConversionErr NotTwoDimensional)

-- | Convert 3D HDF5 dataset to a materialized 3D Massiv array
toMassivArray3D
  :: SupportedElement a
  => HDF5Dataset a
  -> Either MassivError (Array U Ix3 a)
toMassivArray3D (HDF5Dataset metadata bytes dims) = case dims of
  Dims3D (M.Ix3 n1 n2 n3) -> do
    arr1d <- byteDataToArray1D (n1 * n2 * n3) bytes (amByteOrder metadata) (amElementSize metadata)
    let indexFn (M.Ix3 i j k) = arr1d M.! M.Ix1 (i * n2 * n3 + j * n3 + k)
    Right $ M.makeArray M.Par (M.Sz3 n1 n2 n3) indexFn
  _ -> Left (ConversionErr NotThreeDimensional)

-- | Helper function to convert lazy ByteString to 1D array
byteDataToArray1D
  :: SupportedElement a
  => Int              -- ^ Expected number of elements
  -> ByteString       -- ^ Lazy byte data from mmap
  -> ByteOrder        -- ^ Byte order
  -> Int              -- ^ Bytes per element
  -> Either MassivError (Array U Ix1 a)
byteDataToArray1D n bytes order elemSize = do
  let byteList = BL.unpack bytes
  elements <- bytesToElements byteList order elemSize
  if length elements /= n
    then Left (ConversionErr (ElementCountMismatch n (length elements)))
    else Right $ M.fromList M.Par elements

-- | Helper function to split raw bytes into elements
bytesToElements
  :: SupportedElement a
  => [Word8]
  -> ByteOrder
  -> Int
  -> Either MassivError [a]
bytesToElements bytes order elemSize = go bytes []
  where
    go [] acc = Right (reverse acc)
    go bs acc =
      let (elemBytes, rest) = splitAt elemSize bs
      in if length elemBytes /= elemSize
         then Left (ConversionErr IncompleteElementData)
         else case readElement order elemBytes of
           Left err -> Left (ConversionErr (ElementReadFailure err))
           Right e -> go rest (e : acc)

-- | Convert from a materialized Massiv array to HDF5 dataset (internal)
--   Creates ByteString from array elements for in-memory operations
fromMassivArrayGeneric
  :: SupportedElement a
  => [Int]              -- ^ Dimensions extracted from array
  -> [a]                -- ^ Array elements
  -> Either MassivError (HDF5Dataset a)
fromMassivArrayGeneric dims elems = case elems of
  [] -> Left EmptyArrayError
  (sample:_) -> do
      dimStruct <- extractDimensions dims
      let metadata = ArrayMetadata
            { amDimensions = dimStruct
            , amTotalElements = length elems
            , amDatatype = toHDF5Type sample
            , amByteOrder = LittleEndian
            , amElementSize = length (writeElement LittleEndian sample)
            }
          rawData = BL.pack $ concat [writeElement LittleEndian x | x <- elems]
      Right $ HDF5Dataset metadata rawData dimStruct

-- | Convert from a materialized 1D Massiv array to HDF5 dataset
fromMassivArray1D
  :: SupportedElement a
  => Array U Ix1 a
  -> Either MassivError (HDF5Dataset a)
fromMassivArray1D arr =
  let n = case M.size arr of M.Sz (M.Ix1 n') -> n'
  in fromMassivArrayGeneric [n] (M.toList arr)

-- | Convert from a materialized 2D Massiv array to HDF5 dataset
fromMassivArray2D
  :: SupportedElement a
  => Array U Ix2 a
  -> Either MassivError (HDF5Dataset a)
fromMassivArray2D arr =
  let M.Sz (M.Ix2 n1 n2) = M.size arr
  in fromMassivArrayGeneric [n1, n2] (M.toList arr)

-- | Convert from a materialized 3D Massiv array to HDF5 dataset
fromMassivArray3D
  :: SupportedElement a
  => Array U Ix3 a
  -> Either MassivError (HDF5Dataset a)
fromMassivArray3D arr =
  let M.Sz (M.Ix3 n1 n2 n3) = M.size arr
  in fromMassivArrayGeneric [n1, n2, n3] (M.toList arr)

-- | Convert MassivError to an HDF5Exception for IO operations
massivErrorToException :: MassivError -> HDF5Exception
massivErrorToException err = ParseError (show err)

-- | Internal helper function to read arrays from HDF5 files with proper mmap usage
--   Eliminates code duplication across withArray1D/2D/3DFromFile
--   Uses a single mmap for both metadata discovery and data extraction
withArrayFromFileInternal
  :: Int                                    -- ^ Expected rank (1, 2, or 3)
  -> FilePath                               -- ^ HDF5 file path
  -> String                                 -- ^ Dataset name (empty = first dataset)
  -> (HDF5Dataset a -> Either MassivError b) -- ^ Converter (toMassivArray1D/2D/3D)
  -> (b -> IO c)                            -- ^ User action
  -> IO c
withArrayFromFileInternal expectedRank path datasetName converter action =
  withMmapFile path $ \mmapFile -> do
    -- Discover datasets from existing mmap (no file re-read)
    let fileContents = mmapToByteString mmapFile
        datasets = discoverDatasets fileContents
    
    -- Find the requested dataset
    case findDataset datasetName datasets of
      Nothing -> throwIO $ massivErrorToException $ 
        DatasetNotFound $ "Dataset '" ++ datasetName ++ "' not found in " ++ path
      
      Just dsInfo -> do
        -- Validate rank
        case validateDatasetRank expectedRank dsInfo of
          Left err -> throwIO $ massivErrorToException err
          Right () -> do
            -- Create metadata from parsed info
            metadata <- case createMetadataFromInfo dsInfo of
              Left err -> throwIO $ massivErrorToException err
              Right m -> return m
            
            -- Extract data from existing mmap (no new mmap)
            bytes <- catch (extractDataFromMmap mmapFile (dsiLayout dsInfo))
                           (\(e :: SomeException) -> throwIO $ MmapIOError $ 
                             "Failed to extract data from layout: " ++ show e)
            
            -- Create dataset with real metadata and dimensions
            let spaceDims = I.dsDimensions (dsiDataspace dsInfo)
                dims = map fromIntegral spaceDims
            dimStruct <- case extractDimensions dims of
              Left err -> throwIO $ massivErrorToException err
              Right d -> return d
            
            let ds = HDF5Dataset metadata bytes dimStruct
            
            -- Convert to Massiv array and run action
            result <- catch (case converter ds of
                              Left err -> throwIO $ massivErrorToException err
                              Right arr -> action arr)
                            (\(e :: SomeException) -> throwIO $ MmapIOError $ 
                              "Failed to read array: " ++ show e)
            return result

-- | Safely read a 1D array from a file with mmap
--   The MmapFile is guaranteed to stay open during the callback.
--   
--   Example:
--   @
--   withArray1DFromFile "data.h5" "dataset_name" $ \(arr :: Array U Ix1 Int32) -> do
--     print arr
--   @
--   
--   Use empty string "" for dataset name to auto-select the first dataset.
withArray1DFromFile
  :: SupportedElement a
  => FilePath          -- ^ HDF5 file path
  -> String            -- ^ Dataset name (empty string = first dataset)
  -> (Array U Ix1 a -> IO b)
  -> IO b
withArray1DFromFile path datasetName action =
  withArrayFromFileInternal 1 path datasetName toMassivArray1D action

-- | Safely read a 2D array from a file with mmap
--   The MmapFile is guaranteed to stay open during the callback.
--   
--   Use empty string "" for dataset name to auto-select the first dataset.
withArray2DFromFile
  :: SupportedElement a
  => FilePath          -- ^ HDF5 file path
  -> String            -- ^ Dataset name (empty string = first dataset)
  -> (Array U Ix2 a -> IO b)
  -> IO b
withArray2DFromFile path datasetName action =
  withArrayFromFileInternal 2 path datasetName toMassivArray2D action

-- | Safely read a 3D array from a file with mmap
--   The MmapFile is guaranteed to stay open during the callback.
--   
--   Use empty string "" for dataset name to auto-select the first dataset.
withArray3DFromFile
  :: SupportedElement a
  => FilePath          -- ^ HDF5 file path
  -> String            -- ^ Dataset name (empty string = first dataset)
  -> (Array U Ix3 a -> IO b)
  -> IO b
withArray3DFromFile path datasetName action =
  withArrayFromFileInternal 3 path datasetName toMassivArray3D action

-- | Write a 1D Massiv array to a file
writeArrayAsDataset1D
  :: SupportedElement a
  => FilePath
  -> Array U Ix1 a
  -> IO ()
writeArrayAsDataset1D path arr = 
  catch (case fromMassivArray1D arr of
          Left err -> throwIO $ massivErrorToException err
          Right ds -> writeHDF5File path (dsDataLazy ds))
        (\(e :: SomeException) -> throwIO $ MmapIOError ("Failed to write 1D array: " ++ show e))

-- | Write a 2D Massiv array to a file
writeArrayAsDataset2D
  :: SupportedElement a
  => FilePath
  -> Array U Ix2 a
  -> IO ()
writeArrayAsDataset2D path arr =
  catch (case fromMassivArray2D arr of
          Left err -> throwIO $ massivErrorToException err
          Right ds -> writeHDF5File path (dsDataLazy ds))
        (\(e :: SomeException) -> throwIO $ MmapIOError ("Failed to write 2D array: " ++ show e))

-- | Write a 3D Massiv array to a file
writeArrayAsDataset3D
  :: SupportedElement a
  => FilePath
  -> Array U Ix3 a
  -> IO ()
writeArrayAsDataset3D path arr =
  catch (case fromMassivArray3D arr of
          Left err -> throwIO $ massivErrorToException err
          Right ds -> writeHDF5File path (dsDataLazy ds))
        (\(e :: SomeException) -> throwIO $ MmapIOError ("Failed to write 3D array: " ++ show e))
-- ============================================================================
-- HDF5 Metadata Discovery API
-- ============================================================================

-- | The unified dataset discovery function (re-exported from Internal)
-- This is the single authoritative implementation for discovering datasets
-- in HDF5 files. Both discoverDatasets (pure) and discoverDatasetsFromFile (IO)
-- converge on the same logic.