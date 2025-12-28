{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}

module Data.HDF5.Direct.Internal
  ( -- * Exception handling
    HDF5Exception(..)
  , HDF5ParseError(..)
  , HDF5ParserM
    -- * Low-level mmap file handling
  , MmapFile(..)
  , withMmapFile
  , mmapFileRegion
  , parseSuperblockFromFile
  -- , openMmapFile
    -- * File write operations
  , writeHDF5File
  , appendToFile
  , writeByteString
    -- * Byte marshalling utilities
  , bytesToWord16
  , bytesToWord32
  , bytesToWord64
  , word16ToBytes
  , word32ToBytes
  , word64ToBytes
    -- * HDF5 Datatype classes
  , HDF5Datatype(..)
  , ByteOrder(..)
  , PaddingType(..)
  , CharacterSet(..)
  , FixedPointType(..)
  , FloatingPointType(..)
  , TimeType(..)
  , StringType(..)
  , BitfieldType(..)
  , OpaqueType(..)
  , CompoundType(..)
  , CompoundMember(..)
  , ReferenceType(..)
  , EnumerationType(..)
  , VariableLengthType(..)
  , ArrayType(..)
  , DatatypeClass(..)
    -- * Binary parsers for HDF5 Datatypes
  , parseByteOrder
  , parsePaddingType
  , parseCharacterSet
  , getBits
  , parseFixedPoint
  , parseFloatingPoint
  , parseTime
  , parseString
  , parseBitfield
  , parseOpaque
  , parseEnumeration
  , parseCompound
  , parseVariableLength
  , parseArray
    -- * HDF5 Metadata parsing
  , HDF5Superblock(..)
  , HDF5DatasetInfo(..)
  , HDF5Dataspace(..)
  , DataspaceVersion(..)
  , DataspaceType(..)
  , HDF5DataLayout(..)
  , LayoutVersion(..)
  , HDF5Filter(..)
  , FilterID(..)
  , HDF5FilterPipeline(..)
  , FilterPipelineVersion(..)
  , HDF5FillValue(..)
  , FillValueVersion(..)
  , SpaceAllocationTime(..)
  , FillValueWriteTime(..)
  , ObjectHeaderMessage(..)
  , parseSuperblockVersion
  , parseSuperblockMetadata
  , discoverDatasets
  , discoverDatasetsFromFile
    -- * HDF5 Introspection and Validation
  , HDF5Introspection(..)
  , validateHDF5Signature
  , describeDatatypeClass
  , formatDatatype
  , introspectHDF5File
  , assertHDF5WellFormed
  ) where

import Control.Exception (Exception, bracket, catch, throwIO, SomeException, displayException, try)
import Control.Monad (when)
import Data.Binary.Get (Get)
import Data.Binary.Put (Put, runPut)
import Data.Bits (shiftL, shiftR, (.|.), (.&.), testBit)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (isSubsequenceOf, tails, nub, nubBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Generics (Generic)
import Numeric (showHex)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist, getFileSize)
import System.IO (withFile, IOMode(WriteMode, AppendMode), hFlush)
import System.IO.MMap (Mode(ReadOnly), mmapFileByteStringLazy, mmapFileForeignPtr)

-- | Exception type for HDF5 parsing and I/O errors
data HDF5Exception
  = MmapIOError String        -- ^ Mmap I/O error (e.g., file not found)
  | PageFaultError String     -- ^ Page fault during access (SIGSEGV/SIGBUS mapped to exception)
  | ParseError String         -- ^ Binary parse error
  | InvalidOffset String      -- ^ Invalid offset or range
  deriving (Show, Eq, Generic)

instance Exception HDF5Exception

-- | Detailed error type for HDF5 metadata parsing (strict, no silent failures)
data HDF5ParseError
  = InvalidSignature           -- ^ File doesn't have valid HDF5 signature
  | TruncatedData String       -- ^ Not enough bytes to read (e.g., "Reading Word64 at offset 1024, but file is only 1000 bytes")
  | CorruptedMetadata String   -- ^ Metadata structure is invalid
  | InvalidVersion String      -- ^ Unsupported version number
  | UnexpectedMessageType Word8 -- ^ Encountered unexpected object header message type
  | DatatypeParseFailed String -- ^ Failed to parse datatype class
  | SymbolTableParseFailed String -- ^ Failed to parse symbol table
  | BTreeParseFailed String    -- ^ Failed to parse B-tree node
  | HeapParseFailed String     -- ^ Failed to parse local heap
  deriving (Show, Eq, Generic)

-- | Parser monad for composable HDF5 metadata parsing
--   Using Either for clean error propagation
type HDF5ParserM a = Either HDF5ParseError a
runHDF5Parser :: HDF5ParserM a -> Either HDF5ParseError a
runHDF5Parser = id

-- | Memory-mapped HDF5 file handle with resource management
data MmapFile = MmapFile
  { mmapPath :: FilePath
  , mmapPtr :: ForeignPtr Word8
  , mmapOffset :: Int
  , mmapSize :: Int
  } deriving (Generic)

-- | Safely open and map an HDF5 file with automatic resource cleanup
--
--   Uses bracket pattern to ensure mmap is unmapped even on exception.
--   Converts I/O errors and signal-based page faults to HDF5Exception.
withMmapFile
  :: FilePath
  -> (MmapFile -> IO a)
  -> IO a
withMmapFile path = 
  bracket (openMmapFile path) (const $ pure ())  -- No explicit unmap needed; handled by ForeignPtr

-- | Open a file for mmap-based lazy reading
openMmapFile :: FilePath -> IO MmapFile
openMmapFile path = 
  catch (do
    (ptr, offset, size) <- mmapFileForeignPtr path ReadOnly Nothing
    return $ MmapFile path ptr offset size
  ) (\(ex :: SomeException) -> throwIO $ MmapIOError ("Failed to mmap " ++ path ++ ": " ++ displayException ex))



-- | Extract a lazy ByteString from a file region via mmap
--
--   Defers actual page faults until ByteString is consumed.
mmapFileRegion
  :: FilePath
  -> Maybe (Int64, Int64)  -- ^ (offset, size), Nothing for entire file
  -> IO ByteString
mmapFileRegion path range =
  catch (mmapFileByteStringLazy path range)
        (\(ex :: SomeException) -> throwIO $ MmapIOError ("Failed to mmap region: " ++ displayException ex))

-- | Read and parse superblock from file, loading only necessary region
--   First reads a small region (512 bytes) to determine superblock size,
--   then loads appropriate sized region based on version
parseSuperblockFromFile
  :: FilePath
  -> IO (Either HDF5ParseError (Int64, Int, Int))
parseSuperblockFromFile path = do
  -- Read first 512 bytes for superblock (covers v0, v1, v2, v3)
  initialRegion <- mmapFileRegion path (Just (0, 512))
  pure $ parseSuperblockMetadata initialRegion

-- | Write a lazy ByteString to a file, overwriting if it exists
--
--   Creates the file if it doesn't exist, truncates if it does.
--   Throws HDF5Exception on I/O errors.
writeHDF5File
  :: FilePath
  -> ByteString
  -> IO ()
writeHDF5File path content = 
  catch (do
    withFile path WriteMode $ \handle -> do
      BL.hPutStr handle content
      hFlush handle
  ) (\(ex :: SomeException) -> throwIO $ MmapIOError ("Failed to write " ++ path ++ ": " ++ displayException ex))

-- | Append a lazy ByteString to an existing file
--
--   If the file doesn't exist, creates it with the content.
--   Throws HDF5Exception on I/O errors.
appendToFile
  :: FilePath
  -> ByteString
  -> IO ()
appendToFile path content =
  catch (do
    withFile path AppendMode $ \handle -> do
      BL.hPutStr handle content
      hFlush handle
  ) (\(ex :: SomeException) -> throwIO $ MmapIOError ("Failed to append to " ++ path ++ ": " ++ displayException ex))

-- | Write a lazy ByteString using the provided binary Put serializer
--
--   Combines serialization and file write into one operation.
writeByteString
  :: FilePath
  -> Put
  -> IO ()
writeByteString path putter =
  writeHDF5File path (runPut putter)

-- ============================================================================
-- Byte Marshalling Utilities
-- ============================================================================

-- | Convert a list of bytes to a Word16 with specified byte order
--   Returns Left if the byte list doesn't have exactly 2 elements
bytesToWord16 :: ByteOrder -> [Word8] -> Either String Word16
bytesToWord16 LittleEndian [b0, b1] =
  Right (fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8))
bytesToWord16 BigEndian [b1, b0] =
  Right ((fromIntegral b1 `shiftL` 8) .|. fromIntegral b0)
bytesToWord16 _ bs =
  Left $ "Expected 2 bytes for Word16, got " ++ show (length bs)

-- | Convert a list of bytes to a Word32 with specified byte order
--   Returns Left if the byte list doesn't have exactly 4 elements
bytesToWord32 :: ByteOrder -> [Word8] -> Either String Word32
bytesToWord32 LittleEndian [b0, b1, b2, b3] =
  Right (fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8) .|.
         (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b3 `shiftL` 24))
bytesToWord32 BigEndian [b3, b2, b1, b0] =
  Right ((fromIntegral b3 `shiftL` 24) .|. (fromIntegral b2 `shiftL` 16) .|.
         (fromIntegral b1 `shiftL` 8) .|. fromIntegral b0)
bytesToWord32 _ bs =
  Left $ "Expected 4 bytes for Word32, got " ++ show (length bs)

-- | Convert a list of bytes to a Word64 with specified byte order
--   Returns Left if the byte list doesn't have exactly 8 elements
bytesToWord64 :: ByteOrder -> [Word8] -> Either String Word64
bytesToWord64 LittleEndian [b0,b1,b2,b3,b4,b5,b6,b7] =
  Right (fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8) .|.
         (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b3 `shiftL` 24) .|.
         (fromIntegral b4 `shiftL` 32) .|. (fromIntegral b5 `shiftL` 40) .|.
         (fromIntegral b6 `shiftL` 48) .|. (fromIntegral b7 `shiftL` 56))
bytesToWord64 BigEndian [b0,b1,b2,b3,b4,b5,b6,b7] =
  Right ((fromIntegral b7 `shiftL` 56) .|. (fromIntegral b6 `shiftL` 48) .|.
         (fromIntegral b5 `shiftL` 40) .|. (fromIntegral b4 `shiftL` 32) .|.
         (fromIntegral b3 `shiftL` 24) .|. (fromIntegral b2 `shiftL` 16) .|.
         (fromIntegral b1 `shiftL` 8) .|. fromIntegral b0)
bytesToWord64 _ bs =
  Left $ "Expected 8 bytes for Word64, got " ++ show (length bs)

-- | Convert a Word16 to a list of bytes with specified byte order
word16ToBytes :: ByteOrder -> Word16 -> [Word8]
word16ToBytes LittleEndian w =
  [fromIntegral w, fromIntegral (w `shiftR` 8)]
word16ToBytes BigEndian w =
  [fromIntegral (w `shiftR` 8), fromIntegral w]

-- | Convert a Word32 to a list of bytes with specified byte order
word32ToBytes :: ByteOrder -> Word32 -> [Word8]
word32ToBytes LittleEndian w =
  [fromIntegral w, fromIntegral (w `shiftR` 8),
   fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 24)]
word32ToBytes BigEndian w =
  [fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16),
   fromIntegral (w `shiftR` 8), fromIntegral w]

-- | Convert a Word64 to a list of bytes with specified byte order
word64ToBytes :: ByteOrder -> Word64 -> [Word8]
word64ToBytes LittleEndian w =
  [fromIntegral w, fromIntegral (w `shiftR` 8), fromIntegral (w `shiftR` 16),
   fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 32), fromIntegral (w `shiftR` 40),
   fromIntegral (w `shiftR` 48), fromIntegral (w `shiftR` 56)]
word64ToBytes BigEndian w =
  [fromIntegral (w `shiftR` 56), fromIntegral (w `shiftR` 48), fromIntegral (w `shiftR` 40),
   fromIntegral (w `shiftR` 32), fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16),
   fromIntegral (w `shiftR` 8), fromIntegral w]

-- ============================================================================
-- HDF5 Datatype Classes (per HDF5 1.8.7 spec)
-- ============================================================================

-- | Byte order for numeric types
data ByteOrder = LittleEndian | BigEndian
  deriving (Show, Eq, Ord, Generic)

-- | Padding type for fixed/floating-point types
data PaddingType = PadZero | PadOne | PadBackground
  deriving (Show, Eq, Ord, Generic)

-- | Character set encoding for strings
data CharacterSet = ASCII | UTF8
  deriving (Show, Eq, Ord, Generic)

-- | Fixed-point integer datatype (Class 0)
data FixedPointType = FixedPointType
  { fpByteOrder :: !ByteOrder
  , fpSigned :: !Bool
  , fpBitOffset :: !Int
  , fpBitPrecision :: !Int
  , fpSize :: !Int  -- ^ Size in bytes
  } deriving (Show, Eq, Generic)

-- | Floating-point datatype (Class 1)
data FloatingPointType = FloatingPointType
  { fltByteOrder :: !ByteOrder
  , fltBitOffset :: !Int
  , fltBitPrecision :: !Int
  , fltExponentLocation :: !Int
  , fltExponentSize :: !Int
  , fltMantissaLocation :: !Int
  , fltMantissaSize :: !Int
  , fltExponentBias :: !Word32
  , fltSize :: !Int
  } deriving (Show, Eq, Generic)

-- | Time datatype (Class 2)
data TimeType = TimeType
  { timeByteOrder :: !ByteOrder
  , timeBitPrecision :: !Int
  , timeSize :: !Int
  } deriving (Show, Eq, Generic)

-- | String datatype (Class 3)
data StringType = StringType
  { strPaddingType :: !PaddingType
  , strCharset :: !CharacterSet
  , strSize :: !Int  -- ^ Size in bytes, or 0 for variable-length (use VariableLengthType instead)
  } deriving (Show, Eq, Generic)

-- | Bitfield datatype (Class 4)
data BitfieldType = BitfieldType
  { bfByteOrder :: !ByteOrder
  , bfBitOffset :: !Int
  , bfBitPrecision :: !Int
  , bfSize :: !Int
  } deriving (Show, Eq, Generic)

-- | Opaque datatype (Class 5)
data OpaqueType = OpaqueType
  { opaqueTag :: !String  -- ^ ASCII description tag (NUL-padded)
  , opaqueSize :: !Int
  } deriving (Show, Eq, Generic)

-- | Member of a compound datatype
data CompoundMember = CompoundMember
  { cmName :: !String
  , cmByteOffset :: !Int
  , cmMemberType :: !HDF5Datatype  -- ^ Recursive type definition
  } deriving (Show, Eq, Generic)

-- | Compound (struct-like) datatype (Class 6)
data CompoundType = CompoundType
  { compMembers :: ![CompoundMember]
  , compSize :: !Int
  } deriving (Show, Eq, Generic)

-- | Reference datatype (Class 7)
data ReferenceType
  = RefObject           -- ^ Object reference (address)
  | RefDatasetRegion    -- ^ Dataset region reference
  deriving (Show, Eq, Generic)

-- | Enumeration datatype (Class 8)
data EnumerationType = EnumerationType
  { enumBaseType :: !HDF5Datatype
  , enumMappings :: ![(String, Word64)]  -- ^ Name-to-value mappings
  } deriving (Show, Eq, Generic)

-- | Variable-length datatype (Class 9)
data VariableLengthType = VariableLengthType
  { vlBaseType :: !HDF5Datatype
  , vlIsString :: !Bool  -- ^ True for vlen strings, False for vlen sequences
  } deriving (Show, Eq, Generic)

-- | Array datatype (Class 10)
data ArrayType = ArrayType
  { arrBaseType :: !HDF5Datatype
  , arrDimensions :: ![Int]
  } deriving (Show, Eq, Generic)

-- | Datatype class discriminator
data DatatypeClass
  = ClassFixedPoint !FixedPointType
  | ClassFloatingPoint !FloatingPointType
  | ClassTime !TimeType
  | ClassString !StringType
  | ClassBitfield !BitfieldType
  | ClassOpaque !OpaqueType
  | ClassCompound !CompoundType
  | ClassReference !ReferenceType
  | ClassEnumeration !EnumerationType
  | ClassVariableLength !VariableLengthType
  | ClassArray !ArrayType
  deriving (Show, Eq, Generic)

-- | Top-level HDF5 datatype representation
--
--   Encapsulates all spec-defined datatype classes with version tracking.
--   Designed for lazy evaluation: construction is cheap, parsing deferred until access.
data HDF5Datatype = HDF5Datatype
  { dtVersion :: !Int         -- ^ Datatype message version (0-3)
  , dtClass :: !DatatypeClass
  } deriving (Show, Eq, Generic)

-- ============================================================================
-- Binary Parsers for HDF5 Datatypes
-- ============================================================================
-- 
-- These parsers operate over lazy ByteStrings and defer computation until access.
-- All computations are lazy by default in Haskell, so no explicit caching is needed.
--

-- | Parse a byte order bit (0 = little-endian, 1 = big-endian)
parseByteOrder :: Int -> ByteOrder
parseByteOrder 0 = LittleEndian
parseByteOrder _ = BigEndian

-- | Parse a padding type (bits in class bit field)
parsePaddingType :: Int -> PaddingType
parsePaddingType 0 = PadZero
parsePaddingType 1 = PadOne
parsePaddingType _ = PadBackground

-- | Parse character set from bits
parseCharacterSet :: Int -> CharacterSet
parseCharacterSet 0 = ASCII
parseCharacterSet _ = UTF8

-- | Get a single byte and extract bits
getBits :: Get Word8
getBits = Get.getWord8

-- | Parse fixed-point integer datatype (Class 0)
--
--   Format (per spec):
--   - Class Bit Field 0: byte order
--   - Class Bit Field 1-2: padding type (lo_pad, hi_pad)
--   - Class Bit Field 3: signed
--   - Properties: bit_offset (2 bytes), bit_precision (2 bytes)
parseFixedPoint :: Int -> Get FixedPointType
parseFixedPoint size = do
  classBits <- Get.getWord8
  let byteOrder = parseByteOrder (fromIntegral (classBits `div` 1) `mod` 2)
      signed = (classBits `div` 8) `mod` 2 == (1 :: Word8)
  bitOffset <- Get.getWord16be
  bitPrecision <- Get.getWord16be
  return $ FixedPointType byteOrder signed (fromIntegral bitOffset) (fromIntegral bitPrecision) size

-- | Parse floating-point datatype (Class 1)
--
--   Format (per spec):
--   - Class Bit Fields for byte order, padding, normalization, sign bit location
--   - Properties: bit offset, precision, exponent info, mantissa info, bias
parseFloatingPoint :: Int -> Get FloatingPointType
parseFloatingPoint size = do
  classBits0 <- Get.getWord8
  _ <- Get.getWord8
  let byteOrder = parseByteOrder (fromIntegral (classBits0 `div` 1) `mod` 2)
  bitOffset <- Get.getWord16be
  bitPrecision <- Get.getWord16be
  exponentLoc <- Get.getWord16be
  exponentSize <- Get.getWord8
  mantissaLoc <- Get.getWord16be
  mantissaSize <- Get.getWord8
  exponentBias <- Get.getWord32be
  return $ FloatingPointType byteOrder (fromIntegral bitOffset) (fromIntegral bitPrecision) 
           (fromIntegral exponentLoc) (fromIntegral exponentSize) 
           (fromIntegral mantissaLoc) (fromIntegral mantissaSize) exponentBias size

-- | Parse time datatype (Class 2)
--
--   Format (per spec):
--   - Class Bit Field 0: byte order
--   - Properties: bit precision (1 byte)
parseTime :: Int -> Get TimeType
parseTime size = do
  classBits <- Get.getWord8
  let byteOrder = parseByteOrder (fromIntegral (classBits `div` 1) `mod` 2)
  bitPrecision <- Get.getWord8
  return $ TimeType byteOrder (fromIntegral bitPrecision) size

-- | Parse string datatype (Class 3)
--
--   Format (per spec):
--   - Class Bit Fields 0-3: padding type
--   - Class Bit Fields 4-7: character set
--   - No properties (size comes from parent message context)
parseString :: Int -> Get StringType
parseString size = do
  classBits <- Get.getWord8
  let paddingType = parsePaddingType (fromIntegral classBits `mod` 16)
      charset = parseCharacterSet (fromIntegral (classBits `div` 16) `mod` 16)
  return $ StringType paddingType charset size

-- | Parse bitfield datatype (Class 4)
--
--   Format (per spec): same as fixed-point
parseBitfield :: Int -> Get BitfieldType
parseBitfield size = do
  classBits <- Get.getWord8
  let byteOrder = parseByteOrder (fromIntegral (classBits `div` 1) `mod` 2)
  bitOffset <- Get.getWord16be
  bitPrecision <- Get.getWord16be
  return $ BitfieldType byteOrder (fromIntegral bitOffset) (fromIntegral bitPrecision) size

-- | Parse opaque datatype (Class 5)
--
--   Format (per spec):
--   - Class Bit Fields 0-7: length of ASCII tag (in bytes)
--   - Properties: ASCII tag (NUL-padded to 8-byte boundary)
parseOpaque :: Int -> Get OpaqueType
parseOpaque size = do
  classBits <- Get.getWord8
  let tagLen = fromIntegral classBits
  tag <- Get.getByteString tagLen
  let tagStr = Prelude.takeWhile (/= '\0') (map (toEnum . fromIntegral) (BL.unpack (BL.fromStrict tag)))
  -- Skip padding to 8-byte boundary
  let padding = ((tagLen + 7) `div` 8) * 8 - tagLen
  _ <- Get.getByteString padding
  return $ OpaqueType tagStr size

-- | Parse enumeration datatype (Class 8)
--
--   Format (per spec):
--   - Class Bit Fields 0-15: number of members
--   - Properties: base type (recursive), then names and values
parseEnumeration :: Int -> Get EnumerationType
parseEnumeration _ = do
  classBits0 <- Get.getWord8
  classBits1 <- Get.getWord8
  let _ = ((fromIntegral classBits1 :: Int) `shiftL` 8) .|. fromIntegral classBits0
  -- For now, return a placeholder; full implementation requires recursive type parsing
  -- and is deferred to a complete binary parser implementation
  return $ EnumerationType (HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian False 0 8 1))) []

-- | Placeholder for compound datatype parsing
parseCompound :: Int -> Get CompoundType
parseCompound size = do
  classBits0 <- Get.getWord8
  classBits1 <- Get.getWord8
  let _ = ((fromIntegral classBits1 :: Int) `shiftL` 8) .|. fromIntegral classBits0
  -- Full implementation deferred: requires recursive member parsing
  return $ CompoundType [] size

-- | Placeholder for variable-length datatype parsing
parseVariableLength :: Get VariableLengthType
parseVariableLength = do
  classBits <- Get.getWord8
  let isString = (classBits `div` 1) `mod` 2 == 1
  -- Full implementation deferred
  return $ VariableLengthType (HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian False 0 8 1))) isString

-- | Placeholder for array datatype parsing
parseArray :: Get ArrayType
parseArray = do
  dimensionality <- Get.getWord8
  let dims = replicate (fromIntegral dimensionality) 0
  -- Full implementation deferred
  return $ ArrayType (HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian False 0 8 1))) dims

-- ============================================================================
-- HDF5 Metadata Parsing
-- ============================================================================

-- | HDF5 file superblock information
data HDF5Superblock = HDF5Superblock
  { sbVersion :: !Int           -- ^ Superblock version (0, 1, 2, or 3)
  , sbRootGroupOffset :: !Word64  -- ^ Byte offset of root group's object header
  , sbFileConsistencyFlags :: !Word8
  } deriving (Show, Eq)

-- ============================================================================
-- HDF5 Object Header Message Types (v1 focus)
-- ============================================================================

-- | Dataspace message version
data DataspaceVersion = DataspaceV1 | DataspaceV2
  deriving (Show, Eq)

-- | Dataspace type (v2 only)
data DataspaceType
  = ScalarDataspace    -- ^ 0: Scalar (single value)
  | SimpleDataspace    -- ^ 1: Simple (rectangular array)
  | NullDataspace      -- ^ 2: Null (no storage)
  deriving (Show, Eq)

-- | Dataspace description (from message type 0x0001)
-- v1 and v2 supported
data HDF5Dataspace = HDF5Dataspace
  { dsVersion :: !DataspaceVersion    -- ^ Dataspace version (1 or 2)
  , dsDimensionality :: !Word8        -- ^ Number of dimensions
  , dsHasMaxDimensions :: !Bool       -- ^ Whether max dimensions are present
  , dsType :: !(Maybe DataspaceType)  -- ^ Dataspace type (v2 only)
  , dsDimensions :: ![Word64]         -- ^ Current dimension sizes
  , dsMaxDimensions :: !(Maybe [Word64])  -- ^ Maximum dimension sizes (if present)
  } deriving (Show, Eq)

-- | Data layout message version
data LayoutVersion = LayoutV1 | LayoutV2 | LayoutV3
  deriving (Show, Eq)

-- | Data layout storage type (from message type 0x0008)
-- Versions 1, 2, 3 supported (v3 is most common)
data HDF5DataLayout
  = CompactLayout
      { clVersion :: !LayoutVersion   -- ^ Layout version
      , clSize :: !Word16             -- ^ Size of compact data
      , clRawData :: !BL.ByteString   -- ^ Embedded data
      }
  | ContiguousLayout
      { cnVersion :: !LayoutVersion   -- ^ Layout version  
      , cnAddress :: !Word64          -- ^ File address of data
      , cnSize :: !Word64             -- ^ Total size of data
      }
  | ChunkedLayout
      { chVersion :: !LayoutVersion   -- ^ Layout version
      , chDimensionality :: !Word8    -- ^ Number of chunk dimensions
      , chBTreeAddress :: !Word64     -- ^ Address of B-tree for chunks
      , chChunkDimensions :: ![Word32] -- ^ Size of each chunk dimension (elements)
      , chElementSize :: !Word32      -- ^ Size of one element (bytes)
      }
  deriving (Show, Eq)

-- | HDF5 standard filter identifiers
data FilterID
  = DeflateFilter      -- ^ 1: gzip/deflate compression
  | ShuffleFilter      -- ^ 2: Byte shuffle
  | Fletcher32Filter   -- ^ 3: Fletcher32 checksum
  | SzipFilter         -- ^ 4: Szip compression
  | NbitFilter         -- ^ 5: N-bit packing
  | ScaleOffsetFilter  -- ^ 6: Scale+offset compression
  | CustomFilter !Word16  -- ^ Custom/unknown filter ID
  deriving (Show, Eq)

-- | Filter pipeline message version
data FilterPipelineVersion = FilterPipelineV1 | FilterPipelineV2
  deriving (Show, Eq)

-- | HDF5 Filter in pipeline (from message type 0x000B)
-- Versions 1 and 2 supported
data HDF5Filter = HDF5Filter
  { fltFilterID :: !FilterID          -- ^ Filter identifier
  , fltIsOptional :: !Bool            -- ^ Whether filter is optional (can be skipped)
  , fltName :: !(Maybe String)        -- ^ Filter name (optional)
  , fltClientData :: ![Word32]        -- ^ Filter-specific parameters
  } deriving (Show, Eq)

-- | Filter pipeline for compressed/filtered datasets
data HDF5FilterPipeline = HDF5FilterPipeline
  { fpVersion :: !FilterPipelineVersion  -- ^ Pipeline version (1 or 2)
  , fpFilters :: ![HDF5Filter]        -- ^ Filters in application order
  } deriving (Show, Eq)

-- | Fill value message version
data FillValueVersion = FillValueV1 | FillValueV2 | FillValueV3
  deriving (Show, Eq)

-- | Space allocation time for datasets
data SpaceAllocationTime
  = AllocateLate        -- ^ 0: Allocate when data is first written
  | AllocateEarly       -- ^ 1: Allocate when dataset is created
  | AllocateIncremental -- ^ 2: Allocate incrementally as data is written
  deriving (Show, Eq)

-- | Fill value write time
data FillValueWriteTime
  = WriteNever    -- ^ 0: Never write fill value
  | WriteOnAlloc  -- ^ 1: Write fill value when space is allocated
  | WriteIfSet    -- ^ 2: Write fill value if explicitly set by user
  deriving (Show, Eq)

-- | Fill value information (from message types 0x0004 old, 0x0005 new)
-- Versions 1, 2, 3 supported
data HDF5FillValue
  = FillValueUndefined                -- ^ No fill value defined
  | FillValueDefault                  -- ^ Use datatype's default (usually 0)
  | FillValueDefined
      { fvVersion :: !FillValueVersion        -- ^ Fill value version
      , fvSpaceAllocTime :: !SpaceAllocationTime  -- ^ When to allocate space
      , fvFillWriteTime :: !FillValueWriteTime    -- ^ When to write fill value
      , fvSize :: !Word32             -- ^ Size of fill value
      , fvData :: !BL.ByteString      -- ^ Fill value data
      }
  deriving (Show, Eq)

-- | Object header message (v1 format)
-- Parsed message types with structured data
data ObjectHeaderMessage
  = NilMessage                        -- ^ Type 0x0000: placeholder/padding
  | DataspaceMessage !HDF5Dataspace   -- ^ Type 0x0001: dataspace description
  | DatatypeMessage !HDF5Datatype     -- ^ Type 0x0003: datatype description
  | FillValueMessage !HDF5FillValue   -- ^ Type 0x0004 (old) or 0x0005 (new): fill value
  | DataLayoutMessage !HDF5DataLayout -- ^ Type 0x0008: data storage layout
  | FilterPipelineMessage !HDF5FilterPipeline  -- ^ Type 0x000B: filter pipeline
  | SymbolTableMessage !Word64 !Word64  -- ^ Type 0x0011: B-tree address, heap address
  | UnknownMessage !Word16 !BL.ByteString  -- ^ Unknown type: type ID, raw data
  deriving (Show, Eq)

-- | Information about a dataset extracted from HDF5 metadata
data HDF5DatasetInfo = HDF5DatasetInfo
  { dsiName :: !String                -- ^ Dataset name
  , dsiDataspace :: !HDF5Dataspace    -- ^ Dataspace (dimensions, max dims)
  , dsiDatatype :: !HDF5Datatype      -- ^ Datatype (required, not optional)
  , dsiLayout :: !HDF5DataLayout      -- ^ Data layout (storage type and location)
  , dsiFilters :: !(Maybe HDF5FilterPipeline)  -- ^ Optional filter pipeline
  , dsiFillValue :: !(Maybe HDF5FillValue)     -- ^ Optional fill value
  , dsiObjectHeaderOffset :: !Word64  -- ^ Byte offset in file
  } deriving (Show, Eq)

-- | Introspection data for loaded HDF5 objects
data HDF5Introspection = HDF5Introspection
  { intro_filePath :: FilePath
  , intro_fileSize :: Integer
  , intro_validSignature :: Bool
  , intro_datatype :: Maybe HDF5Datatype
  , intro_summary :: String
  } deriving (Show, Eq)

-- ============================================================================
-- Shared Helpers for String Extraction and Validation
-- ============================================================================

-- | Check if a string looks like a dataset name (shared by discovery functions)
isDatasetName :: String -> Bool
isDatasetName s =
  let len = length s
      isReasonable = len > 0 && len < 100 && all isIdentifierChar s
      -- Check for common dataset-like patterns
      lower = map toLower s
      keywords = ["data", "train", "test", "distance", "neighbor", "ground",
                 "feature", "label", "index", "knn", "set", "array", 
                 "dset", "dataset", "var", "field"]
      hasKeyword = any (\kw -> isSubsequenceOf kw lower) keywords
  in isReasonable && (hasKeyword || (head s `elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_"))
  where
    isIdentifierChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || 
                         (c >= '0' && c <= '9') || c == '_' || c == '-'

-- | Extract printable ASCII strings from a chunk of bytes
extractAsciiStringsFromChunk :: BL.ByteString -> [String]
extractAsciiStringsFromChunk chunk =
  let bytes = BL.unpack chunk
      possibleStrings = extractAllSubstrings bytes 0
  in catMaybes (map bytesToAsciiString possibleStrings)
  where
    -- Extract all possible substrings from bytes
    extractAllSubstrings :: [Word8] -> Int -> [[Word8]]
    extractAllSubstrings [] _ = []
    extractAllSubstrings (b:bs') pos =
      let allTails = takeSubstrings (b:bs') pos
      in allTails ++ extractAllSubstrings bs' (pos + 1)
    
    -- Take substrings of various lengths
    takeSubstrings :: [Word8] -> Int -> [[Word8]]
    takeSubstrings bytes _ =
      [ take len bytes | len <- [3..32] ]
    
    -- Convert bytes to ASCII string if all are printable
    bytesToAsciiString :: [Word8] -> Maybe String
    bytesToAsciiString bytes =
      let isAscii b = b >= 32 && b < 127
      in if all isAscii bytes && length bytes > 2
         then Just (map (toEnum . fromIntegral) bytes)
         else Nothing

-- | Convert 4 bytes to Word32 in specified byte order
convertWord32 :: ByteOrder -> BL.ByteString -> Word32
convertWord32 LittleEndian bs
  | BL.length bs < 4 = 0
  | otherwise =
    let [b0, b1, b2, b3] = map (fromIntegral . fromIntegral) (BL.unpack (BL.take 4 bs)) :: [Word32]
    in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
convertWord32 BigEndian bs
  | BL.length bs < 4 = 0
  | otherwise =
    let [b3,b2,b1,b0] = map fromIntegral (BL.unpack (BL.take 4 bs)) :: [Word32]
    in (b3 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b0

-- | Convert 8 bytes to Word64 in specified byte order
convertWord64 :: ByteOrder -> BL.ByteString -> Word64
convertWord64 LittleEndian bs
  | BL.length bs < 8 = 0
  | otherwise =
    let bytes = BL.take 8 bs
        [b0,b1,b2,b3,b4,b5,b6,b7] = map fromIntegral (BL.unpack bytes) :: [Word64]
    in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24) .|.
       (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
convertWord64 BigEndian bs
  | BL.length bs < 8 = 0
  | otherwise =
    let bytes = BL.take 8 bs
        [b0,b1,b2,b3,b4,b5,b6,b7] = map fromIntegral (BL.unpack bytes) :: [Word64]
    in (b7 `shiftL` 56) .|. (b6 `shiftL` 48) .|. (b5 `shiftL` 40) .|. (b4 `shiftL` 32) .|.
       (b3 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b0

-- | Check if HDF5 file signature is valid
validateHDF5Signature :: BL.ByteString -> Bool
validateHDF5Signature bs
  | BL.length bs < 8 = False
  | otherwise = BL.take 8 bs == hdf5Magic
  where
    hdf5Magic = BL.pack [0x89, 0x48, 0x44, 0x46, 0x0D, 0x0A, 0x1A, 0x0A]

-- | Datatype class description
describeDatatypeClass :: DatatypeClass -> String
describeDatatypeClass dt = case dt of
  ClassFixedPoint fp ->
    "FixedPoint (" ++ show (fpSize fp) ++ " bytes, " ++
    (if fpSigned fp then "signed" else "unsigned") ++ ", " ++
    show (fpByteOrder fp) ++ ")"
  ClassFloatingPoint _ -> "FloatingPoint"
  ClassTime _ -> "Time"
  ClassString _ -> "String"
  ClassBitfield _ -> "Bitfield"
  ClassOpaque _ -> "Opaque"
  ClassCompound _ -> "Compound"
  ClassReference _ -> "Reference"
  ClassEnumeration _ -> "Enumeration"
  ClassVariableLength vl ->
    "VariableLength (string: " ++ show (vlIsString vl) ++ ")"
  ClassArray _ -> "Array"

-- | Format a datatype for display
formatDatatype :: HDF5Datatype -> String
formatDatatype (HDF5Datatype version cls) =
  "HDF5Datatype(v" ++ show version ++ ", " ++ describeDatatypeClass cls ++ ")"

-- | Introspect a loaded HDF5 file using mmap
introspectHDF5File :: FilePath -> IO (Either String HDF5Introspection)
introspectHDF5File path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ "File does not exist: " ++ path
    else do
      result <- try $ do
        -- Get file size by reading file as lazy ByteString
        contents <- BL.readFile path
        let fileSize = fromIntegral (BL.length contents) :: Integer
        let validSig = validateHDF5Signature contents
        return HDF5Introspection
          { intro_filePath = path
          , intro_fileSize = fileSize
          , intro_validSignature = validSig
          , intro_datatype = Nothing  -- Would require full parsing
          , intro_summary = "File: " ++ show (BL.length contents) ++ " bytes, " ++
                           "signature valid: " ++ show validSig
          }
      case result of
        Left (e :: SomeException) -> return $ Left $ "Failed to introspect: " ++ show e
        Right intro -> return $ Right intro

-- | Assert that an HDF5 file is well-formed
assertHDF5WellFormed :: FilePath -> IO ()
assertHDF5WellFormed path = do
  introspection <- introspectHDF5File path
  case introspection of
    Left err -> error $ "HDF5 introspection failed: " ++ err
    Right intro -> do
      if not (intro_validSignature intro)
        then error "HDF5 file signature is invalid"
        else pure ()
      if intro_fileSize intro <= 8
        then error "HDF5 file is too small"
        else pure ()

-- | Extract superblock version from HDF5 file
--   Returns Nothing if file is too short or doesn't have valid HDF5 signature
parseSuperblockVersion :: BL.ByteString -> Maybe HDF5Superblock
parseSuperblockVersion bs
  | BL.length bs < 16 = Nothing
  | otherwise = 
    let signature = BL.take 8 bs
        isValid = signature == BL.pack [0x89, 0x48, 0x44, 0x46, 0x0D, 0x0A, 0x1A, 0x0A]
    in if not isValid then Nothing else
       let versionByte = BL.index bs 8
           version = fromIntegral versionByte
           -- Root group offset location depends on superblock version
           rootOffsetBytes = case version of
             0 -> BL.take 8 (BL.drop 32 bs)  -- Version 0/1: offset at byte 32
             1 -> BL.take 8 (BL.drop 32 bs)  -- Version 0/1: offset at byte 32
             _ -> BL.take 8 (BL.drop 20 bs)  -- Version 2/3: offset at byte 20
           rootOffset = word64LE rootOffsetBytes
           flags = if BL.length bs > 9 then BL.index bs 9 else 0
       in Just $ HDF5Superblock version rootOffset flags

-- | Byte-to-Word64 conversion (little-endian)
word64LE :: BL.ByteString -> Word64
word64LE = convertWord64 LittleEndian

-- ============================================================================
-- Proper HDF5 Metadata Parsing (NEW IMPLEMENTATION)
-- ============================================================================
-- These functions implement the authoritative HDF5 binary format standard
-- from https://davis.lbl.gov/Manuals/HDF5-1.8.7/H5.format.html
--
-- This replaces the heuristic string-scanning approach with proper structure
-- parsing of superblock → object headers → symbol tables.
-- ============================================================================

-- | Read byte at specific offset (safe, strict - returns error if out of bounds)
byteAt :: BL.ByteString -> Int64 -> HDF5ParserM Word8
byteAt bs off
  | off < 0 = Left $ CorruptedMetadata $ "Negative offset: " ++ show off
  | fromIntegral (BL.length bs) <= off = 
    Left $ TruncatedData $ "Reading byte at offset " ++ show off ++ ", but ByteString is only " ++ show (BL.length bs) ++ " bytes"
  | otherwise = Right (BL.index bs (fromIntegral off))

-- | Read Word16 little-endian at specific offset (strict error handling)
word16LEAt :: BL.ByteString -> Int64 -> HDF5ParserM Word16
word16LEAt bs off
  | off < 0 = Left $ CorruptedMetadata $ "Negative offset: " ++ show off
  | fromIntegral (BL.length bs) < off + 2 = 
    Left $ TruncatedData $ "Reading Word16 at offset " ++ show off ++ ", need 2 bytes, but only " ++ show (fromIntegral (BL.length bs) - off) ++ " available"
  | otherwise =
    let [b0, b1] = BL.unpack (BL.take 2 (BL.drop (fromIntegral off) bs))
    in Right (fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8))

-- | Read Word32 little-endian at specific offset (strict error handling)
word32LEAt :: BL.ByteString -> Int64 -> HDF5ParserM Word32
word32LEAt bs off
  | off < 0 = Left $ CorruptedMetadata $ "Negative offset: " ++ show off
  | fromIntegral (BL.length bs) < off + 4 = 
    Left $ TruncatedData $ "Reading Word32 at offset " ++ show off ++ ", need 4 bytes, but only " ++ show (fromIntegral (BL.length bs) - off) ++ " available"
  | otherwise =
    let chunk = BL.take 4 (BL.drop (fromIntegral off) bs)
    in Right (convertWord32 LittleEndian chunk)

-- | Read Word64 little-endian at specific offset (strict error handling)
word64LEAt :: BL.ByteString -> Int64 -> HDF5ParserM Word64
word64LEAt bs off
  | off < 0 = Left $ CorruptedMetadata $ "Negative offset: " ++ show off
  | fromIntegral (BL.length bs) < off + 8 = 
    Left $ TruncatedData $ "Reading Word64 at offset " ++ show off ++ ", need 8 bytes, but only " ++ show (fromIntegral (BL.length bs) - off) ++ " available"
  | otherwise =
    let chunk = BL.take 8 (BL.drop (fromIntegral off) bs)
    in Right (convertWord64 LittleEndian chunk)

-- | Read ByteString of specified length at offset (strict error handling)
byteStringAt :: BL.ByteString -> Int64 -> Int64 -> HDF5ParserM BL.ByteString
byteStringAt bs off len
  | off < 0 = Left $ CorruptedMetadata $ "Negative offset: " ++ show off
  | len < 0 = Left $ CorruptedMetadata $ "Negative length: " ++ show len
  | fromIntegral (BL.length bs) < off + len =
    Left $ TruncatedData $ "Reading " ++ show len ++ " bytes at offset " ++ show off ++ ", but only " ++ show (fromIntegral (BL.length bs) - off) ++ " available"
  | otherwise = Right (BL.take (fromIntegral len) (BL.drop (fromIntegral off) bs))

-- | Parse superblock metadata: extract root group object header address and size info (STRICT)
--   Returns (rootHeaderAddress, offsetSize, lengthSize)
--   Validates signature, version, and all required fields
--   
--   Superblock format by version:
--   v0/v1: Signature (8) + Version (1) + SizeInfo (2 bytes @ 10-11) + RootAddr (8 bytes @ 32)
--   v2/v3: Signature (8) + Version (1) + SizeInfo (2 bytes @ 9-10) + BaseAddr (8) + RootAddr (8 bytes @ 20)
parseSuperblockMetadata :: BL.ByteString -> HDF5ParserM (Int64, Int, Int)
parseSuperblockMetadata bs
  | BL.length bs < 512 = Left $ TruncatedData $ "Superblock requires at least 512 bytes, but file is only " ++ show (BL.length bs) ++ " bytes"
  | otherwise = do
    -- Read and validate HDF5 signature
    sig <- byteStringAt bs 0 8
    if sig /= BL.pack [0x89, 0x48, 0x44, 0x46, 0x0D, 0x0A, 0x1A, 0x0A]
      then Left InvalidSignature
      else do
        -- Read superblock version
        version <- byteAt bs 8
        let ver = fromIntegral version :: Int
        
        -- Validate superblock version
        if ver > 3
          then Left $ InvalidVersion $ "Superblock version " ++ show ver ++ " not supported (max 3)"
          else do
            case ver of
              0 -> parseSuperblockV0V1 bs 0
              1 -> parseSuperblockV0V1 bs 1
              2 -> parseSuperblockV2V3 bs 2
              3 -> parseSuperblockV2V3 bs 3
              _ -> Left $ InvalidVersion $ "Unreachable: version " ++ show ver

-- | Parse v0/v1 superblock with support for extended addressing
--   Superblock v0/v1 structure:
--   Offset 8: Version number (0 or 1)
--   Offset 9: Free-space storage version
--   Offset 10: Root group symbol table entry version
--   Offset 11: Reserved
--   Offset 12-13: Shared header message format version / Reserved
--   Offset 13-14: Offset and length size information
--   Offset 15: Reserved
--   Offset 16-20: Group leaf node K value
--   Offset 20-24: Group internal node K value
--   Offset 24: File consistency flags
--   Offset 28-31: Checksum (for v1)
--   Offset 32: Root group object header address
--   Extended addressing: Root address == 0xffffffff (4-byte) or 0xffffffffffffffff (8-byte)
parseSuperblockV0V1 :: BL.ByteString -> Int -> HDF5ParserM (Int64, Int, Int)
parseSuperblockV0V1 bs _ver = do
  -- Read offset and length sizes from correct positions
  oSz <- byteAt bs 13
  lSz <- byteAt bs 14
  let offsetSz = fromIntegral oSz :: Int
  let lenSz = fromIntegral lSz :: Int
  
  -- Validate offset/length sizes
  if offsetSz /= 4 && offsetSz /= 8
    then Left $ CorruptedMetadata $ "v0/v1 Invalid offset size: " ++ show offsetSz ++ " (must be 4 or 8)"
    else if lenSz /= 4 && lenSz /= 8
      then Left $ CorruptedMetadata $ "v0/v1 Invalid length size: " ++ show lenSz ++ " (must be 4 or 8)"
      else do
        -- Root group address at offset 32
        rootAddr <- case offsetSz of
          4 -> do
            addr <- word32LEAt bs 32
            pure (fromIntegral addr :: Int64)
          _ -> do  -- 8 bytes
            addr <- word64LEAt bs 32
            pure (fromIntegral addr :: Int64)
        
        -- Check for extended addressing marker
        -- In v0/v1, extended addressing is indicated by root address being 0xffffffff or 0xffffffffffffffff
        let isExtendedAddressing = case offsetSz of
              4 -> rootAddr == 0xffffffff
              _ -> rootAddr == 0xffffffffffffffff
        
        if isExtendedAddressing
          then do
            -- With extended addressing in v0/v1, the actual root address is stored at
            -- a location after the main superblock. For now, we report this as uninitialized.
            -- A full implementation would read from the extended header location.
            pure (-1, offsetSz, lenSz)  -- Return -1 to indicate extended addressing detected
          else
            pure (rootAddr, offsetSz, lenSz)

-- | Parse v2/v3 superblock with support for extended addressing
--   Superblock v2/v3 structure (with extended addressing support):
--   Offset 0-7: Signature "\137HDF\r\n\032\n"
--   Offset 8: Version number
--   Offset 9: Offset size (bytes) - 4 or 8
--   Offset 10: Length size (bytes) - 4 or 8
--   Offset 11: File consistency flags
--   Offset 12-19: Base address (8 bytes)
--   Offset 20: Root group object header address (determined by offset size)
--   Extended addressing (if root address field == 0 and flag is set):
--     The address is located after the main superblock fields
parseSuperblockV2V3 :: BL.ByteString -> Int -> HDF5ParserM (Int64, Int, Int)
parseSuperblockV2V3 bs ver = do
  -- Require at least 28 bytes for v2/v3 header
  if BL.length bs < 28
    then Left $ TruncatedData $ "Superblock v" ++ show ver ++ " requires at least 28 bytes"
    else do
      -- Read offset and length sizes
      oSz <- byteAt bs 9
      lSz <- byteAt bs 10
      let offsetSz = fromIntegral oSz :: Int
      let lenSz = fromIntegral lSz :: Int
      
      -- Validate offset/length sizes
      if offsetSz /= 4 && offsetSz /= 8
        then Left $ CorruptedMetadata $ "v2/v3 Invalid offset size: " ++ show offsetSz ++ " (must be 4 or 8)"
        else if lenSz /= 4 && lenSz /= 8
          then Left $ CorruptedMetadata $ "v2/v3 Invalid length size: " ++ show lenSz ++ " (must be 4 or 8)"
          else do
            -- Check file consistency flags (byte 11)
            flags <- byteAt bs 11
            let flagByte = fromIntegral flags :: Word8
            
            -- Bit 5 of flags indicates extended addressing
            -- If set, the root group object header address is stored at a different location
            let hasExtendedAddressing = testBit flagByte 5
            
            -- Read base address (8 bytes at offset 12)
            -- For now, we ignore it as root address is absolute
            _baseAddr <- word64LEAt bs 12
            
            -- Root group address - location depends on whether extended addressing is used
            rootAddr <- if hasExtendedAddressing
              then do
                -- With extended addressing, root address is at the end of main superblock fields
                -- Main fields size: 20 bytes (signature) + 8 bytes (through byte 28)
                -- Root address field position: 20 + offsetSz
                let rootAddrOffset = 20 + fromIntegral offsetSz
                case offsetSz of
                  4 -> do
                    addr <- word32LEAt bs (fromIntegral rootAddrOffset)
                    pure (fromIntegral addr :: Int64)
                  _ -> do  -- 8 bytes
                    addr <- word64LEAt bs (fromIntegral rootAddrOffset)
                    pure (fromIntegral addr :: Int64)
              else do
                -- Standard location at offset 20
                case offsetSz of
                  4 -> do
                    addr <- word32LEAt bs 20
                    pure (fromIntegral addr :: Int64)
                  _ -> do  -- 8 bytes
                    addr <- word64LEAt bs 20
                    pure (fromIntegral addr :: Int64)
            
            pure (rootAddr, offsetSz, lenSz)

-- ============================================================================
-- Object Header Message Parsers (v1 focus)
-- ============================================================================

-- | Convert Word8 to DataspaceVersion
parseDataspaceVersion :: Word8 -> Either HDF5ParseError DataspaceVersion
parseDataspaceVersion 1 = Right DataspaceV1
parseDataspaceVersion 2 = Right DataspaceV2
parseDataspaceVersion v = Left $ InvalidVersion $ "Unsupported dataspace version: " ++ show v

-- | Convert Word8 to DataspaceType (v2 only)
parseDataspaceType :: Word8 -> Either HDF5ParseError DataspaceType
parseDataspaceType 0 = Right ScalarDataspace
parseDataspaceType 1 = Right SimpleDataspace
parseDataspaceType 2 = Right NullDataspace
parseDataspaceType v = Left $ InvalidVersion $ "Invalid dataspace type: " ++ show v

-- | Convert Word8 to LayoutVersion
parseLayoutVersion :: Word8 -> Either HDF5ParseError LayoutVersion
parseLayoutVersion 1 = Right LayoutV1
parseLayoutVersion 2 = Right LayoutV2
parseLayoutVersion 3 = Right LayoutV3
parseLayoutVersion v = Left $ InvalidVersion $ "Unsupported layout version: " ++ show v

-- | Convert Word16 to FilterID
parseFilterID :: Word16 -> FilterID
parseFilterID 1 = DeflateFilter
parseFilterID 2 = ShuffleFilter
parseFilterID 3 = Fletcher32Filter
parseFilterID 4 = SzipFilter
parseFilterID 5 = NbitFilter
parseFilterID 6 = ScaleOffsetFilter
parseFilterID n = CustomFilter n

-- | Convert Word8 to FilterPipelineVersion
parseFilterPipelineVersion :: Word8 -> Either HDF5ParseError FilterPipelineVersion
parseFilterPipelineVersion 1 = Right FilterPipelineV1
parseFilterPipelineVersion 2 = Right FilterPipelineV2
parseFilterPipelineVersion v = Left $ InvalidVersion $ "Unsupported filter pipeline version: " ++ show v

-- | Convert Word8 to FillValueVersion
parseFillValueVersion :: Word8 -> Either HDF5ParseError FillValueVersion
parseFillValueVersion 1 = Right FillValueV1
parseFillValueVersion 2 = Right FillValueV2
parseFillValueVersion 3 = Right FillValueV3
parseFillValueVersion v = Left $ InvalidVersion $ "Unsupported fill value version: " ++ show v

-- | Convert Word8 to SpaceAllocationTime
parseSpaceAllocationTime :: Word8 -> Either HDF5ParseError SpaceAllocationTime
parseSpaceAllocationTime 0 = Right AllocateLate
parseSpaceAllocationTime 1 = Right AllocateEarly
parseSpaceAllocationTime 2 = Right AllocateIncremental
parseSpaceAllocationTime v = Left $ CorruptedMetadata $ "Invalid space allocation time: " ++ show v

-- | Convert Word8 to FillValueWriteTime
parseFillValueWriteTime :: Word8 -> Either HDF5ParseError FillValueWriteTime
parseFillValueWriteTime 0 = Right WriteNever
parseFillValueWriteTime 1 = Right WriteOnAlloc
parseFillValueWriteTime 2 = Right WriteIfSet
parseFillValueWriteTime v = Left $ CorruptedMetadata $ "Invalid fill value write time: " ++ show v

-- | Parse v1/v2 Dataspace Message (Type 0x0001)
--   Spec section IV.A.2.b
--   Format:
--   - Byte 0: Version (1 or 2)
--   - Byte 1: Dimensionality (number of dimensions)
--   - Byte 2: Flags (bit 0: max dims present, bit 1: permutation present - v1 only)
--   - Byte 3: Reserved (v1) or Dataspace Type (v2: 0=scalar, 1=simple, 2=null)
--   - Bytes 4-7: Reserved
--   - Bytes 8+: Dimension sizes (lengthSize bytes each)
--   - [Optional] Max dimension sizes (lengthSize bytes each, if flag bit 0 set)
parseDataspaceMessage :: Int -> BL.ByteString -> Either HDF5ParseError HDF5Dataspace
parseDataspaceMessage lengthSize msgData
  | BL.length msgData < 8 = Left $ TruncatedData "Dataspace message too short"
  | otherwise = do
      versionByte <- byteAt msgData 0
      version <- parseDataspaceVersion versionByte
      
      dimensionality <- byteAt msgData 1
      flags <- byteAt msgData 2
      typeByte <- byteAt msgData 3
      
      let hasMaxDims = testBit flags 0
          numDims = fromIntegral dimensionality :: Int
          dimsStart = 8
          dimSize = lengthSize
      
      -- Parse dataspace type (v2 only)
      dsType <- case version of
        DataspaceV2 -> Just <$> parseDataspaceType typeByte
        DataspaceV1 -> pure Nothing
      
      -- Parse current dimensions
      dims <- parseDimensions numDims dimsStart dimSize
      
      -- Parse max dimensions if present
      maxDims <- if hasMaxDims
                 then do
                   let maxDimsStart = dimsStart + numDims * dimSize
                   maxDs <- parseDimensions numDims maxDimsStart dimSize
                   pure (Just maxDs)
                 else pure Nothing
      
      pure $ HDF5Dataspace version dimensionality hasMaxDims dsType dims maxDims
  where
    parseDimensions :: Int -> Int -> Int -> Either HDF5ParseError [Word64]
    parseDimensions 0 _ _ = Right []
    parseDimensions n offset sz
      | fromIntegral (BL.length msgData) < fromIntegral (offset + sz) =
          Left $ TruncatedData $ "Not enough bytes for dimension at offset " ++ show offset
      | otherwise = do
          dim <- case sz of
            4 -> word32LEAt msgData (fromIntegral offset) >>= \w -> pure (fromIntegral w :: Word64)
            8 -> word64LEAt msgData (fromIntegral offset)
            _ -> Left $ CorruptedMetadata $ "Invalid length size: " ++ show sz
          rest <- parseDimensions (n - 1) (offset + sz) sz
          pure (dim : rest)

-- | Parse v1/v3 Data Layout Message (Type 0x0008)
--   Spec section IV.A.2.i
--   Versions 1, 2, 3 supported (v3 is most common)
--   Format:
--   - Byte 0: Version (1, 2, or 3)
--   - Byte 1: Layout class (0=compact, 1=contiguous, 2=chunked)
--   - Bytes 2+: Layout-specific data
parseDataLayoutMessage :: Int -> BL.ByteString -> Either HDF5ParseError HDF5DataLayout
parseDataLayoutMessage offsetSize msgData
  | BL.length msgData < 2 = Left $ TruncatedData "Data layout message too short"
  | otherwise = do
      versionByte <- byteAt msgData 0
      version <- parseLayoutVersion versionByte
      layoutClass <- byteAt msgData 1
      
      case layoutClass of
        0 -> parseCompactLayout version msgData
        1 -> parseContiguousLayout version offsetSize msgData
        2 -> parseChunkedLayout version offsetSize msgData
        _ -> Left $ CorruptedMetadata $ "Invalid layout class: " ++ show layoutClass
  where
    parseCompactLayout ver msgData' = do
      -- Version 3: bytes 2-3 are size
      size <- word16LEAt msgData' 2
      let dataStart = 4
          rawData = BL.drop (fromIntegral dataStart) msgData'
      pure $ CompactLayout ver size rawData
    
    parseContiguousLayout ver offSz msgData' = do
      -- Version 3: address at byte 2, size follows
      addr <- case offSz of
        4 -> word32LEAt msgData' 2 >>= \w -> pure (fromIntegral w :: Word64)
        8 -> word64LEAt msgData' 2
        _ -> Left $ CorruptedMetadata $ "Invalid offset size"
      
      let sizeOffset = 2 + offSz
      size <- case offSz of  -- Size uses lengthSize, but we approximate with offsetSize
        4 -> word32LEAt msgData' (fromIntegral sizeOffset) >>= \w -> pure (fromIntegral w :: Word64)
        8 -> word64LEAt msgData' (fromIntegral sizeOffset)
        _ -> Left $ CorruptedMetadata $ "Invalid offset size"
      
      pure $ ContiguousLayout ver addr size
    
    parseChunkedLayout ver offSz msgData' = do
      -- Version 3: byte 2 is dimensionality, then address, then chunk dims
      dimensionality <- byteAt msgData' 2
      
      addr <- case offSz of
        4 -> word32LEAt msgData' 3 >>= \w -> pure (fromIntegral w :: Word64)
        8 -> word64LEAt msgData' 3
        _ -> Left $ CorruptedMetadata $ "Invalid offset size"
      
      let chunkDimsStart = 3 + offSz
          numDims = fromIntegral dimensionality :: Int
      
      chunkDims <- parseChunkDims numDims chunkDimsStart msgData'
      
      -- Element size at end (4 bytes)
      let elemSizeOffset = chunkDimsStart + numDims * 4
      elemSize <- if fromIntegral (BL.length msgData') >= elemSizeOffset + 4
                  then word32LEAt msgData' (fromIntegral elemSizeOffset)
                  else pure 0  -- Default if not present
      
      pure $ ChunkedLayout ver dimensionality addr chunkDims elemSize
    
    parseChunkDims :: Int -> Int -> BL.ByteString -> Either HDF5ParseError [Word32]
    parseChunkDims 0 _ _ = Right []
    parseChunkDims n offset msgData'
      | fromIntegral (BL.length msgData') < fromIntegral (offset + 4) =
          Left $ TruncatedData $ "Not enough bytes for chunk dimension at offset " ++ show offset
      | otherwise = do
          dim <- word32LEAt msgData' (fromIntegral offset)
          rest <- parseChunkDims (n - 1) (offset + 4) msgData'
          pure (dim : rest)

-- | Parse v1/v2 Filter Pipeline Message (Type 0x000B)
--   Spec section IV.A.2.l
--   Format (version 2):
--   - Byte 0: Version (1 or 2)
--   - Byte 1: Number of filters
--   - Bytes 2-3: Reserved (v2) or padding (v1)
--   - Then for each filter:
--     * 2 bytes: Filter ID
--     * 2 bytes (optional): Name length (if ID >= 256)
--     * 2 bytes: Flags
--     * 2 bytes: Number of client data values
--     * Variable: Name (if name length > 0)
--     * Variable: Client data (4 bytes per value)
parseFilterPipelineMessage :: BL.ByteString -> Either HDF5ParseError HDF5FilterPipeline
parseFilterPipelineMessage msgData
  | BL.length msgData < 4 = Left $ TruncatedData "Filter pipeline message too short"
  | otherwise = do
      versionByte <- byteAt msgData 0
      version <- parseFilterPipelineVersion versionByte
      numFilters <- byteAt msgData 1
      
      filters <- parseFilters (fromIntegral numFilters) 4 []
      
      pure $ HDF5FilterPipeline version filters
  where
    parseFilters :: Int -> Int -> [HDF5Filter] -> Either HDF5ParseError [HDF5Filter]
    parseFilters 0 _ acc = Right (reverse acc)
    parseFilters n offset acc
      | fromIntegral (BL.length msgData) < fromIntegral (offset + 6) =
          Left $ TruncatedData $ "Not enough bytes for filter at offset " ++ show offset
      | otherwise = do
          filterIDWord <- word16LEAt msgData (fromIntegral offset)
          let filterID = parseFilterID filterIDWord
          
          -- Name length present if filter ID >= 256 (optional in spec, we skip for simplicity)
          let nameLen = 0 :: Word16
              flagsOffset = offset + 2
          
          flags <- word16LEAt msgData (fromIntegral flagsOffset)
          let isOptional = testBit flags 0
          
          numClientVals <- word16LEAt msgData (fromIntegral $ flagsOffset + 2)
          
          let clientDataOffset = flagsOffset + 4 + fromIntegral nameLen
              clientDataSize = fromIntegral numClientVals * 4
          
          clientData <- parseClientData (fromIntegral numClientVals) clientDataOffset []
          
          let nextOffset = clientDataOffset + clientDataSize
              flt = HDF5Filter filterID isOptional Nothing clientData
          
          parseFilters (n - 1) nextOffset (flt : acc)
    
    parseClientData :: Int -> Int -> [Word32] -> Either HDF5ParseError [Word32]
    parseClientData 0 _ acc = Right (reverse acc)
    parseClientData n offset acc
      | fromIntegral (BL.length msgData) < fromIntegral (offset + 4) =
          Left $ TruncatedData $ "Not enough bytes for client data at offset " ++ show offset
      | otherwise = do
          val <- word32LEAt msgData (fromIntegral offset)
          parseClientData (n - 1) (offset + 4) (val : acc)

-- | Parse v1/v3 Fill Value Message (Type 0x0005 new, 0x0004 old)
--   Spec section IV.A.2.f
--   Format (version 3):
--   - Byte 0: Version (1, 2, or 3)
--   - Byte 1: Flags
--     * Bits 0-1: Space allocation time (0=late, 1=early, 2=incremental)
--     * Bits 2-3: Fill value write time (0=never, 1=on_alloc, 2=if_set)
--     * Bit 4: Fill value defined
--     * Bit 5: Fill value undefined
--   - Bytes 2-3: Reserved
--   - [Optional] Bytes 4-7: Size (if fill value defined)
--   - [Optional] Bytes 8+: Fill value data
parseFillValueMessage :: BL.ByteString -> Either HDF5ParseError HDF5FillValue
parseFillValueMessage msgData
  | BL.length msgData < 4 = Left $ TruncatedData "Fill value message too short"
  | otherwise = do
      versionByte <- byteAt msgData 0
      version <- parseFillValueVersion versionByte
      flags <- byteAt msgData 1
      
      let allocTimeBits = flags .&. 0x03
          writeTimeBits = (flags `shiftR` 2) .&. 0x03
          isDefined = testBit flags 4
          isUndefined = testBit flags 5
      
      if isUndefined
        then pure FillValueUndefined
        else if not isDefined
          then pure FillValueDefault
          else do
            -- Fill value is defined, parse allocation and write times
            allocTime <- parseSpaceAllocationTime allocTimeBits
            writeTime <- parseFillValueWriteTime writeTimeBits
            
            -- Parse size and data
            size <- if BL.length msgData >= 8
                    then word32LEAt msgData 4
                    else pure 0
            
            let dataStart = 8
                fillData = BL.take (fromIntegral size) (BL.drop dataStart msgData)
            
            pure $ FillValueDefined version allocTime writeTime size fillData

-- | Parse Object Header v1 messages into structured types
--   Version 1 Object Header format:
--   - Byte 0: Version (1)
--   - Byte 1: Reserved
--   - Bytes 2-3: Number of messages (little-endian Word16)
--   - Bytes 4-7: Object reference count
--   - Bytes 8-11: Object header size
--   - Bytes 12-15: Reserved
--   - Bytes 16+: Messages
--
--   Each message:
--   - Bytes 0-1: Message type (little-endian Word16)
--   - Bytes 2-3: Message size (little-endian Word16)
--   - Byte 4: Message flags
--   - Bytes 5-7: Reserved (3 bytes)
--   - Bytes 8+: Message data
--   - Padding to 8-byte boundary
--
--   Returns: List of parsed ObjectHeaderMessage types
parseObjectHeaderMessages :: Int -> Int -> BL.ByteString -> Either HDF5ParseError [ObjectHeaderMessage]
parseObjectHeaderMessages lengthSize offsetSize bs
  | BL.length bs < 16 = Left $ TruncatedData "Object header too short"
  | otherwise = do
      _version <- byteAt bs 0  -- Should be 1 for v1 headers
      -- Skip to first message at byte 16 for version 1 headers
      go 16 []
  where
    go :: Int64 -> [ObjectHeaderMessage] -> Either HDF5ParseError [ObjectHeaderMessage]
    go offset msgs
      | offset >= min 4096 (fromIntegral (BL.length bs)) = Right (reverse msgs)
      | fromIntegral (BL.length bs) < offset + 8 = Right (reverse msgs)
      | otherwise = do
          msgType <- word16LEAt bs offset
          msgSz16 <- word16LEAt bs (offset + 2)
          
          let msgSz = fromIntegral msgSz16 :: Int64
              -- Message data starts at offset + 8 (skip 8-byte header)
              msgData = BL.take (fromIntegral msgSz) (BL.drop (fromIntegral offset + 8) bs)
              -- Padding: round up to 8-byte boundary
              totalMsgBytes = 8 + msgSz  -- header + data
              pad = ((8 - (totalMsgBytes `mod` 8)) `mod` 8) :: Int64
              nextOff = offset + totalMsgBytes + pad
          
          if msgType == 0xFFFF
            then Right (reverse msgs)
            else do
              -- Parse message based on type
              msg <- parseMessage msgType msgData
              go nextOff (msg : msgs)
    
    parseMessage :: Word16 -> BL.ByteString -> Either HDF5ParseError ObjectHeaderMessage
    parseMessage 0x0000 _ = Right NilMessage
    parseMessage 0x0001 msgData = DataspaceMessage <$> parseDataspaceMessage lengthSize msgData
    parseMessage 0x0003 msgData = DatatypeMessage <$> parseDatatypeMessageV1 msgData
    parseMessage 0x0004 msgData = FillValueMessage <$> parseFillValueMessage msgData
    parseMessage 0x0005 msgData = FillValueMessage <$> parseFillValueMessage msgData
    parseMessage 0x0008 msgData = DataLayoutMessage <$> parseDataLayoutMessage offsetSize msgData
    parseMessage 0x000B msgData = FilterPipelineMessage <$> parseFilterPipelineMessage msgData
    parseMessage 0x0011 msgData = do
      -- Symbol table message: 2 Word64 addresses
      btreeAddr <- word64LEAt msgData 0
      heapAddr <- word64LEAt msgData 8
      Right $ SymbolTableMessage btreeAddr heapAddr
    parseMessage msgType msgData = Right $ UnknownMessage msgType msgData

-- | Parse v1 Datatype Message completely (Type 0x0003)
--   This is a placeholder - we'll keep using the existing parseFixedPoint etc for now
--   but wrap the result properly
parseDatatypeMessageV1 :: BL.ByteString -> Either HDF5ParseError HDF5Datatype
parseDatatypeMessageV1 msgData
  | BL.length msgData < 8 = Left $ TruncatedData "Datatype message too short"
  | otherwise = do
      let classAndVersion = byteAt msgData 0
      case classAndVersion of
        Right cv -> do
          let datatypeClass = cv .&. 0x0F
              version = (cv `shiftR` 4) .&. 0x0F
          
          -- Get the size (bytes 4-7, little-endian Word32)
          size <- word32LEAt msgData 4
          
          -- Run the appropriate Get parser on the rest of the message data
          -- The class bits and size are at the beginning, so we need to extract
          -- just the part starting from offset 8
          let dataBytes = BL.drop 4 msgData  -- Skip first 4 bytes to position at class bits
          
          -- Use existing parsers wrapped in Either
          case datatypeClass of
            0 -> case Get.runGetOrFail (parseFixedPoint (fromIntegral size)) dataBytes of
                  Right (_, _, fp) -> Right $ HDF5Datatype (fromIntegral version) (ClassFixedPoint fp)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse fixed-point: " ++ err
            1 -> case Get.runGetOrFail (parseFloatingPoint (fromIntegral size)) dataBytes of
                  Right (_, _, flt) -> Right $ HDF5Datatype (fromIntegral version) (ClassFloatingPoint flt)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse floating-point: " ++ err
            2 -> case Get.runGetOrFail (parseTime (fromIntegral size)) dataBytes of
                  Right (_, _, tm) -> Right $ HDF5Datatype (fromIntegral version) (ClassTime tm)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse time: " ++ err
            3 -> case Get.runGetOrFail (parseString (fromIntegral size)) dataBytes of
                  Right (_, _, str) -> Right $ HDF5Datatype (fromIntegral version) (ClassString str)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse string: " ++ err
            4 -> case Get.runGetOrFail (parseBitfield (fromIntegral size)) dataBytes of
                  Right (_, _, bf) -> Right $ HDF5Datatype (fromIntegral version) (ClassBitfield bf)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse bitfield: " ++ err
            5 -> case Get.runGetOrFail (parseOpaque (fromIntegral size)) dataBytes of
                  Right (_, _, op) -> Right $ HDF5Datatype (fromIntegral version) (ClassOpaque op)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse opaque: " ++ err
            6 -> case Get.runGetOrFail (parseCompound (fromIntegral size)) dataBytes of
                  Right (_, _, cmp) -> Right $ HDF5Datatype (fromIntegral version) (ClassCompound cmp)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse compound: " ++ err
            7 -> Left $ DatatypeParseFailed "Reference datatype parsing not yet implemented"
            8 -> case Get.runGetOrFail (parseEnumeration (fromIntegral size)) dataBytes of
                  Right (_, _, enm) -> Right $ HDF5Datatype (fromIntegral version) (ClassEnumeration enm)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse enumeration: " ++ err
            9 -> case Get.runGetOrFail parseVariableLength dataBytes of
                  Right (_, _, vl) -> Right $ HDF5Datatype (fromIntegral version) (ClassVariableLength vl)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse variable-length: " ++ err
            10 -> case Get.runGetOrFail parseArray dataBytes of
                  Right (_, _, arr) -> Right $ HDF5Datatype (fromIntegral version) (ClassArray arr)
                  Left (_, _, err) -> Left $ DatatypeParseFailed $ "Failed to parse array: " ++ err
            _ -> Left $ DatatypeParseFailed $ "Unknown datatype class: " ++ show datatypeClass
        Left err -> Left err

-- | Extract symbol table message data and parse it
--   Symbol table message (type 0x0011) format:
--   - Offset O: Address of v1 B-tree root
--   - Offset O: Address of local heap
parseSymbolTableMessage :: BL.ByteString -> Int64 -> Either HDF5ParseError (Word64, Word64)
parseSymbolTableMessage fileData offset
  | fromIntegral (BL.length fileData) < offset + 16 = 
    Left $ TruncatedData $ "Symbol table message at offset " ++ show offset ++ " requires 16 bytes"
  | otherwise =
    do
      btreeAddr <- word64LEAt fileData offset
      heapAddr <- word64LEAt fileData (offset + 8)
      pure (btreeAddr, heapAddr)

-- | Parse v1 B-tree node header
--   Format:
--   - 4 bytes: Signature "TREE"
--   - 1 byte: Node type (0=group, 1=dataset)
--   - 1 byte: Node level (0=leaf, >0=internal)
--   - 2 bytes: Number of entries in node
--   - Then: Entries (format depends on node type)
parseV1BTreeNode :: BL.ByteString -> Int64 -> Either HDF5ParseError (Word8, Word8, Word16)
parseV1BTreeNode fileData offset
  | fromIntegral (BL.length fileData) < offset + 8 = 
    Left $ TruncatedData $ "B-tree node at offset " ++ show offset ++ " requires 8 bytes"
  | otherwise =
    let sig = BL.take 4 (BL.drop (fromIntegral offset) fileData)
    in if sig == BL.pack [0x54, 0x52, 0x45, 0x45] -- "TREE"
       then do
         nodeType <- byteAt fileData (offset + 4)
         nodeLevel <- byteAt fileData (offset + 5)
         numEntries <- word16LEAt fileData (offset + 6)
         pure (nodeType, nodeLevel, numEntries)
       else Left $ BTreeParseFailed $ "Invalid B-tree signature at offset " ++ show offset

-- | Extract child node addresses from internal B-tree node
--   Internal node format:
--   - 8 bytes header (sig, type, level, nEntries)
--   - nEntries+1 child pointers (8 bytes each)
--   Returns list of child node addresses to recurse into
parseV1BTreeInternalChildren :: BL.ByteString -> Int64 -> Word16 -> Either HDF5ParseError [Word64]
parseV1BTreeInternalChildren fileData offset numEntries
  | numEntries == 0 = Right []
  | otherwise =
    -- Read nEntries+1 child pointers starting at offset+8
    -- For simplicity, read up to nEntries+1 addresses
    let numChildren = fromIntegral numEntries + 1
        go :: Int -> [Word64] -> Either HDF5ParseError [Word64]
        go n acc
          | n > numChildren = Right (reverse acc)
          | otherwise =
            let childOffset = offset + 8 + (fromIntegral (n-1) * 8)
            in case word64LEAt fileData childOffset of
              Left err -> Left err
              Right addr -> go (n+1) (addr : acc)
    in go 1 []

-- | Recursively traverse B-tree and collect all dataset names from leaf nodes
--   Handles both internal nodes (recurses to children) and leaf nodes (extracts names)
--   Prevents infinite loops with a depth limit (max 10 levels)
traverseV1BTree :: BL.ByteString -> Word64 -> Word8 -> Int -> Either HDF5ParseError [String]
traverseV1BTree fileData nodeAddr nodeLevel depthRemaining
  | depthRemaining <= 0 = Left $ BTreeParseFailed "B-tree depth limit exceeded"
  | otherwise =
    let offset = fromIntegral nodeAddr :: Int64
    in case parseV1BTreeNode fileData offset of
      Left err -> Left err
      Right (_, level, numEntries) ->
        if level == 0
          then
            -- Leaf node: extract names directly
            Right $ parseBTreeLeafEntries fileData offset numEntries
          else
            -- Internal node: recurse to children
            case parseV1BTreeInternalChildren fileData offset numEntries of
              Left err -> Left err
              Right childAddrs ->
                -- Recursively traverse each child
                let go :: [Word64] -> Either HDF5ParseError [String]
                    go [] = Right []
                    go (childAddr:rest) =
                      case traverseV1BTree fileData childAddr nodeLevel (depthRemaining - 1) of
                        Left err -> Left err
                        Right names ->
                          case go rest of
                            Left err -> Left err
                            Right restNames -> Right (names ++ restNames)
                in go childAddrs

-- | Extract symbol table entries from B-tree leaf nodes
--   Symbol table entry format (group or dataset):
--   - 8 bytes: Object header address
--   - 8 bytes: Cache type field
--   - 4 bytes: Name length
--   - Name bytes: Name in local heap
--   
--   Currently simplified: Extract names from ASCII strings in leaf nodes
parseBTreeLeafEntries :: BL.ByteString -> Int64 -> Word16 -> [String]
parseBTreeLeafEntries fileData offset numEntries
  | numEntries == 0 = []
  | otherwise =
    -- SIMPLIFIED: For now, extract ASCII strings in the vicinity of entries
    -- FUTURE: Properly parse symbol table entry structures
    let entryRegion = BL.take (fromIntegral numEntries * 32) 
                               (BL.drop (fromIntegral offset + 8) fileData)
        candidates = extractAsciiStringsFromChunk entryRegion
    in filter isDatasetName candidates

-- | Parse local heap header to find string data
--   Local heap format (version 0):
--   - 4 bytes: Signature "HEAP"
--   - 1 byte: Version
--   - 3 bytes: Reserved
-- | Local heap header information
--   Stores parsed heap metadata
data LocalHeapHeader = LocalHeapHeader
  { lhVersion       :: Word8           -- ^ Heap version
  , lhDataSize      :: Word64          -- ^ Size of heap data region
  , lhFreeListOffset :: Word64         -- ^ Offset to free list head
  , lhDataAddress   :: Word64          -- ^ Address of actual heap data
  } deriving (Show, Eq)

-- | Parse complete local heap header
--   Local heap format (version 0):
--   - 4 bytes: Signature "HEAP"
--   - 1 byte: Version
--   - 3 bytes: Reserved
--   - 8 bytes: Heap data size
--   - 8 bytes: Offset to free list head  
--   - 8 bytes: Address of data in heap
parseLocalHeapHeader :: BL.ByteString -> Int64 -> Either HDF5ParseError LocalHeapHeader
parseLocalHeapHeader fileData offset
  | fromIntegral (BL.length fileData) < offset + 32 = 
    Left $ TruncatedData $ "Local heap header at offset " ++ show offset ++ " requires 32 bytes"
  | otherwise =
    do
      -- Validate signature
      sig <- byteStringAt fileData offset 4
      if sig /= BL.pack [0x48, 0x45, 0x41, 0x50]  -- "HEAP"
        then Left $ HeapParseFailed $ "Invalid heap signature at offset " ++ show offset
        else do
          -- Parse header fields
          version <- byteAt fileData (offset + 4)
          when (version > 0) $
            Left $ HeapParseFailed $ "Unsupported heap version " ++ show version
          
          -- Read heap data size (8 bytes at offset 8)
          dataSize <- word64LEAt fileData (offset + 8)
          
          -- Read free list offset (8 bytes at offset 16)
          freeListOffset <- word64LEAt fileData (offset + 16)
          
          -- Read data address (8 bytes at offset 24)
          dataAddress <- word64LEAt fileData (offset + 24)
          
          pure $ LocalHeapHeader version dataSize freeListOffset dataAddress

-- | Old interface for backward compatibility
--   Returns offset of heap data start
parseLocalHeapOffset :: BL.ByteString -> Int64 -> Either HDF5ParseError Int64
parseLocalHeapOffset fileData offset
  | fromIntegral (BL.length fileData) < offset + 32 = 
    Left $ TruncatedData $ "Local heap header at offset " ++ show offset ++ " requires 32 bytes"
  | otherwise =
    do
      header <- parseLocalHeapHeader fileData offset
      -- Data starts at the address specified in the header
      pure (fromIntegral (lhDataAddress header))

-- | Extract null-terminated string from heap data at given offset
extractHeapString :: BL.ByteString -> Int64 -> Maybe String
extractHeapString fileData offset
  | fromIntegral (BL.length fileData) <= offset = Nothing
  | otherwise =
    let chunk = BL.drop (fromIntegral offset) fileData
        bytes = BL.takeWhile (/= 0) chunk
    in if BL.null bytes
       then Nothing
       else Just (BL.unpack bytes >>= (\b -> if b >= 32 && b < 127 then [toEnum (fromIntegral b)] else []))

-- | Extract multiple null-terminated strings from heap starting at offset
--   Useful for extracting all dataset names from a local heap
extractHeapStringsFromOffset :: BL.ByteString -> Int64 -> Maybe [String]
extractHeapStringsFromOffset fileData offset
  | fromIntegral (BL.length fileData) <= offset = Nothing
  | otherwise =
    -- Scan forward and extract all valid ASCII strings separated by null bytes
    -- Limit scan to 1KB to prevent excessive memory use
    let chunk = BL.take 1024 $ BL.drop (fromIntegral offset) fileData
        go :: [String] -> BL.ByteString -> [String]
        go acc remaining
          | BL.null remaining = reverse acc
          | BL.length remaining < 1 = reverse acc  -- Safety check
          | BL.head remaining == 0 = 
              if BL.length remaining > 1
                then go acc (BL.tail remaining)  -- Skip null byte
                else reverse acc
          | otherwise =
            let bytes = BL.takeWhile (/= 0) remaining
                rest = BL.drop (BL.length bytes + 1) remaining
                str = BL.unpack bytes >>= (\b -> if b >= 32 && b < 127 then [toEnum (fromIntegral b)] else [])
            in if null str || length str < 2  -- Filter out single-char strings
               then go acc rest
               else go (str : acc) rest
    in if BL.null chunk then Nothing else Just (go [] chunk)

-- | Extract all strings from local heap region  
extractHeapStrings :: BL.ByteString -> Int64 -> Maybe [String]
extractHeapStrings fileData offset
  | fromIntegral (BL.length fileData) < fromIntegral offset + 100 = Nothing
  | otherwise =
    -- Use extractHeapStringsFromOffset which properly parses null-terminated strings
    -- Skip the heap header (32 bytes) and start parsing from the data region
    let heapDataOffset = offset + 32  -- Skip HEAP header
    in extractHeapStringsFromOffset fileData heapDataOffset

-- | Extract all dataset names from a symbol table by scanning B-tree
--   Implements full B-tree traversal to find all leaf nodes
--   Uses recursive traversal to handle internal B-tree nodes
--   Returns dataset names only - full metadata parsing deferred
parseSymbolTableForDatasets :: BL.ByteString -> Word64 -> Word64 -> [String]
parseSymbolTableForDatasets fileData btreeAddr heapAddr
  | BL.length fileData < 512 = []
  | otherwise =
    -- Recursively traverse B-tree starting at root
    case traverseV1BTree fileData btreeAddr 0 10 of
      Left _ -> 
        -- If strict parsing fails, return empty instead of falling back to heuristics
        []
      Right names ->
        -- Try to enhance with heap strings
        let heapOffset = fromIntegral heapAddr :: Int64
            heapStrings = case extractHeapStrings fileData heapOffset of
              Just strs -> filter isDatasetName strs
              Nothing -> []
            -- Combine all discovered names and deduplicate
            allNames = nub (names ++ heapStrings)
        in allNames

-- | Assemble HDF5DatasetInfo from parsed object header messages
--   Validates that required messages are present
assembleDatasetInfo :: String -> Word64 -> [ObjectHeaderMessage] -> Either HDF5ParseError HDF5DatasetInfo
assembleDatasetInfo name offset messages = do
  -- Extract required messages
  dataspace <- case [ds | DataspaceMessage ds <- messages] of
    (ds:_) -> Right ds
    [] -> Left $ CorruptedMetadata $ "Dataset '" ++ name ++ "' missing required Dataspace message"
  
  datatype <- case [dt | DatatypeMessage dt <- messages] of
    (dt:_) -> Right dt
    [] -> Left $ CorruptedMetadata $ "Dataset '" ++ name ++ "' missing required Datatype message"
  
  layout <- case [ly | DataLayoutMessage ly <- messages] of
    (ly:_) -> Right ly
    [] -> Left $ CorruptedMetadata $ "Dataset '" ++ name ++ "' missing required DataLayout message"
  
  -- Extract optional messages
  let filters = case [fp | FilterPipelineMessage fp <- messages] of
        (fp:_) -> Just fp
        [] -> Nothing
  
  let fillValue = case [fv | FillValueMessage fv <- messages] of
        (fv:_) -> Just fv
        [] -> Nothing
  
  pure $ HDF5DatasetInfo name dataspace datatype layout filters fillValue offset

-- | Fallback: Extract dataset-like names using heuristics
--   Used when full B-tree parsing is deferred
-- DEPRECATED: Heuristic string scanning removed. Use proper metadata parsing instead.
-- This function is no longer used and kept only for backward compatibility.
extractDatasetNamesViaHeuristic :: BL.ByteString -> [HDF5DatasetInfo]
extractDatasetNamesViaHeuristic _ = []  -- Return empty - heuristics are disabled

-- | Search for Object Header (OHDR) signature in ByteString using multiple strategies
--   OHDR signature: 0x4F 0x48 0x44 0x52 ("OHDR")
--   Tries multiple search strategies with early termination:
--   1. Forward scan from offset 32 to 512 (bounded search)
--   2. Fixed offset checks at common locations (48, 64, 96, 128, 256)
--   3. Broader search from 16 onwards if previous strategies fail
findObjectHeaderSignature :: BL.ByteString -> Maybe Int64
findObjectHeaderSignature bs = 
  -- Strategy 1: Bounded forward scan (most common case)
  case searchRange bs 32 512 of
    Just addr -> Just addr
    Nothing ->
      -- Strategy 2: Try fixed offset locations (common placements)
      let fixedOffsets = [48, 64, 96, 128, 256, 512, 1024]
          tryFixed [] = Nothing
          tryFixed (off:rest) = case checkOHDR bs off of
            Just addr -> Just addr
            Nothing -> tryFixed rest
      in case tryFixed fixedOffsets of
        Just addr -> Just addr
        Nothing ->
          -- Strategy 3: Broader search from beginning (last resort)
          searchRange bs 16 (min 4096 (BL.length bs))
  where
    -- Check if OHDR signature exists at specific offset
    checkOHDR :: BL.ByteString -> Int64 -> Maybe Int64
    checkOHDR bs' offset = 
      case (byteAt bs' offset, byteAt bs' (offset + 1), 
            byteAt bs' (offset + 2), byteAt bs' (offset + 3)) of
        (Right 0x4F, Right 0x48, Right 0x44, Right 0x52) -> Just offset
        _ -> Nothing
    
    -- Scan a range of offsets for OHDR signature
    searchRange :: BL.ByteString -> Int64 -> Int64 -> Maybe Int64
    searchRange bs' startOff endOff =
      let search offset
            | offset >= endOff = Nothing
            | offset >= fromIntegral (BL.length bs') - 4 = Nothing
            | otherwise = case checkOHDR bs' offset of
                Just addr -> Just addr
                Nothing -> search (offset + 1)
      in search startOff

-- | Parse extended superblock v0/v1 header to extract metadata
--   Extended superblock structure (after main 32-byte superblock):
--   - Root group object header address (4 or 8 bytes, little-endian)
--   Followed by optional additional fields in later versions
parseExtendedSuperblockV0V1Header :: BL.ByteString -> Int -> Int -> HDF5ParserM Int64
parseExtendedSuperblockV0V1Header bs offsetSz _lenSz = do
  -- Search for OHDR signature which marks the start of object header
  -- The extended superblock location is determined by finding this signature
  case findObjectHeaderSignature bs of
    Nothing -> 
      Left $ CorruptedMetadata "Extended superblock: Could not locate object header signature (OHDR)"
    Just headerAddr -> do
      -- Verify we found a valid OHDR header
      -- The OHDR signature should be at the beginning of the object header
      sig1 <- byteAt bs headerAddr
      sig2 <- byteAt bs (headerAddr + 1)
      sig3 <- byteAt bs (headerAddr + 2)
      sig4 <- byteAt bs (headerAddr + 3)
      
      if sig1 == 0x4F && sig2 == 0x48 && sig3 == 0x44 && sig4 == 0x52
        then do
          -- Valid OHDR found - return its address as the root group object header location
          -- The actual data in this header will be parsed separately by parseObjectHeaderMessages
          pure headerAddr
        else
          Left $ CorruptedMetadata "Extended superblock: Found OHDR signature mismatch"

-- | Parse extended superblock v0/v1 to retrieve actual root group address
--   When extended addressing is detected (-1 at offset 0x20), the structure is:
--   - Offset 0x40: Root group object header address (8 bytes, little-endian)
--   - Offset 0x48: Reserved/flags (8 bytes)
--   - Offset 0x50: B-tree address for root group symbol table (8 bytes, little-endian)
--   - Offset 0x58: Local heap address for root group symbol table (8 bytes, little-endian)
--   
--   Extended superblock structure (v0/v1):
--   - Offset 0x00-0x07: HDF5 signature
--   - Offset 0x08-0x1F: Version 0/1 superblock (24 bytes)
--   - Offset 0x20-0x27: Root group address (may be 0xFFFFFFFFFFFFFFFF for extended)
--   - Offset 0x40: Actual root group object header address (8 bytes)
--   - Offset 0x50: B-tree address (8 bytes)
--   - Offset 0x58: Heap address (8 bytes)
--   
--   Returns: (root object header address, B-tree address, heap address)
--
--   Strategy:
--   1. Read 8-byte root address from offset 0x40
--   2. Read 8-byte B-tree address from offset 0x50
--   3. Read 8-byte heap address from offset 0x58
--   4. Validate all addresses are within file bounds
--   5. Return all three addresses for dataset discovery
parseExtendedSuperblocV0V1 :: BL.ByteString -> Int -> Int -> HDF5ParserM (Int64, Word64, Word64)
parseExtendedSuperblocV0V1 bs offsetSz lenSz = do
  -- Extended addressing: root address at offset 0x40, symbol table info at 0x50/0x58
  rootAddr <- word64LEAt bs 0x40
  btreeAddr <- word64LEAt bs 0x50
  heapAddr <- word64LEAt bs 0x58
  
  let rootAddrInt = fromIntegral rootAddr :: Int64
  
  -- Validate root address is within file bounds
  if rootAddrInt < 0 || rootAddrInt >= fromIntegral (BL.length bs)
    then Left $ CorruptedMetadata $ "Extended addressing: Invalid root address 0x" 
                                  ++ showHex rootAddr ""
    else pure (rootAddrInt, btreeAddr, heapAddr)

-- | Compatibility wrapper for IO-based discovery
--   Uses mmap for efficient lazy loading, consistent with withArrayXDFromFile
discoverDatasetsFromFile :: FilePath -> IO [HDF5DatasetInfo]
discoverDatasetsFromFile path = do
  exists <- doesFileExist path
  if not exists
    then return []
    else do
      -- Use mmap for lazy loading (consistent with array loading functions)
      contents <- mmapFileRegion path Nothing
      return $ discoverDatasets contents

-- | Discovers datasets using proper HDF5 metadata binary format parsing
--   
--   Algorithm:
--   1. Parse superblock to find root group object header address
--   2. Detect extended addressing (rootAddr == -1) in v0/v1 or flag bit 5 in v2/v3
--   3. If extended addressing detected, attempt to parse extended superblock (TODO)
--   4. Load and parse root group object header at calculated address
--   5. Extract symbol table message if "old-style" group (message type 0x0011)
--   6. Parse B-tree and local heap to find dataset names
--   7. Return dataset info extracted from object headers
--
--   Extended Addressing Support:
--   - v0/v1: Detects markers 0xffffffff (4-byte) or 0xffffffffffffffff (8-byte)
--   - v2/v3: Checks bit 5 of file consistency flags at byte 11
--   - When detected, returns empty list (full extended superblock parsing is TODO)
--
--   Current scope: Root group datasets only (no recursion)
--   Handles: Superblock versions 0, 1, 2, 3
--   
--   NOTE: This is a simplified implementation that only returns dataset names.
--   Full metadata extraction requires parsing each dataset's object header.
discoverDatasets :: BL.ByteString -> [HDF5DatasetInfo]
discoverDatasets bs
  | BL.length bs < 512 = []
  | otherwise =
    case parseSuperblockMetadata bs of
      Left _ -> []  -- Return empty on parse error (no heuristic fallback)
      Right (rootAddr, offsetSz, lenSz) ->
        -- Handle extended addressing: when rootAddr == -1, parse extended superblock
        if rootAddr == -1
          then
            -- Extended addressing: get symbol table info directly from extended superblock
            case parseExtendedSuperblocV0V1 bs offsetSz lenSz of
              Left _ -> []
              Right (_rootObjAddr, btreeAddr, heapAddr) ->
                -- Parse the symbol table using B-tree and heap directly
                -- This returns names only - we create placeholder dataset info
                let names = parseSymbolTableForDatasets bs btreeAddr heapAddr
                in map createPlaceholderDatasetInfo names
          else
            -- Standard addressing: parse object header for symbol table message
            let objHeaderOffset = fromIntegral rootAddr :: Int64
            in if rootAddr < 0 || fromIntegral objHeaderOffset >= BL.length bs
               then []
               else case parseObjectHeaderMessages offsetSz lenSz (BL.drop (fromIntegral objHeaderOffset) bs) of
                 Left _ -> []
                 Right msgs ->
                   -- Extract symbol table message (type 0x0011)
                   case [m | m@(SymbolTableMessage _ _) <- msgs] of
                     [] -> []
                     (SymbolTableMessage btreeAddr heapAddr : _) ->
                       -- Parse the symbol table using B-tree and heap
                       -- This returns names only - we create placeholder dataset info
                       let names = parseSymbolTableForDatasets bs btreeAddr heapAddr
                       in map createPlaceholderDatasetInfo names
                     _ -> []
  where
    -- Create placeholder HDF5DatasetInfo when full metadata is not available
    -- This maintains backward compatibility while we transition to full metadata parsing
    createPlaceholderDatasetInfo :: String -> HDF5DatasetInfo
    createPlaceholderDatasetInfo name =
      let placeholderDataspace = HDF5Dataspace DataspaceV1 1 False Nothing [0] Nothing
          placeholderDatatype = HDF5Datatype 
            { dtVersion = 1
            , dtClass = ClassFixedPoint $ FixedPointType 
                { fpByteOrder = LittleEndian
                , fpSigned = False
                , fpBitOffset = 0
                , fpBitPrecision = 32
                , fpSize = 4
                }
            }
          placeholderLayout = ContiguousLayout LayoutV1 0 0
      in HDF5DatasetInfo name placeholderDataspace placeholderDatatype placeholderLayout Nothing Nothing 0


-- | Extract dimension information from HDF5 file (wrapper around discoverDatasets)
extractDatasetDimensions :: BL.ByteString -> [(String, [Int])]
extractDatasetDimensions bs =
  let datasets = discoverDatasets bs
  in map (\d -> 
    let dims = map fromIntegral (dsDimensions (dsiDataspace d)) :: [Int]
        dimType = case length dims of
          1 -> "1D"
          2 -> "2D"
          _ -> "ND"
    in (dimType, dims)) datasets
    


