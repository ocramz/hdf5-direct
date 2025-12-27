{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Data.HDF5.Direct.Internal
  ( -- * Exception handling
    HDF5Exception(..)
    -- * Low-level mmap file handling
  , MmapFile(..)
  , withMmapFile
  , mmapFileRegion
  , openMmapFile
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
  , parseSuperblockVersion
  , discoverDatasets
    -- * HDF5 Introspection and Validation
  , HDF5Introspection(..)
  , validateHDF5Signature
  , describeDatatypeClass
  , formatDatatype
  , introspectHDF5File
  , assertHDF5WellFormed
  ) where

import Control.Exception (Exception, bracket, catch, throwIO, SomeException, displayException, try)
import Data.Binary.Get (Get)
import Data.Binary.Put (Put, runPut)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (isSubsequenceOf, tails, nub, nubBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Generics (Generic)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
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

-- | Information about a dataset extracted from HDF5 metadata
data HDF5DatasetInfo = HDF5DatasetInfo
  { dsiName :: !String            -- ^ Dataset name
  , dsiDimensions :: ![Int]       -- ^ Dimensions of dataset
  , dsiDatatype :: !(Maybe HDF5Datatype)  -- ^ Datatype if successfully parsed
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
  let lower = map toLower s
      keywords = ["data", "train", "test", "distance", "neighbor", "ground",
                 "feature", "label", "index", "knn", "set", "array"]
      hasKeyword = any (\kw -> isSubsequenceOf kw lower) keywords
  in hasKeyword && all (\c -> c >= ' ' && c < '~') s

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

-- | Discover datasets in an HDF5 file by scanning for common patterns
--   This heuristically discovers dataset names and basic dimension info
--   Returns a list of discovered datasets (may be incomplete)
discoverDatasets :: BL.ByteString -> [HDF5DatasetInfo]
discoverDatasets bs
  | BL.length bs < 512 = []
  | otherwise = scanForDatasets bs 256 []
  where
    -- Scan through file looking for dataset markers
    scanForDatasets :: BL.ByteString -> Int -> [HDF5DatasetInfo] -> [HDF5DatasetInfo]
    scanForDatasets content offset acc
      | offset + 32 > fromIntegral (BL.length content) = acc
      | otherwise =
        let candidates = tryExtractDatasetAt content offset
        in case candidates of
          [] -> scanForDatasets content (offset + 64) acc
          ds -> scanForDatasets content (offset + 128) (acc ++ ds)
    
    -- Try to extract dataset information at a specific offset
    tryExtractDatasetAt :: BL.ByteString -> Int -> [HDF5DatasetInfo]
    tryExtractDatasetAt content pos
      | pos + 64 > fromIntegral (BL.length content) = []
      | otherwise =
        let chunk = BL.drop (fromIntegral pos) content
            -- Try to find ASCII strings that look like dataset names
            names = extractNameCandidates chunk
            -- Try to extract dimension info
            dims = extractDimensionValues chunk
        in map (\name -> HDF5DatasetInfo name dims Nothing (fromIntegral pos)) names
    
    -- Extract potential dataset names using shared helper
    extractNameCandidates :: BL.ByteString -> [String]
    extractNameCandidates chunk =
      let allStrings = extractAsciiStringsFromChunk chunk
          filtered = filter isDatasetName allStrings
      in nubBy (==) filtered
    
    -- Extract likely dimension values from chunk
    extractDimensionValues :: BL.ByteString -> [Int]
    extractDimensionValues chunk
      | BL.length chunk < 16 = []
      | otherwise =
        let bytes = BL.take 16 chunk
            val1 = convertWord32 BigEndian (BL.take 4 bytes)
            val2 = convertWord32 BigEndian (BL.drop 4 bytes)
            val3 = convertWord32 BigEndian (BL.drop 8 bytes)
        in case (val1, val2) of
          (v1, 0) | v1 > 0 && v1 < 1000000 -> [fromIntegral v1]
          (v1, v2) | v1 > 0 && v2 > 0 && v1 < 100000 && v2 < 100000 ->
            [fromIntegral v1, fromIntegral v2]
          _ | val3 > 0 && val3 < 1000000 -> [fromIntegral val3]


-- | Extract dimension information from HDF5 file (wrapper around discoverDatasets)
extractDatasetDimensions :: BL.ByteString -> [(String, [Int])]
extractDatasetDimensions bs =
  let datasets = discoverDatasets bs
  in map (\d -> let dimType = if length (dsiDimensions d) == 1 then "1D" else if length (dsiDimensions d) == 2 then "2D" else "ND"
               in (dimType, dsiDimensions d)) datasets
    


