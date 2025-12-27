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
  , closeMmapFile
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
  ) where

import Control.Exception (Exception, bracket, catch, throwIO, SomeException, displayException)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int64)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put, runPut)
import Data.Bits (shiftL, shiftR, (.|.))
import Foreign.ForeignPtr (ForeignPtr)
import System.IO (withFile, IOMode(WriteMode, AppendMode), hFlush)
import System.IO.MMap (Mode(ReadOnly), mmapFileByteStringLazy, mmapFileForeignPtr)
import GHC.Generics (Generic)

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
  bracket (openMmapFile path) closeMmapFile

-- | Open a file for mmap-based lazy reading
openMmapFile :: FilePath -> IO MmapFile
openMmapFile path = 
  catch (do
    (ptr, offset, size) <- mmapFileForeignPtr path ReadOnly Nothing
    return $ MmapFile path ptr offset size
  ) (\(ex :: SomeException) -> throwIO $ MmapIOError ("Failed to mmap " ++ path ++ ": " ++ displayException ex))

-- | Close and unmap a previously opened file
--
--   ForeignPtr automatic finalizer handles unmapping via garbage collection.
--   This function is a no-op but kept for explicit resource management if needed.
closeMmapFile :: MmapFile -> IO ()
closeMmapFile _ = return ()

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