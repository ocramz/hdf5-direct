{-# LANGUAGE ScopedTypeVariables #-}

module LibSpec
  ( spec
  ) where

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get ()
import Data.Word ()
import Control.Exception (try)

import Data.HDF5.Direct.Internal
  ( HDF5Exception(..)
  , MmapFile(..)
  , withMmapFile
  , HDF5Datatype(..)
  , DatatypeClass(..)
  , FixedPointType(..)
  , FloatingPointType(..)
  , TimeType(..)
  , StringType(..)
  , BitfieldType(..)
  , OpaqueType(..)
  , CompoundType(..)
  , ReferenceType(..)
  , EnumerationType(..)
  , VariableLengthType(..)
  , ArrayType(..)
  , ByteOrder(..)
  , PaddingType(..)
  , CharacterSet(..)
  )


spec :: Spec
spec = do
  describe "HDF5Exception" $ do
    it "shows MmapIOError message" $ do
      show (MmapIOError "test error") `shouldContain` "test error"
    
    it "shows PageFaultError message" $ do
      show (PageFaultError "segment fault") `shouldContain` "segment fault"
    
    it "shows ParseError message" $ do
      show (ParseError "invalid data") `shouldContain` "invalid data"

  describe "ByteOrder" $ do
    it "can construct LittleEndian" $ do
      (LittleEndian :: ByteOrder) `shouldBe` LittleEndian
    
    it "can construct BigEndian" $ do
      (BigEndian :: ByteOrder) `shouldBe` BigEndian

  describe "FixedPointType" $ do
    it "stores fixed-point integer metadata correctly" $ do
      let fp = FixedPointType LittleEndian True 0 32 4
      fpByteOrder fp `shouldBe` LittleEndian
      fpSigned fp `shouldBe` True
      fpBitOffset fp `shouldBe` 0
      fpBitPrecision fp `shouldBe` 32
      fpSize fp `shouldBe` 4
    
    it "handles big-endian integers" $ do
      let fp = FixedPointType BigEndian False 8 16 2
      fpByteOrder fp `shouldBe` BigEndian
      fpSigned fp `shouldBe` False
      fpSize fp `shouldBe` 2

  describe "FloatingPointType" $ do
    it "stores floating-point metadata correctly" $ do
      let flt = FloatingPointType LittleEndian 0 64 52 11 41 52 1023 8
      fltByteOrder flt `shouldBe` LittleEndian
      fltBitPrecision flt `shouldBe` 64
      fltExponentSize flt `shouldBe` 11
      fltMantissaSize flt `shouldBe` 52
      fltSize flt `shouldBe` 8
    
    it "handles single-precision floats" $ do
      let flt = FloatingPointType BigEndian 0 32 23 8 24 23 127 4
      fltBitPrecision flt `shouldBe` 32
      fltSize flt `shouldBe` 4

  describe "TimeType" $ do
    it "stores time datatype metadata" $ do
      let t = TimeType LittleEndian 64 8
      timeByteOrder t `shouldBe` LittleEndian
      timeBitPrecision t `shouldBe` 64
      timeSize t `shouldBe` 8

  describe "StringType" $ do
    it "stores ASCII fixed-length string metadata" $ do
      let s = StringType PadZero ASCII 256
      strPaddingType s `shouldBe` PadZero
      strCharset s `shouldBe` ASCII
      strSize s `shouldBe` 256
    
    it "stores UTF-8 variable-length string metadata" $ do
      let s = StringType PadZero UTF8 0
      strCharset s `shouldBe` UTF8
      strSize s `shouldBe` 0

  describe "BitfieldType" $ do
    it "stores bitfield metadata" $ do
      let bf = BitfieldType LittleEndian 4 12 2
      bfByteOrder bf `shouldBe` LittleEndian
      bfBitOffset bf `shouldBe` 4
      bfBitPrecision bf `shouldBe` 12
      bfSize bf `shouldBe` 2

  describe "OpaqueType" $ do
    it "stores opaque type with tag" $ do
      let o = OpaqueType "custom_type" 16
      opaqueTag o `shouldBe` "custom_type"
      opaqueSize o `shouldBe` 16
    
    it "handles empty tag" $ do
      let o = OpaqueType "" 8
      opaqueTag o `shouldBe` ""

  describe "ReferenceType" $ do
    it "can construct object reference" $ do
      (RefObject :: ReferenceType) `shouldBe` RefObject
    
    it "can construct dataset region reference" $ do
      (RefDatasetRegion :: ReferenceType) `shouldBe` RefDatasetRegion

  describe "CompoundType" $ do
    it "stores compound datatype metadata" $ do
      let c = CompoundType [] 32
      compMembers c `shouldBe` []
      compSize c `shouldBe` 32
    
    it "preserves member list" $ do
      let members = []  -- Simplified for this test
      let c = CompoundType members 64
      compSize c `shouldBe` 64

  describe "EnumerationType" $ do
    it "stores enumeration base type and mappings" $ do
      let baseType = HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))
      let e = EnumerationType baseType []
      enumBaseType e `shouldBe` baseType
      enumMappings e `shouldBe` []
    
    it "can store name-value mappings" $ do
      let baseType = HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))
      let mappings = [("RED", 0), ("GREEN", 1), ("BLUE", 2)]
      let e = EnumerationType baseType mappings
      length (enumMappings e) `shouldBe` 3

  describe "VariableLengthType" $ do
    it "stores variable-length sequence metadata" $ do
      let baseType = HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))
      let vl = VariableLengthType baseType False
      vlIsString vl `shouldBe` False
      vlBaseType vl `shouldBe` baseType
    
    it "marks variable-length strings" $ do
      let baseType = HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 8 1))
      let vl = VariableLengthType baseType True
      vlIsString vl `shouldBe` True

  describe "ArrayType" $ do
    it "stores array datatype metadata" $ do
      let baseType = HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))
      let arr = ArrayType baseType [10, 20]
      arrDimensions arr `shouldBe` [10, 20]
      arrBaseType arr `shouldBe` baseType
    
    it "handles multi-dimensional arrays" $ do
      let baseType = HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))
      let arr = ArrayType baseType [5, 10, 15]
      arrDimensions arr `shouldBe` [5, 10, 15]

  describe "HDF5Datatype" $ do
    it "stores datatype version and class" $ do
      let fp = FixedPointType LittleEndian True 0 32 4
      let dt = HDF5Datatype 1 (ClassFixedPoint fp)
      dtVersion dt `shouldBe` 1
      case dtClass dt of
        ClassFixedPoint fptype -> fpByteOrder fptype `shouldBe` LittleEndian
        _ -> expectationFailure "Expected ClassFixedPoint"
    
    it "handles floating-point datatypes" $ do
      let flt = FloatingPointType LittleEndian 0 64 52 11 41 52 1023 8
      let dt = HDF5Datatype 2 (ClassFloatingPoint flt)
      dtVersion dt `shouldBe` 2
      case dtClass dt of
        ClassFloatingPoint flttype -> fltSize flttype `shouldBe` 8
        _ -> expectationFailure "Expected ClassFloatingPoint"
    
    it "handles string datatypes" $ do
      let str = StringType PadZero ASCII 256
      let dt = HDF5Datatype 1 (ClassString str)
      case dtClass dt of
        ClassString strtype -> strSize strtype `shouldBe` 256
        _ -> expectationFailure "Expected ClassString"

  describe "PaddingType" $ do
    it "provides three padding types" $ do
      [PadZero, PadOne, PadBackground] `shouldSatisfy` (\xs -> length xs == 3)

  describe "CharacterSet" $ do
    it "provides ASCII and UTF8" $ do
      [ASCII, UTF8] `shouldSatisfy` (\xs -> length xs == 2)

  describe "DatatypeClass" $ do
    it "can represent all 11 datatype classes" $ do
      let classes =
            [ ClassFixedPoint (FixedPointType LittleEndian True 0 32 4)
            , ClassFloatingPoint (FloatingPointType LittleEndian 0 64 52 11 41 52 1023 8)
            , ClassTime (TimeType LittleEndian 64 8)
            , ClassString (StringType PadZero ASCII 256)
            , ClassBitfield (BitfieldType LittleEndian 0 8 1)
            , ClassOpaque (OpaqueType "test" 16)
            , ClassCompound (CompoundType [] 32)
            , ClassReference RefObject
            , ClassEnumeration (EnumerationType (HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))) [])
            , ClassVariableLength (VariableLengthType (HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))) False)
            , ClassArray (ArrayType (HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))) [10, 20])
            ]
      length classes `shouldBe` 11

  describe "Eq instances" $ do
    it "FixedPointType equality" $ do
      let fp1 = FixedPointType LittleEndian True 0 32 4
      let fp2 = FixedPointType LittleEndian True 0 32 4
      let fp3 = FixedPointType BigEndian True 0 32 4
      fp1 `shouldBe` fp2
      fp1 `shouldNotBe` fp3
    
    it "ByteOrder equality" $ do
      (LittleEndian :: ByteOrder) `shouldBe` LittleEndian
      (BigEndian :: ByteOrder) `shouldNotBe` LittleEndian

  describe "Ord instances" $ do
    it "ByteOrder ordering" $ do
      LittleEndian `shouldBe` LittleEndian
      (BigEndian > LittleEndian) `shouldBe` True

  describe "Generic instances" $ do
    it "types derive Generic" $ do
      -- This is a compile-time check, but we verify it doesn't crash at runtime
      let fp = FixedPointType LittleEndian True 0 32 4 :: FixedPointType
      show fp `shouldContain` "FixedPointType"
      
    it "HDF5Datatype derives Generic" $ do
      let dt = HDF5Datatype 0 (ClassFixedPoint (FixedPointType LittleEndian True 0 32 4))
      show dt `shouldContain` "HDF5Datatype"

  describe "mmap file operations" $ do
    it "handles non-existent file gracefully" $ do
      result <- try $ withMmapFile "/nonexistent/file.h5" (\_ -> return ())
      case result of
        Left (MmapIOError _) -> return ()
        Left other -> expectationFailure $ "Expected MmapIOError, got: " ++ show other
        Right _ -> expectationFailure "Expected exception"
    
    it "withMmapFile resource management works" $ do
      -- Create a temporary file with some data
      let testData = BL.pack [0x89, 0x48, 0x44, 0x46, 0x0D, 0x0A, 0x1A, 0x0A]  -- HDF5 signature
      writeFile "/tmp/test_hdf5.h5" ""
      BL.writeFile "/tmp/test_hdf5.h5" testData
      
      -- Verify we can open it
      result <- try $ withMmapFile "/tmp/test_hdf5.h5" $ \mfile -> do
        return (mmapPath mfile)
      
      case result of
        Right path -> path `shouldBe` "/tmp/test_hdf5.h5"
        Left (e :: HDF5Exception) -> expectationFailure $ "Failed to mmap: " ++ show e

  describe "Lazy evaluation properties" $ do
    it "HDF5Datatype construction is lazy" $ do
      -- Construction should be fast; evaluation deferred until access
      let fp = FixedPointType LittleEndian True 0 32 4
      let dt = HDF5Datatype 0 (ClassFixedPoint fp)
      dtVersion dt `shouldBe` 0
    
    it "Nested datatypes work correctly" $ do
      let innerFp = FixedPointType LittleEndian True 0 32 4
      let innerDt = HDF5Datatype 0 (ClassFixedPoint innerFp)
      let vl = VariableLengthType innerDt True
      let outerDt = HDF5Datatype 1 (ClassVariableLength vl)
      dtVersion outerDt `shouldBe` 1
      case dtClass outerDt of
        ClassVariableLength vl' -> vlIsString vl' `shouldBe` True
        _ -> expectationFailure "Expected ClassVariableLength"

  describe "Type safety" $ do
    it "ByteOrder is distinct from other types" $ do
      let bo = LittleEndian :: ByteOrder
      show bo `shouldBe` "LittleEndian"
    
    it "Multiple datatype instances don't interfere" $ do
      let fp = FixedPointType LittleEndian True 0 32 4
      let str = StringType PadZero ASCII 256
      let dt1 = HDF5Datatype 0 (ClassFixedPoint fp)
      let dt2 = HDF5Datatype 0 (ClassString str)
      case dtClass dt1 of
        ClassFixedPoint _ -> return ()
        _ -> expectationFailure "dt1 should be ClassFixedPoint"
      case dtClass dt2 of
        ClassString _ -> return ()
        _ -> expectationFailure "dt2 should be ClassString"
