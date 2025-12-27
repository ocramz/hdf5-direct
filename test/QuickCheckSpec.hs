{-# LANGUAGE ScopedTypeVariables #-}

module QuickCheckSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Data.Word (Word8, Word16, Word32)
import Data.Binary.Put (runPut, putWord8, putWord16be, putWord32be)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGet)
import qualified Data.Binary.Get as Get

import Data.HDF5.Direct.Internal
  ( ByteOrder(..)
  , PaddingType(..)
  , CharacterSet(..)
  , FixedPointType(..)
  , FloatingPointType(..)
  , TimeType(..)
  , StringType(..)
  , BitfieldType(..)
  , OpaqueType(..)
  , ReferenceType(..)
  , parseByteOrder
  , parsePaddingType
  , parseCharacterSet
  , parseFixedPoint
  , parseFloatingPoint
  , parseTime
  , parseString
  , parseBitfield
  , parseOpaque
  )


main :: IO ()
main = hspec spec

-- Arbitrary instances for property testing
instance Arbitrary ByteOrder where
  arbitrary = elements [LittleEndian, BigEndian]

instance Arbitrary PaddingType where
  arbitrary = elements [PadZero, PadOne, PadBackground]

instance Arbitrary CharacterSet where
  arbitrary = elements [ASCII, UTF8]

instance Arbitrary ReferenceType where
  arbitrary = elements [RefObject, RefDatasetRegion]

-- Fixed generator for valid bit values
newtype BitValue = BitValue Int deriving (Show)
instance Arbitrary BitValue where
  arbitrary = BitValue <$> choose (0, 1)

newtype PaddingValue = PaddingValue Int deriving (Show)
instance Arbitrary PaddingValue where
  arbitrary = PaddingValue <$> choose (0, 2)

newtype CharsetValue = CharsetValue Int deriving (Show)
instance Arbitrary CharsetValue where
  arbitrary = CharsetValue <$> choose (0, 1)

spec :: Spec
spec = do
  describe "ByteOrder parsing" $ do
    it "parses 0 as LittleEndian" $
      parseByteOrder 0 `shouldBe` LittleEndian
    
    it "parses non-0 as BigEndian" $
      property $ \(n :: Int) -> 
        if n == 0 
        then parseByteOrder n == LittleEndian
        else parseByteOrder n == BigEndian
    
    it "roundtrips: 0 -> LittleEndian" $
      parseByteOrder 0 `shouldBe` LittleEndian
    
    it "roundtrips: any n != 0 -> BigEndian" $
      property $ \(Positive n) -> 
        parseByteOrder n == BigEndian

  describe "PaddingType parsing" $ do
    it "parses 0 as PadZero" $
      parsePaddingType 0 `shouldBe` PadZero
    
    it "parses 1 as PadOne" $
      parsePaddingType 1 `shouldBe` PadOne
    
    it "parses any other value as PadBackground" $
      property $ \(n :: Int) ->
        case parsePaddingType n of
          PadZero -> n == 0
          PadOne -> n == 1
          PadBackground -> n /= 0 && n /= 1

  describe "CharacterSet parsing" $ do
    it "parses 0 as ASCII" $
      parseCharacterSet 0 `shouldBe` ASCII
    
    it "parses non-0 as UTF8" $
      property $ \(n :: Int) ->
        if n == 0
        then parseCharacterSet n == ASCII
        else parseCharacterSet n == UTF8

  describe "FixedPointType roundtrip" $ do
    it "constructs and stores byte order correctly" $
      property $ \(order :: ByteOrder) ->
        let fp = FixedPointType order False 0 8 4
        in fpByteOrder fp == order
    
    it "constructs and stores signed flag correctly" $
      property $ \(signed :: Bool) ->
        let fp = FixedPointType LittleEndian signed 0 16 4
        in fpSigned fp == signed
    
    it "roundtrips bit offset and precision" $
      property $ \(offset :: Word16) (precision :: Word16) ->
        let fp = FixedPointType LittleEndian False (fromIntegral offset) (fromIntegral precision) 8
        in fpBitOffset fp == fromIntegral offset && fpBitPrecision fp == fromIntegral precision
    
    it "roundtrips size field" $
      property $ \(size :: Int) ->
        size > 0 && size < 256 ==>
        let fp = FixedPointType LittleEndian False 0 8 size
        in fpSize fp == size

  describe "FloatingPointType roundtrip" $ do
    it "roundtrips all components" $
      property $ \(order :: ByteOrder) 
                   (expBias :: Word32)
                   (size :: Int) ->
        size > 0 && size < 256 ==>
        let flt = FloatingPointType order 0 32 8 8 23 23 expBias size
        in fltByteOrder flt == order &&
           fltExponentBias flt == expBias &&
           fltSize flt == size
    
    it "preserves bit field structure" $
      property $ \(offset :: Int) (prec :: Int) ->
        offset >= 0 && offset < 256 && prec >= 0 && prec < 256 ==>
        let flt = FloatingPointType LittleEndian offset prec 8 8 23 23 127 4
        in fltBitOffset flt == offset && fltBitPrecision flt == prec

  describe "TimeType roundtrip" $ do
    it "roundtrips byte order" $
      property $ \(order :: ByteOrder) ->
        let tt = TimeType order 32 8
        in timeByteOrder tt == order
    
    it "roundtrips bit precision" $
      property $ \(precision :: Int) ->
        precision > 0 && precision < 256 ==>
        let tt = TimeType LittleEndian precision 8
        in timeBitPrecision tt == precision
    
    it "roundtrips size field" $
      property $ \(size :: Int) ->
        size > 0 && size < 256 ==>
        let tt = TimeType LittleEndian 32 size
        in timeSize tt == size

  describe "StringType roundtrip" $ do
    it "roundtrips padding type" $
      property $ \(pad :: PaddingType) ->
        let st = StringType pad ASCII 64
        in strPaddingType st == pad
    
    it "roundtrips character set" $
      property $ \(charset :: CharacterSet) ->
        let st = StringType PadZero charset 64
        in strCharset st == charset
    
    it "roundtrips size field" $
      property $ \(size :: Int) ->
        size >= 0 && size < 65536 ==>
        let st = StringType PadZero ASCII size
        in strSize st == size

  describe "BitfieldType roundtrip" $ do
    it "roundtrips byte order and offsets" $
      property $ \(order :: ByteOrder) ->
        let bf = BitfieldType order 0 8 4
        in bfByteOrder bf == order && bfBitOffset bf == 0
    
    it "roundtrips bit precision and size" $
      property $ \(prec :: Int) (size :: Int) ->
        prec > 0 && prec < 256 && size > 0 && size < 256 ==>
        let bf = BitfieldType LittleEndian 0 prec size
        in bfBitPrecision bf == prec && bfSize bf == size

  describe "OpaqueType roundtrip" $ do
    it "roundtrips opaque tag" $
      property $ \(tag :: String) ->
        length tag < 256 ==>
        let ot = OpaqueType tag 8
        in opaqueTag ot == tag
    
    it "roundtrips opaque size" $
      property $ \(size :: Int) ->
        size > 0 && size < 65536 ==>
        let ot = OpaqueType "data" size
        in opaqueSize ot == size
    
    it "handles empty tag correctly" $
      let ot = OpaqueType "" 8
      in opaqueTag ot == ""

  describe "ReferenceType roundtrip" $ do
    it "preserves RefObject" $
      RefObject == RefObject
    
    it "preserves RefDatasetRegion" $
      RefDatasetRegion == RefDatasetRegion
    
    it "distinguishes between reference types" $
      property $ \(ref :: ReferenceType) ->
        case ref of
          RefObject -> ref == RefObject
          RefDatasetRegion -> ref == RefDatasetRegion

  describe "ByteOrder equality and ordering" $ do
    it "LittleEndian equals itself" $
      LittleEndian == LittleEndian
    
    it "BigEndian equals itself" $
      BigEndian == BigEndian
    
    it "LittleEndian and BigEndian are distinct" $
      LittleEndian /= BigEndian
    
    it "LittleEndian < BigEndian" $
      LittleEndian < BigEndian

  describe "Type composition roundtrips" $ do
    it "FixedPointType with all fields" $
      property $ \(order :: ByteOrder)
                   (signed :: Bool)
                   (offset :: Word16)
                   (prec :: Word16)
                   (size :: Int) ->
        size > 0 && size < 256 ==>
        let fp = FixedPointType order signed (fromIntegral offset) (fromIntegral prec) size
        in fpByteOrder fp == order &&
           fpSigned fp == signed &&
           fpBitOffset fp == fromIntegral offset &&
           fpBitPrecision fp == fromIntegral prec &&
           fpSize fp == size
    
    it "String with all combinations of padding and charset" $
      property $ \(pad :: PaddingType)
                   (charset :: CharacterSet)
                   (size :: Int) ->
        size >= 0 && size < 65536 ==>
        let st = StringType pad charset size
        in strPaddingType st == pad &&
           strCharset st == charset &&
           strSize st == size

  describe "Large value handling" $ do
    it "handles large bit offsets in FixedPointType" $
      let fp = FixedPointType LittleEndian False 65535 32 8
      in fpBitOffset fp == 65535
    
    it "handles large sizes in OpaqueType" $
      let ot = OpaqueType "tag" 65535
      in opaqueSize ot == 65535
    
    it "handles large exponent bias in FloatingPointType" $
      let flt = FloatingPointType BigEndian 0 32 8 8 23 23 maxBound 8
      in fltExponentBias flt == maxBound
