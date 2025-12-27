{-# LANGUAGE ScopedTypeVariables #-}

module MassivMultidimSpec where

import Test.Hspec
import Data.HDF5.Direct.Massiv
import qualified Data.Massiv.Array as M
import Data.Int (Int32)
import Data.Word (Word32)

spec :: Spec
spec = do
  describe "1D Arrays" $ do
    it "creates a 1D dataset" $
      (createDataset1D 10 (0 :: Int32) :: Either MassivError (HDF5Dataset Int32)) `shouldSatisfy` either (const False) (const True)
    
    it "gets dimensions from 1D dataset" $ do
      case createDataset1D 5 (42 :: Word32) of
        Right ds -> case getDatasetDimensions ds of
          (Dims1D (M.Ix1 n)) -> n `shouldBe` 5
          _ -> fail "Wrong dimension type"
        Left err -> fail (show err)

    it "converts 1D Massiv array to HDF5 and back" $ do
      let arr = M.fromList M.Par [1..10 :: Int32]
      case fromMassivArray1D arr of
        Right ds -> case toMassivArray1D ds of
          Right arr' -> M.toList arr' `shouldBe` M.toList arr
          Left err -> fail (show err)
        Left err -> fail (show err)

  describe "2D Arrays" $ do
    it "creates a 2D dataset" $
      (createDataset2D 4 5 (0 :: Int32) :: Either MassivError (HDF5Dataset Int32)) `shouldSatisfy` either (const False) (const True)

    it "gets dimensions from 2D dataset" $ do
      case createDataset2D 3 4 (99 :: Word32) of
        Right ds -> case getDatasetDimensions ds of
          (Dims2D (M.Ix2 n1 n2)) -> do
            n1 `shouldBe` 3
            n2 `shouldBe` 4
          _ -> fail "Wrong dimension type"
        Left err -> fail (show err)

    it "converts 2D Massiv array to HDF5 and back" $ do
      let arr = M.makeArray M.Par (M.Sz2 2 3) (\(M.Ix2 i j) -> (fromIntegral i * 3 + fromIntegral j) :: Int32)
      case fromMassivArray2D arr of
        Right ds -> case toMassivArray2D ds of
          Right arr' -> M.toList arr' `shouldBe` M.toList arr
          Left err -> fail (show err)
        Left err -> fail (show err)

  describe "3D Arrays" $ do
    it "creates a 3D dataset" $
      (createDataset3D 2 3 4 (0 :: Int32) :: Either MassivError (HDF5Dataset Int32)) `shouldSatisfy` either (const False) (const True)

    it "gets dimensions from 3D dataset" $ do
      case createDataset3D 2 3 4 (77 :: Word32) of
        Right ds -> case getDatasetDimensions ds of
          (Dims3D (M.Ix3 n1 n2 n3)) -> do
            n1 `shouldBe` 2
            n2 `shouldBe` 3
            n3 `shouldBe` 4
          _ -> fail "Wrong dimension type"
        Left err -> fail (show err)

    it "converts 3D Massiv array to HDF5 and back" $ do
      let arr = M.makeArray M.Par (M.Sz3 2 2 2) (\(M.Ix3 i j k) -> (fromIntegral i * 4 + fromIntegral j * 2 + fromIntegral k) :: Int32)
      case fromMassivArray3D arr of
        Right ds -> case toMassivArray3D ds of
          Right arr' -> M.toList arr' `shouldBe` M.toList arr
          Left err -> fail (show err)
        Left err -> fail (show err)

  describe "Dimension Extraction" $ do
    it "extracts 1D dimensions" $ do
      case extractDimensions [10] of
        Right (Dims1D (M.Ix1 n)) -> n `shouldBe` 10
        _ -> fail "Failed to extract 1D dimensions"

    it "extracts 2D dimensions" $ do
      case extractDimensions [5, 4] of
        Right (Dims2D (M.Ix2 n1 n2)) -> do
          n1 `shouldBe` 5
          n2 `shouldBe` 4
        _ -> fail "Failed to extract 2D dimensions"

    it "extracts 3D dimensions" $ do
      case extractDimensions [2, 3, 4] of
        Right (Dims3D (M.Ix3 n1 n2 n3)) -> do
          n1 `shouldBe` 2
          n2 `shouldBe` 3
          n3 `shouldBe` 4
        _ -> fail "Failed to extract 3D dimensions"

    it "extracts higher rank dimensions" $ do
      case extractDimensions [2, 3, 4, 5] of
        Right (DimsHigherRank dims) -> dims `shouldBe` [2, 3, 4, 5]
        _ -> fail "Failed to extract higher rank dimensions"

    it "rejects empty dimensions" $
      extractDimensions [] `shouldSatisfy` either (const True) (const False)

    it "rejects non-positive dimensions" $ do
      extractDimensions [0, 5] `shouldSatisfy` either (const True) (const False)
      extractDimensions [-1, 5] `shouldSatisfy` either (const True) (const False)

  describe "Dimension Queries" $ do
    it "queries 1D dimensions at rank 1" $ do
      case extractDimensions [10] of
        Right dims -> case getDimensionsAtRank Rank1D dims of
          Right [n] -> n `shouldBe` 10
          _ -> fail "Failed to query 1D dimensions"
        _ -> fail "Failed to extract dimensions"

    it "rejects 1D dimensions queried as 2D" $ do
      case extractDimensions [10] of
        Right dims -> case getDimensionsAtRank Rank2D dims of
          Left _ -> True `shouldBe` True
          _ -> fail "Should reject mismatched rank"
        _ -> fail "Failed to extract dimensions"

    it "queries 2D dimensions at rank 2" $ do
      case extractDimensions [5, 4] of
        Right dims -> case getDimensionsAtRank Rank2D dims of
          Right [n1, n2] -> do
            n1 `shouldBe` 5
            n2 `shouldBe` 4
          _ -> fail "Failed to query 2D dimensions"
        _ -> fail "Failed to extract dimensions"

main :: IO ()
main = hspec spec
