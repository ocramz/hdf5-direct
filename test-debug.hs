{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Bits
import Data.Int
import Numeric (showHex)

-- Read address at given offset
word64LEAt :: BL.ByteString -> Int64 -> Either String Word64
word64LEAt bs off
  | BL.length bs < off + 8 = Left "Out of bounds"
  | otherwise =
    let bytes = map fromIntegral $ BL.unpack $ BL.take 8 $ BL.drop (fromIntegral off) bs
        val = foldl (\acc (i, b) -> acc .|. (fromIntegral b `shiftL` (i * 8))) 0 (zip [0..7] bytes)
    in Right val

byteAt :: BL.ByteString -> Int64 -> Either String Word8
byteAt bs off
  | BL.length bs <= off = Left "Out of bounds"
  | otherwise = Right $ BL.index bs off

main :: IO ()
main = do
  bs <- BL.readFile "test-data/kosarak-jaccard.hdf5"
  
  putStrLn "=== Kosarak HDF5 Debug ==="
  putStrLn $ "File size: " ++ show (BL.length bs) ++ " bytes"
  
  -- Check superblock
  putStrLn "\n=== Superblock at offset 0x20 ==="
  case word64LEAt bs 0x20 of
    Right addr -> putStrLn $ "Root addr @ 0x20: 0x" ++ showHex addr "" ++ if addr == 0xFFFFFFFFFFFFFFFF then " (EXTENDED)" else ""
    Left err -> putStrLn $ "Error: " ++ err
  
  -- Check extended address at 0x40
  putStrLn "\n=== Extended superblock at offset 0x40 ==="
  case word64LEAt bs 0x40 of
    Right addr -> putStrLn $ "Actual root addr @ 0x40: 0x" ++ showHex addr " (" ++ show addr ++ " decimal)"
    Left err -> putStrLn $ "Error: " ++ err
  
  -- Parse object header at 0x60
  putStrLn "\n=== Object Header at offset 0x60 ==="
  case byteAt bs 0x60 of
    Right ver -> putStrLn $ "Version: " ++ show ver
    Left err -> putStrLn $ "Error: " ++ err
  
  case (byteAt bs 0x62, byteAt bs 0x63) of
    (Right lo, Right hi) -> do
      let numMsgs = fromIntegral lo .|. (fromIntegral hi `shiftL` 8) :: Word16
      putStrLn $ "Number of messages: " ++ show numMsgs
    (Left err, _) -> putStrLn $ "Error: " ++ err
    (_, Left err) -> putStrLn $ "Error: " ++ err
  
  -- Look for message type 0x11 (symbol table)
  putStrLn "\n=== Searching for Symbol Table Message (0x11) ==="
  let searchStart = 0x60 + 16  -- Messages start at +16 from object header
  let scanMessages offset depth
        | depth > 20 = putStrLn "Depth limit reached"
        | offset >= 0x400 = putStrLn "Search limit reached"
        | otherwise = do
          case (byteAt bs offset, byteAt bs (offset + 1)) of
            (Right lo, Right hi) -> do
              let msgType = fromIntegral lo .|. (fromIntegral hi `shiftL` 8) :: Word16
              case (byteAt bs (offset + 2), byteAt bs (offset + 3)) of
                (Right slo, Right shi) -> do
                  let msgSize = fromIntegral slo .|. (fromIntegral shi `shiftL` 8) :: Word16
                  putStrLn $ "  Offset 0x" ++ showHex offset "" ++ ": Type 0x" ++ showHex msgType "" ++ " (" ++ show msgType ++ "), Size: " ++ show msgSize
                  
                  when (msgType == 0x11) $ do
                    putStrLn "    >>> SYMBOL TABLE MESSAGE FOUND <<<"
                    -- Message data starts at offset + 8 (8-byte header)
                    case (word64LEAt bs (offset + 8), word64LEAt bs (offset + 16)) of
                      (Right btree, Right heap) -> do
                        putStrLn $ "    B-tree addr: 0x" ++ showHex btree ""
                        putStrLn $ "    Heap addr: 0x" ++ showHex heap ""
                      _ -> putStrLn "    Error reading addresses"
                  
                  -- Calculate next offset with correct padding
                  let totalBytes = 8 + fromIntegral msgSize  -- 8-byte header + data
                  let pad = ((8 - (totalBytes `mod` 8)) `mod` 8)
                  let nextOff = offset + totalBytes + pad
                  scanMessages nextOff (depth + 1)
                _ -> putStrLn "  Error reading message size"
            (Left _, _) -> putStrLn $ "  Error at offset 0x" ++ showHex offset ""
            (_, Left _) -> putStrLn $ "  Error at offset 0x" ++ showHex offset ""
  
  scanMessages searchStart 0
  
  where
    when True action = action
    when False _ = return ()
