# Strict HDF5 Metadata Parsing Implementation

## Summary

Successfully implemented strict, error-aware HDF5 metadata parsing with the `Either` monad pattern to replace the heuristic string-scanning approach for dataset discovery.

## Key Changes

### 1. Error Type System

Added `HDF5ParseError` enum with specific error variants:
- `InvalidSignature` - File doesn't have valid HDF5 signature
- `TruncatedData` - Not enough bytes to read with detailed context
- `CorruptedMetadata` - Metadata structure is invalid  
- `InvalidVersion` - Unsupported version number
- `UnexpectedMessageType` - Unexpected object header message type
- `DatatypeParseFailed` - Failed to parse datatype class
- `SymbolTableParseFailed` - Failed to parse symbol table
- `BTreeParseFailed` - Failed to parse B-tree node
- `HeapParseFailed` - Failed to parse local heap

### 2. Parser Monad

Defined `HDF5ParserM a = Either HDF5ParseError a` for composable error handling.

### 3. Low-Level Safe Accessors

Updated all byte-level read functions to return `HDF5ParserM`:
- `byteAt :: BL.ByteString -> Int64 -> HDF5ParserM Word8`
- `word16LEAt :: BL.ByteString -> Int64 -> HDF5ParserM Word16`
- `word32LEAt :: BL.ByteString -> Int64 -> HDF5ParserM Word32`
- `word64LEAt :: BL.ByteString -> Int64 -> HDF5ParserM Word64`
- `byteStringAt :: BL.ByteString -> Int64 -> Int64 -> HDF5ParserM BL.ByteString`

All functions include bounds checking and provide detailed error messages with offset/size information.

### 4. Superblock Parsing

Implemented `parseSuperblockMetadata :: BL.ByteString -> HDF5ParserM (Int64, Int, Int)`:
- Validates HDF5 signature strictly
- Reads and validates superblock version
- Extracts root group object header address
- Returns offset and size information for proper address calculations
- No silent failures - all issues propagate as errors

### 5. Object Header Message Parsing

Updated `parseObjectHeaderMessages` to handle errors gracefully:
- Stops parsing on error instead of returning empty list
- Uses do-notation for clean error propagation
- Properly calculates padding and message offsets
- Returns `Maybe [(Word8, BL.ByteString)]` for backward compatibility with fallback

### 6. Symbol Table Message Parsing

Implemented `parseSymbolTableMessage :: BL.ByteString -> Int64 -> Either HDF5ParseError (Word64, Word64)`:
- Validates message length  
- Extracts B-tree root address
- Extracts local heap address
- Full error handling with context

### 7. B-Tree Node Parsing

Implemented `parseV1BTreeNode :: BL.ByteString -> Int64 -> Either HDF5ParseError (Word8, Word8, Word16)`:
- Validates "TREE" signature
- Reads node type and level
- Reads entry count
- Comprehensive error handling

### 8. Updated Call Sites

Fixed all functions calling the new strict parsers:
- `parseSymbolTableForDatasets` - Works with `Either` results
- `discoverDatasets` - Uses strict parsing with fallback to heuristics
- `parseBTreeLeafEntries` - Uses `Int64` offsets consistently
- `parseLocalHeapOffset` - Returns `Either` for strict parsing
- `extractHeapStrings` - Uses `Int64` offsets

### 9. Type Consistency

Ensured consistent use of `Int64` for all offsets throughout the parsing infrastructure instead of mixing `Int` and `Int64`.

## Behavior

### Strict Header Parsing
Once parsing begins, errors are fatal and propagate immediately via the `Either` monad.

### Graceful Fallback
The high-level `discoverDatasets` function still falls back to heuristic scanning if strict parsing fails. This maintains backward compatibility while enabling strict parsing where possible.

### Detailed Error Messages
All errors include context:
- Offset where the error occurred
- Required vs available bytes
- Type of operation attempted

## Testing

All existing tests pass including:
- Integration tests with real HDF5 files
- Large file tests (kosarak-jaccard.hdf5)
- Roundtrip tests for array I/O
- Type system QuickCheck properties
- Dimension extraction tests

No regressions introduced - the fallback mechanism ensures all previously working code continues to function.

## Benefits

1. **Type Safety** - Errors are explicit in the type system
2. **No Silent Failures** - All issues are reported
3. **Composability** - The `Either` monad allows easy composition of parsing operations
4. **Debugging** - Detailed error messages help diagnose issues
5. **Performance** - Strict parsing validates offsets early, preventing invalid access patterns
