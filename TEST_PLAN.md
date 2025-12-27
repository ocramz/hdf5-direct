# HDF5 Direct - Test Plan and Integration Strategy

## Overview
This document outlines the testing strategy for the HDF5 Direct library, which provides lazy-loading binary HDF5 file access using memory-mapped I/O.

## Current Testing Status

### Unit Tests (42 tests - ALL PASSING ✓)
All core datatype classes, exception handling, and lazy evaluation properties are tested:
- **Exception Handling** (3 tests): MmapIOError, PageFaultError, ParseError
- **Byte Order Handling** (2 tests): LittleEndian, BigEndian construction and equality
- **All 11 HDF5 Datatype Classes** (23+ tests):
  - Class 0: FixedPointType (signed/unsigned integers)
  - Class 1: FloatingPointType (IEEE 754-like floats)
  - Class 2: TimeType (timestamp data)
  - Class 3: StringType (fixed/variable strings)
  - Class 4: BitfieldType (bit-packed data)
  - Class 5: OpaqueType (opaque binary data)
  - Class 6: CompoundType (struct-like types)
  - Class 7: ReferenceType (object/region references)
  - Class 8: EnumerationType (enumerated values)
  - Class 9: VariableLengthType (variable-length sequences/strings)
  - Class 10: ArrayType (multi-dimensional arrays)
- **Type System** (9 tests): Eq/Ord instances, Generic derivation, type discrimination
- **Resource Management** (2 tests): mmap file operations, bracket pattern cleanup
- **Lazy Evaluation** (2 tests): Construction speed, nested datatype correctness

### Integration Tests (9 tests - 7 PASSING, 2 PENDING)
Framework tests for HDF5 file structure parsing:
- **File Signature Verification** (1 test): Validates HDF5 magic bytes
- **Superblock Parsing** (1 test): Version information extraction
- **Datatype Message Parsing** (4 tests): Binary representation, fixed/variable-length types
- **Compound Datatypes** (1 test): Member parsing framework
- **Error Handling** (1 test): Graceful failure on missing files
- **Pending Tests** (2): Require actual HDF5 test files from HDFGroup

## Test Files to Download

From the official HDFGroup repository: https://github.com/HDFGroup/hdf5/tree/develop/test/testfiles

### Priority 1 - Basic Structure Tests (Recommended first)
These files help validate our superblock and file structure parsing:
- `be_data.h5` - Big-endian test data (validates byte order handling)
- `le_data.h5` - Little-endian test data (validates byte order handling)
- `h5_file_image.h5` - In-memory HDF5 file representation

### Priority 2 - Datatype Tests
These files test our datatype class implementations:
- `charsets.h5` - String charset handling (StringType class)
- `bitops.h5` - Bitfield operations (BitfieldType class)
- `dtypes*.h5` - Comprehensive datatype coverage
- `vlen*.h5` - Variable-length sequence tests (VariableLengthType class)

### Priority 3 - Complex Structure Tests
These files test more advanced features:
- `aggr.h5` - Aggregated data structures
- `compound.h5` - Compound datatype tests (CompoundType class)
- `array*.h5` - Array datatype tests (ArrayType class)
- `ref.h5` - Reference datatype tests (ReferenceType class)
- `enum*.h5` - Enumeration datatype tests (EnumerationType class)

### Priority 4 - Performance and Stress Tests
These test lazy loading and resource management:
- `large_*.h5` - Large file handling (tests lazy page faults)
- `chunked*.h5` - Chunked dataset tests
- `compress*.h5` - Compressed data handling (if supported)

## Testing Roadmap

### Phase 1: File Format Validation (Current)
✓ Unit tests for all datatype classes complete
✓ Exception handling and resource management validated
→ Download Priority 1 files and add signature/superblock tests

### Phase 2: Datatype Parsing (Next)
→ Implement full Binary parsers for recursive types (Compound, Enumeration, Array, VariableLength)
→ Test against Priority 2 files
→ Validate lazy evaluation on real file data

### Phase 3: Complex Structures (Later)
→ Test compound types with nested members
→ Test references and cross-object access patterns
→ Validate resource cleanup on large files

### Phase 4: Performance Benchmarking (Optional)
→ Compare lazy vs eager loading on Priority 4 files
→ Measure page fault overhead
→ Profile memory usage patterns

## Building and Running Tests

```bash
# Run all unit and integration tests
stack test

# Run with verbose output
stack test --verbose

# Run specific test module
stack test --test-arguments='-m "ByteOrder"'

# Run with coverage (requires hpc)
stack test --coverage
```

## Expected Test Coverage

After all phases complete:
- **Unit Tests**: 100% of datatype classes
- **Integration Tests**: All 11 datatype classes with real HDF5 files
- **Resource Management**: Cleanup verified on large files
- **Error Handling**: All exception types tested with real failure scenarios
- **Lazy Evaluation**: Verified with actual page-fault benchmarks

## Notes on Test Design

### Lazy Evaluation
The library is designed for lazy evaluation - expensive operations (file I/O, parsing) are deferred until the data is accessed. Tests verify:
1. Construction is instantaneous even for nested types
2. Actual parsing happens on field access
3. No caching overhead from mmap (Haskell's native laziness handles thunk retention)
4. Resource cleanup is automatic via GC finalizers

### Exception Safety
Tests verify that:
1. All HDF5Exception types are properly caught and displayed
2. File handle cleanup occurs even when parsing fails
3. Signal-based page faults (SIGSEGV/SIGBUS) are converted to HDF5Exception
4. Bracket pattern ensures no resource leaks

### Type Safety
Tests validate Haskell's type system ensures:
1. Cannot accidentally mix datatypes from different classes
2. Binary parsers use exact field sizes from HDF5 spec
3. Recursive types (Compound, Array) correctly maintain type invariants

## Future Enhancements

- Performance benchmarking framework
- Automated testing against all HDFGroup test files
- Property-based testing with QuickCheck for parser robustness
- Fuzzing framework for binary parsing safety
