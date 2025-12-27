# hdf5-direct

[![Quick Check](https://github.com/ocramz/hdf5-direct/workflows/Quick%20Check/badge.svg)](https://github.com/ocramz/hdf5-direct/actions/workflows/quick-check.yml)
[![Stack CI](https://github.com/ocramz/hdf5-direct/workflows/Stack%20CI/badge.svg)](https://github.com/ocramz/hdf5-direct/actions/workflows/stack.yml)
[![Haskell CI](https://github.com/ocramz/hdf5-direct/workflows/Haskell%20CI/badge.svg)](https://github.com/ocramz/hdf5-direct/actions/workflows/haskell.yml)

Direct HDF5 bindings for Haskell with lazy-loading via memory-mapped I/O and Massiv array integration.

## Overview

A pure Haskell library for reading HDF5 files with:
- **Memory-mapped I/O**: Zero-copy lazy loading of large datasets
- **Type-safe arrays**: Integration with the Massiv library for N-dimensional arrays
- **Structured errors**: Proper error handling without `String` error types
- **Resource safety**: Bracket-based API guarantees proper cleanup
- **Real-world testing**: 21 test files from HDFGroup + ann-benchmarks kosarak-jaccard.hdf5 (33MB)

## Quick Start

```bash
# Show available tasks
make help

# Build the library
make build

# Run all 149 tests (unit + integration)
make test

# Download HDF5 test files
bash download-test-files.sh

# Download real-world ann-benchmarks dataset (33MB)
bash download-kosarak.sh

# Generate documentation
make docs
```

## Common Commands

### Building & Testing
```bash
make build           # Build the library
make rebuild         # Clean and rebuild everything
make test            # Run all 149 tests
make test-unit       # Run unit tests
make test-integration # Run integration tests
make test-verbose    # Run tests with detailed output
make coverage        # Run tests with code coverage
```

### Development
```bash
make ghci            # Start interactive Haskell REPL
make watch           # Watch for changes and rebuild
make watch-test      # Watch for changes and run tests
make quick           # Quick build and test cycle
make dev             # Build, test, and start REPL
```

### Documentation
```bash
make docs            # Generate and open API docs
make docs-gen        # Generate docs without opening
make readme          # View README
make docs-index      # View documentation index
make test-results    # View test results documentation
```

### Cleaning
```bash
make clean           # Remove build artifacts
make distclean        # Full clean including dependencies
```

### Information
```bash
make info            # Show project information
make status          # Show build and test status
make check-tests     # List downloaded HDF5 test files
make version         # Show Stack and GHC versions
make diagnose        # Run full diagnostics
```

## Features

- ✅ **All 11 HDF5 datatype classes** (fixed-point, floating-point, strings, etc.)
- ✅ **Lazy-loading via mmap** - Zero-copy file access with deferred page faults
- ✅ **Massiv array integration** - Type-safe N-dimensional arrays (1D, 2D, 3D+)
- ✅ **Structured error handling** - Proper error types instead of String
- ✅ **Resource safety** - Bracket-based API with guaranteed cleanup
- ✅ **Comprehensive test suite** - 149 tests covering:
  - 21 HDFGroup test files (various datatypes and structures)
  - Real-world ann-benchmarks kosarak-jaccard.hdf5 (33MB)
  - Large-scale data handling (10K+ element arrays)
- ✅ **No unsafe functions** - No partial functions (head, tail, !!)
- ✅ **Automated CI/CD** - GitHub Actions for multi-version testing

## Build Requirements

- GHC 9.10.3+
- Stack
- Core libraries: base, bytestring, binary, mmap

## API Design

The library provides two layers of API:

### Bracket-based (Production)
Safe, resource-managed I/O operations that guarantee proper cleanup:

```haskell
-- Read 1D array from HDF5 file
withArray1DFromFile :: SupportedElement a
  => FilePath
  -> (Array U Ix1 a -> IO b)
  -> IO b

-- Same for 2D and 3D arrays
withArray2DFromFile, withArray3DFromFile :: ...
```

The `MmapFile` is guaranteed to stay open during the callback and automatically cleaned up afterward.

### Internal Helpers (Testing)
Pure functions for creating and converting arrays in tests:

```haskell
-- Create from element list
createDataset1D :: SupportedElement a => Int -> a -> Either MassivError (HDF5Dataset a)

-- Convert to Massiv arrays
toMassivArray1D :: SupportedElement a => HDF5Dataset a -> Either MassivError (Array U Ix1 a)

-- Convert from Massiv arrays
fromMassivArray1D :: SupportedElement a => Array U Ix1 a -> Either MassivError (HDF5Dataset a)
```

### Error Handling
Structured error types instead of strings:

```haskell
data MassivError
  = DimensionErr DimensionError
  | ConversionErr ConversionError
  | EmptyArrayError

data DimensionError = DimensionEmpty | DimensionNonPositive [Int] | DimensionRankMismatch

data ConversionError = NotOneDimensional | NotTwoDimensional | ... | ElementReadFailure ElementReadError
```

### Supported Element Types
- `Int8`, `Int16`, `Int32`, `Int64`
- `Word8`, `Word16`, `Word32`, `Word64`

All elements support both little-endian and big-endian byte orders.

## Continuous Integration

The project includes automated CI/CD workflows:
- **Quick Check**: Fast build & test on every push
- **Stack CI**: Comprehensive testing with caching
- **Haskell CI**: Multi-version testing (GHC 9.8.1, 9.10.1)

See [CI.md](CI.md) for detailed workflow documentation.

## Test Files

The project includes 21 official HDF5 test files from the HDFGroup repository, organized across 6 priority categories:

**Priority 1: Byte Order** (2 files)
- be_data.h5 - Big-endian test data
- le_data.h5 - Little-endian test data

**Priority 2: Datatypes** (4 files)
- charsets.h5 - String charset tests
- bitops.h5 - Bitfield operations
- dtype_attr.h5 - Datatype attributes
- dtype_objatt.h5 - Object attributes

**Priority 3: Complex Structures** (4 files)
- aggr.h5 - Aggregated data structures
- array.h5 - Array datatypes
- enum.h5 - Enumeration datatypes
- ref_deprecated.h5 - Reference types

**Priority 4: Attributes** (3 files)
- attr.h5 - Attribute tests
- attrib.h5 - Additional attributes
- simple_attr.h5 - Simple attributes

**Priority 5: Datasets** (4 files)
- dataset.h5 - Dataset structure tests
- dset_bzip2.h5 - BZIP2 compressed datasets
- dset_deflate.h5 - DEFLATE compressed datasets
- simple.h5 - Simple datasets

**Priority 6: Groups** (3 files)
- group.h5 - Group operations
- groups.h5 - Multiple groups
- grp_create.h5 - Group creation

```bash
make check-tests     # List all available test files
make download-tests  # Download additional test files from HDFGroup
bash download-test-files.sh  # Manual download script
```

## Real-World Data Testing

The project includes integration tests with real-world HDF5 data from the ann-benchmarks project:

**kosarak-jaccard.hdf5** (33 MB)
- Large-scale similarity search dataset
- Source: [ann-benchmarks.com](http://ann-benchmarks.com/)
- Used to validate performance with production-scale files
- Tests mmap handling of 33MB+ files
- Exercises lazy-loading on substantial datasets

```bash
# Download the ann-benchmarks kosarak-jaccard.hdf5 file
bash download-kosarak.sh
```

This file serves as a real-world integration test, validating that:
- The library can handle large files (>30MB)
- Memory-mapped I/O performs efficiently
- Resource management works correctly with production data
- Error handling is robust with complex HDF5 structures
