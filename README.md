# hdf5-direct

[![Build Status](https://travis-ci.org/ocramz/hdf5-direct.png)](https://travis-ci.org/ocramz/hdf5-direct)
[![Quick Check](https://github.com/ocramz/hdf5-direct/workflows/Quick%20Check/badge.svg)](https://github.com/ocramz/hdf5-direct/actions/workflows/quick-check.yml)
[![Stack CI](https://github.com/ocramz/hdf5-direct/workflows/Stack%20CI/badge.svg)](https://github.com/ocramz/hdf5-direct/actions/workflows/stack.yml)
[![Haskell CI](https://github.com/ocramz/hdf5-direct/workflows/Haskell%20CI/badge.svg)](https://github.com/ocramz/hdf5-direct/actions/workflows/haskell.yml)

Direct HDF5 bindings for Haskell with lazy-loading via memory-mapped I/O.

## Quick Start

```bash
# Show available tasks
make help

# Build the library
make build

# Run all 56 tests
make test

# Generate documentation
make docs
```

## Common Commands

### Building & Testing
```bash
make build           # Build the library
make rebuild         # Clean and rebuild everything
make test            # Run all 56 tests (42 unit + 14 integration)
make test-unit       # Run only unit tests (42 tests)
make test-integration # Run only integration tests (14 tests)
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

- ✅ All 11 HDF5 datatype classes
- ✅ Lazy-loading via memory-mapped I/O
- ✅ Zero-copy file access
- ✅ Type-safe Haskell bindings
- ✅ Comprehensive test suite (56 tests, 100% passing)
- ✅ Real HDF5 file integration
- ✅ Automated CI/CD with GitHub Actions

## Build Requirements

- GHC 9.10.3+
- Stack
- Core libraries: base, bytestring, binary, mmap

## Continuous Integration

The project includes automated CI/CD workflows:
- **Quick Check**: Fast build & test on every push
- **Stack CI**: Comprehensive testing with caching
- **Haskell CI**: Multi-version testing (GHC 9.8.1, 9.10.1)

See [CI.md](CI.md) for detailed workflow documentation.

## Test Files

The project includes 6 official HDF5 test files from the HDFGroup repository:

```bash
make check-tests     # List all available test files
make download-tests  # Download additional test files
```
