# GitHub Actions CI/CD Workflows

This project includes three automated CI/CD workflows that use the official Haskell setup action and our Makefile recipes.

## Workflows Overview

### 1. Quick Check (quick-check.yml) ⚡
**Trigger**: Every push to `main` and all pull requests
**Duration**: ~5-10 minutes

Lightweight workflow for fast feedback:
- Builds the library
- Runs all 56 tests
- Reports project status

**Best for**: Catching immediate build/test failures on every commit

### 2. Stack CI (stack.yml) 📦
**Trigger**: Push to `main`/`develop`, pull requests, weekly schedule
**Duration**: ~10-15 minutes per matrix combination

Comprehensive Stack-focused testing:
- Tests on Ubuntu and macOS
- Parallel test matrix (unit, integration, coverage)
- Downloads HDF5 test files
- Caches dependencies for speed

**Jobs**:
- **stack**: Primary build and test on multiple OS
- **benchmark**: Performance benchmarking suite (parseSuperblockFromFile, discoverDatasets)
- **test-matrix**: Parallel test suite execution
- **quick-check**: Sanity check for code consistency

### 3. Haskell CI (haskell.yml) 🔬
**Trigger**: Push to `main`/`develop`, pull requests
**Duration**: ~20-30 minutes per matrix combination

Advanced multi-version testing:
- Tests on Ubuntu, macOS, and Windows
- Multiple GHC versions (9.8.1, 9.10.1)
- Code quality checks (HLint)
- Code coverage analysis
- Comprehensive diagnostics

**Jobs**:
- **build**: Matrix testing across OS and GHC versions
- **lint**: HLint code quality checks
- **coverage**: Test coverage with Codecov upload
- **diagnostics**: Project health diagnostics

## How Workflows Use the Makefile

All workflows leverage our Makefile recipes for consistency:

```yaml
- name: Build
  run: make build

- name: Run tests
  run: make test

- name: Run unit tests only
  run: make test-unit

- name: Download test files
  run: |
    chmod +x download-test-files.sh
    ./download-test-files.sh

- name: Show status
  run: make status
```

### Available Recipes Used

| Recipe | Purpose |
|--------|---------|
| `make build` | Build library and tests |
| `make test` | Run all 56 tests |
| `make test-unit` | Run unit tests only |
| `make test-integration` | Run integration tests only |
| `make coverage` | Run tests with coverage |
| `make bench` | Run benchmark suite |
| `make status` | Show project build status |
| `make info` | Display project information |
| `make diagnose` | Run full diagnostics |

## Workflow Details

### Quick Check Workflow

```yaml
name: Quick Check

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  quick-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
      - run: make build
      - run: make test
      - run: make status
```

**Key features**:
- Fast feedback on every commit
- Single OS (Ubuntu)
- Comprehensive caching
- Reports final status

### Stack Workflow

Includes:
- **stack job**: Tests on Ubuntu and macOS
- **test-matrix job**: Parallel execution of unit/integration/coverage tests
- **quick-check job**: Basic sanity check

```yaml
matrix:
  os: [ubuntu-latest, macos-latest]
  ghc: ['9.10.3']
```

### Haskell Workflow

Includes:
- **build job**: Tests multiple GHC versions and operating systems
- **lint job**: HLint code quality checks
- **coverage job**: Code coverage with Codecov
- **diagnostics job**: Project health checks

```yaml
matrix:
  os: [ubuntu-latest, macos-latest, windows-latest]
  ghc: ['9.8.1', '9.10.1']
```

## Caching Strategy

All workflows implement intelligent caching:

```yaml
- uses: actions/cache@v3
  with:
    path: |
      ~/.stack
      .stack-work
    key: ${{ runner.os }}-stack-${{ hashFiles('stack.yaml') }}
```

**Benefits**:
- Faster builds after first run
- Reduced network traffic
- Typical subsequent build: ~2-3 minutes (vs 10+ uncached)

## Test Files

All workflows download HDF5 test files:

```yaml
- name: Download test files
  run: |
    chmod +x download-test-files.sh
    ./download-test-files.sh
```

This ensures integration tests have access to official HDF5 files from the HDFGroup repository.

## Viewing Workflow Results

### On GitHub

1. Go to the repository
2. Click **Actions** tab
3. Select a workflow run
4. View logs and status for each job

### Local Testing

Run the same recipes locally:

```bash
# Quick check (what CI runs)
make quick

# Full suite
make test

# Verbose output
make test-verbose

# Specific tests
make test-specific PATTERN="ByteOrder"
```

## Workflow Status Badge

Add to README.md:

```markdown
![Build Status](https://github.com/ocramz/hdf5-direct/workflows/Quick%20Check/badge.svg)
```

## Troubleshooting

### Build Fails on Windows

Some Haskell tools have limited Windows support. Check:
- Stack version compatibility
- GHC version availability
- Path separators in commands

### Cache Issues

If tests fail unexpectedly:
1. Clear cache in GitHub Actions settings
2. Rerun the workflow
3. Check if stack.yaml has changed

### Long Build Times

First build is slow due to:
- Dependency compilation
- Test file downloads
- No prior cache

Subsequent builds use cache and complete in 2-3 minutes.

## Best Practices

1. **Quick Check**: Fast feedback, run on every push
2. **Stack CI**: Comprehensive testing, run on releases
3. **Haskell CI**: Deep testing, run on main branch

## Extending Workflows

### Add a new job

```yaml
  custom-job:
    name: Custom Job
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
      - run: make custom-recipe  # If recipe exists
```

### Add a new OS

```yaml
matrix:
  os: [ubuntu-latest, macos-latest, windows-latest]  # Add windows-latest
```

### Add a new GHC version

```yaml
matrix:
  ghc: ['9.8.1', '9.10.1', '9.12.1']  # Add new version
```

## Related Files

- **Makefile** - All available recipes for CI
- **stack.yaml** - Stack configuration
- **hdf5-direct.cabal** - Project dependencies
- **download-test-files.sh** - Test file downloader

## Quick Reference

| File | Purpose | Trigger |
|------|---------|---------|
| `quick-check.yml` | Fast build + test | Every push to main |
| `stack.yml` | Stack-focused CI | main/develop, weekly |
| `haskell.yml` | Multi-version testing | main/develop |

---

## Example: Full Test Run

A complete `haskell.yml` workflow run includes:

```
✓ Checkout code
✓ Setup Haskell (GHC 9.10.1)
✓ Cache dependencies
✓ Build library (make build)
✓ Run all 56 tests (make test)
✓ Run unit tests (make test-unit)
✓ Generate documentation (make docs-gen)
✓ Run HLint checks
✓ Generate coverage reports
✓ Run diagnostics

Total time: ~15 minutes
```

All recipes are consistent between local development and CI, ensuring "it works on my machine" issues are rare.
