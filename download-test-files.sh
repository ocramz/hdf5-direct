#!/bin/bash
# Download HDF5 test files from the official HDFGroup repository
# These files are used to test the hdf5-direct library against real HDF5 files

set -e  # Exit on error

REPO_URL="https://raw.githubusercontent.com/HDFGroup/hdf5/develop/test/testfiles"
TEST_DATA_DIR="test-data"

# Create test-data directory if it doesn't exist
mkdir -p "$TEST_DATA_DIR"

echo "Downloading HDF5 test files from $REPO_URL"
echo "Saving to $TEST_DATA_DIR/"

# Priority 1 - Basic Structure Tests (Byte Order)
echo ""
echo "=== Priority 1: Basic Structure Tests (byte order endianness) ==="
for file in be_data.h5 le_data.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    if curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null; then
      echo "    ✓ Downloaded $file"
    else
      echo "    ✗ Failed to download $file"
    fi
  else
    echo "  Already have $file"
  fi
done

# Priority 2 - Datatype Tests (Various Datatypes)
echo ""
echo "=== Priority 2: Datatype Tests (string charsets, bitops) ==="
for file in charsets.h5 bitops.h5 dtype_attr.h5 dtype_objatt.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    if curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null; then
      echo "    ✓ Downloaded $file"
    else
      echo "    ✗ Failed to download $file (may not exist)"
    fi
  else
    echo "  Already have $file"
  fi
done

# Priority 3 - Complex Structure Tests
echo ""
echo "=== Priority 3: Complex Structures (aggregated data, references) ==="
for file in aggr.h5 ref_deprecated.h5 array.h5 enum.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    if curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null; then
      echo "    ✓ Downloaded $file"
    else
      echo "    ✗ Failed to download $file (may not exist)"
    fi
  else
    echo "  Already have $file"
  fi
done

# Priority 4 - Attribute Tests
echo ""
echo "=== Priority 4: Attribute Tests (metadata) ==="
for file in attr.h5 attrib.h5 simple_attr.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    if curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null; then
      echo "    ✓ Downloaded $file"
    else
      echo "    ✗ Failed to download $file (may not exist)"
    fi
  else
    echo "  Already have $file"
  fi
done

# Priority 5 - Dataset Tests
echo ""
echo "=== Priority 5: Dataset Tests (various dataset structures) ==="
for file in dataset.h5 dset_bzip2.h5 dset_deflate.h5 simple.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    if curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null; then
      echo "    ✓ Downloaded $file"
    else
      echo "    ✗ Failed to download $file (may not exist)"
    fi
  else
    echo "  Already have $file"
  fi
done

# Priority 6 - Group Tests
echo ""
echo "=== Priority 6: Group Tests (hierarchy) ==="
for file in group.h5 groups.h5 grp_create.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    if curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null; then
      echo "    ✓ Downloaded $file"
    else
      echo "    ✗ Failed to download $file (may not exist)"
    fi
  else
    echo "  Already have $file"
  fi
done

# List downloaded files
echo ""
echo "=== Downloaded test files ==="
if ls "$TEST_DATA_DIR"/*.h5 2>/dev/null | head -20; then
  echo ""
  total=$(ls "$TEST_DATA_DIR"/*.h5 2>/dev/null | wc -l)
  size=$(du -sh "$TEST_DATA_DIR" 2>/dev/null | cut -f1)
  echo "Total files: $total"
  echo "Total size: $size"
else
  echo "No .h5 files found in $TEST_DATA_DIR"
fi

echo ""
echo "Download complete!"
echo "To run integration tests: stack test"

