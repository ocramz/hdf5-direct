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

# Priority 1 - Basic Structure Tests
echo "Downloading Priority 1 files (basic structure)..."
for file in be_data.h5 le_data.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null || echo "    Warning: Failed to download $file"
  else
    echo "  Already have $file"
  fi
done

# Priority 2 - Datatype Tests
echo "Downloading Priority 2 files (datatype coverage)..."
for file in charsets.h5 bitops.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null || echo "    Warning: Failed to download $file"
  else
    echo "  Already have $file"
  fi
done

# Priority 3 - Complex Structure Tests (selection)
echo "Downloading Priority 3 files (complex structures)..."
for file in aggr.h5 ref_deprecated.h5; do
  if [ ! -f "$TEST_DATA_DIR/$file" ]; then
    echo "  Downloading $file..."
    curl -L -o "$TEST_DATA_DIR/$file" "$REPO_URL/$file" 2>/dev/null || echo "    Warning: Failed to download $file"
  else
    echo "  Already have $file"
  fi
done

# List downloaded files
echo ""
echo "Downloaded test files:"
ls -lh "$TEST_DATA_DIR"/*.h5 2>/dev/null || echo "No .h5 files found in $TEST_DATA_DIR"

echo ""
echo "Download complete!"
echo "To run integration tests: stack test"
