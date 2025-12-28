#!/bin/bash

# Download kosarak-jaccard.hdf5 from ann-benchmarks for integration testing
# This is a real-world HDF5 file with substantial data

set -e

KOSARAK_URL="http://ann-benchmarks.com/kosarak-jaccard.hdf5"
KOSARAK_FILE="test-data/kosarak-jaccard.hdf5"
TEST_DIR="test-data"

echo "Downloading kosarak-jaccard.hdf5 for integration testing..."
echo "Source: $KOSARAK_URL"

# Create test-data directory if it doesn't exist
mkdir -p "$TEST_DIR"

# Check if file already exists
if [ -f "$KOSARAK_FILE" ]; then
    echo "✓ kosarak-jaccard.hdf5 already exists"
    ls -lh "$KOSARAK_FILE"
    exit 0
fi

# Download the file
echo "Downloading... (this may take a moment)"
if curl -f -L -o "$KOSARAK_FILE" "$KOSARAK_URL"; then
    echo "✓ Successfully downloaded kosarak-jaccard.hdf5"
    ls -lh "$KOSARAK_FILE"
    
    # Show file info
    echo ""
    echo "File size: $(du -h "$KOSARAK_FILE" | cut -f1)"
else
    echo "✗ Failed to download from $KOSARAK_URL"
    echo ""
    echo "You can manually download it from:"
    echo "  $KOSARAK_URL"
    echo "And place it at: $KOSARAK_FILE"
    exit 1
fi
