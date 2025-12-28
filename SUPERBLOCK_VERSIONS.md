# Superblock Version Support Enhancement

## Current Status
- ✅ v0/v1 - Full support (addresses at offset 32)
- ⚠️ v2/v3 - Partial support (recognizes versions but uses simple offset calculation)

## What Needs Enhancement

### v0/v1 Format (addresses at offset 32)
```
0:  HDF5 signature (8 bytes)
8:  Version (1 byte)
9:  Free-space storage (1 byte)
10: Root group symbol table offset (8 bytes)
18: Root group symbol table heap address (8 bytes)
26: Checksum (4 bytes)
```

But offsets are specified in bytes 10-11:
```
10: Offset size (1 byte)
11: Length size (1 byte) 
```

Root group address at offset 32 (8 bytes).

### v2 Format (addresses at offset 20)
```
0:  HDF5 signature (8 bytes)
8:  Version (1 byte)
9:  Offset size (1 byte)
10: Length size (1 byte)
11: File consistency flags (1 byte)
12: Base address (8 bytes)
20: Root group object header address (8 bytes) ← Different position!
28: Checksum (4 bytes)
```

Additional fields at 20+8 for v2+.

### v3 Format (same as v2)
Same as v2 but with newer field definitions.

## Implementation Notes

The current code uses:
- v0/v1: read sizes from bytes 10-11, address from 32
- v2/v3: read sizes from bytes 9-10, address from 20

This is mostly correct but needs:
1. Better documentation of the structure
2. Validation of file consistency flags (v2/v3)
3. Handling of base address (v2/v3 at offset 12)
4. Support for extension addressing (superblock extension)
