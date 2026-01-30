# Changelog

## rsyncrosim 2.1.11

### Breaking changes:

### Bug fixes:

- Improved column matching when populating datasheets to treat column
  names case-insensitively, reducing mismatche
- When a case-insensitive match is found, sheet column names are aligned
  to the canonical names for consistent outpu
- Preserved previous behaviors for ID assignment and full-selection
  imports to maintain compatibility
- Restrict renaming of “ID”→“Id” to primary key and foreign key columns
  only, avoiding unintended renames in other column

### New features:

### Minor improvements and fixes:

- Updated package dependencies

### Deprecations:

- datasheetSpatRaster() is now deprecated; use datasheet() instead to
  obtain raster file paths.
