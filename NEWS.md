## AcidSingleCell 0.1.9 (2021-09-13)

### Minor changes

- Added `aggregate` method designed for `SingleCellExperiment`. Legacy method
  support for `aggregateCols` is still provided, but now works as a passthrough
  to `aggregate`, which is defined in AcidExperiment instead of here.

## AcidSingleCell 0.1.8 (2021-09-03)

### Minor changes

- Updated minimum dependencies and improved documentation.
- Improved code coverage, getting back to 100%.
- Improved CLI messages, using updated functions from AcidCLI.

## AcidSingleCell 0.1.7 (2021-03-02)

### Minor changes

- Added `logcounts` and `normcounts` as reexports.

## AcidSingleCell 0.1.6 (2021-02-26)

### Minor changes

- Renamed all instances of "blacklist" to "denylist".

## AcidSingleCell 0.1.5 (2021-02-22)

### Minor changes

- Enforcing lower camel case in `sampleData`.

## AcidSingleCell 0.1.4 (2021-02-12)

### Minor changes

- Reworked NAMESPACE to reduce the number of imported packages.
- Renamed package title in DESCRIPTION to better match AcidExperiment.

## AcidSingleCell 0.1.3 (2021-02-09)

### Minor changes

- Added `SingleCellExperiment-class` to reexports.

## AcidSingleCell 0.1.2 (2021-02-08)

### Minor changes

- Added `SingleCellExperiment` to reexports.

## AcidSingleCell 0.1.1 (2021-02-08)

### Minor changes

- Improved code coverage, migrating unit tests from basejump.
- Improved package documentation.

## AcidSingleCell 0.1.0 (2021-02-05)

Initial release, migrating code previously defined in basejump.
