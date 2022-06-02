# AcidSingleCell 0.3.2 (2022-06-02)

## Minor changes

- Hardened temporary file handling in working examples and unit tests, to
  improve Windows compatibility.

# AcidSingleCell 0.3.1 (2022-05-24)

## Minor changes

- Updated lintr checks and testthat unit tests.

# AcidSingleCell 0.3.0 (2022-05-05)

## Major changes

- Now requiring R 4.2 / Bioconductor 3.15.
- S4 classes that contain `DataFrame` were updated to inherit `DFrame` instead,
  due to a breaking change in Bioconductor 3.15.

## Minor changes

- Reformatted package code using styler conventions.
- `metrics`: Removed `tbl_df` return option, to remove dependency on tibble
  package, in favor of simply using Bioconductor S4Vectors.
- Removed strong dependency on stringr in favor of stringi package.
- Removed dependency on scales package, by removing `percent` usage in
  `filterCells` reporting statistics.

# AcidSingleCell 0.2.0 (2022-03-11)

Significantly reworked the package, migrating some single-cell RNA-seq analysis
code that was previously defined in pointilllism package, but is generally
applicable for any type of single-cell RNA-seq. We are reworking the pointillism
package to function primarily as an extension toolkit for Seurat and monocle3.

## New functions and classes

- Migrated `barcodeRanksPerSample`, `cellCountsPerCluster`, `cpm`, `diffExp`,
  `diffExpPerCluster`, `findMarkers`, and `normalize` methods that dispatch on
  `SingleCellExperiment` here from pointillism.
- Migrated `CellCycleMarkers`, `CellTypeMarkers`, and `KnownMarkers` classes
  here from pointillism.

## Major changes

- `export`: Reworked `SingleCellExperiment` method to support new BiocIO
  generic approach. This method functions similarly to `SummarizedExperiment`
  method, but also exports `reducedDims` matrices to disk as well.

## Minor changes

- Multiple changes to NAMESPACE, reflecting migration of code from pointillism.
- `filterCells`: Package metadata is now defined as `"packageName"` and
  `"packageVersion"`, instead of previously using just `"version"`.
- Migrated `KnownMarkers` generic from here to AcidGenerics. Previously this
  was defined in pointillism package.

# AcidSingleCell 0.1.9 (2021-09-13)

## Minor changes

- Added `aggregate` method designed for `SingleCellExperiment`. Legacy method
  support for `aggregateCols` is still provided, but now works as a passthrough
  to `aggregate`, which is defined in AcidExperiment instead of here.
- Added `assay` support for `filterCells` and `zerosVsDepth`
  SingleCellExperiment methods.

# AcidSingleCell 0.1.8 (2021-09-03)

## Minor changes

- Updated minimum dependencies and improved documentation.
- Improved code coverage, getting back to 100%.
- Improved CLI messages, using updated functions from AcidCLI.

# AcidSingleCell 0.1.7 (2021-03-02)

## Minor changes

- Added `logcounts` and `normcounts` as reexports.

# AcidSingleCell 0.1.6 (2021-02-26)

## Minor changes

- Renamed all instances of "blacklist" to "denylist".

# AcidSingleCell 0.1.5 (2021-02-22)

## Minor changes

- Enforcing lower camel case in `sampleData`.

# AcidSingleCell 0.1.4 (2021-02-12)

## Minor changes

- Reworked NAMESPACE to reduce the number of imported packages.
- Renamed package title in DESCRIPTION to better match AcidExperiment.

# AcidSingleCell 0.1.3 (2021-02-09)

## Minor changes

- Added `SingleCellExperiment-class` to reexports.

# AcidSingleCell 0.1.2 (2021-02-08)

## Minor changes

- Added `SingleCellExperiment` to reexports.

# AcidSingleCell 0.1.1 (2021-02-08)

## Minor changes

- Improved code coverage, migrating unit tests from basejump.
- Improved package documentation.

# AcidSingleCell 0.1.0 (2021-02-05)

Initial release, migrating code previously defined in basejump.
