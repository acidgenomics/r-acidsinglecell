context("export : SCE")

## NOTE The SingleCellExperiment_Seurat object has reducedDims slotted,
## whereas the SingleCellExperiment (splatter) example doesn't.

testdir <- file.path(tempdir(), "example")

test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
    unlink(testdir, recursive = TRUE)
    object <- sce_seurat
    out <- export(
        object = object,
        con = testdir,
        compress = TRUE
    )
    prefix <- realpath(testdir)
    assays <- file.path(prefix, "assays")
    expect_identical(
        object = out,
        expected = list(
            "assays" = list(
                "counts" = c(
                    "matrix" = file.path(assays, "counts.mtx.gz"),
                    "barcodes" = file.path(assays, "counts.mtx.gz.colnames"),
                    "genes" = file.path(assays, "counts.mtx.gz.rownames")
                ),
                "logcounts" = c(
                    "matrix" = file.path(assays, "logcounts.mtx.gz"),
                    "barcodes" = file.path(assays, "logcounts.mtx.gz.colnames"),
                    "genes" = file.path(assays, "logcounts.mtx.gz.rownames")
                )
            ),
            "colData" = file.path(prefix, "colData.csv.gz"),
            "rowData" = file.path(prefix, "rowData.csv.gz"),
            "reducedDims" = list(
                "umap" = file.path(prefix, "reducedDims", "umap.csv.gz")
            )
        )
    )
    unlink(testdir, recursive = TRUE)
})

test_that("Deprecated : 'dir' argument, no 'name'", {
    unlink(testdir, recursive = TRUE)
    object <- sce_seurat
    out <- export(
        object = object,
        dir = testdir,
        compress = TRUE
    )
    prefix <- realpath(file.path(testdir, "object"))
    assays <- file.path(prefix, "assays")
    expect_identical(
        object = out,
        expected = list(
            "assays" = list(
                "counts" = c(
                    "matrix" = file.path(assays, "counts.mtx.gz"),
                    "barcodes" = file.path(assays, "counts.mtx.gz.colnames"),
                    "genes" = file.path(assays, "counts.mtx.gz.rownames")
                ),
                "logcounts" = c(
                    "matrix" = file.path(assays, "logcounts.mtx.gz"),
                    "barcodes" = file.path(assays, "logcounts.mtx.gz.colnames"),
                    "genes" = file.path(assays, "logcounts.mtx.gz.rownames")
                )
            ),
            "colData" = file.path(prefix, "colData.csv.gz"),
            "rowData" = file.path(prefix, "rowData.csv.gz"),
            "reducedDims" = list(
                "umap" = file.path(prefix, "reducedDims", "umap.csv.gz")
            )
        )
    )
    unlink(testdir, recursive = TRUE)
})
