context("export")

## Note that the SingleCellExperiment_Seurat object has reducedDims slotted,
## whereas the SingleCellExperiment (splatter) example doesn't.

test_that("'dir' argument, no 'name'", {
    x <- export(sce_seurat, name = NULL, dir = "XXX", compress = TRUE)
    prefix <- realpath(file.path("XXX", "sce_seurat"))
    assays <- file.path(prefix, "assays")
    expect_identical(
        x,
        list(
            assays = list(
                counts = c(
                    matrix = file.path(assays, "counts.mtx.gz"),
                    barcodes = file.path(assays, "counts.mtx.gz.colnames"),
                    genes = file.path(assays, "counts.mtx.gz.rownames")
                ),
                logcounts = c(
                    matrix = file.path(assays, "logcounts.mtx.gz"),
                    barcodes = file.path(assays, "logcounts.mtx.gz.colnames"),
                    genes = file.path(assays, "logcounts.mtx.gz.rownames")
                )
            ),
            colData = file.path(prefix, "colData.csv.gz"),
            rowData = file.path(prefix, "rowData.csv.gz"),
            reducedDims = list(
                umap = file.path(prefix, "reducedDims", "umap.csv.gz")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})

test_that("Both 'name' and 'dir' declared", {
    x <- export(sce_seurat, name = "test", dir = "XXX")
    prefix <- realpath(file.path("XXX", "test"))
    assays <- file.path(prefix, "assays")
    expect_identical(
        x,
        list(
            assays = list(
                counts = c(
                    matrix = file.path(assays, "counts.mtx"),
                    barcodes = file.path(assays, "counts.mtx.colnames"),
                    genes = file.path(assays, "counts.mtx.rownames")
                ),
                logcounts = c(
                    matrix = file.path(assays, "logcounts.mtx"),
                    barcodes = file.path(assays, "logcounts.mtx.colnames"),
                    genes = file.path(assays, "logcounts.mtx.rownames")
                )
            ),
            colData = file.path(prefix, "colData.csv"),
            rowData = file.path(prefix, "rowData.csv"),
            reducedDims = list(
                umap = file.path(prefix, "reducedDims", "umap.csv")
            )
        )
    )
    unlink("XXX", recursive = TRUE)
})
