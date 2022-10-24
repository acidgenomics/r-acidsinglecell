## NOTE The SingleCellExperiment_Seurat object has reducedDims slotted,
## whereas the SingleCellExperiment (splatter) example doesn't.

test_that("New 'con' BiocIO approach", {
    testdir <- tempdir2()
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
                    "rownames" = file.path(assays, "counts.mtx.gz.rownames"),
                    "colnames" = file.path(assays, "counts.mtx.gz.colnames")
                ),
                "logcounts" = c(
                    "matrix" = file.path(assays, "logcounts.mtx.gz"),
                    "rownames" = file.path(assays, "logcounts.mtx.gz.rownames"),
                    "colnames" = file.path(assays, "logcounts.mtx.gz.colnames")
                )
            ),
            "rowData" = file.path(prefix, "rowData.csv.gz"),
            "colData" = file.path(prefix, "colData.csv.gz"),
            "reducedDims" = list(
                "PCA" = file.path(prefix, "reducedDims", "PCA.csv.gz"),
                "TSNE" = file.path(prefix, "reducedDims", "TSNE.csv.gz"),
                "UMAP" = file.path(prefix, "reducedDims", "UMAP.csv.gz")
            )
        )
    )
    unlink2(testdir)
})
