## NOTE The SingleCellExperiment_Seurat object has reducedDims slotted,
## whereas the SingleCellExperiment (splatter) example doesn't.

test_that("New 'con' BiocIO approach, instead of deprecated 'dir'", {
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
                "PCA" = file.path(prefix, "reducedDims", "PCA.csv.gz"),
                "TSNE" = file.path(prefix, "reducedDims", "TSNE.csv.gz"),
                "UMAP" = file.path(prefix, "reducedDims", "UMAP.csv.gz")
            )
        )
    )
    unlink2(testdir)
})

test_that("Deprecated : 'dir' argument, no 'name'", {
    testdir <- tempdir2()
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
                "PCA" = file.path(prefix, "reducedDims", "PCA.csv.gz"),
                "TSNE" = file.path(prefix, "reducedDims", "TSNE.csv.gz"),
                "UMAP" = file.path(prefix, "reducedDims", "UMAP.csv.gz")
            )
        )
    )
    unlink2(testdir)
})
