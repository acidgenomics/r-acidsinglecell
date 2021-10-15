context("normalize")

test_that("SCE", {
    object <- sce_seurat
    object <- normalize(object)
    expect_s4_class(object, "SingleCellExperiment")
    expect_s4_class(logcounts(object), "Matrix")
    expect_true(is.numeric(sizeFactors(object)))
})
