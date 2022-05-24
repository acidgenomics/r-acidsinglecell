test_that("Minimal mode", {
    assays <- assays(sce)
    x <- makeSingleCellExperiment(assays = assays)
    expect_s4_class(x, "SingleCellExperiment")
})
