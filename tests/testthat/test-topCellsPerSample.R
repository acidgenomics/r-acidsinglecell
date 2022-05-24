test_that("SingleCellExperiment", {
    object <- sce
    x <- topCellsPerSample(object = object, n = 5L)
    expect_type(x, "list")
    expect_named(
        object = x,
        expected = paste0("sample", seq(4L))
    )
})
