test_that("SingleCellExperiment", {
    object <- sce
    x <- topCellsPerSample(object = object, n = 5L)
    expect_is(x, "list")
    expect_identical(
        object = names(x),
        expected = paste0("sample", seq(4L))
    )
})
