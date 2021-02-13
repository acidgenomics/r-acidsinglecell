context("zerosVsDepth")

test_that("SummarizedExperiment", {
    object <- rse
    x <- zerosVsDepth(object)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = round(mean(x[["dropout"]]), digits = 3L),
        expected = 0.096
    )
    expect_identical(
        object = round(mean(x[["depth"]]), digits = 2L),
        expected = 22140.67
    )
})

test_that("SingleCellExperiment", {
    object <- sce
    x <- zerosVsDepth(object)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = round(mean(x[["dropout"]]), digits = 2L),
        expected = 0.45
    )
    expect_identical(
        object = round(mean(x[["depth"]]), digits = 2L),
        expected = 56429.18
    )
})
