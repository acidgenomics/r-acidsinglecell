context("sampleNames")

test_that("SingleCellExperiment", {
    object <- sce
    expect_identical(
        object = sampleNames(object),
        expected = c(
            "sample3" = "sample3",
            "sample4" = "sample4",
            "sample1" = "sample1",
            "sample2" = "sample2"
        )
    )
})

test_that("SCE assignment", {
    object <- sce
    sampleNames(object) <- c(
        "sample1" = "a",
        "sample2" = "b",
        "sample3" = "c",
        "sample4" = "d"
    )
    expect_identical(
        object = sampleNames(object),
        expected = c(
            "sample3" = "c",
            "sample4" = "d",
            "sample1" = "a",
            "sample2" = "b"
        )
    )
})
