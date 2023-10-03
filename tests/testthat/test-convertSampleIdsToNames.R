test_that("SCE", {
    expect_message(
        object = convertSampleIdsToNames(sce),
        regexp = "unmodified"
    )
    expect_identical(
        object = convertSampleIdsToNames(sce),
        expected = sce
    )
})
