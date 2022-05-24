test_that("SCE", {
    expect_message(
        object = convertSampleIDsToNames(sce),
        regexp = "unmodified"
    )
    expect_identical(
        object = convertSampleIDsToNames(sce),
        expected = sce
    )
})
