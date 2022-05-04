context("metrics")

test_that("SCE", {
    object <- metrics(sce)
    expect_s4_class(object, "DataFrame")
    expect_identical(
        object = colnames(object),
        expected = c(
            "sampleId",
            "sampleName",
            "interestingGroups"
        )
    )
    expect_identical(
        object = rownames(object)[[1L]],
        expected = "cell001"
    )
})
