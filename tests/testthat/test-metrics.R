context("metrics")

test_that("SCE : tbl_df", {
    object <- metrics(sce, return = "tbl_df")
    expect_s3_class(object, "tbl_df")
    expect_identical(
        object = colnames(object),
        expected = c(
            "cellId",
            "sampleId",
            "sampleName",
            "interestingGroups"
        )
    )
})

test_that("SCE : DataFrame", {
    object <- metrics(sce, return = "DataFrame")
    expect_s4_class(object, "DataFrame")
    expect_identical(
        object = colnames(object),
        expected = c(
            "sampleId",
            "sampleName",
            "interestingGroups"
        )
    )
})
