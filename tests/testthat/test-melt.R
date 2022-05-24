test_that("SCE", {
    x <- melt(sce)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = colnames(x),
        expected = c(
            "rowname",
            "colname",
            "value",
            "sampleId",
            "sampleName",
            "interestingGroups"
        )
    )
})
