test_that("SCE factor return", {
    x <- cell2sample(sce, return = "factor")
    expect_s3_class(x, "factor")
    expect_identical(
        object = levels(x),
        expected = c("sample1", "sample2", "sample3", "sample4")
    )
    expect_named(x)
})

test_that("SCE DFrame return", {
    x <- cell2sample(sce, return = "DFrame")
    expect_s4_class(x, "DFrame")
    expect_identical(colnames(x), c("cellId", "sampleId"))
})
