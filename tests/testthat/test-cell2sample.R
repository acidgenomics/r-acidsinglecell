context("cell2sample")

test_that("SCE factor return", {
    x <- cell2sample(sce, return = "factor")
    expect_is(x, "factor")
    expect_identical(
        object = levels(x),
        expected = c("sample1", "sample2", "sample3", "sample4")
    )
    expect_true(hasNames(x))
})

test_that("SCE DataFrame return", {
    x <- cell2sample(sce, return = "DataFrame")
    expect_s4_class(x, "DataFrame")
    expect_identical(colnames(x), c("cellId", "sampleId"))
})
