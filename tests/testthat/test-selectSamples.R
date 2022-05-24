test_that("SingleCellExperiment", {
    object <- selectSamples(sce, sampleId = "sample1")
    expect_identical(
        object = sampleNames(object),
        expected = c(sample1 = "sample1")
    )
    expect_identical(
        object = rownames(sampleData(object)),
        expected = "sample1"
    )
})
