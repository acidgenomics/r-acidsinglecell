context("sampleNames")

test_that("SingleCellExperiment", {
    expect_identical(
        object = sort(sampleNames(sce)),
        expected = c(
            sample1 = "sample1",
            sample2 = "sample2"
        )
    )
})

test_that("SCE assignment", {
    oldSamples <- sampleNames(sce)
    newSamples <- letters[seq_along(oldSamples)]
    names(newSamples) <- names(oldSamples)
    sampleNames(sce) <- newSamples
    expect_identical(
        object = sampleNames(sce),
        expected = c(sample1 = "a", sample2 = "b")
    )
})
