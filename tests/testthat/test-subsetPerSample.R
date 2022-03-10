context("subsetPerSample")

test_that("List mode", {
    object <- sce
    n <- c(
        "sample1" = 95L,
        "sample2" = 96L,
        "sample3" = 121L,
        "sample4" = 88L
    )
    expect_identical(
        object = summary(colData(object)[["sampleId"]]),
        expected = n
    )
    x <- subsetPerSample(sce, assignAndSave = FALSE)
    expect_is(x, "list")
    expect_identical(
        object = names(x),
        expected = paste0("sample", seq_len(4L))
    )
    expect_identical(
        object = vapply(X = x, FUN = ncol, FUN.VALUE = integer(1L)),
        expected = n
    )
})

## This is useful for larger datasets.
test_that("Assign and save mode", {
    object <- sce
    envir <- new.env()
    files <-
        subsetPerSample(
            object = object,
            assignAndSave = TRUE,
            envir = envir,
            dir = "subsetPerSample"
        )
    expect_identical(
        object = files,
        expected = c(
            "sample1" = realpath(file.path("subsetPerSample", "sample1.rds")),
            "sample2" = realpath(file.path("subsetPerSample", "sample2.rds")),
            "sample3" = realpath(file.path("subsetPerSample", "sample3.rds")),
            "sample4" = realpath(file.path("subsetPerSample", "sample4.rds"))
        )
    )
    expect_identical(
        object = ls(envir),
        expected = paste0("sample", seq_len(4L))
    )
    expect_identical(
        object = sort(list.files("subsetPerSample")),
        expected = paste0("sample", seq_len(4L), ".rds")
    )
    unlink("subsetPerSample", recursive = TRUE)
})
