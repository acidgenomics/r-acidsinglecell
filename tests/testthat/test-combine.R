context("combine")

str_pad <- stringr::str_pad  # nolint

test_that("SCE", {
    x <- sce
    colnames(x) <- paste0(
        "cell",
        str_pad(
            string = seq_len(ncol(x)),
            width = 4L,
            pad = "0"
        )
    )
    y <- x
    colnames(y) <- paste0(
        "cell",
        str_pad(
            string = seq_len(ncol(y)) + ncol(y),
            width = 4L,
            pad = "0"
        )
    )
    c <- combine(x, y)
    expect_s4_class(c, "SingleCellExperiment")
    expect_identical(dim(c), c(500L, 200L))
    expect_identical(
        object = sampleData(c),
        expected = DataFrame(
            "sampleName" = as.factor(c("sample1", "sample2")),
            "interestingGroups" = as.factor(c("sample1", "sample2")),
            row.names = c("sample1", "sample2")
        )
    )
})
