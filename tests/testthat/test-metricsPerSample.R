context("metricsPerSample")

test_that("SingleCellExperiment", {
    mapply(
        fun = eval(formals(`metricsPerSample,SCE`)[["fun"]]),
        expected = list(
            "mean" = c(54508L, 58066L),
            "median" = c(54318L, 58486L),
            "sum" = c(2507353L, 3135565L)
        ),
        FUN = function(fun, expected) {
            x <- sce
            x <- calculateMetrics(x)
            x <- metricsPerSample(x, fun = fun)
            x <- as.integer(round(x[["nCount"]]))
            expect_identical(object = x, expected = expected)
        },
        SIMPLIFY = FALSE
    )
})
