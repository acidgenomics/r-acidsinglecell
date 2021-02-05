context("metricsPerSample")

test_that("SingleCellExperiment", {
    mapply(
        fun = eval(formals(`metricsPerSample,SCE`)[["fun"]]),
        expected = list(
            mean = c(60587L, 54304L),
            median = c(56661L, 52534L),
            sum = c(3211137L, 2552299L)
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
