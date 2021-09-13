context("metricsPerSample")

## NOTE These values can change when we update AcidTest.
test_that("SCE", {
    mapply(
        fun = eval(formals(`metricsPerSample,SCE`)[["fun"]]),
        expected = list(
            "mean" = c(55843L, 55755L),
            "median" = c(56991L, 53230L),
            "sum" = c(2624611L, 2955035L)
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
    expect_s4_class(
        object = metricsPerSample(sce, return = "DataFrame"),
        class = "DataFrame"
    )
})
