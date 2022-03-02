context("metricsPerSample")

## NOTE These expected values can change when we update AcidTest.

test_that("SCE", {
    x <- sce
    x <- calculateMetrics(x)
    mapply(
        fun = eval(formals(`metricsPerSample,SCE`)[["fun"]]),
        expected = list(
            "mean" = c(60415L, 55842L),
            "median" = c(59518L, 53450L),
            "sum" = c(3262399L, 2568709L)
        ),
        MoreArgs = list("x" = x),
        FUN = function(x, fun, expected) {
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
