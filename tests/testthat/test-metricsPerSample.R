## NOTE These expected values can change when we update AcidTest.

test_that("SCE", {
    x <- sce
    x <- calculateMetrics(x)
    Map(
        fun = eval(formals(`metricsPerSample,SCE`)[["fun"]]),
        expected = list(
            "mean" = c(59947L, 62775L, 61158L, 60788L),
            "median" = c(58770L, 60390L, 59907L, 59275L),
            "sum" = c(7253620L, 5524187L, 5810045L, 5835694L)
        ),
        MoreArgs = list("x" = x),
        f = function(x, fun, expected) {
            x <- metricsPerSample(x, fun = fun)
            expect_s4_class(x, "DFrame")
            x <- as.integer(round(x[["nCount"]]))
            expect_identical(object = x, expected = expected)
        }
    )
})
