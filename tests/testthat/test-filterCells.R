## NOTE These expected values can change when we update AcidTest.

object <- sce
object <- calculateMetrics(object)

test_that("No filtering applied", {
    x <- filterCells(object, minCellsPerFeature = 0L)
    expect_s4_class(x, "SingleCellExperiment")
    expect_identical(dim(x), dim(object))
    expect_null(metadata(x)[["filterCells"]])
})

test_that("No cells pass", {
    expect_error(
        filterCells(object, minCounts = Inf),
        "No cells passed filtering."
    )
    expect_error(
        filterCells(object, maxCounts = 1L),
        "No cells passed filtering."
    )
    expect_error(
        filterCells(object, minFeatures = Inf),
        "No cells passed filtering."
    )
    expect_error(
        filterCells(object, maxFeatures = 1L),
        "No cells passed filtering."
    )
    expect_error(
        filterCells(object, minNovelty = 1L),
        "No cells passed filtering."
    )
    ## Skipping `mitoRatio` check here because it's not in the example object.
})

test_that("No features pass", {
    expect_error(
        filterCells(object, minCellsPerFeature = Inf),
        "No features passed filtering."
    )
})

## May tighten this up and restrict in the future, but currently this approach
## is in use by the bcbioSingleCell QC template.
test_that("Double filtering is allowed", {
    x <- filterCells(object)
    x <- filterCells(x)
    expect_s4_class(x, "SingleCellExperiment")
})

## This matches per sample.
test_that("Top cells only", {
    x <- filterCells(object, nCells = 2L)
    expect_identical(dim(x), c(100L, 8L))
})

## Refer to the quality control R Markdown for actual recommended cutoffs.
## These are skewed, and designed to work with our minimal dataset.
test_that("Cell filtering", {
    Map(
        args = list(
            list("minCounts" = 50000L),
            list("maxCounts" = 50000L),
            list("minFeatures" = 100L),
            list("maxFeatures" = 98L),
            list("minNovelty" = 0.42)
        ),
        dim = list(
            c(100L, 326L),
            c(100L, 74L),
            c(100L, 59L),
            c(100L, 215L),
            c(100L, 139L)
        ),
        f = function(args, dim) {
            x <- do.call(
                what = filterCells,
                args = append(
                    x = args,
                    values = list("object" = object)
                )
            )
            expect_s4_class(x, "SingleCellExperiment")
            expect_identical(dim(x), dim)
        }
    )
})

test_that("Feature filtering", {
    x <- filterCells(object, minCellsPerFeature = 400L)
    expect_identical(
        object = dim(x),
        expected = c(91L, 400L)
    )
})

test_that("minCounts", {
    x <- filterCells(
        object = object,
        minCounts = c(
            "sample1" = 10000L,
            "sample2" = 20000L,
            "sample3" = 30000L,
            "sample4" = 40000L
        )
    )
    expect_identical(
        object = dim(x),
        expected = c(100L, 398L)
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minCounts"]],
            m[["sample2"]][["minCounts"]],
            m[["sample3"]][["minCounts"]],
            m[["sample4"]][["minCounts"]]
        ),
        expected = c(95L, 96L, 121L, 86L)
    )
})

test_that("maxCounts", {
    x <- filterCells(
        object = object,
        maxCounts = c(
            "sample1" = 50000L,
            "sample2" = 48000L,
            "sample3" = 46000L,
            "sample4" = 44000L
        )
    )
    expect_identical(
        object = dim(x),
        expected = c(100L, 60L)
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["maxCounts"]],
            m[["sample2"]][["maxCounts"]],
            m[["sample3"]][["maxCounts"]],
            m[["sample4"]][["maxCounts"]]
        ),
        expected = c(23L, 15L, 18L, 4L)
    )
})

test_that("minFeatures", {
    x <- filterCells(
        object = object,
        minFeatures = c(
            "sample1" = 100L,
            "sample2" = 99L,
            "sample3" = 98L,
            "sample4" = 97L
        )
    )
    expect_identical(
        object = dim(x),
        expected = c(100L, 235L)
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minFeatures"]],
            m[["sample2"]][["minFeatures"]],
            m[["sample3"]][["minFeatures"]],
            m[["sample4"]][["minFeatures"]]
        ),
        expected = c(13L, 49L, 91L, 82L)
    )
})

test_that("maxFeatures", {
    x <- filterCells(
        object = object,
        maxFeatures = c(
            "sample1" = 99L,
            "sample2" = 98L,
            "sample3" = 97L,
            "sample4" = 96L
        )
    )
    expect_identical(
        object = dim(x),
        expected = c(100L, 165L)
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["maxFeatures"]],
            m[["sample2"]][["maxFeatures"]],
            m[["sample3"]][["maxFeatures"]],
            m[["sample4"]][["maxFeatures"]]
        ),
        expected = c(82L, 47L, 30L, 6L)
    )
})

test_that("minNovelty", {
    x <- filterCells(
        object = object,
        minNovelty = c(
            "sample1" = 0.40,
            "sample2" = 0.41,
            "sample3" = 0.42,
            "sample4" = 0.43
        )
    )
    expect_identical(
        object = dim(x),
        expected = c(100L, 224L)
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minNovelty"]],
            m[["sample2"]][["minNovelty"]],
            m[["sample3"]][["minNovelty"]],
            m[["sample4"]][["minNovelty"]]
        ),
        expected = c(94L, 82L, 46L, 2L)
    )
})

test_that("nCells", {
    x <- filterCells(
        object = object,
        nCells = c(
            "sample1" = 1L,
            "sample2" = 2L,
            "sample3" = 3L,
            "sample4" = 4L
        )
    )
    expect_identical(
        object = dim(x),
        expected = c(100L, 10L)
    )
    m <- metadata(x)[["filterCells"]][["topCellsPerSample"]]
    expect_identical(
        lapply(m, length),
        list(
            "sample1" = 1L,
            "sample2" = 2L,
            "sample3" = 3L,
            "sample4" = 4L
        )
    )
})
