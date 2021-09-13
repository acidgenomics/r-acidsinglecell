context("filterCells")

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

## Note that this matches per sample.
test_that("Top cells only", {
    x <- filterCells(object, nCells = 2L)
    expect_identical(ncol(x), 4L)
})

## Refer to the quality control R Markdown for actual recommended cutoffs.
## These are skewed, and designed to work with our minimal dataset.
## NOTE These values can change when we update AcidTest.
test_that("Cell filtering", {
    mapply(
        args = list(
            list("minCounts" = 50000L),
            list("maxCounts" = 50000L),
            list("minFeatures" = 250L),
            list("maxFeatures" = 250L),
            list("minNovelty" = 0.5)
        ),
        dim = list(
            c(488L, 63L),
            c(472L, 37L),
            c(488L, 68L),
            c(472L, 35L),
            c(489L, 97L)
        ),
        FUN = function(args, dim) {
            x <- do.call(
                what = filterCells,
                args = append(
                    x = args,
                    values = list("object" = object)
                )
            )
            expect_s4_class(x, "SingleCellExperiment")
            expect_identical(dim(x), dim)
        },
        SIMPLIFY = FALSE
    )
})

test_that("Feature filtering", {
    x <- filterCells(object, minCellsPerFeature = 50L)
    expect_identical(dim(x), c(280L, 100L))
})



context("filterCells : Per sample filtering")

test_that("minCounts", {
    x <- filterCells(
        object = object,
        minCounts = c(
            "sample1" = 50000L,
            "sample2" = 25000L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minCounts"]],
            m[["sample2"]][["minCounts"]]
        ),
        expected = c(29L, 53L)
    )
    expect_identical(dim(x), c(489L, 82L))
})

test_that("maxCounts", {
    x <- filterCells(
        object = object,
        maxCounts = c(
            "sample1" = 50000L,
            "sample2" = 25000L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["maxCounts"]],
            m[["sample2"]][["maxCounts"]]
        ),
        expected = c(18L, 0L)
    )
    expect_identical(dim(x), c(460L, 18L))
})

test_that("minFeatures", {
    x <- filterCells(
        object = object,
        minFeatures = c(
            "sample1" = 300L,
            "sample2" = 250L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minFeatures"]],
            m[["sample2"]][["minFeatures"]]
        ),
        expected = c(5L, 30L)
    )
    expect_identical(dim(x), c(482L, 35L))
})

test_that("maxFeatures", {
    x <- filterCells(
        object = object,
        maxFeatures = c(
            "sample1" = 300L,
            "sample2" = 250L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["maxFeatures"]],
            m[["sample2"]][["maxFeatures"]]
        ),
        expected = c(42L, 26L)
    )
    expect_identical(dim(x), c(481L, 68L))
})

test_that("minNovelty", {
    x <- filterCells(
        object = object,
        minNovelty = c(
            "sample1" = 0.5,
            "sample2" = 0.6
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minNovelty"]],
            m[["sample2"]][["minNovelty"]]
        ),
        expected = c(47L, 0L)
    )
    expect_identical(dim(x), c(481L, 47L))
})

test_that("nCells", {
    x <- filterCells(
        object = object,
        nCells = c(
            "sample1" = 2L,
            "sample2" = 4L
        )
    )
    expect_identical(ncol(x), 6L)
    m <- metadata(x)[["filterCells"]][["topCellsPerSample"]]
    expect_identical(
        lapply(m, length),
        list(
            "sample1" = 2L,
            "sample2" = 4L
        )
    )
})
