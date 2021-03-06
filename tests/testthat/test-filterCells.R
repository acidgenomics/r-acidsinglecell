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
            list(minCounts = 50000L),
            list(maxCounts = 50000L),
            list(minFeatures = 250L),
            list(maxFeatures = 250L),
            list(minNovelty = 0.5)
        ),
        dim = list(
            c(487L, 69L),
            c(472L, 31L),
            c(488L, 90L),
            c(429L, 10L),
            c(487L, 99L)
        ),
        FUN = function(args, dim) {
            args[["object"]] <- object
            x <- do.call(what = filterCells, args = args)
            expect_s4_class(x, "SingleCellExperiment")
            expect_identical(dim(x), dim)
        },
        SIMPLIFY = FALSE
    )
})

test_that("Feature filtering", {
    x <- filterCells(object, minCellsPerFeature = 50L)
    expect_identical(dim(x), c(296L, 100L))
})



context("filterCells : Per sample filtering")

test_that("minCounts", {
    ## minCounts
    x <- filterCells(
        object = object,
        minCounts = c(
            sample1 = 50000L,
            sample2 = 25000L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minCounts"]],
            m[["sample2"]][["minCounts"]]
        ),
        expected = c(29L, 54L)
    )
    expect_identical(dim(x), c(487L, 83L))
})

test_that("maxCounts", {
    x <- filterCells(
        object = object,
        maxCounts = c(
            sample1 = 50000L,
            sample2 = 25000L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["maxCounts"]],
            m[["sample2"]][["maxCounts"]]
        ),
        expected = c(17L, 0L)
    )
    expect_identical(dim(x), c(458L, 17L))
})

test_that("minFeatures", {
    x <- filterCells(
        object = object,
        minFeatures = c(
            sample1 = 300L,
            sample2 = 250L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minFeatures"]],
            m[["sample2"]][["minFeatures"]]
        ),
        expected = c(1L, 51L)
    )
    expect_identical(dim(x), c(480L, 52L))
})

test_that("maxFeatures", {
    x <- filterCells(
        object = object,
        maxFeatures = c(
            sample1 = 300L,
            sample2 = 250L
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["maxFeatures"]],
            m[["sample2"]][["maxFeatures"]]
        ),
        expected = c(45L, 3L)
    )
    expect_identical(dim(x), c(484L, 48L))
})

test_that("minNovelty", {
    x <- filterCells(
        object = object,
        minNovelty = c(
            sample1 = 0.5,
            sample2 = 0.6
        )
    )
    m <- metadata(x)[["filterCells"]][["perSamplePass"]]
    expect_identical(
        object = c(
            m[["sample1"]][["minNovelty"]],
            m[["sample2"]][["minNovelty"]]
        ),
        expected = c(46L, 0L)
    )
    expect_identical(dim(x), c(483L, 46L))
})

test_that("nCells", {
    x <- filterCells(
        object = object,
        nCells = c(
            sample1 = 2L,
            sample2 = 4L
        )
    )
    expect_identical(ncol(x), 6L)
    m <- metadata(x)[["filterCells"]][["topCellsPerSample"]]
    expect_identical(
        lapply(m, length),
        list(
            sample1 = 2L,
            sample2 = 4L
        )
    )
})
