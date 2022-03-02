sce_lanesplit2 <- readRDS(file.path("cache", "sce_lanesplit.rds"))  # nolint



context("aggregate")

test_that("SingleCellExperiment", {
    object <- sce_lanesplit
    object <- aggregate(object, fun = "sum", MARGIN = 2L)
    expect_s4_class(object, "SingleCellExperiment")
    expect_identical(dim(object), c(100L, 8L))
    ## NOTE These values can change when we update AcidTest.
    expect_identical(
        object = head(colSums(counts(object))),
        expected = c(
            ## nolint start
            "sample_1_AAAAAA_AAAAAA_AAAAAA" = 60859,
            "sample_1_CCCCCC_CCCCCC_CCCCCC" = 47116,
            "sample_1_GGGGGG_GGGGGG_GGGGGG" = 50867,
            "sample_1_TTTTTT_TTTTTT_TTTTTT" = 45794,
            "sample_2_AAAAAA_AAAAAA_AAAAAA" = 51758,
            "sample_2_CCCCCC_CCCCCC_CCCCCC" = 43405
            ## nolint end
        )
    )
})

test_that("Aggregation methods defined in AcidExperiment", {
    object <- sce_lanesplit
    rowData(object)[["aggregate"]] <- as.factor("AAA")
    for (object in list(
        aggregate(object, MARGIN = 1L),
        aggregate(object, MARGIN = 2L),
        aggregateRows(object),
        aggregateCols(object)
    )) {
        expect_s4_class(object, "SingleCellExperiment")
    }
})

test_that("sce_lanesplit2", {
    object <- sce_lanesplit2
    object <- aggregate(object, fun = "sum", MARGIN = 2L)
    expect_s4_class(object, "SingleCellExperiment")
    expect_identical(dim(object), c(100L, 6432L))
    expect_identical(
        object = head(colSums(counts(object))),
        expected = c(
            ## nolint start
            "CD3H_AAAGAA_AAAGAA_TGCTAA" = 0,
            "CD3H_AAAGAA_ACTGCA_CCTCTA" = 97,
            "CD3H_AAAGAA_ACTGCA_GCCAGA" = 0,
            "CD3H_AAAGAA_AGCACG_CGGTCC" = 23,
            "CD3H_AAAGAA_AGTCTG_GCGCGG" = 1,
            "CD3H_AAAGAA_ATTAGT_AGATGT" = 1
            ## nolint end
        )
    )
})



context("aggregateCellsToSamples")

test_that("SingleCellExperiment", {
    object <- sce
    object <- aggregateCellsToSamples(object)
    ## NOTE These values can change we we update AcidTest.
    expect_identical(
        object = colSums(counts(object)),
        expected = c(
            ## nolint start
            "sample1" = 3262399,
            "sample2" = 2568709
            ## nolint end
        )
    )
})

test_that("sce_lanesplit2", {
    object <- sce_lanesplit2
    object <- aggregateCellsToSamples(object)
    expect_identical(
        object = head(colSums(counts(object))),
        expected = c(
            ## nolint start
            CD3H_1_L001 = 3991,
            CD3H_1_L002 = 3961,
            CD3H_1_L003 = 3644,
            CD3H_1_L004 = 3629,
            CD3H_2_L001 = 2226,
            CD3H_2_L002 = 2228
            ## nolint end
        )
    )
})
