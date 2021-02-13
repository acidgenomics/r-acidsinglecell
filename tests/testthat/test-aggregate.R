sce_lanesplit2 <- readRDS(file.path("cache", "sce_lanesplit.rds"))  # nolint



context("aggregateCols")

test_that("SingleCellExperiment", {
    object <- sce_lanesplit
    object <- aggregateCols(object, fun = "sum")
    expect_s4_class(object, "SingleCellExperiment")
    expect_identical(dim(object), c(100L, 8L))
    sums <-
        counts(object) %>%
        colSums() %>%
        as("integer") %>%
        sort(decreasing = TRUE) %>%
        head()
    ## NOTE These values can change when we update AcidTest.
    expect_identical(
        object = sums,
        expected = c(
            "sample_1_TTTTTT_TTTTTT_TTTTTT" = 45635L,
            "sample_2_TTTTTT_TTTTTT_TTTTTT" = 43923L,
            "sample_1_CCCCCC_CCCCCC_CCCCCC" = 41455L,
            "sample_2_GGGGGG_GGGGGG_GGGGGG" = 40484L,
            "sample_1_GGGGGG_GGGGGG_GGGGGG" = 38776L,
            "sample_2_AAAAAA_AAAAAA_AAAAAA" = 37864L
        )

    )
})

test_that("sce_lanesplit2", {
    object <- sce_lanesplit2
    object <- aggregateCols(object, fun = "sum")
    expect_s4_class(object, "SingleCellExperiment")
    expect_identical(dim(object), c(100L, 6432L))
    sums <-
        counts(object) %>%
        colSums() %>%
        as("integer") %>%
        sort(decreasing = TRUE) %>%
        head()
    expect_identical(
        object = sums,
        expected = c(
            "CD3H_CCGTAA_CGGTCC_TTCTTG" = 439L,
            "CD3I_CGCATA_GGTGCT_TCTAGC" = 423L,
            "CD3I_CGCATA_GGATTG_CATAGA" = 403L,
            "CD3H_CTAGGT_CTCAAT_TGCGGT" = 381L,
            "CD3H_TTCTTG_CAACCG_CTATTA" = 376L,
            "CD3H_GCCGTT_TGGCAG_ATATAC" = 340L
        )

    )
})



context("aggregateCellsToSamples")

test_that("SingleCellExperiment", {
    object <- sce
    object <- aggregateCellsToSamples(object)
    sums <-
        counts(object) %>%
        colSums() %>%
        as("integer")
    ## NOTE These values can change we we update AcidTest.
    expect_identical(
        object = sums,
        expected = c(
            "sample1" = 2507353L,
            "sample2" = 3135565L
        )
    )
})


test_that("sce_lanesplit2", {
    object <- sce_lanesplit2
    object <- aggregateCellsToSamples(object)
    sums <-
        assay(object) %>%
        colSums() %>%
        as("integer") %>%
        sort(decreasing = TRUE) %>%
        head()
    expect_identical(
        object = sums,
        expected = c(
            "CD3H_3_L001" = 4764L,
            "CD3H_3_L002" = 4600L,
            "CD3H_3_L003" = 4325L,
            "CD3H_3_L004" = 4279L,
            "CD3H_1_L001" = 3991L,
            "CD3H_1_L002" = 3961L
        )
    )
})
