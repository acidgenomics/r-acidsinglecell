context("sampleData")

## Note that this doesn't sort the row names automatically. Here we're doing
## this internally in the check, to make the example "sce" object more resistant
## to code-breaking updates.
test_that("Return", {
    samples <- paste0("sample", seq_len(2L))
    sd <- sampleData(sce)
    expect_identical(sort(rownames(sd)), samples)
    expect_identical(
        sd[samples, ],
        DataFrame(
            "sampleName" = as.factor(samples),
            "interestingGroups" = as.factor(samples),
            row.names = samples
        )
    )
})

test_that("Empty return", {
    sce <- SingleCellExperiment()
    expect_identical(sampleData(sce), DataFrame())
})

test_that("No sample info", {
    colData(sce) <- DataFrame(row.names = colnames(sce))
    expect_identical(
        sampleData(sce),
        DataFrame(
            "sampleName" = factor("unknown"),
            "interestingGroups" = factor("unknown"),
            row.names = "unknown"
        )
    )
})

test_that("Assignment", {
    sd <- sampleData(sce)
    sd <- sd[sort(rownames(sd)), , drop = FALSE]
    batch <- as.factor(seq_len(nrow(sd)))
    sd[["batch"]] <- batch
    sampleData(sce) <- sd
    sd <- sampleData(sce)
    sd <- sd[sort(rownames(sd)), , drop = FALSE]
    samples <- paste0("sample", seq(2L))
    expect_identical(
        object = sd,
        expected = DataFrame(
            "sampleName" = as.factor(samples),
            "batch" = batch,
            "interestingGroups" = as.factor(samples),
            row.names = samples
        )
    )
})
