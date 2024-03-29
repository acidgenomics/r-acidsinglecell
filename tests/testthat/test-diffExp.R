skip_if_not_installed("DESeq2")
skip_if_not_installed("edgeR")

## Compare expression in cluster 3 relative to 2.
object <- sce_seurat
ident <- clusters(object)
numerator <- names(ident)[ident == "2"]
denominator <- names(ident)[ident == "1"]

test_that("diffExp", {
    ## edgeR.
    x <- diffExp(
        object = object,
        numerator = numerator,
        denominator = denominator,
        caller = "edgeR"
    )
    expect_s4_class(x, "DGELRT")
    ## DESeq2.
    x <- diffExp(
        object = object,
        numerator = numerator,
        denominator = denominator,
        caller = "DESeq2"
    )
    expect_s4_class(x, "DESeqResults")
})

test_that("findMarkers", {
    ## edgeR.
    x <- findMarkers(object, caller = "edgeR")
    expect_type(x, "list")
    invisible(lapply(
        X = x,
        FUN = function(x) {
            expect_s4_class(x, "DGELRT")
        }
    ))
    ## DESeq2. Slow for large datasets.
    ## Expecting warning about degenerate design matrix.
    suppressWarnings({
        x <- findMarkers(object, caller = "DESeq2")
    })
    expect_type(x, "list")
    invisible(lapply(
        X = x,
        FUN = function(x) {
            expect_s4_class(x, "DESeqResults")
        }
    ))
})
