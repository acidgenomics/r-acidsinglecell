#' @name aggregate
#' @inherit AcidExperiment::aggregate
#' @note Updated 2021-09-13.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment_lanesplit, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment_lanesplit
#' levels(colData(x)[["aggregate"]])
#' x <- aggregate(
#'     x = x,
#'     col = "aggregate",
#'     fun = "sum",
#'     MARGIN = 2L
#' )
#' print(x)
NULL



#' Aggregate cellular barcodes across columns
#'
#' @note Updated 2021-09-13.
#' @noRd
`aggregateCols,SCE` <-  # nolint
    function(x, col, fun) {
        validObject(x)
        assert(
            hasColnames(x),
            isString(col),
            isString(fun)
        )
        ## Remap cellular barcodes.
        colData <- colData(x)
        cellCol <- "cellId"
        sampleCol <- matchSampleColumn(colData)
        aggregateCol <- col
        assert(
            isSubset(c(sampleCol, aggregateCol), colnames(colData)),
            is.factor(colData[[aggregateCol]])
        )
        map <- colData(x)[, c(aggregateCol, sampleCol)]
        map[["cellId"]] <- rownames(map)
        alert(sprintf(
            "Remapping cells to aggregate samples: %s",
            toInlineString(
                x = sort(levels(map[[aggregateCol]])),
                n = 5L,
                class = "val"
            )
        ))
        assert(
            all(mapply(
                FUN = grepl,
                x = map[[cellCol]],
                pattern = paste0("^", map[[sampleCol]]),
                SIMPLIFY = TRUE
            )),
            msg = "Cell identifiers are not prefixed with sample identifiers."
        )
        by <- mapply(
            FUN = gsub,
            x = map[[cellCol]],
            pattern = paste0("^", map[[sampleCol]]),
            replacement = map[[aggregateCol]],
            SIMPLIFY = TRUE,
            USE.NAMES = TRUE
        )
        by <- as.factor(by)
        cell2sample <- as.factor(map[[aggregateCol]])
        names(cell2sample) <- as.character(by)
        ## Reslot the `aggregate` column using these groupings.
        assert(identical(names(by), colnames(x)))
        colData(x)[[aggregateCol]] <- by
        ## Generate SingleCellExperiment ---------------------------------------
        ## Using `SummarizedExperiment` method here.
        rse <- as(x, "RangedSummarizedExperiment")
        colData(rse)[[sampleCol]] <- NULL
        rse <- aggregate(x = rse, fun = fun, MARGIN = 2L)
        assert(
            is(rse, "RangedSummarizedExperiment"),
            identical(nrow(rse), nrow(x))
        )
        ## Update the sample data.
        colData <- colData(rse)
        assert(isSubset(rownames(colData), names(cell2sample)))
        colData[[sampleCol]] <- cell2sample[rownames(colData)]
        if (isSubset("sampleName", colnames(colData))) {
            colData[["sampleName"]] <- colData[[sampleCol]]  # nocov
        }
        colData(rse) <- colData
        ## Update the metadata.
        metadata <- metadata(x)
        metadata[["aggregate"]] <- TRUE
        metadata[["aggregateCols"]] <- by
        ## Now ready to generate aggregated SCE.
        sce <- SingleCellExperiment(
            assays = assays(rse),
            rowRanges = rowRanges(x),
            colData = colData(rse),
            metadata = list(
                "aggregate" = TRUE,
                "aggregateCols" = by,
                "interestingGroups" = interestingGroups(x)
            )
        )
        validObject(sce)
        sce
    }



## Updated 2021-09-13.
`aggregate,SCE` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum",
        MARGIN = 1L  # nolint
    ) {
        assert(
            isInt(MARGIN),
            isInRange(MARGIN, lower = 1L, upper = 2L)
        )
        args <- list(
            "x" = x,
            "col" = col,
            "fun" = fun
        )
        switch(
            EXPR = as.character(MARGIN),
            "1" = {
                args[["MARGIN"]] <- MARGIN
                what <- methodFunction(
                    f = "aggregate",
                    signature = "SummarizedExperiment",
                    package = "AcidExperiment"
                )
            },
            "2" = {
                what <- `aggregateCols,SCE`
            },
        )
        do.call(what = what, args = args)
    }



#' @rdname aggregate
#' @export
setMethod(
    f = "aggregate",
    signature = signature("SingleCellExperiment"),
    definition = `aggregate,SCE`
)
