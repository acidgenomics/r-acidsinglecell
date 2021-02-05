#' @name aggregateCols
#' @inherit AcidExperiment::aggregateCols
#' @note Updated 2021-02-05.
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
#' levels(SummarizedExperiment::colData(x)[["aggregate"]])
#' x <- aggregateCols(x = x, col = "aggregate", fun = "sum")
#' print(x)
NULL



## Updated 2021-02-05.
`aggregateCols,SCE` <-  # nolint
    function(
        x,
        col = "aggregate",
        fun = "sum"
    ) {
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
            toString(sort(levels(map[[aggregateCol]])), width = 100L)
        ))
        ## Check to see if we can aggregate.
        if (!all(mapply(
            FUN = grepl,
            x = map[[cellCol]],
            pattern = paste0("^", map[[sampleCol]]),
            SIMPLIFY = TRUE
        ))) {
            stop("Cell identifiers are not prefixed with sample identifiers.")
        }
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
        rse <- aggregateCols(x = rse, fun = fun)
        assert(
            is(rse, "RangedSummarizedExperiment"),
            identical(nrow(rse), nrow(x))
        )
        ## Update the sample data.
        colData <- colData(rse)
        assert(isSubset(rownames(colData), names(cell2sample)))
        colData[[sampleCol]] <- cell2sample[rownames(colData)]
        if (isSubset("sampleName", colnames(colData))) {
            colData[["sampleName"]] <- colData[[sampleCol]]
        }
        colData(rse) <- colData
        ## Update the metadata.
        metadata <- metadata(x)
        metadata[["aggregate"]] <- TRUE
        metadata[["aggregateCols"]] <- by
        ## Now ready to generate aggregated SCE.
        sce <- SingleCellExperiment(
            assays = SimpleList(counts = counts(rse)),
            rowRanges = rowRanges(x),
            colData = colData(rse),
            metadata = list(
                aggregate = TRUE,
                aggregateCols = by,
                interestingGroups = interestingGroups(x)
            )
        )
        validObject(sce)
        sce
    }



#' @rdname aggregateCols
#' @export
setMethod(
    f = "aggregateCols",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCols,SCE`
)
