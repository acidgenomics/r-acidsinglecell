## Updated 2021-01-16.
`aggregateCols,SCE` <-  # nolint
    function(
        x,
        fun  # nolint
    ) {
        validObject(x)
        assert(isString(fun))
        ## Remap cellular barcodes.
        colData <- colData(x)
        sampleCol <- matchSampleColumn(colData)
        cellCol <- "cellId"
        aggregateCol <- "aggregate"
        assert(
            isString(sampleCol),
            isString(aggregateCol),
            isSubset(c(sampleCol, aggregateCol), colnames(colData)),
            is.factor(colData[[aggregateCol]])
        )
        cli_alert(sprintf(
            "Remapping cells to aggregate samples: %s",
            toString(sort(levels(colData[[aggregateCol]])), width = 100L)
        ))
        map <- as_tibble(colData(x), rownames = cellCol)
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
