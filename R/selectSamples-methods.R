#' @name selectSamples
#' @inherit AcidGenerics::selectSamples
#' @note Updated 2022-03-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' sample <- Biobase::sampleNames(object)[[1L]]
#' print(sample)
#' subset <- selectSamples(object, sampleName = sample)
#' print(subset)
NULL



## Updated 2019-08-19.
`selectSamples,SCE` <- # nolint
    function(object, ...) {
        validObject(object)
        assert(isSubset("sampleId", colnames(colData(object))))
        ## Here the `args` are captured as a named character vector. The names
        ## of the arguments represent the column names. The value of the
        ## arguments should be a string that can be used for logical grep
        ## matching here internally.
        args <- list(...)
        assert(
            all(vapply(
                X = args,
                FUN = is.atomic,
                FUN.VALUE = logical(1L)
            )),
            msg = "Arguments must be atomic."
        )
        ## Match the arguments against the sample metadata.
        sampleData <- sampleData(object)
        ## Allowing the user to select by "sampleId".
        sampleData[["sampleId"]] <- rownames(sampleData)
        assert(hasRownames(sampleData))
        matches <- Map(
            col = names(args),
            arg = args,
            f = function(col, arg) {
                ## Check that column is present.
                if (!col %in% colnames(sampleData)) {
                    abort(sprintf(
                        "{.var %s} isn't present in {.fun %s}.",
                        col, "sampleData"
                    ))
                }
                ## Check that all items in argument are present.
                if (!all(arg %in% sampleData[[col]])) {
                    missing <- arg[which(!arg %in% sampleData[[col]])]
                    abort(sprintf(
                        "{.var %s} metadata column doesn't contain: %s.",
                        col, toInlineString(missing, n = 5L)
                    ))
                }
                ## Get the sample ID matches.
                keep <- sampleData[[col]] %in% arg
                data <- sampleData[keep, , drop = FALSE]
                rownames(data)
            }
        )
        samples <- Reduce(f = intersect, x = matches)
        assert(hasLength(samples))
        ## Output to the user which samples matched, using the `sampleName`
        ## metadata column, which is more descriptive than `sampleId`
        sampleNames <- sampleData[samples, "sampleName", drop = TRUE]
        sampleNames <- sort(unique(as.character(sampleNames)))
        alertInfo(sprintf(
            "%d %s matched: %s.",
            length(sampleNames),
            ngettext(
                n = length(sampleNames),
                msg1 = "sample",
                msg2 = "samples"
            ),
            toInlineString(sampleNames, n = 5L)
        ))

        colData <- colData(object)
        keep <- colData[["sampleId"]] %in% samples
        colData <- colData[keep, , drop = FALSE]
        cells <- rownames(colData)
        alertInfo(sprintf(
            "%d %s matched.",
            length(cells),
            ngettext(
                n = length(cells),
                msg1 = "cell",
                msg2 = "cells"
            )
        ))
        object <- object[, cells, drop = FALSE]
        metadata(object)[["selectSamples"]] <- TRUE
        object
    }



#' @rdname selectSamples
#' @export
setMethod(
    f = "selectSamples",
    signature = signature(object = "SingleCellExperiment"),
    definition = `selectSamples,SCE`
)
