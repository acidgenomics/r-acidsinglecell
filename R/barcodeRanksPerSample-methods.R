#' @name barcodeRanksPerSample
#' @inherit AcidGenerics::barcodeRanksPerSample
#' @note Requires DropletUtils package to be installed.
#' @note Updated 2025-04-12.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to `DropletUtils::barcodeRanks()`.
#'
#' @seealso
#' - `DropletUtils::barcodeRanks()`.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' if (requireNamespace("DropletUtils", quietly = TRUE)) {
#'     object <- SingleCellExperiment_splatter
#'     x <- barcodeRanksPerSample(object)
#'     names(x)
#' }
NULL



## nolint start
## Ensure we muffle this warning:
## > Warning in smooth.spline(x[new.keep], y[new.keep], df = df, ...) :
## > not using invalid df; must have 1 < df <= n := #{unique x} = 13
## > Calls: barcodeRanksPerSample ... lapply -> FUN -> do.call -> <Anonymous>
## > -> smooth.spline
## nolint end



## Updated 2025-04-12.
`barcodeRanksPerSample,SCE` <- # nolint
    function(object,
             assay = 1L,
             ...) {
        assert(isScalar(assay))
        requireNamespaces("DropletUtils")
        counts <- assay(object, i = assay)
        c2s <- cellToSample(object)
        samples <- levels(c2s)
        ## Subset the counts per sample into a list.
        countsPerSample <- lapply(
            X = samples,
            FUN = function(sample, counts) {
                cells <- names(c2s)[which(c2s == sample)]
                counts[, cells, drop = FALSE]
            },
            counts = counts
        )
        names(countsPerSample) <- samples
        ## Calculate the ranks per sample.
        ## Note that this now supports sparse matrices.
        out <- lapply(
            X = countsPerSample,
            FUN = function(counts) {
                x <- withCallingHandlers(
                    expr = {
                        DropletUtils::barcodeRanks(m = counts, ...)
                    },
                    warning = function(w) {
                        if (isTRUE(grepl(
                            pattern = "invalid df",
                            x = as.character(w)
                        ))) {
                            invokeRestart("muffleWarning")
                        } else {
                            w
                        }
                    }
                )
                ## Check DropletUtils return.
                assert(
                    is(x, "DFrame"),
                    ## Note that "fitted" column removed in DropletUtils 1.27
                    ## release, so adjusted the assert check here.
                    isSubset(
                        x = c("rank", "total"),
                        y = colnames(x)
                    ),
                    isSubset(
                        x = names(metadata(x)),
                        y = c("knee", "inflection")
                    )
                )
                x
            }
        )
        out <- DataFrameList(out)
        out
    }



#' @rdname barcodeRanksPerSample
#' @export
setMethod(
    f = "barcodeRanksPerSample",
    signature = signature(object = "SingleCellExperiment"),
    definition = `barcodeRanksPerSample,SCE`
)
