#' @name convertSampleIdsToNames
#' @inherit AcidGenerics::convertSampleIdsToNames
#' @note Updated 2022-03-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' convertSampleIdsToNames(object)
NULL



## Updated 2021-02-02.
`convertSampleIdsToNames,SCE` <- # nolint
    function(object) {
        alertWarning(sprintf(
            paste(
                "{.var %s} contains cells instead of samples.",
                "Returning with column names unmodified."
            ),
            "SingleCellExperiment"
        ))
        object
    }



#' @rdname convertSampleIdsToNames
#' @export
setMethod(
    f = "convertSampleIdsToNames",
    signature = signature(object = "SingleCellExperiment"),
    definition = `convertSampleIdsToNames,SCE`
)
