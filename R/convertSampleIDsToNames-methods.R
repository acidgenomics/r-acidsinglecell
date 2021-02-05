#' @name convertSampleIDsToNames
#' @inherit AcidGenerics::convertSampleIDsToNames
#' @note Updated 2022-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' convertSampleIDsToNames(object)
NULL



## Updated 2021-02-02.
`convertSampleIDsToNames,SCE` <-  # nolint
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



#' @rdname convertSampleIDsToNames
#' @export
setMethod(
    f = "convertSampleIDsToNames",
    signature = signature("SingleCellExperiment"),
    definition = `convertSampleIDsToNames,SCE`
)
