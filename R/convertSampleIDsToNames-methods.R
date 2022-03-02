#' @name convertSampleIDsToNames
#' @inherit AcidGenerics::convertSampleIDsToNames
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
    signature = signature(object = "SingleCellExperiment"),
    definition = `convertSampleIDsToNames,SCE`
)
