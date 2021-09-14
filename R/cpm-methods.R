#' @name cpm
#' @inherit AcidGenerics::cpm
#' @keywords internal
#' @note Updated 2021-09-14.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `SingleCellExperiment::cpm()`.
#' - `edgeR::cpm()`.
#' - `scater::calculateCPM()`.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- estimateSizeFactors(object)
#' cpm <- cpm(object)
#' class(cpm)
#' mean(cpm)
NULL



## Updated 2021-09-14.
`cpm,SCE` <-  # nolint
    function(object) {
        ## Early return if cpm assay is defined.
        if (isSubset("cpm", assayNames(object))) {
            return(assay(x = object, i = "cpm"))
        }
        assert(
            is.numeric(sizeFactors(object)),
            msg = "Size factors are not defined in object."
        )
        alert(sprintf(
            "Calculating CPM with {.pkg %s}::{.fun %s}.",
            "scater", "calculateCPM"
        ))
        requireNamespaces("scater")
        scater::calculateCPM(object)
    }



#' @rdname cpm
#' @export
setMethod(
    f = "cpm",
    signature = signature("SingleCellExperiment"),
    definition = `cpm,SCE`
)
