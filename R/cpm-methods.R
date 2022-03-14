#' @name cpm
#' @inherit AcidGenerics::cpm
#' @keywords internal
#' @note Updated 2022-03-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `SingleCellExperiment::cpm()`.
#' - `edgeR::cpm()`.
#' - `scuttle::calculateCPM()`.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' object <- AcidExperiment::estimateSizeFactors(object)
#' cpm <- cpm(object)
#' class(cpm)
#' mean(cpm)
NULL



## Updated 2021-10-15.
`cpm,SCE` <- # nolint
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
            "scuttle", "calculateCPM"
        ))
        requireNamespaces("scuttle")
        out <- scuttle::calculateCPM(object)
        assert(is(out, "sparseMatrix"))
        out
    }



#' @rdname cpm
#' @export
setMethod(
    f = "cpm",
    signature = signature(object = "SingleCellExperiment"),
    definition = `cpm,SCE`
)
