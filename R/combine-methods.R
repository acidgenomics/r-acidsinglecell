#' @name combine
#' @inherit AcidExperiment::combine
#' @note Updated 2022-03-21.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment_splatter
#' colnames(x) <- paste0(
#'     "cell",
#'     AcidBase::strPad(
#'         x = as.character(seq_len(ncol(x))),
#'         width = 4L,
#'         side = "left",
#'         pad = "0"
#'     )
#' )
#' y <- x
#' colnames(y) <- paste0(
#'     "cell",
#'     AcidBase::strPad(
#'         x = as.character(seq_len(ncol(y)) + ncol(y)),
#'         width = 4L,
#'         side = "left",
#'         pad = "0"
#'     )
#' )
#' ## Combine the two objects.
#' c <- combine(x, y)
#' sampleData(c)
#' print(c)
NULL



## Updated 2019-08-27.
`combine,SCE` <- # nolint
    function(x, y) {
        validObject(x)
        validObject(y)
        x <- as(object = x, Class = "RangedSummarizedExperiment")
        y <- as(object = y, Class = "RangedSummarizedExperiment")
        rse <- combine(x = x, y = y)
        validObject(rse)
        sce <- as(rse, "SingleCellExperiment")
        validObject(sce)
        sce
    }



#' @rdname combine
#' @export
setMethod(
    f = "combine",
    signature = signature(
        x = "SingleCellExperiment",
        y = "SingleCellExperiment"
    ),
    definition = `combine,SCE`
)
