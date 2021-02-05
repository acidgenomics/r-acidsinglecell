#' @name combine
#' @inherit AcidExperiment::combine
#' @note Updated 2021-02-05.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#' str_pad <- stringr::str_pad
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment
#' colnames(x) <- paste0(
#'     "cell",
#'     str_pad(
#'         string = seq_len(ncol(x)),
#'         width = 4L,
#'         pad = "0"
#'     )
#' )
#'
#' y <- x
#' colnames(y) <- paste0(
#'     "cell",
#'     str_pad(
#'         string = seq_len(ncol(y)) + ncol(y),
#'         width = 4L,
#'         pad = "0"
#'     )
#' )
#'
#' ## Combine the two objects.
#' c <- combine(x, y)
#' sampleData(c)
#' print(c)
NULL



## Updated 2019-08-27.
`combine,SCE` <-  # nolint
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
