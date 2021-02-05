## FIXME SIMPLIFY DOCUMENTATION, REMOVING RSE EXAMPLE...
## FIXME DOES THIS RETURN RSE OR SCE?



#' @name combine
#' @inherit AcidGenerics::combine return title
#' @note Updated 2021-02-21.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @note We're attempting to make this as strict as possible, requiring:
#'
#' - Rows (genes) across objects must be identical.
#' - [rowRanges][SummarizedExperiment::rowRanges] and/or
#'   [rowData][SummarizedExperiment::rowData]
#'   [metadata][S4Vectors::metadata] must be identical.
#' - [colData][SummarizedExperiment::colData] must contain the same columns.
#' - Specific metadata must be identical (see `metadata` argument).
#'
#' @seealso
#' - `help("merge.Matrix", "Matrix.utils")`.
#'
#' @return `SummarizedExperiment`.
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
