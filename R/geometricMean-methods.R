#' @name geometricMean
#' @inherit AcidGenerics::geometricMean
#' @note Updated 2022-10-24.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams base::apply
#' @param ... Additional arguments.
#'
#' @examples
#' data(sparseMatrix, package = "AcidTest")
#'
#' ## Matrix ====
#' x <- sparseMatrix
#' geometricMean(x)
NULL



## Updated 2022-10-24.
`geometricMean,sparseMatrix` <- # nolint
    methodFunction(
        f = "geometricMean",
        signature = "matrix",
        package = "AcidBase"
    )



#' @rdname geometricMean
#' @export
setMethod(
    f = "geometricMean",
    signature = signature(x = "sparseMatrix"),
    definition = `geometricMean,sparseMatrix`
)
