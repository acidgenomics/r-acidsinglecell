#' Show an object
#' @name show
#' @author Michael Steinbuagh
#' @inherit methods::show title description params
#' @note Updated 2021-10-15.
#'
#' @return No return.
#'
#' @examples
#' data(cellCycleMarkersList, cellTypeMarkersList)
#'
#' ## CellCycleMarkers ====
#' object <- cellCycleMarkersList[[1L]]
#' show(object)
#'
#' ## CellTypeMarkers ====
#' object <- cellTypeMarkersList[[1L]]
#' show(object)
NULL



## Updated 2022-03-02.
`show,CellCycleMarkers` <- # nolint
    function(object) {
        validObject(object)
        ## Include the organism information.
        organism <- metadata(object)[["organism"]]
        release <- metadata(object)[["release"]]
        lengths <- dims(object)[, 1L]
        genes <- vapply(
            X = object,
            FUN = function(x) {
                x <- as.character(x[["geneName"]])
                x <- sort(x)
                x <- c(head(x, n = 2L), "...", tail(x, n = 2L))
                paste(x, collapse = " ")
            },
            FUN.VALUE = character(1L),
            USE.NAMES = TRUE
        )
        return <- c(
            class(object),
            paste0(organism, " (Ensembl ", release, ")"),
            paste0(names(genes), "(", lengths, "): ", genes)
        )
        cat(return, sep = "\n")
    }



## Updated 2022-03-02.
`show,CellTypeMarkers` <- # nolint
    function(object) {
        validObject(object)
        ## Include the organism information.
        organism <- metadata(object)[["organism"]]
        release <- metadata(object)[["release"]]
        lengths <- dims(object)[, 1L]
        genes <- vapply(
            X = object,
            FUN = function(x) {
                x <- as.character(x[["geneName"]])
                x <- sort(x)
                x <- c(head(x, n = 2L), "...", tail(x, n = 2L))
                paste(x, collapse = " ")
            },
            FUN.VALUE = character(1L),
            USE.NAMES = TRUE
        )
        return <- c(
            class(object),
            paste0(organism, " (Ensembl ", release, ")"),
            paste0(names(genes), "(", lengths, "): ", genes)
        )
        cat(return, sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "CellCycleMarkers"),
    definition = `show,CellCycleMarkers`
)

#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "CellTypeMarkers"),
    definition = `show,CellTypeMarkers`
)
