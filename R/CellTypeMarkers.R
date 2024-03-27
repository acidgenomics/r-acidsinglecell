#' Cell-type markers
#'
#' @name CellTypeMarkers
#' @note Updated 2024-03-27.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams AcidGenomes::makeGRangesFromEnsembl
#'
#' @return `CellTypeMarkers`.
#'
#' @examples
#' markersDir <- system.file(
#'     "extdata", "markers",
#'     package = "AcidSingleCell"
#' )
#' cellTypeDir <- file.path(markersDir, "cell-type")
#' files <- sort(list.files(
#'     path = cellTypeDir,
#'     pattern = "*.csv",
#'     full.names = TRUE
#' ))
#' file <- files[[1L]]
#' organism <- syntactic::sentenceCase(
#'     gsub(
#'         pattern = "-",
#'         replacement = " ",
#'         x = AcidBase::basenameSansExt(file)
#'     )
#' )
#' releaseFile <- file.path(markersDir, "ensembl-release.txt")
#' release <- as.integer(readLines(releaseFile))
#' object <- importCellTypeMarkers(
#'     file = file,
#'     organism = organism,
#'     release = release
#' )
#' print(object)
NULL



#' @rdname CellTypeMarkers
#' @export
CellTypeMarkers <- # nolint
    function(object, geneToSymbol) {
        assert(is(object, "DFrame"))
        data <- .CellMarkers(
            object = object,
            geneToSymbol = geneToSymbol,
            class = "CellTypeMarkers"
        )
        new(Class = "CellTypeMarkers", data)
    }



#' @rdname CellTypeMarkers
#' @export
importCellTypeMarkers <-
    function(file,
             organism,
             release,
             ignoreVersion = TRUE) {
        object <- import(file)
        object <- as(object, "DFrame")
        gr <- makeGRangesFromEnsembl(
            organism = organism,
            release = release,
            ignoreVersion = ignoreVersion,
            extraMcols = FALSE
        )
        geneToSymbol <- GeneToSymbol(gr)
        out <- CellTypeMarkers(
            object = object,
            geneToSymbol = geneToSymbol
        )
        out
    }
