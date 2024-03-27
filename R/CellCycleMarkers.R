#' Cell-cycle markers
#'
#' @name CellCycleMarkers
#' @note Updated 2024-03-27.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams AcidGenomes::makeGRangesFromEnsembl
#'
#' @return `CellCycleMarkers`.
#'
#' @examples
#' markersDir <- system.file(
#'     "extdata", "markers",
#'     package = "AcidSingleCell"
#' )
#' cellCycleDir <- file.path(markersDir, "cell-cycle")
#' files <- sort(list.files(
#'     path = cellCycleDir,
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
#' object <- importCellCycleMarkers(
#'     file = file,
#'     organism = organism,
#'     release = release
#' )
#' print(object)
NULL



#' @rdname CellCycleMarkers
#' @export
CellCycleMarkers <- # nolint
    function(object, geneToSymbol) {
        assert(is(object, "DFrame"))
        data <- .CellMarkers(
            object = object,
            geneToSymbol = geneToSymbol,
            class = "CellCycleMarkers"
        )
        new(Class = "CellCycleMarkers", data)
    }



#' @rdname CellCycleMarkers
#' @export
importCellCycleMarkers <-
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
        out <- CellCycleMarkers(
            object = object,
            geneToSymbol = geneToSymbol
        )
        out
    }
