#' @name export
#' @inherit pipette::export
#' @note Updated 2022-09-13.
#'
#' @details
#' This method extends `SummarizedExperiment` but also handles export of the
#' `reducedDims` slot (i.e. containing UMAP, tSNE, and PCA).
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams AcidExperiment::export
#' @inheritParams pipette::export
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' con <- AcidBase::tempdir2()
#' x <- export(object = object, con = con)
#' print(x)
#' AcidBase::unlink2(con)
NULL



## Updated 2023-09-20.
`export,SCE` <- # nolint
    function(object,
             con,
             compress = FALSE,
             overwrite = TRUE,
             quiet = FALSE) {
        assert(
            validObject(object),
            isString(con),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        dir <- initDir(con)
        ## Export SummarizedExperiment-compatible slots.
        files <- export(
            object = as(object, "RangedSummarizedExperiment"),
            con = dir,
            compress = compress,
            overwrite = overwrite,
            quiet = quiet
        )
        ## Export dimensionality reduction data.
        reducedDimNames <- reducedDimNames(object)
        if (hasLength(reducedDimNames)) {
            if (!isTRUE(quiet)) {
                alert(sprintf(
                    "Exporting {.var %s}: %s.",
                    "reducedDims",
                    toInlineString(reducedDimNames, n = 5L)
                ))
            }
            files[["reducedDims"]] <- lapply(
                X = reducedDimNames,
                FUN = function(name, dir) {
                    file <- file.path(dir, name)
                    reducedDim <- reducedDim(
                        x = object,
                        type = name,
                        withDimnames = TRUE
                    )
                    if (is(reducedDim, "matrix")) {
                        ext <- "csv"
                    } else if (is(reducedDim, "sparseMatrix")) { # nocov
                        ext <- "mtx" # nocov
                    }
                    if (isTRUE(compress)) {
                        ext <- paste0(ext, ".gz")
                    }
                    file <- paste0(file, ".", ext)
                    export(
                        object = reducedDim,
                        con = file,
                        overwrite = overwrite,
                        quiet = quiet
                    )
                },
                dir = initDir(file.path(dir, "reducedDims"))
            )
            names(files[["reducedDims"]]) <- reducedDimNames
        }
        assert(hasNames(files))
        invisible(files)
    }



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "SingleCellExperiment",
        con = "character"
    ),
    definition = `export,SCE`
)
