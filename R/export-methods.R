#' @name export
#' @inherit pipette::export
#' @note Updated 2022-03-02.
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
#' con <- file.path(tempdir(), "example")
#' x <- export(object = object, con = con)
#' print(x)
#' unlink(con, recursive = TRUE)
NULL



## Updated 2021-10-14.
`export,SCE` <- # nolint
    function(object,
             con,
             format, # NULL
             compress = getOption(
                 x = "acid.export.compress",
                 default = FALSE
             ),
             overwrite = getOption(
                 x = "acid.overwrite",
                 default = TRUE
             ),
             quiet = getOption(
                 x = "acid.quiet",
                 default = FALSE
             )) {
        validObject(object)
        if (missing(format)) {
            format <- NULL
        }
        assert(
            isString(con),
            is.null(format),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        dir <- initDir(con)
        ## Export SummarizedExperiment-compatible slots.
        files <- export(
            object = as(object, "RangedSummarizedExperiment"),
            con = dir,
            format = format,
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



## Updated 2021-10-15.
`export,SCE,deprecated` <- # nolint
    methodFunction(
        f = "export",
        signature = signature(
            object = "SummarizedExperiment",
            con = "missingOrNULL",
            format = "missingOrNULL"
        ),
        package = "AcidExperiment"
    )



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "SingleCellExperiment",
        con = "character",
        format = "missingOrNULL"
    ),
    definition = `export,SCE`
)

#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature(
        object = "SingleCellExperiment",
        con = "missingOrNULL",
        format = "missingOrNULL"
    ),
    definition = `export,SCE,deprecated`
)
