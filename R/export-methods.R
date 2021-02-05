## FIXME MIGRATE SCE EXPORT METHOD FROM PIPETTE HERE.



#' Export `SingleCellExperiment` method
#'
#' @note Updated 2020-08-11.
#' @noRd
#'
#' @details
#' This method extends `SummarizedExperiment` but also handles export of the
#' `reducedDims` slot (i.e. containing UMAP, tSNE, and PCA).
`export,SCE` <-  # nolint
    function(
        object,
        name = NULL,
        dir,
        compress,
        overwrite,
        quiet
    ) {
        validObject(object)
        assert(
            isString(name, nullOK = TRUE),
            isString(dir),
            isFlag(compress),
            isFlag(overwrite),
            isFlag(quiet)
        )
        call <- standardizeCall()
        if (is.null(name)) {
            sym <- call[["object"]]
            assert(is.symbol(sym))
            name <- as.character(sym)
        }
        ## Export SummarizedExperiment-compatible slots.
        files <- export(
            object = as(object, "RangedSummarizedExperiment"),
            name = name,
            dir = dir,
            compress = compress,
            overwrite = overwrite,
            quiet = quiet
        )
        ## Export dimensionality reduction data.
        dir <- initDir(file.path(dir, name))
        reducedDimNames <- reducedDimNames(object)
        if (hasLength(reducedDimNames)) {
            if (!isTRUE(quiet)) {
                alert(sprintf(
                    "Exporting {.var reducedDims}: {.var %s}",
                    toString(reducedDimNames)
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
                    } else if (is(reducedDim, "sparseMatrix")) {  # nocov
                        ext <- "mtx"  # nocov
                    }
                    if (isTRUE(compress)) {
                        ext <- paste0(ext, ".gz")
                    }
                    file <- paste0(file, ".", ext)
                    export(
                        object = reducedDim,
                        file = file,
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

formals(`export,SCE`)[
    c("compress", "dir", "overwrite", "quiet")] <-
    formalsList[c("export.compress", "export.dir", "overwrite", "quiet")]



#' @rdname export
#' @export
setMethod(
    f = "export",
    signature = signature("SingleCellExperiment"),
    definition = `export,SCE`
)
