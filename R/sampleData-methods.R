#' @name sampleData
#' @inherit AcidGenerics::sampleData
#' @note Updated 2023-03-27.
#'
#' @section SingleCellExperiment:
#'
#' Recommended `colData`:
#'
#' - `sampleId`: `factor` defining cell-to-sample mappings. These mappings
#' should use syntactically valid names. Note that this is not currently
#' required as we're supporting `SingleCellExperiment` objects from 1 sample,
#' but it's required for working with multiple samples in a single object.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param clean `logical(1)`.
#' Only return `factor` columns. Useful when working with objects that contain
#' quality control metrics in `colData()`.
#' For example, `bcbioRNASeq` and `DESeqDataSet` objects often contain
#' additional columns that aren't informative sample metadata.
#'
#' @param ignoreCols `character` or `NULL`.
#' Only applies when `clean = TRUE`. Additional factor columns defined in
#' `colData` to be ignored as sample-level metadata. Particularly useful for
#' `SingleCellExperiment` objects, where cell-to-sample mappings are defined
#' using the `sampleId` column.
#'
#' @param denylistCols `character` or `NULL`.
#' Column names that should not be treated as sample-level metadata.
#' Currently applicable only to `SingleCellExperiment` objects, which have
#' cell-level columns that can be difficult to distinguish, especially when
#' processed using Seurat, scater, etc.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' sampleData(object)
#'
#' ## Assignment support.
#' sampleData(object)[["batch"]] <- 1L
#' ## `batch` column should be now defined.
#' sampleData(object)
NULL



## Don't run validity checks here.
## Updated 2024-03-27.
`sampleData,SCE` <- # nolint
    function(object,
             clean = TRUE,
             ignoreCols = c(
                 "^description$",
                 "^genomeBuild$",
                 "^qualityFormat$",
                 "^samRef$"
             ),
             denylistCols = c(
                 "^ident$",
                 "^g2mScore$",
                 "^sScore$",
                 "^phase$",
                 "^oldIdent$",
                 "^origIdent$",
                 "^res[0-9]+"
             )) {
        data <- colData(object)
        if (!hasRows(data)) {
            return(data)
        }
        if (hasColnames(data)) {
            colnames(data) <- camelCase(colnames(data), strict = TRUE)
        }
        assert(
            hasRownames(data),
            isFlag(clean),
            isCharacter(ignoreCols, nullOk = TRUE),
            isCharacter(denylistCols, nullOk = TRUE),
            areDisjointSets("interestingGroups", colnames(data))
        )
        interestingGroups <- matchInterestingGroups(object)
        ## Prepare columns -----------------------------------------------------
        ## Generate `sampleId` and `sampleName` columns, if necessary. We're not
        ## requiring `sampleId` because many SingleCellExperiment objects are
        ## derived from a single sample (e.g. 10X PBMC example data). Note that
        ## `SummarizedExperiment` method differs by not allowing the `sampleId`
        ## column, which are the `colnames` of the object.
        ## `SingleCellExperiment` maps cells to `colnames` instead of samples,
        ## so this factor is necessary when handling multiple samples.
        if (!"sampleId" %in% colnames(data)) {
            data[["sampleId"]] <- factor("unknown")
        }
        assert(is.factor(data[["sampleId"]]))
        if (!"sampleName" %in% colnames(data)) {
            data[["sampleName"]] <- data[["sampleId"]]
        }
        assert(is.factor(data[["sampleName"]]))
        ## Drop any denylisted cell-level columns.
        if (is.character(denylistCols)) {
            keep <- !grepl(
                pattern = paste(denylistCols, collapse = "|"),
                x = camelCase(colnames(data), strict = TRUE)
            )
            data <- data[, keep, drop = FALSE]
        }
        ## Clean mode ----------------------------------------------------------
        if (isTRUE(clean)) {
            ## Return only a subset of factor columns.
            keep <- bapply(X = data, FUN = is.factor)
            data <- data[, keep, drop = FALSE]
            ## Drop any additional uninformative columns to ignore.
            if (is.character(ignoreCols)) {
                keep <- !grepl(
                    pattern = paste(ignoreCols, collapse = "|"),
                    x = camelCase(colnames(data), strict = TRUE)
                )
                data <- data[, keep, drop = FALSE]
            }
        }
        ## Drop rows with too many uniques (cell level) ------------------------
        nSamples <- length(unique(data[["sampleId"]]))
        assert(all(isPositive(nSamples)))
        ## Keep columns that have have less than or equal the same number of
        ## uniques as the the number of samples. Note that this step is really
        ## essential, especially when QC metrics are slotted into `colData()`.
        keep <- bapply(
            X = data,
            FUN = function(x) {
                length(unique(x)) <= nSamples
            }
        )
        data <- data[, keep, drop = FALSE]
        ## Check rows with same number of uniques ------------------------------
        ## For columns that have the exact same number of uniques as the number
        ## of samples, they need to match our `sampleId` column factor levels
        ## exactly, otherwise we can run into issues where cell-level values
        ## appear to be sample level. Create a factor integer table to check for
        ## this. Shouldn't apply too often but can happen for some edge cases.
        ftable <- data
        keep <- bapply(
            X = ftable,
            FUN = function(x) {
                identical(length(unique(x)), nSamples)
            }
        )
        ftable <- ftable[, keep, drop = FALSE]
        ## Don't use `as.factor()` to `as.integer()` chain here. If you have
        ## factors with different levels when sorted alphabetically, this will
        ## not work as expected.
        ##
        ## Example edge case:
        ## - sampleId: sample2, sample
        ## - sampleName: a, b
        ##
        ## See how these factor levels will flip in the index and not match.
        ## Instead, we need to enforce the factor levels by order of appearance.
        ##
        ## ftable here represents a numeric factor index table.
        factorToInteger <- function(x) {
            as.integer(factor(x, levels = unique(x)))
        }
        ftable <- mutateAll(object = ftable, fun = factorToInteger)
        trash <- !bapply(
            X = ftable,
            FUN = function(x) {
                identical(x, ftable[["sampleId"]])
            }
        )
        if (any(trash)) {
            keep <- setdiff(colnames(data), names(trash[trash]))
            data <- data[, keep, drop = FALSE]
        }
        assert(isSubset(c("sampleId", interestingGroups), colnames(data)))
        ## Collapse to sample level --------------------------------------------
        ## Collapse and set the row names to `sampleId`.
        rownames(data) <- NULL
        data <- unique(data)
        if (nrow(data) > nSamples || hasDuplicates(data[["sampleId"]])) {
            abort(sprintf(
                fmt = paste(
                    "Failed to collapse {.fun %s} to sample level.",
                    "Check: %s.",
                    sep = "\n"
                ),
                "colData",
                toInlineString(colnames(data), n = 10L)
            ))
        }
        rownames(data) <- data[["sampleId"]]
        ## Returning arranged by `sampleId`. Use `setdiff()` approach instead of
        ## `NULL` assignment on `sampleId` column to maintain backwards
        ## compatibility prior to BioC 3.8.
        data <- data[
            rownames(data),
            setdiff(colnames(data), "sampleId"),
            drop = FALSE
        ]
        ## Add interesting groups column.
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = interestingGroups
        )
        ## Return.
        assert(
            is.factor(data[["interestingGroups"]]),
            is.factor(data[["sampleName"]])
        )
        data
    }



## Updated 2023-10-04.
`sampleData<-,SCE,DFrame` <- # nolint
    function(object, value) {
        assert(hasRownames(value))
        denylist <- c("interestingGroups", "rowname", "sampleId")
        keep <- setdiff(colnames(value), denylist)
        assert(hasLength(keep))
        value <- value[, keep, drop = FALSE]
        value[["sampleId"]] <- as.factor(rownames(value))
        colData <- colData(object)
        assert(isSubset("sampleId", colnames(colData)))
        colData <- colData[
            ,
            c("sampleId", setdiff(colnames(colData), colnames(value))),
            drop = FALSE
        ]
        out <- leftJoin(colData, value, by = "sampleId")
        print(out)
        assert(
            is(out, "DFrame"),
            hasRownames(out)
        )
        colData(object) <- out
        object
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleData",
    signature = signature(object = "SingleCellExperiment"),
    definition = `sampleData,SCE`
)



#' @rdname sampleData
#' @export
setReplaceMethod(
    f = "sampleData",
    signature = signature(
        object = "SingleCellExperiment",
        value = "DFrame"
    ),
    definition = `sampleData<-,SCE,DFrame`
)
