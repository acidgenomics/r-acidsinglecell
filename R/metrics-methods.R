#' @name metrics
#' @inherit AcidGenerics::metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2022-03-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' object <- AcidExperiment::calculateMetrics(object)
#' x <- metrics(object)
#' print(x)
#' x <- metricsPerSample(object, fun = "mean")
#' print(x)
NULL



## Updated 2022-05-04.
`metrics,SCE` <- # nolint
    function(object) {
        validObject(object)
        denylist <- c("cell", "sample")
        df <- colData(object)
        df <- df[, setdiff(colnames(df), denylist), drop = FALSE]
        ## Decode columns that contain Rle, if necessary.
        df <- decode(df)
        ## Automatically assign `sampleId` column, if necessary.
        if (!isSubset("sampleId", colnames(df))) {
            df[["sampleId"]] <- factor("unknown") # nocov
        }
        ## Automatically assign `sampleName` column, if necessary.
        if (!isSubset("sampleName", colnames(df))) {
            df[["sampleName"]] <- df[["sampleId"]]
        }
        df <- uniteInterestingGroups(
            object = df,
            interestingGroups = matchInterestingGroups(object)
        )
        df
    }



## Updated 2022-05-04.
`metricsPerSample,SCE` <- # nolint
    function(object,
             fun = c("mean", "median", "sum")) {
        fun <- match.arg(fun)
        return <- match.arg(return)
        alert(sprintf("Calculating %s per sample.", fun))
        ## Consider using `getFromNamespace` here instead.
        ## Note that we're using uppercase here, because `fun` is matched arg.
        FUN <- get(fun, inherits = TRUE) # nolint
        assert(is.function(FUN))
        data <- colData(object)
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        # Subset the relevant metrics columns.
        if (identical(fun, "sum")) {
            pattern <- "^n[A-Z0-9]"
            assert(
                any(grepl(pattern, colnames(data))),
                msg = sprintf(
                    fmt = paste(
                        "'%s' method only applies to '%s' columns ",
                        "prefixed with '%s' (e.g. '%s')."
                    ),
                    "sum()", "colData()",
                    "n", "nCount"
                )
            )
            ## Sum only the `n*` columns containing counts.
            ## Supress: Adding missing grouping variables: `sampleId`.
            keep <- grepl(pattern = pattern, x = colnames(data))
        } else {
            ## Summarize all numeric columns.
            keep <- bapply(data, is.numeric)
        }
        split <- split(data, f = data[["sampleId"]])
        split <- split[, keep]
        split <- DataFrameList(lapply(
            X = split,
            FUN = function(x) {
                DataFrame(lapply(X = x, FUN = FUN))
            }
        ))
        data <- unlist(split, recursive = FALSE, use.names = TRUE)
        sampleData <- sampleData(object)
        data <- data[rownames(sampleData), , drop = FALSE]
        data <- cbind(sampleData, data)
        data
    }



#' @describeIn metrics Cell-level metrics.
#' @export
setMethod(
    f = "metrics",
    signature = signature(object = "SingleCellExperiment"),
    definition = `metrics,SCE`
)

#' @describeIn metrics Sample-level metrics.
#' @export
setMethod(
    f = "metricsPerSample",
    signature = signature(object = "SingleCellExperiment"),
    definition = `metricsPerSample,SCE`
)
