#' @name metrics
#' @inherit AcidGenerics::metrics
#' @author Michael Steinbaugh, Rory Kirchner
#' @note Updated 2021-02-26.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- calculateMetrics(object)
#' x <- metrics(object)
#' print(x)
#' x <- metricsPerSample(object, fun = "mean")
#' print(x)
NULL



## Updated 2021-02-26.
`metrics,SCE` <-  # nolint
    function(object, return = c("tbl_df", "DataFrame")) {
        validObject(object)
        return <- match.arg(return)
        denylist <- c("cell", "sample")
        data <- colData(object)
        data <- data[, setdiff(colnames(data), denylist), drop = FALSE]
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        ## Automatically assign `sampleId` column, if necessary.
        if (!isSubset("sampleId", colnames(data))) {
            data[["sampleId"]] <- factor("unknown")
        }
        ## Automatically assign `sampleName` column, if necessary.
        if (!isSubset("sampleName", colnames(data))) {
            data[["sampleName"]] <- data[["sampleId"]]
        }
        data <- uniteInterestingGroups(
            object = data,
            interestingGroups = matchInterestingGroups(object)
        )
        switch(
            EXPR = return,
            "DataFrame" = data,
            "tbl_df" = as_tibble(data, rownames = "cellId")
        )
    }



#' @describeIn metrics Cell-level metrics.
#' @export
setMethod(
    f = "metrics",
    signature = signature("SingleCellExperiment"),
    definition = `metrics,SCE`
)



## Updated 2020-01-20.
`metricsPerSample,SCE` <-  # nolint
    function(
        object,
        fun = c("mean", "median", "sum"),
        return = c("tbl_df", "DataFrame")
    ) {
        fun <- match.arg(fun)
        return <- match.arg(return)
        alert(sprintf("Calculating %s per sample.", fun))
        ## Consider using `getFromNamespace` here instead.
        ## Note that we're using uppercase here, because `fun` is matched arg.
        FUN <- get(fun, inherits = TRUE)  # nolint
        assert(is.function(FUN))
        data <- colData(object)
        ## Decode columns that contain Rle, if necessary.
        data <- decode(data)
        # Subset the relevant metrics columns.
        if (identical(fun, "sum")) {
            pattern <- "^n[A-Z0-9]"
            if (!any(grepl(pattern, colnames(data)))) {
                stop(
                    "'sum()' method only applies to 'colData()' columns ",
                    "prefixed with 'n' (e.g. 'nCount')."
                )
            }
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
        switch(
            EXPR = return,
            "DataFrame" = data,
            "tbl_df" = as_tibble(data, rownames = "sampleId")
        )
    }



#' @describeIn metrics Sample-level metrics.
#' @export
setMethod(
    f = "metricsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = `metricsPerSample,SCE`
)
