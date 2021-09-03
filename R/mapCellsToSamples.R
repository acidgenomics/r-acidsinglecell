#' Map cells to samples
#'
#' This function extracts `sampleId` from the `cellId` column using grep
#' matching.
#'
#' @name mapCellsToSamples
#' @note Updated 2021-02-02.
#' @export
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `factor`.
#' Cells as the names and samples as the levels.
#'
#' @examples
#' samples <- paste0("sample", seq_len(2L))
#' print(samples)
#' cells <- paste(samples, c("AAAAAAAA", "CCCCCCCC"), sep = "_")
#' print(cells)
#' mapCellsToSamples(cells, samples)
mapCellsToSamples <- function(cells, samples) {
    assert(
        isCharacter(cells), hasNoDuplicates(cells),
        isCharacter(samples), hasNoDuplicates(samples)
    )
    ## Early return as simple factor for single sample.
    ## This code is useful for working with some example objects (e.g. PBMC).
    if (isString(samples)) {
        ## Check that cells input contains expected barcodes with DNA bases.
        assert(allAreMatchingRegex(x = cells, pattern = "[ACGT]+$"))
        cell2sample <- factor(replicate(n = length(cells), expr = samples))
        names(cell2sample) <- cells
        return(cell2sample)
    }
    ## Check that all cells contain a separator.
    assert(allAreMatchingRegex(x = cells, pattern = "[_-]"))
    list <- lapply(X = samples, FUN = function(sample) {
        pattern <- paste0("^(", sample, barcodePattern)
        match <- str_match(string = cells, pattern = pattern)
        assert(
            !all(is.na(match[, 1L])),
            msg = sprintf(
                "'%s' sample failed to match any cells.", sample
            )
        )
        ## Trailing number is for matching Cell Ranger output.
        ## This gets used in the Chromium package.
        colnames(match) <- c(
            "cellId",
            "sampleId",
            "cellularBarcode",
            "trailingNumber"
        )
        match <- match[, c("cellId", "sampleId")]
        keep <- !is.na(match[, "sampleId", drop = TRUE])
        match <- match[keep, , drop = FALSE]
        out <- match[, "sampleId", drop = TRUE]
        names(out) <- match[, "cellId", drop = TRUE]
        out
    })
    cell2sample <- unlist(list, recursive = FALSE, use.names = TRUE)
    assert(identical(length(cells), length(cell2sample)))
    as.factor(cell2sample)
}
