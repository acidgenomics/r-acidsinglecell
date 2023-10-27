#' Get the inDrops sample index counts from i5 FASTQ files
#'
#' @export
#' @note Updated 2023-10-27.
#'
#' @param dir `character(1)`.
#' Directory containing inDrops FASTQ files.
#' Must contain R3 files, which contain the i5 index barcodes.
#'
#' @param r3Pattern `character(1)`.
#' Tail pattern for matching the R3 FASTQ files.
#' Passed to `list.files()` internally as `pattern` argument.
#'
#' @param n `integer(1)`.
#' Number of top barcodes to return.
#'
#' @return `SimpleList`.
#' Names correspond to matched file names.
#'
#' @examples
#' ## > indropsSampleIndexCounts(dir = file.path("indrops", "fastq"))
indropsSampleIndexCounts <-
    function(dir,
             r3Pattern = "_R3\\.fastq\\.gz$",
             n = 24L) {
        assert(
            isADir(dir),
            isString(r3Pattern),
            isInt(n)
        )
        dir <- realpath(dir)
        files <- sort(list.files(
            path = dir,
            pattern = r3Pattern,
            full.names = TRUE,
            recursive = FALSE
        ))
        assert(
            hasLength(files),
            msg = sprintf(
                "Failed to match any R3 FASTQ files with '%s' pattern in '%s'.",
                r3Pattern, dir
            )
        )
        alert(sprintf(
            "Processing inDrops i5 barcode index counts in {.path %s}.",
            dir
        ))
        out <- lapply(
            X = files,
            n = n,
            FUN = function(file, n) {
                alert(sprintf("Processing {.file %s}.", file))
                dna <- import(file)
                assert(is(dna, "DNAStringSet"))
                tbl <- table(dna)
                tbl <- sort(tbl, decreasing = TRUE)
                tbl <- head(tbl, n = n)
                out <- as.integer(tbl)
                names(out) <- names(tbl)
                out
            }
        )
        out <- SimpleList(out)
        names(out) <- basename(files)
        metadata(out) <- list("dir" = dir)
        invisible(out)
    }
