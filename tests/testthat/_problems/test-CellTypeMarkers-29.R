# Extracted from test-CellTypeMarkers.R:29

# nolint start

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "AcidSingleCell", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
markersDir <- system.file(
        "extdata", "markers",
        package = .pkgName
    )
cellTypeDir <- file.path(markersDir, "cell-type")
files <- sort(list.files(
        path = cellTypeDir,
        pattern = "*.csv",
        full.names = TRUE
    ))
file <- files[[1L]]
organism <- sentenceCase(
        gsub(
            pattern = "-",
            replacement = " ",
            x = basenameSansExt(file)
        )
    )
releaseFile <- file.path(markersDir, "ensembl-release.txt")
release <- as.integer(readLines(releaseFile))
object <- with_collate(
        new = "C",
        code = {
            importCellTypeMarkers(
                file = file,
                organism = organism,
                release = release
            )
        }
    )

# nolint end
