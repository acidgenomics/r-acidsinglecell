test_that("importCellTypeMarkers", {
    markersDir <- system.file(
        file.path("extdata", "markers"),
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
    expect_s4_class(object, "CellTypeMarkers")
    expect_named(
        object = object,
        expected = c(
            "crystal_cell",
            "lamellocyte",
            "plasmatocyte",
            "prohemocyte"
        )
    )
    expect_identical(
        object = object[["crystal_cell"]],
        expected = DataFrame(
            "cellType" = factor(
                x = rep("Crystal Cell", times = 5L),
                levels = c(
                    "Crystal Cell",
                    "Lamellocyte",
                    "Plasmatocyte",
                    "Prohemocyte"
                )
            ),
            "geneId" = c(
                "FBgn0283437",
                "FBgn0039938",
                "FBgn0013469",
                "FBgn0002576",
                "FBgn0002643"
            ),
            "geneName" = c(
                "PPO1",
                "Sox102F",
                "klu",
                "lz",
                "mam"
            )
        )
    )
})
