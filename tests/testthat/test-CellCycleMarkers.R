test_that("importCellCycleMarkers", {
    markersDir <- system.file(
        file.path("extdata", "markers"),
        package = .pkgName
    )
    cellCycleDir <- file.path(markersDir, "cell-cycle")
    files <- sort(list.files(
        path = cellCycleDir,
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
            importCellCycleMarkers(
                file = file,
                organism = organism,
                release = release
            )
        }
    )
    expect_s4_class(object, "CellCycleMarkers")
    expect_named(object, c("g2m", "s"))
    expect_identical(
        object = object[["g2m"]],
        expected = DataFrame(
            "geneId" = c(
                "FBgn0031696", "FBgn0263855", "FBgn0035769", "FBgn0039680",
                "FBgn0004106", "FBgn0037613", "FBgn0000405", "FBgn0264291",
                "FBgn0278608", "FBgn0030082", "FBgn0039019", "FBgn0037675",
                "FBgn0004378", "FBgn0034657", "FBgn0034282", "FBgn0000063",
                "FBgn0029970", "FBgn0031091", "FBgn0003346", "FBgn0003607",
                "FBgn0284220", "FBgn0000147", "FBgn0040233", "FBgn0040232",
                "FBgn0001086", "FBgn0015391", "FBgn0033845", "FBgn0027948",
                "FBgn0011692", "FBgn0003041", "FBgn0005683", "FBgn0029881",
                "FBgn0261385", "FBgn0003525", "FBgn0003545", "FBgn0002673",
                "FBgn0264848"
            ),
            "geneName" = c(
                "Bub1", "BubR1", "CTCF", "Cap-D2", "Cdk1", "Cks85A", "CycB",
                "Det", "Dsp1", "HP1b", "HP1c", "HP1e", "Klp61F", "LBR",
                "Mapmodulin", "Mps1", "Nek2", "Phf7", "RanGAP", "Su(var)205",
                "Top2", "aurA", "cana", "cmet", "fzy", "glu", "mars", "msps",
                "pav", "pbl", "pie", "pigs", "scra", "stg", "sub", "twe", "vih"
            ),
            "phase" = factor(
                x = rep("G2/M", times = 37L),
                levels = c("G2/M", "S")
            )
        )
    )
    expect_identical(
        object = object[["s"]],
        expected = DataFrame(
            "geneId" = c(
                "FBgn0002906", "FBgn0026143", "FBgn0032698", "FBgn0034495",
                "FBgn0032635", "FBgn0033526", "FBgn0035918", "FBgn0052251",
                "FBgn0010382", "FBgn0259113", "FBgn0011762", "FBgn0025832",
                "FBgn0014861", "FBgn0017577", "FBgn0025815", "FBgn0005655",
                "FBgn0032813", "FBgn0261976", "FBgn0032906", "FBgn0260985",
                "FBgn0011704", "FBgn0041186", "FBgn0024920", "FBgn0028476",
                "FBgn0015929", "FBgn0013548", "FBgn0015546", "FBgn0003479",
                "FBgn0015553"
            ),
            "geneName" = c(
                "Blm", "CDC45L", "CG10336", "CG11788", "CG15141", "Caf1-105",
                "Cdc6", "Claspin", "CycE", "DNApol-alpha180", "DNApol-alpha50",
                "Fen1", "Mcm2", "Mcm5", "Mcm6", "PCNA", "PCNA2", "Psf2", "RPA2",
                "RfC4", "RnrS", "Slbp", "Ts", "Usp1", "dpa", "l(2)dtl", "spel1",
                "spn-A", "tos"
            ),
            "phase" = factor(
                x = rep("S", times = 29L),
                levels = c("G2/M", "S")
            )
        )
    )
})
