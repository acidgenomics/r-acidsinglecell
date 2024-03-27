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
    object <- importCellCycleMarkers(
        file = file,
        organism = organism,
        release = release
    )
    expect_s4_class(object, "CellCycleMarkers")
    expect_named(object, c("g2m", "s"))
    expect_identical(
        object = object[["g2m"]],
        expected = DataFrame(
            "geneId" = c(
                "FBgn0000147", "FBgn0031696", "FBgn0263855", "FBgn0040233",
                "FBgn0039680", "FBgn0004106", "FBgn0037613", "FBgn0040232",
                "FBgn0035769", "FBgn0000405", "FBgn0264291", "FBgn0278608",
                "FBgn0001086", "FBgn0015391", "FBgn0030082", "FBgn0039019",
                "FBgn0037675", "FBgn0004378", "FBgn0034657", "FBgn0034282",
                "FBgn0033845", "FBgn0000063", "FBgn0027948", "FBgn0029970",
                "FBgn0011692", "FBgn0003041", "FBgn0031091", "FBgn0005683",
                "FBgn0029881", "FBgn0003346", "FBgn0261385", "FBgn0003525",
                "FBgn0003607", "FBgn0003545", "FBgn0284220", "FBgn0002673",
                "FBgn0264848"
            ),
            "geneName" = c(
                "aurA", "Bub1", "BubR1", "cana", "Cap-D2", "Cdk1", "Cks85A",
                "cmet", "CTCF", "CycB", "Det", "Dsp1", "fzy", "glu", "HP1b",
                "HP1c", "HP1e", "Klp61F", "LBR", "Mapmodulin", "mars", "Mps1",
                "msps", "Nek2", "pav", "pbl", "Phf7", "pie", "pigs", "RanGAP",
                "scra", "stg", "Su(var)205", "sub", "Top2", "twe", "vih"
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
                "FBgn0002906", "FBgn0033526", "FBgn0026143", "FBgn0035918",
                "FBgn0032698", "FBgn0034495", "FBgn0032635", "FBgn0052251",
                "FBgn0010382", "FBgn0259113", "FBgn0011762", "FBgn0015929",
                "FBgn0025832", "FBgn0013548", "FBgn0014861", "FBgn0017577",
                "FBgn0025815", "FBgn0005655", "FBgn0032813", "FBgn0261976",
                "FBgn0260985", "FBgn0011704", "FBgn0032906", "FBgn0041186",
                "FBgn0015546", "FBgn0003479", "FBgn0015553", "FBgn0024920",
                "FBgn0028476"
            ),
            "geneName" = c(
                "Blm", "Caf1-105", "CDC45L", "Cdc6", "CG10336", "CG11788",
                "CG15141", "Claspin", "CycE", "DNApol-alpha180",
                "DNApol-alpha50", "dpa", "Fen1", "l(2)dtl", "Mcm2", "Mcm5",
                "Mcm6", "PCNA", "PCNA2", "Psf2", "RfC4", "RnrS", "RPA2", "Slbp",
                "spel1", "spn-A", "tos", "Ts", "Usp1"
            ),
            "phase" = factor(
                x = rep("S", times = 29L),
                levels = c("G2/M", "S")
            )
        )
    )
})
