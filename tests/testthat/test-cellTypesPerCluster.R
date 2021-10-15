context("cellTypesPerCluster")

test_that("KnownMarkers", {
    object <- km
    expect_identical(
        object = cellTypesPerCluster(object),
        expected = DataFrame(
            "cluster" = factor(c(1L, 2L)),
            "cellType" = factor(c("Macrophage", "Dendritic Cell")),
            "name" = c(
                "IL1B",
                toString(c(
                    "HLA-DQB1",
                    "HLA-DRB1",
                    "HLA-DRA",
                    "HLA-DQA1"
                ))
            ),
            "geneId" = c(
                "ENSG00000125538",
                toString(c(
                    "ENSG00000179344",
                    "ENSG00000196126",
                    "ENSG00000204287",
                    "ENSG00000196735"
                ))
            ),
            "geneName" = c(
                "IL1B",
                toString(c(
                    "HLA-DQB1",
                    "HLA-DRB1",
                    "HLA-DRA",
                    "HLA-DQA1"
                ))
            ),
            "n" = c(1L, 4L)
        )
    )
})
