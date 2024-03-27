## Updated 2024-03-27.
.CellMarkers <- # nolint
    function(object,
             geneToSymbol,
             class = c("CellCycleMarkers", "CellTypeMarkers")) {
        assert(
            is(object, "DFrame"),
            is(geneToSymbol, "GeneToSymbol")
        )
        class <- match.arg(class)
        group <- switch(
            EXPR = class,
            "CellCycleMarkers" = "phase",
            "CellTypeMarkers" = "cellType"
        )
        x <- object
        x <- camelCase(x, strict = TRUE)
        cols <- c(group, "geneId")
        x <- x[, cols]
        x <- x[complete.cases(x), , drop = FALSE]
        x <- unique(x)
        ## Warn user about markers that aren't present in the gene-to-symbol
        ## mappings. This is useful for informing about putative markers that
        ## aren't expressed.
        setdiff <- setdiff(x[["geneId"]], geneToSymbol[["geneId"]])
        if (hasLength(setdiff)) {
            alertWarning(sprintf(
                "Markers missing from {.cls %s}: %s.",
                "GeneToSymbol",
                toString(setdiff, width = 200L)
            ))
        }
        intersect <- intersect(x[["geneId"]], geneToSymbol[["geneId"]])
        assert(hasLength(intersect))
        keep <- x[["geneId"]] %in% intersect
        x <- x[keep, , drop = FALSE]
        x <- leftJoin(
            x = x,
            y = as(geneToSymbol, "DFrame"),
            by = "geneId"
        )
        x <- x[, sort(colnames(x)), drop = FALSE]
        x <- x[order(x[[group]], x[["geneName"]]), , drop = FALSE]
        x <- mutateIf(x, is.character, as.factor)
        x <- split(x, f = x[[group]])
        names(x) <- snakeCase(names(x))
        ## Specific fix for G2/M input (cell-cycle markers).
        names(x) <- sub("g2_slash_m", "g2m", names(x))
        metadata(x) <- metadata(geneToSymbol)
        x
    }
