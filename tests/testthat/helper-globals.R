## nolint start

data(
    KnownMarkers,
    RangedSummarizedExperiment,
    SingleCellExperiment_Seurat,
    SingleCellExperiment_lanesplit,
    SingleCellExperiment_splatter,
    package = "AcidTest",
    envir = environment()
)

km <- KnownMarkers
rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment_splatter
sce_lanesplit <- SingleCellExperiment_lanesplit
sce_seurat <- SingleCellExperiment_Seurat

`rowData<-` <- SummarizedExperiment::`rowData<-`
`sampleNames<-` <- Biobase::`sampleNames<-`
SingleCellExperiment <- SingleCellExperiment::SingleCellExperiment
aggregateCols <- AcidExperiment::aggregateCols
aggregateRows <- AcidExperiment::aggregateRows
logcounts <- SingleCellExperiment::logcounts
rowData <- SummarizedExperiment::rowData
simpleClass <- AcidBase::simpleClass
strPad <- AcidBase::strPad
tempdir2 <- AcidBase::tempdir2
unlink2 <- AcidBase::unlink2

## nolint end
