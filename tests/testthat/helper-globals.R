data(
    km,
    package = "AcidSingleCell",
    envir = environment()
)
data(
    RangedSummarizedExperiment,
    SingleCellExperiment_Seurat,
    SingleCellExperiment_lanesplit,
    SingleCellExperiment_splatter,
    package = "AcidTest",
    envir = environment()
)

## nolint start
rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment_splatter
sce_seurat <- SingleCellExperiment_Seurat
sce_lanesplit <- SingleCellExperiment_lanesplit
## nolint end

## nolint start
`rowData<-` <- SummarizedExperiment::`rowData<-`
`sampleNames<-` <- Biobase::`sampleNames<-`
SingleCellExperiment <- SingleCellExperiment::SingleCellExperiment
aggregateCols <- AcidExperiment::aggregateCols
aggregateRows <- AcidExperiment::aggregateRows
logcounts <- SingleCellExperiment::logcounts
rowData <- SummarizedExperiment::rowData
simpleClass <- AcidBase::simpleClass
## nolint end
