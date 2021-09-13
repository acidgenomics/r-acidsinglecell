data(
    RangedSummarizedExperiment,
    SingleCellExperiment,
    SingleCellExperiment_Seurat,
    SingleCellExperiment_lanesplit,
    package = "AcidTest",
    envir = environment()
)

## nolint start
rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment
sce_seurat <- SingleCellExperiment_Seurat
sce_lanesplit <- SingleCellExperiment_lanesplit
## nolint end

## nolint start
`rowData<-` <- AcidExperiment::`rowData<-`
SingleCellExperiment <- SingleCellExperiment::SingleCellExperiment
aggregateCols <- AcidExperiment::aggregateCols
aggregateRows <- AcidExperiment::aggregateRows
rowData <- AcidExperiment::rowData
simpleClass <- AcidBase::simpleClass
## nolint end
