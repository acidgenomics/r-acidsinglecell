## Package description =========================================================
#' AcidSingleCell
#'
#' Toolkit for single-cell RNA-seq analysis that extends the functionality
#' of SingleCellExperiment.
#'
#' @keywords internal
"_PACKAGE"


## Classes ====================================================================

#' @importClassesFrom IRanges CompressedSplitDFrameList DFrameList
#' @importClassesFrom IRanges SplitDFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
NULL


## S4 generics and methods =====================================================

#' @importFrom AcidGenerics aggregateCellsToSamples barcodeRanksPerSample
#' @importFrom AcidGenerics calculateMetrics camelCase cellToSample
#' @importFrom AcidGenerics cellCountsPerCluster cellTypesPerCluster cpm
#' @importFrom AcidGenerics clusters
#' @importFrom AcidGenerics convertSampleIdsToNames diffExp diffExpPerCluster
#' @importFrom AcidGenerics droplevels2 encode export filterCells findMarkers
#' @importFrom AcidGenerics geometricMean import interestingGroups leftJoin melt
#' @importFrom AcidGenerics metrics metricsPerSample mutateAll sampleData
#' @importFrom AcidGenerics sampleData<- selectSamples snakeCase subsetPerSample
#' @importFrom AcidGenerics topCellsPerSample uniteInterestingGroups
#' @importFrom AcidGenerics zerosVsDepth
#' @importFrom Biobase sampleNames
#' @importFrom BiocGenerics combine counts do.call estimateSizeFactors normalize
#' @importFrom BiocGenerics counts<- normalize sizeFactors
#' @importFrom IRanges dims
#' @importFrom Matrix colSums rowSums
#' @importFrom S4Vectors DataFrame SimpleList aggregate complete.cases decode
#' @importFrom S4Vectors head metadata na.omit split tail
#' @importFrom S4Vectors metadata<-
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom SummarizedExperiment assay assayNames assays colData rowRanges
#' @importFrom SummarizedExperiment colData<-
#' @importFrom methods show
NULL

#' @importMethodsFrom AcidBase geometricMean
#' @importMethodsFrom AcidExperiment aggregate calculateMetrics combine decode
#' @importMethodsFrom AcidExperiment droplevels2 encode estimateSizeFactors
#' @importMethodsFrom AcidExperiment export interestingGroups matchSampleColumn
#' @importMethodsFrom AcidExperiment melt metrics sampleData sampleData<-
#' @importMethodsFrom AcidExperiment sampleNames selectSamples
#' @importMethodsFrom AcidExperiment uniteInterestingGroups
#' @importMethodsFrom AcidPlyr leftJoin melt mutateAll
#' @importMethodsFrom pipette droplevels2 export import
#' @importMethodsFrom syntactic camelCase snakeCase
NULL


## S3 generics and methods =====================================================

#' @importFrom stats model.matrix relevel
NULL


## Standard functions ==========================================================

#' @importFrom AcidBase barcodePattern basenameSansExt initDir methodFormals
#' @importFrom AcidBase methodFunction printString realpath standardizeCall
#' @importFrom AcidBase strMatch
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning dl
#' @importFrom AcidCLI toInlineString txt ul verbatim
#' @importFrom AcidExperiment makeSummarizedExperiment matchInterestingGroups
#' @importFrom AcidGenomes GeneToSymbol makeGRangesFromEnsembl
#' @importFrom IRanges DataFrameList SplitDataFrameList
#' @importFrom goalie allAreMatchingRegex allArePositive areDisjointSets
#' @importFrom goalie areSetEqual assert bapply hasClusters hasColnames
#' @importFrom goalie hasDuplicates hasLength hasMetrics hasNames
#' @importFrom goalie hasNoDuplicates
#' @importFrom goalie hasNonzeroRowsAndCols hasRownames hasRows hasValidNames
#' @importFrom goalie isADir isAny isCharacter isFlag isInLeftOpenRange
#' @importFrom goalie isInRange
#' @importFrom goalie isInstalled isInt isIntegerish isNonNegative isPositive
#' @importFrom goalie isScalar isString isSubset requireNamespaces validate
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom pipette assignAndSaveData
#' @importFrom utils packageName packageVersion
NULL
