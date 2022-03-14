## Classes ====================================================================

#' @importClassesFrom IRanges DataFrameList SplitDataFrameList
#' @importClassesFrom Matrix Matrix
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics aggregateCellsToSamples barcodeRanksPerSample
#' calculateMetrics camelCase cell2sample cellCountsPerCluster
#' cellTypesPerCluster cpm clusters convertSampleIDsToNames diffExp
#' diffExpPerCluster encode filterCells findMarkers geometricMean
#' interestingGroups leftJoin melt metrics metricsPerSample mutateAll mutateIf
#' sampleData sampleData<- selectSamples snakeCase subsetPerSample
#' topCellsPerSample uniteInterestingGroups zerosVsDepth
#' @importFrom Biobase sampleNames
#' @importFrom BiocGenerics colSums combine counts counts<- do.call
#' estimateSizeFactors normalize rowSums normalize sizeFactors
#' @importFrom BiocIO export import
#' @importFrom IRanges dims
#' @importFrom S4Vectors DataFrame SimpleList aggregate complete.cases decode
#' head metadata metadata<- na.omit split tail
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#' reducedDim
#' @importFrom SummarizedExperiment assay assayNames assays colData colData<-
#' rowRanges
#' @importFrom methods coerce show
#'
#' @importMethodsFrom AcidBase geometricMean
#' @importMethodsFrom AcidExperiment aggregate calculateMetrics combine decode
#' encode estimateSizeFactors export interestingGroups matchSampleColumn melt
#' metrics sampleData sampleData<- sampleNames selectSamples
#' uniteInterestingGroups
#' @importMethodsFrom AcidPlyr leftJoin melt mutateAll mutateIf
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom pipette coerce export import
#' @importMethodsFrom syntactic camelCase snakeCase
NULL



## S3 generics and methods =====================================================

#' @importFrom pipette as_tibble
#' @importFrom stats model.matrix relevel
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase barcodePattern basenameSansExt initDir methodFormals
#' methodFunction printString realpath requireNamespaces standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning dl h1 h2
#' toInlineString txt ul verbatim
#' @importFrom AcidExperiment makeSummarizedExperiment matchInterestingGroups
#' @importFrom AcidGenomes makeGene2SymbolFromEnsembl
#' @importFrom IRanges DataFrameList SplitDataFrameList
#' @importFrom Matrix Matrix
#' @importFrom goalie allAreMatchingRegex allArePositive areDisjointSets
#' areSetEqual assert bapply hasClusters hasColnames hasLength hasMetrics
#' hasNames hasNoDuplicates hasNonzeroRowsAndCols hasRownames hasRows
#' hasValidNames isAny isBiocParallelParam isCharacter isFlag
#' isInLeftOpenRange isInRange isInstalled isInt isIntegerish isNonNegative
#' isPositive isScalar isString isSubset validate
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom pipette assignAndSaveData tibble
#' @importFrom scales percent
#' @importFrom stringr str_match
#' @importFrom utils packageName packageVersion
NULL
