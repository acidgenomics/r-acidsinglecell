## Classes ====================================================================

#' @importClassesFrom IRanges CompressedSplitDFrameList DFrameList
#' SplitDFrameList
#' @importClassesFrom Matrix sparseMatrix
#' @importClassesFrom S4Vectors DFrame
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom SummarizedExperiment RangedSummarizedExperiment
#' SummarizedExperiment
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics aggregateCellsToSamples barcodeRanksPerSample
#' calculateMetrics camelCase cell2sample cellCountsPerCluster
#' cellTypesPerCluster cpm clusters convertSampleIDsToNames diffExp
#' diffExpPerCluster droplevels2 encode filterCells findMarkers geometricMean
#' interestingGroups leftJoin melt metrics metricsPerSample mutateAll mutateIf
#' sampleData sampleData<- selectSamples snakeCase subsetPerSample
#' topCellsPerSample uniteInterestingGroups zerosVsDepth
#' @importFrom Biobase sampleNames
#' @importFrom BiocGenerics combine counts counts<- do.call estimateSizeFactors
#' normalize normalize sizeFactors
#' @importFrom BiocIO export import
#' @importFrom IRanges dims
#' @importFrom Matrix colSums rowSums
#' @importFrom S4Vectors DataFrame SimpleList aggregate complete.cases decode
#' head metadata metadata<- na.omit split tail
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#' reducedDim
#' @importFrom SummarizedExperiment assay assayNames assays colData colData<-
#' rowRanges
#' @importFrom methods show
NULL

#' @importMethodsFrom AcidBase geometricMean
#' @importMethodsFrom AcidExperiment aggregate calculateMetrics combine decode
#' droplevels2 encode estimateSizeFactors export interestingGroups
#' matchSampleColumn melt metrics sampleData sampleData<- sampleNames
#' selectSamples uniteInterestingGroups
#' @importMethodsFrom AcidPlyr leftJoin melt mutateAll mutateIf
#' @importMethodsFrom pipette droplevels2 export import
#' @importMethodsFrom syntactic camelCase snakeCase
NULL



## S3 generics and methods =====================================================

#' @importFrom stats model.matrix relevel
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase barcodePattern basenameSansExt initDir methodFormals
#' methodFunction printString realpath standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning dl
#' toInlineString txt ul verbatim
#' @importFrom AcidExperiment makeSummarizedExperiment matchInterestingGroups
#' @importFrom AcidGenomes makeGene2SymbolFromEnsembl
#' @importFrom IRanges DataFrameList SplitDataFrameList
#' @importFrom goalie allAreMatchingRegex allArePositive areDisjointSets
#' areSetEqual assert bapply hasClusters hasColnames hasDuplicates hasLength
#' hasMetrics hasNames hasNoDuplicates hasNonzeroRowsAndCols hasRownames hasRows
#' hasValidNames isAny isCharacter isFlag isInLeftOpenRange isInRange
#' isInstalled isInt isIntegerish isNonNegative isPositive isScalar isString
#' isSubset requireNamespaces validate
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom pipette assignAndSaveData
#' @importFrom stringi stri_match_first_regex
#' @importFrom utils packageName packageVersion
NULL
