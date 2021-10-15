#' Known cell markers
#'
#' @export
#' @note Updated 2021-10-15.
#'
#' @description
#' Both the `markers` and `known` objects must contain Ensembl gene identifiers
#' in the `geneId` column. We must avoid any matching operations based on the
#' gene names, since these change often and can mismatch easily.
#'
#' @param markers
#'   Object containing cell markers from differential expression analysis.
#' @param known `CellTypeMarkers`.
#'   Grouped by `cellType` column.
#'   Known markers `data.frame` imported by `importCellTypeMarkers` or pulled
#'   from internal cell cycle markers data.
#' @param ... Additional arguments.
#'
#' @return `KnownMarkers`.
#'
#' @examples
#' showMethods("KnownMarkers")
setGeneric(
    name = "KnownMarkers",
    def = function(markers, known, ...) {
        standardGeneric("KnownMarkers")
    }
)



#' @name aggregateCellsToSamples
#' @importFrom AcidGenerics aggregateCellsToSamples
#' @usage aggregateCellsToSamples(x, ...)
#' @export
NULL



#' @rdname aggregate
#' @name aggregate
#' @importFrom AcidGenerics aggregate
#' @importMethodsFrom AcidExperiment aggregate
#' @usage aggregate(x, ...)
#' @export
NULL



#' @rdname barcodeRanksPerSample
#' @name barcodeRanksPerSample
#' @importFrom AcidGenerics barcodeRanksPerSample
#' @usage barcodeRanksPerSample(object, ...)
#' @export
NULL



#' @rdname cell2sample
#' @name cell2sample
#' @importFrom AcidGenerics cell2sample
#' @usage cell2sample(object, ...)
#' @export
NULL



#' @rdname cellCountsPerCluster
#' @name cellCountsPerCluster
#' @importFrom AcidGenerics cellCountsPerCluster
#' @usage cellCountsPerCluster(object, ...)
#' @export
NULL



#' @rdname cellTypesPerCluster
#' @name cellTypesPerCluster
#' @importFrom AcidGenerics cellTypesPerCluster
#' @usage cellTypesPerCluster(object, ...)
#' @export
NULL



#' @rdname clusters
#' @name clusters
#' @importFrom AcidGenerics clusters
#' @usage clusters(object, ...)
#' @export
NULL



#' @rdname combine
#' @name combine
#' @importFrom AcidGenerics combine
#' @importMethodsFrom AcidExperiment combine
#' @usage combine(x, y, ...)
#' @export
NULL



#' @rdname convertSampleIDsToNames
#' @name convertSampleIDsToNames
#' @importFrom AcidGenerics convertSampleIDsToNames
#' @usage convertSampleIDsToNames(object, ...)
#' @export
NULL



#' @rdname cpm
#' @name cpm
#' @importFrom AcidGenerics cpm
#' @usage cpm(object, ...)
#' @export
NULL



#' @rdname diffExp
#' @name diffExp
#' @importFrom AcidGenerics diffExp
#' @usage diffExp(object, ...)
#' @export
NULL



#' @rdname diffExpPerCluster
#' @name diffExpPerCluster
#' @importFrom AcidGenerics diffExpPerCluster
#' @usage diffExpPerCluster(object, ...)
#' @export
NULL



#' @rdname findMarkers
#' @name findMarkers
#' @importFrom AcidGenerics findMarkers
#' @usage findMarkers(object, ...)
#' @export
NULL



#' @rdname export
#' @name export
#' @importFrom AcidGenerics export
#' @importMethodsFrom AcidExperiment export
#' @importMethodsFrom pipette export
#' @usage export(object, con, format, ...)
#' @export
NULL



#' @rdname filterCells
#' @name filterCells
#' @importFrom AcidGenerics filterCells
#' @usage filterCells(object, ...)
#' @export
NULL



#' @rdname geometricMean
#' @name geometricMean
#' @importFrom AcidGenerics geometricMean
#' @importMethodsFrom AcidBase geometricMean
#' @usage geometricMean(x, ...)
#' @export
NULL



#' @rdname melt
#' @name melt
#' @importFrom AcidGenerics melt
#' @importMethodsFrom AcidExperiment melt
#' @importMethodsFrom AcidPlyr melt
#' @usage melt(object, ...)
#' @export
NULL



#' @rdname metrics
#' @name metrics
#' @importFrom AcidGenerics metrics
#' @importMethodsFrom AcidExperiment metrics
#' @usage metrics(object, ...)
#' @export
NULL



#' @rdname metrics
#' @name metricsPerSample
#' @importFrom AcidGenerics metricsPerSample
#' @usage metricsPerSample(object, ...)
#' @export
NULL



#' @rdname normalize
#' @name normalize
#' @importFrom AcidGenerics normalize
#' @usage normalize(object, ...)
#' @export
NULL



#' @rdname sampleData
#' @name sampleData
#' @importFrom AcidGenerics sampleData
#' @importMethodsFrom AcidExperiment sampleData
#' @usage sampleData(object, ...)
#' @export
NULL

#' @rdname sampleData
#' @name sampleData<-
#' @importFrom AcidGenerics sampleData<-
#' @importMethodsFrom AcidExperiment sampleData<-
#' @usage sampleData(object, ...) <- value
#' @export
NULL



#' @rdname selectSamples
#' @name selectSamples
#' @importFrom AcidGenerics selectSamples
#' @importMethodsFrom AcidExperiment selectSamples
#' @usage selectSamples(object, ...)
#' @export
NULL



#' @rdname show
#' @name show
#' @importFrom AcidGenerics show
#' @usage show(object)
#' @export
NULL



#' @rdname subsetPerSample
#' @name subsetPerSample
#' @importFrom AcidGenerics subsetPerSample
#' @usage subsetPerSample(object, ...)
#' @export
NULL



#' @rdname topCellsPerSample
#' @name topCellsPerSample
#' @importFrom AcidGenerics topCellsPerSample
#' @usage topCellsPerSample(object, ...)
#' @export
NULL



#' @rdname zerosVsDepth
#' @name zerosVsDepth
#' @importFrom AcidGenerics zerosVsDepth
#' @usage zerosVsDepth(object, ...)
#' @export
NULL
