.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' AcidSingleCell test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2021-09-13.
#'
#' @examples
#' AcidSingleCellTestsURL
AcidSingleCellTestsURL <- # nolint
    paste0(
        "https://r.acidgenomics.com/testdata/acidsinglecell/",
        "v", .pkgVersion$major, ".", .pkgVersion$minor # nolint
    )
