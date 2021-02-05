#' Package version
#'
#' @note Updated 2021-02-05.
#' @noRd
.version <- packageVersion(packageName())



#' AcidSingleCell test data URL
#'
#' @export
#' @keywords internal
#' @note Updated 2021-02-05.
#'
#' @examples
#' AcidSingleCellTestsURL
AcidSingleCellTestsURL <-  # nolint
    paste0(
        "https://r.acidgenomics.com/testdata/acidsinglecell/",
        "v", .version$major, ".", .version$minor  # nolint
    )
