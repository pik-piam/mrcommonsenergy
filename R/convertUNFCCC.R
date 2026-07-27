#' Convert UNFCCC data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readUNFCCC()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke, Simon Krogmann
#'
#' @param subtype Can be either "annex-1" (default) or "non-annex-1"
#'
convertUNFCCC <- function(x, subtype = "annex-1") {
  if (subtype == "annex-1") {
    result <- toolCountryFill(x, verbosity = 2, no_remove_warning = "EUA")
  } else if (subtype == "non-annex-1") {
    result <- toolCountryFill(x, verbosity = 2)
  } else {
    stop("Unsupported subtype: ", subtype)
  }
  return(result)
}
