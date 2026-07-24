#' IEA Energy Balances with fixes for industry subsectors
#'
#' @param ieaVersion Release version of IEA data, either 'default'
#' (vetted and used in REMIND) or 'latest'.
#' @author Falk Benke
calcIeaEnergyBalances <- function(ieaVersion) {

  ieaSubtype <- if (ieaVersion == "default") "EnergyBalances" else "EnergyBalances-latest"

  # read in data and convert from ktoe to EJ
  x <- readSource("IEA", subtype = ieaSubtype) * 4.1868e-5

  x <- toolFixIeaDataForIndustrySubsectors(x)

  return(list(x = x,
              weight = NULL,
              unit = "EJ/yr",
              description = "IEA Energy Balances with fixes for industry subsectors")
  )


}
