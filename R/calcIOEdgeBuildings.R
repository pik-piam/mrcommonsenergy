#' calcIOEdgeBuildings
#'
#' Calculates buildings-related energy flows from the IEA energy balances.
#' 'output_EDGE_buildings' is a key input to EDGE-Buildings providing the
#' historic final energy demand from buildings. 'output_EDGE' does the same for
#' buildings and industry together.
#'
#' @param subtype Data subtype. See default argument for possible values.
#' @param ieaVersion Release version of IEA data, either 'default'
#' (vetted and used in REMIND) or 'latest'.
#' @returns IEA data as MAgPIE object aggregated to country level
#'
#' @author Pascal Sauer, Anastasis Giannousakis, Robin Hasse
#'
#' @examples
#' \dontrun{
#' a <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings")
#' }
#'
#' @importFrom dplyr all_of filter select
#' @importFrom tidyr unite
#' @importFrom madrat readSource toolGetMapping toolAggregate calcOutput
#' @importFrom utils read.csv2
#' @importFrom magclass as.magpie getNames mselect

calcIOEdgeBuildings <- function(subtype = c("output_EDGE_ononspec", "output_EDGE_buildings"),
                                ieaVersion = c("default", "latest")) {

  subtype <- match.arg(subtype)
  ieaVersion <- match.arg(ieaVersion)


  # READ -----------------------------------------------------------------------
  data <- calcOutput("IeaEnergyBalances", ieaVersion = ieaVersion, aggregate = FALSE)

  # AGGREGATE ------------------------------------------------------------------

  target <- switch(subtype,
                   output_EDGE_ononspec = "EDGE_ononspec",
                   output_EDGE_buildings = "EDGE_buildings"
  )

  mapping <- toolGetMapping(type = "sectoral",
                            name = "structuremappingIO_outputs.csv",
                            where = "mrcommonsenergy", returnPathOnly = TRUE) %>%
    read.csv2(stringsAsFactors = FALSE, na.strings = "") %>%
    select(all_of(c("iea_product", "iea_flows", target, "Weight"))) %>%
    stats::na.omit() %>%
    unite("target", all_of(target), sep = ".") %>%
    unite("product.flow", c("iea_product", "iea_flows"), sep = ".", remove = FALSE) %>%
    mutate(Weight = as.numeric(.data[["Weight"]])) %>%
    filter(.data[["product.flow"]] %in% getNames(data),
           .data[["Weight"]] != 0, !is.na(.data[["Weight"]]))


  weight <- as.magpie(mapping[, c("iea_product", "iea_flows", "Weight")])

  data <- toolAggregate(data[, , mapping[["product.flow"]]] * weight,
                        rel = mapping, from = "product.flow", to = "target", dim = 3)
  getSets(data)[3] <- "d3"

  # SPLIT BIOMASS --------------------------------------------------------------
  if (subtype == "output_EDGE_buildings") {
    gdppop <- calcOutput("GDPpc", scenario = "SSP2", average2020 = FALSE, aggregate = FALSE) %>%
      collapseNames()

    data <- toolSplitBiomass(data, gdppop, split = "bioshare")
  }

  return(list(x = data,
              weight = NULL,
              unit = "EJ/yr",
              description = paste("Historic FE demand from buildings",
                                  "(and industry) based on IEA Energy Balances")))
}
