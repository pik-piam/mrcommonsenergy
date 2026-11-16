#' Convert IEA
#'
#' Convert IEA energy data to data on ISO country level.
#'
#' @param x MAgPIE object containing IEA values at IEA mixed country-region
#' resolution
#' @inheritParams readIEA
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis, Renato Rodrigues, Falk Benke
#'
convertIEA <- function(x, subtype) {

  # map to old product and flow names ----

  if (subtype %in% c("EnergyBalances-latest", "EnergyBalances-2025")) {

    # this will be removed once we completely switch to the new format and adjust
    # all the IEA mappings accordingly

    ieaShortNameMapping <- toolGetMapping("ieaShortNameMapping.csv",
                                          type = "sectoral", where = "mrcommonsenergy")

    flowMap <- ieaShortNameMapping %>%
      filter(.data$type == "FLOW") %>%
      select(-"type")

    flowMap <- data.frame(new_name = getNames(x, dim = 2)) %>%
      left_join(flowMap, by = "new_name") %>%
      mutate("old_name" = ifelse(is.na(.data$old_name), .data$new_name, .data$old_name))

    getNames(x, dim = 2) <- flowMap$old_name

    productMap <- ieaShortNameMapping %>%
      filter(.data$type == "PRODUCT") %>%
      select(-"type")

    productMap <- data.frame(new_name = getNames(x, dim = 1)) %>%
      left_join(productMap, by = "new_name") %>%
      mutate("old_name" = ifelse(is.na(.data$old_name), .data$new_name, .data$old_name))

    getNames(x, dim = 1) <- productMap$old_name

  }


  # aggregate Kosovo to Serbia ----
  x1 <- x["KOS", , ]
  getItems(x1, dim = 1) <- "SRB"
  x["SRB", , ] <- x["SRB", , ] + x1
  rm(x1)
  x <- x["KOS", , , invert = TRUE]

  # convert electricity outputs (unit conversion between ktoe and GWh) ----
  x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")] <- 0.0859845 *
    x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")]

  # disaggregating OTHER regions ----

  # note that the mapping is currently not 100% accurate but should be close enough
  # as an approximation, countries that used to belong to an other region until a certain point in time
  # (e.g. Namibia until 1990) are not included at all and countries that were later
  # added to an other region (e.g. Palau from 2010) are included
  # see also the IEA World Balances Documentation for July 2026 (included in sources folder)
  # this should not be problematic, as we rarely look at the data on regional level

  if (subtype == "EnergyBalances-latest") {

    mappingfile <- toolGetMapping(
      type = "regional", name = "regionmappingIeaOther2026.csv",
      returnPathOnly = TRUE, where = "mrcommonsenergy"
    )

  } else {

    mappingfile <- toolGetMapping(
      type = "regional", name = "regionmappingIeaOther2016.csv",
      returnPathOnly = TRUE, where = "mrcommonsenergy"
    )

  }

  mapping <- utils::read.csv2(mappingfile, stringsAsFactors = TRUE) %>%
    filter(!(.data$CountryCode %in% getItems(x, dim = 1)))

  # calculate weight to be used for regional disaggregations
  wp <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[, 2010, ]
  wg <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)[, 2010, ]
  wp <- wp / max(wp)
  wg <- wg / max(wg)
  w <- wp + wg


  xadd <- toolAggregate(
    x = x[unique(mapping$RegionCode), , ],
    rel = mapping,
    weight = w[unique(mapping$CountryCode), , ]
  )
  x <- x[unique(mapping$RegionCode), , , invert = TRUE]
  x <- mbind(x, xadd)
  rm(xadd)

  # disaggregating extinct countries USSR (SUN) and Yugoslavia (YUG) ----

  yugoslavia <- data.frame(
    fromISO = "YUG",
    toISO = c("SRB", "MNE", "SVN", "HRV", "MKD", "BIH"),
    lastYear = "y1989"
  )

  sun <- toolGetMapping("ISOhistorical.csv", where = "madrat") %>%
    filter(.data$fromISO == "SUN") %>%
    mutate("lastYear" = "y1989")

  histMap <- rbind(yugoslavia, sun)

  x <- madrat::toolISOhistorical(x,
    mapping = histMap, overwrite = TRUE,
    additional_weight = w[unique(histMap$toISO), , ]
  )
  x[is.na(x)] <- 0

  # These changes may reduce the amount of CHP plants to below what is actually
  # deployed in a region, because heat reporting is obscure. In some statistics,
  # heat from CHP plant used in the same industrial compound is NOT explicitly
  # reported as heat. If this is the case in the IEA data as well, this would
  # lead to underestimating CHP plants. However, there currently seems to be no
  # better way to create consistent data for CHP/non-CHP electricity production
  # and heat output.

  # for each product: check if the flow "HEMAINC" > 0, if yes, do nothing;
  # if no, add the value of the flow "ELMAINC" to "ELMAINE" and afterwards set "ELMAINC" to zero.

  missingFlows <- setdiff(
    expand.grid(
      iea_product = getItems(x[, , "ELMAINE"], dim = 3.1) %>% # nolint
        union(getItems(x[, , "ELMAINC"], dim = 3.1)) %>%
        union(getItems(x[, , "HEMAINC"], dim = 3.1)), iea_flows = c("ELMAINE", "ELMAINC", "HEMAINC")
    ) %>%
      tidyr::unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
      dplyr::pull("product.flow"),
    getItems(x, dim = 3)
  )

  if (length(missingFlows) > 0) {
    x <- add_columns(x, addnm = missingFlows, dim = 3, fill = 0)
  }

  d <- x[, , c("ELMAINE", "ELMAINC", "HEMAINC")]
  tmp <- magclass::mcalc(d, ELMAINE ~ ifelse(HEMAINC > 0, ELMAINE, ELMAINC + ELMAINE), append = FALSE)
  x[, , "ELMAINE"] <- tmp
  tmp <- magclass::mcalc(d, ELMAINC ~ ifelse(HEMAINC > 0, ELMAINC, 0), append = FALSE)
  x[, , "ELMAINC"] <- tmp

  # for each product: check if the flow "HEAUTOC" > 0, if yes, do nothing;
  # if no, add the value of the flow "ELAUTOC" to "ELAUTOE" and afterwards set ELAUTOC to zero.

  missingFlows <- setdiff(
    expand.grid(
      iea_product = getItems(x[, , "ELAUTOE"], dim = 3.1) %>%
        union(getItems(x[, , "ELAUTOC"], dim = 3.1)) %>%
        union(getItems(x[, , "HEAUTOC"], dim = 3.1)), iea_flows = c("ELAUTOE", "HEAUTOC", "ELAUTOC")
    ) %>%
      tidyr::unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
      dplyr::pull("product.flow"), getItems(x, dim = 3)
  )

  if (length(missingFlows) > 0) {
    x <- add_columns(x, addnm = missingFlows, dim = 3, fill = 0)
  }

  d <- x[, , c("ELAUTOE", "HEAUTOC", "ELAUTOC")]
  tmp <- magclass::mcalc(d, ELAUTOE ~ ifelse(HEAUTOC > 0, ELAUTOE, ELAUTOC + ELAUTOE), append = FALSE)
  x[, , "ELAUTOE"] <- tmp
  tmp <- magclass::mcalc(d, ELAUTOC ~ ifelse(HEAUTOC > 0, ELAUTOC, 0), append = FALSE)
  x[, , "ELAUTOC"] <- tmp

  # Correct transport reporting issue in IEA data for NONBIODIES.MARBUNK in RUS
  # FE is reported in 1990 and 2010 but not in the years in between.
  # This cause problems in the harmonization of EDGE-Transport and the IEA data
  # in 2005 as there is no MARBUNK demand at all for REF regions.
  x["RUS", seq(1990, 2010, 1), "NONBIODIES.MARBUNK"] <-
    x["RUS", c(1990, 2010), "NONBIODIES.MARBUNK"] %>% time_interpolate(seq(1990, 2010, 1))

  # Adjust totals
  x["RUS", seq(1991, 2009, 1), "TOTAL.MARBUNK"] <-
    x["RUS", seq(1991, 2009, 1), "TOTAL.MARBUNK"] + x["RUS", seq(1991, 2009, 1), "NONBIODIES.MARBUNK"]

  # filling missing country data ----
  x <- toolCountryFill(x, 0, verbosity = 2)

  return(x)
}
