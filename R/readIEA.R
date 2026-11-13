#' Read IEA
#'
#' Read-in an IEA csv file as magpie object
#'
#' @param subtype data subtype. Either "EnergyBalances", "EnergyBalances-latest", or "Emissions".
#' - "EnergyBalances": IEA energy balances until 2022 (2023 incomplete), data updated in Sep 2024,
#' the current default for REMIND input data
#' - "EnergyBalances-2025": IEA energy balances until 2023 (2024 incomplete), data updated in Aug 2025,
#' - "EnergyBalances-latest": IEA energy balances update from July 2026 (2025 incomplete), data updated in Nov 2026.
#' @return magpie object of the IEA
#' @author Anastasis Giannousakis, Lavinia Baumstark, Renato Rodrigues, Falk Benke
#'
readIEA <- function(subtype) {
  if (subtype == "EnergyBalances") {
    energyBalancesFile <- "IEA-Energy-Balances-2024/worldbig.csv"
    incomplete <- 2023

    data <- data.table::fread(
      file = energyBalancesFile,
      col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "ktoe"),
      colClasses = c("character", "character", "character", "numeric", "character"),
      sep = ";", stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 2, showProgress = FALSE
    )

    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, warn = FALSE)

    data <- data %>%
      filter(
        !is.na(.data$ktoe),
        !is.na(.data$COUNTRY),
        .data$TIME != incomplete
      ) %>% # exclude latest year with incomplete data
      mutate("ktoe" = as.numeric(.data$ktoe))

    mdata <- as.magpie(data, datacol = 5, spatial = 1, temporal = 4)
  } else if (subtype == "EnergyBalances-2025") {
    data <- NULL
    incomplete <- 2024

    for (i in seq(1, 3)) {
      energyBalancesFile <- file.path("IEA-Energy-Balances-2025", paste0("WBIG", i, ".zip"))

      tmp <- data.table::fread(
        file = energyBalancesFile,
        col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "UNIT", "VALUE"),
        stringsAsFactors = FALSE,
        na.strings = c("x", "..", "c", ""),
        skip = 0,
        showProgress = FALSE,
        select = c(1, 2, 4, 3, 5, 6)
      ) %>%
        filter(
          .data$UNIT %in% c("KTOE", "GWH"),
          .data$TIME != incomplete # exclude latest year with incomplete data
        ) %>%
        mutate("VALUE" = as.numeric(.data$VALUE)) %>%
        select(-"UNIT")

      tmp$COUNTRY <- toolCountry2isocode(tmp$COUNTRY, warn = FALSE, mapping = c(
        "BURKINAFASO" = "BFA",
        "CONGO_DRC" = "COD",
        "CONGO_REPUB" = "COG",
        "DOMINICANREP" = "DOM",
        "NEWZEALAND" = "NZL",
        "SAUDIARABIA" = "SAU",
        "SOUTHAFRICA" = "ZAF",
        "F_USSR" = "SUN",
        "F_YUGOSLAVIA" = "YUG",
        "OTH_NON_OECDAFR" = "IAF",
        "OTH_NON_OECDAM" = "ILA",
        "OTH_NON_OECDAO" = "IAS"
      ))

      tmp <- tmp %>%
        filter(!is.na(.data$COUNTRY), !is.na(.data$VALUE))

      data <- rbind(data, tmp)
      rm(tmp)
    }

    mdata <- as.magpie(data, spatial = 1, temporal = 4, datacol = 5)

    # remove product flows with all zeros
    pf_remove <- magpply(mdata, function(y) all(is.na(y) | y == 0), MARGIN = 3)
    mdata <- mdata[, , !pf_remove]

  } else if (subtype == "EnergyBalances-latest") {

    data <- NULL
    incomplete <- 2025

    files <- list.files("IEA-Energy-Balances-2026", pattern = "*.csv", full.names = TRUE)

    for (f in files) {

      ignoreCountries <- c(
        "AFRICA_UN", "AMERICAS_UN", "ASEAN", "ASIA_UN", "CHINAREG", "EU27", "EU28",
        "EUROPE_UN", "F_USSR_TOTAL", "F_YUGO_TOTAL", "G20", "G7", "G8",
        "IEAFAMILY", "IEATOT", "NON_OECDAFR", "NON_OECDAM", "NON_OECDAO_X_CHINA",
        "NON_OECDEURAS", "NON_OECDME", "NON_OECDTOT", "OCEANIA_UN", "OECDAM",
        "OECDAO", "OECDEUR", "OECDTOT", "OPEC", "WORLDAV", "WORLDMAR", "WORLD"
      )

      tmp <- data.table::fread(
        file = f,
        select = c(3, 7, 5, 11, 17, 15),
        col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "UNIT", "VALUE"),
        na.strings = c("x", "..", "c", ""),
        skip = 0,
        showProgress = FALSE
      ) %>%
        filter(
          !is.na(.data$VALUE),
          !.data$COUNTRY %in% ignoreCountries,
          .data$UNIT %in% c("KTOE", "GWH"),
          .data$TIME != incomplete # exclude latest year with incomplete data
        ) %>%
        mutate("VALUE" = as.numeric(.data$VALUE)) %>%
        # there are duplicates in the source data, e.g AUS|1960|BIOFUEL_NONSPEC.ELMAINE.GWH
        distinct() %>%
        # we assume that the source data is all in KTOE, even though labelled as GWH
        select(-"UNIT")

      if (any(duplicated(tmp))) {
        stop()
      }

      customRegionMappings <- c(
        "BURKINAFASO" = "BFA",
        "CONGO_DRC" = "COD",
        "CONGO_REPUB" = "COG",
        "F_USSR" = "SUN",
        "F_YUGOSLAVIA" = "YUG",
        "DOMINICANREP" = "DOM",
        "NEWZEALAND" = "NZL",
        "SAUDIARABIA" = "SAU",
        "SOUTHAFRICA" = "ZAF",
        # Other non-OECD Africa (previously IAF)
        "OTH_NON_OECDAFR" = "OTH_NON_OECDAFR",
        # Other non-OECD Americas (previously ILA)
        "OTH_NON_OECDAM" = "OTH_NON_OECDAM",
        # Other non-OECD Africa (previously IAS)
        "OTH_NON_OECDAO" = "OTH_NON_OECDAO"
      )

      tmp$COUNTRY <- madrat::toolCountry2isocode(tmp$COUNTRY,
        warn = TRUE,
        mapping = customRegionMappings
      )

      data <- rbind(data, tmp)
      rm(tmp)
    }

    mdata <- as.magpie(data, spatial = 1, temporal = 4, datacol = 5)
    rm(data)

    # remove product flows with all zeros
    pf_remove <- magpply(mdata, function(y) all(is.na(y) | y == 0), MARGIN = 3)
    mdata <- mdata[, , !pf_remove]

  } else {
    stop("Invalid subtype!")
  }
  return(magclass::magpiesort(mdata))
}
