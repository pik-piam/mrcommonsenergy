#' Read UNFCCC data
#'
#' @author Falk Benke, Simon Krogmann
#'
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tibble tibble
#' @importFrom utils tail
#'
#' @param subtype Can be either "annex-1" (default) or "non-annex-1"
#'
readUNFCCC <- function(subtype = "annex-1") {

  #' filter temporary lock files
  .filterTempFiles <- function(files) {
    return(files[substr(files, 1, 2) != "~$"])
  }

  #' finds out if a string is a 4-digit year
  .isYear <- function(string) {
    return(grepl("^[0-9]{4}$", string))
  }

  #' finds out if a string is only capital letters A-Z
  .isOnlyCapitals <- function(string) {
    return(grepl("[A-Z]*$", string))
  }

  .readUNFCCCAnnex1 <- function() {
    # structural definition of the source ----
    sheets <- list(
      "Table1" = list(
        type = "identifier",
        cols = c(1:8),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O", "NOX", "CO", "NMVOC", "SO2"))
      ),
      "Table1.A(a)s1" = list(
        type = "identifier",
        cols = c(1, 7, 8, 9),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
      ),
      "Table1.A(a)s2" = list(
        type = "identifier",
        cols = c(1, 7, 8, 9),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
      ),
      "Table1.A(b)" = list(
        type = "range",
        range = "T11:T29",
        colnames = "kt CO2",
        rows = tibble(
          name = {
            c(
              "Fuel Types|Liquid fossil|Primary fuels|Crude oil",
              "Fuel Types|Liquid fossil|Primary fuels|Orimulsion",
              "Fuel Types|Liquid fossil|Primary fuels|Natural gas liquids",
              NA,
              "Fuel Types|Liquid fossil|Secondary fuels|Gasoline",
              "Fuel Types|Liquid fossil|Secondary fuels|Jet kerosene",
              "Fuel Types|Liquid fossil|Secondary fuels|Other kerosene",
              "Fuel Types|Liquid fossil|Secondary fuels|Shale oil",
              "Fuel Types|Liquid fossil|Secondary fuels|Gas/diesel oil",
              "Fuel Types|Liquid fossil|Secondary fuels|Residual fuel oil",
              "Fuel Types|Liquid fossil|Secondary fuels|Liquefied petroleum gases (LPG)",
              "Fuel Types|Liquid fossil|Secondary fuels|Ethane",
              "Fuel Types|Liquid fossil|Secondary fuels|Naphtha",
              "Fuel Types|Liquid fossil|Secondary fuels|Bitumen",
              "Fuel Types|Liquid fossil|Secondary fuels|Lubricants",
              "Fuel Types|Liquid fossil|Secondary fuels|Petroleum coke",
              "Fuel Types|Liquid fossil|Secondary fuels|Refinery feedstocks",
              "Fuel Types|Liquid fossil|Secondary fuels|Other oil",
              "Fuel Types|Other liquid fossil"
            )
          }
        )
      ),
      "Table2(I)" = list(
        type = "identifier",
        cols = c(1, 2, 3, 4),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
      ),
      "Table3" = list(
        type = "identifier",
        cols = c(1, 2, 3, 4),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
      ),
      "Table4" = list(
        type = "identifier",
        cols = c(1, 2, 3, 4),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
      ),
      "Table5" = list(
        type = "identifier",
        cols = c(1, 2, 3, 4),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
      ),
      "Summary1" = list(
        type = "identifier",
        cols = c(1, 2, 3, 4),
        colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
      )
    )

    # parse directories ----
    basePath <- file.path("annex", "2024")
    dirs <- list.files(path = basePath)
    tmp <- NULL

    for (dir in dirs) {

      tmpCtry <- NULL
      region <- toupper(strsplit(dir, "-", fixed = TRUE)[[1]][1])

      files <- list.files(path = file.path(basePath, dir))
      files <- .filterTempFiles(files)

      for (file in files) {

        rx <- dplyr::case_when(
          region == "AUS" ~ "AUS_2024_([0-9]{4})_.*",
          region == "EUA" ~ "EUA_CRT_([0-9]{4}).*",
          .default = ".{3}-CRT-2024-V[0-9].[0-9]-([0-9]{4})-.*"
        )

        year <- suppressWarnings(as.integer(sub(rx, "\\1", file)))
        if (is.na(year)) {
          stop("No year found in filename ", file)
        }

        availableSheets <- readxl::excel_sheets(file.path(basePath, dir, file))

        if (length(setdiff(names(sheets), availableSheets)) > 0) {
          stop("Some of the expected sheets are missing in file ", file)
        }

        for (i in seq_along(sheets)) {
          # read in data via range
          if (sheets[[i]][["type"]] == "range") {

            s <- suppressMessages(
              readxl::read_xlsx(
                path = file.path(basePath, dir, file),
                sheet = names(sheets[i]),
                range = sheets[[i]][["range"]],
                col_names = sheets[[i]][["colnames"]]
              )
            )

            # some sheets are not filled out, so skip them
            if (nrow(s) == 0) {
              next
            }

            s <- suppressMessages(
              suppressWarnings(
                s %>%
                  dplyr::bind_cols(sheets[[i]]$rows, year = year, region = region) %>%
                  filter(!is.na(.data$name)) %>%
                  pivot_longer(cols = dplyr::starts_with("kt "), names_to = "unit") %>%
                  mutate(
                    "value" = suppressWarnings(as.double(.data$value)),
                    "variable" = paste0(sub("\\.", "_", names(sheets[i])),
                                        "|", .data$name, "|", sub(".+ ", "", .data$unit))
                  ) %>%
                  select("region", "year", "variable", "unit", "value") %>%
                  filter(!is.na(.data$value))
              )
            )

          } else {
            # read in data via identifiers (up to level 1.A.1.a.)
            s <- suppressMessages(
              readxl::read_xlsx(path = file.path(basePath, dir, file), sheet = names(sheets[i]),
                                .name_repair = "minimal")
            )

            # drop first column, when it is not the item colum
            if (all(is.na(s[, 1]))) {
              s <- s[, -1]
            }

            s <- s[sheets[[i]][["cols"]]]

            colnames(s) <- c("variable", sheets[[i]][["colnames"]])

            s <- s %>%
              filter(grepl("[0-9]\\.([A-Z]\\.)?([0-9]\\.)?([a-z]\\.)? ", .data$variable)) %>%
              dplyr::bind_cols(year = year, region = region) %>%
              pivot_longer(cols = dplyr::starts_with("kt "), names_to = "unit") %>%
              mutate(
                "value" = suppressWarnings(as.double(.data$value)),
                # remove appended remarks in brackets from entry
                "variable" = sub("\\(.*\\) *$", "", .data$variable),
                "variable" = trimws(.data$variable),
                "variable" = gsub("  +", " ", .data$variable),
                "variable" = paste0(.data$variable, "|", sub("kt ", "", .data$unit))
              ) %>%
              filter(!is.na(.data$value)) %>%
              select("region", "year", "variable", "unit", "value")

            if (nrow(s) == 0) {
              next
            }
          }

          tmpCtry <- bind_rows(tmpCtry, s)

          # duplicates can be introduced because of repetition in the sheets
          if (any(duplicated(tmpCtry))) {
            tmpCtry <- tmpCtry[-which(duplicated(tmpCtry[, -5])), ]
          }
        }
      }

      tmp <- bind_rows(tmp, tmpCtry)
    }

    tmp %>%
      as.magpie(tidy = TRUE) %>%
      return()
  }

  .readUNFCCCNonAnnex1 <- function() {
    result <- NULL
    basePath <- "non-annex"
    files <- list.files(path = basePath)
    files <- .filterTempFiles(files)

    for (file in files) {
      path <- file.path(basePath, file)
      region <- toupper(strsplit(file, "_", fixed = TRUE)[[1]][1])
      stopifnot(.isOnlyCapitals(region))
      targetSheet <- "Data_by_sector"
      stopifnot(targetSheet %in% readxl::excel_sheets(path))
      data <- readxl::read_xlsx(
        path = path,
        sheet = targetSheet,
        n_max = 8,
        na = c("NE,NO", "NA,NE"),
        .name_repair = "minimal"
      )
      rows <- c(
        "GHG emissions, Gg CO2 equivalent",
        "Summary Total",
        "CO2 emissions without LULUCF / LUCF",
        "CO2 net emissions/removals by LULUCF / LUCF",
        "CO2 net emissions/removals with LULUCF / LUCF",
        "GHG emissions without LULUCF / LUCF",
        "GHG net emissions/removals by LULUCF / LUCF",
        "GHG net emissions/removals with LULUCF / LUCF"
      )
      stopifnot(pull(data[1]) == rows[2:8])

      # remove empty line (row 2) between header and data
      data <- data[-1, ]
      colnames(data)[1] <- "variable"

      # there is often one summary column right of the main data
      if (!.isYear(tail(names(data), 1))) {
        stopifnot(all(is.na(data[, ncol(data)])))
        data <- data[, -ncol(data)]
      }
      stopifnot(.isYear(names(data)[-1]))

      data <- data %>%
        pivot_longer(!1, names_to = "year", values_to = "value") %>%
        mutate(region = region, .before = 1)

      result <- bind_rows(result, data)
    }

    # Something is wrong if the result has too many NA
    stopifnot(!is.null(result))
    stopifnot(sum(is.na(result["value"])) / nrow(result) < 0.5)

    result %>%
      as.magpie(tidy = TRUE) %>%
      return()
  }

  if (subtype == "annex-1") {
    return(.readUNFCCCAnnex1())
  } else if (subtype == "non-annex-1") {
    return(.readUNFCCCNonAnnex1())
  } else {
    stop("Unsupported subtype: ", subtype)
  }
}
