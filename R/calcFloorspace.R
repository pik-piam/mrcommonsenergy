#' Floor space in buildings
#'
#' Residential, commercial and total floor space from EDGE-B. Set
#'
#' @author Antoine Levesque, Robin Hasse
#'
#' @param onlyTotal boolean, only give total instead of sub-sectoral floor space
#' @param perCap boolean, give floor space per capita instead of absolute floor space
#' @param scenario A string (or vector of strings) designating the scenario(s) to be returned.
#' @return MAgPIE object with buildings floor space
#'
#'
calcFloorspace <- function(scenario, onlyTotal = FALSE, perCap = FALSE) {

  ## Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs", to calls of the individual scenarios.
  scenario <- mrdrivers::toolReplaceShortcuts(scenario) %>% unique()

  data <- readSource("EdgeBuildings", subtype = "Floorspace", subset = scenario)

  if (onlyTotal) {
    data <- data[, , "buildings"]
  }

  if (perCap) {
    popScen <- scenario[scenario %in% mrdrivers::toolGetScenarioDefinition(driver = "Population",
                                                                           aslist = TRUE)$scenario]
    pop <- calcOutput("Population",
      scenario = unique(c("SSP2", popScen)),
      years = getYears(data),
      aggregate = FALSE
    )
    ## For scenarios for which no specific Population data exists, use SSP2 data.
    popAdd <- purrr::map(scenario[!scenario %in% popScen], ~ setItems(pop[, , "SSP2"], 3, .x)) %>%
      mbind()
    pop <- pop %>%
      mbind(popAdd) %>%
      mselect(variable = scenario)
    getSets(pop) <- gsub("variable", "scenario", getSets(pop))

    # million m2 / million cap = m2/cap
    data <- data / pop
  }

  if (onlyTotal) {
    data <- collapseNames(data)
  }

  list(x = data,
       weight = if (perCap) pop else NULL,
       unit = if (perCap) "m2/cap" else "million m2",
       description = if (perCap) "Buildings floor space per capita" else "Buildings floor space")
}
