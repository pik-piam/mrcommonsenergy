#' Apply adjustments to industry-related IEA data
#'
#' This function prepares the industry-related IEA before mapping it to REMIND sectors.
#' There are three different types of adjustments done:
#' 1. replace coke oven and blast furnace outputs (`BLFURGS`, `OGASES`, `OVENCOKE`,
#' `COKEOVGS`, `COALTAR`, `NONCRUDE`) by inputs
#' (required for dealing with energy flows from the steel sector to other sectors)
#' 2. prepare industry-related time series
#' 3. apply corrections to IEA data to cope with fragmentary time series
#'
#' The corrections done by this function are rather rudimentary and crude. This
#' gets smoothed away in regional aggregation. But do not use the resulting
#' country-level data without additional scrutiny.
#'
#' Use regional or global averages if IEA industry data lists energy use only as
#' "non-specified".
#'
#' @md
#' @param data MAgPIE object containing the IEA Energy Balances data
#'
#' @return a MAgPIE object
#'
#' @author Michaja Pehl, Felix Schreyer, Falk Benke
#'
#' @importFrom assertr not_na assert
#' @importFrom dplyr anti_join group_by inner_join left_join mutate rename
#'     select summarise semi_join everything ungroup
#' @importFrom quitte character.data.frame sum_total_
#' @importFrom tibble as_tibble tribble
#' @importFrom stats na.omit
#' @importFrom tidyr complete nesting spread
#' @export
#'
toolFixIeaDataForIndustrySubsectors <- function(data) {

  ####

  # This function contains the following steps:

  # (CO = coke oven,
  #  BF = blast furnace)

  # 1. Replace steel sector outputs by inputs
  #   1.1 Define functions
  #   1.2 Prepare data and define flows
  #   1.3 Replace flows of BF outputs and by inputs into BF
  #   1.4 Replace flows of CO outputs by inputs to CO
  #   1.5 Calculate CO Losses
  #   1.6 Recalculate BF inputs w/ CO replacements
  #   1.7 Calculate BF Losses
  #   1.8 Replace IEA data with steel sector adjustments
  # 2. Prepare Industry Subsectors Timeseries
  #   2.1 Define flows and mappings
  #   2.2 Extend industry subsector timeseries
  #   2.3 Apply five-year moving average
  # 3. Fix suspicious industry products
  #   3.1 Prepare data to fix
  #   3.2 Redistribute products to industry-related flows
  #   3.3 Replace and append data

  ####

  # 1. Replace steel sector outputs by inputs ----

  ## 1.1 Define functions ----


  .cleanData <- function(m, keepZeros = FALSE) {
    m %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(iso3c = "Region", year = "Year", product = "Data1", flow = "Data2",
             value = "Value") %>%
      filter(0 != .data$value | keepZeros) %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year))
  }

  ## 1.2 Prepare data and define flows ----

  # nolint start quotes_linter
  # nolint start object_name_linter

  ## flow definitions
  ieaFlows <- tribble(
    ~summary.flow,   ~flow,
    # Total Primary Energy Production
    'TES',          'INDPROD',    # primary energy production
    'TES',          'IMPORTS',
    'TES',          'EXPORTS',
    'TES',          'MARBUNK',    # international marine bunkers
    'TES',          'AVBUNK',     # international aviation bunkers
    NA_character_,  'TRANSFER',   # inter-product transfers, product transfers,
    # and recycling
    'TES',          'STOCKCHA',   # stock changes

    # Transformation Processes
    'TOTTRANF',      'MAINELEC',    # main activity producer electricity plants
    'TOTTRANF',      'AUTOELEC',    # autoproducer electricity plants
    'TOTTRANF',      'MAINCHP',     # main activity producer CHP plants
    'TOTTRANF',      'AUTOCHP',     # autoproducer electricity plants
    'TOTTRANF',      'MAINHEAT',    # main activity producer heat plants
    'TOTTRANF',      'AUTOHEAT',    # autoproducer heat plants
    'TOTTRANF',      'THEAT',       # heat pumps
    'TOTTRANF',      'TBOILER',     # electric boilers
    'TOTTRANF',      'TELE',        # chemical heat for electricity production
    'TOTTRANF',      'TBLASTFUR',   # blast furnaces
    'TOTTRANF',      'TGASWKS',     # gas works
    'TOTTRANF',      'TCOKEOVS',    # coke ovens
    'TOTTRANF',      'TPATFUEL',    # patent fuel plants
    'TOTTRANF',      'TBKB',        # peat briquette plants
    'TOTTRANF',      'TREFINER',    # oil refineries
    'TOTTRANF',      'TPETCHEM',    # petrochemical plants
    'TOTTRANF',      'TCOALLIQ',    # coal liquefaction plants
    'TOTTRANF',      'TGTL',        # gas-to-liquid plants
    'TOTTRANF',      'TBLENDGAS',   # blended natural gas
    'TOTTRANF',      'TCHARCOAL',   # charcoal production plants
    'TOTTRANF',      'TNONSPEC',    # non-specified transformation

    # Energy Industry Own Use and Losses
    'TOTENGY',       'EMINES',      # coal mines
    'TOTENGY',       'EOILGASEX',   # oil and gas extraction
    'TOTENGY',       'EBLASTFUR',   # blast furnaces
    'TOTENGY',       'EGASWKS',     # gas works
    'TOTENGY',       'EBIOGAS',     # gasifications plants for biogases
    'TOTENGY',       'ECOKEOVS',    # coke ovens
    'TOTENGY',       'EPATFUEL',    # patent fuel plants
    'TOTENGY',       'EBKB',        # peat briquette plants
    'TOTENGY',       'EREFINER',    # oil refineries
    'TOTENGY',       'ECOALLIQ',    # coal liquefaction plants
    'TOTENGY',       'ELNG',        # liquefaction/regasification plants
    'TOTENGY',       'EGTL',        # gas-to-liquied plants
    'TOTENGY',       'EPOWERPLT',   # own use in electricity, CHP, and heat

    # plants
    'TOTENGY',       'EPUMPST',     # pumped storage plants
    'TOTENGY',       'ENUC',        # nuclear industry
    'TOTENGY',       'ECHARCOAL',   # charcoal production plants
    'TOTENGY',       'ENONSPEC',    # non-specified energy industry

    # Final Consumption
    'TFC',           'IRONSTL',    # iron and steel
    'TFC',           'CHEMICAL',   # chemical and petrochemical
    'TFC',           'NONFERR',    # non-ferrous metals
    'TFC',           'NONMET',     # non-metallic minerals
    'TFC',           'TRANSEQ',    # transport equipment
    'TFC',           'MACHINE',    # machinery
    'TFC',           'MINING',     # mining and quarrying
    'TFC',           'FOODPRO',    # food production
    'TFC',           'PAPERPRO',   # paper, pulp, and print
    'TFC',           'WOODPRO',    # wood and wood products
    'TFC',           'CONSTRUC',   # construction
    'TFC',           'TEXTILES',   # textiles
    'TFC',           'INONSPEC',   # non-specified industry
    'TFC',           'WORLDAV',    # world aviation bunkers
    'TFC',           'DOMESAIR',   # domestic aviation
    'TFC',           'ROAD',       # road
    'TFC',           'RAIL',       # rail
    'TFC',           'PIPELINE',   # pipeline transport
    'TFC',           'WORLDMAR',   # world marine bunkers
    'TFC',           'DOMESNAV',   # domestic navigation
    'TFC',           'TRNONSPE',   # non-specified transport
    'TFC',           'RESIDENT',   # residential
    'TFC',           'COMMPUB',    # commercial and public services
    'TFC',           'AGRICULT',   # agriculture and forestry
    'TFC',           'FISHING',    # fishing
    'TFC',           'ONONSPEC',   # non-specified other consumption
    'TFC',           'NONENUSE',   # non-energy use

    'TOTIND',        'IRONSTL',    # iron and steel
    'TOTIND',        'CHEMICAL',   # chemical and petrochemical
    'TOTIND',        'NONFERR',    # non-ferrous metals
    'TOTIND',        'NONMET',     # non-metallic minerals
    'TOTIND',        'TRANSEQ',    # transport equipment
    'TOTIND',        'MACHINE',    # machinery
    'TOTIND',        'MINING',     # mining and quarrying
    'TOTIND',        'FOODPRO',    # food production
    'TOTIND',        'PAPERPRO',   # paper, pulp, and print
    'TOTIND',        'WOODPRO',    # wood and wood products
    'TOTIND',        'CONSTRUC',   # construction
    'TOTIND',        'TEXTILES',   # textiles
    'TOTIND',        'INONSPEC',   # non-specified industry

    # Transport
    'TOTTRANS',      'WORLDAV',    # world aviation bunkers
    'TOTTRANS',      'DOMESAIR',   # domestic aviation
    'TOTTRANS',      'ROAD',       # road
    'TOTTRANS',      'RAIL',       # rail
    'TOTTRANS',      'PIPELINE',   # pipeline transport
    'TOTTRANS',      'WORLDMAR',   # world marine bunkers
    'TOTTRANS',      'DOMESNAV',   # domestic navigation
    'TOTTRANS',      'TRNONSPE',   # non-specified transport

    # Other Consumption
    'TOTOTHER',      'RESIDENT',   # residential
    'TOTOTHER',      'COMMPUB',    # commercial and public services
    'TOTOTHER',      'AGRICULT',   # agriculture and forestry
    'TOTOTHER',      'FISHING',    # fishing
    'TOTOTHER',      'ONONSPEC',   # non-specified other consumption

    # Non-Energy Use
    'NONENUSE',      'NEINTREN',   # non-energy use in industry/transformation/
    # energy
    NA_character_,   'NECHEM',     # non-energy use chemical/petrochemical
    'NONENUSE',      'NETRANS',    # non-energy use in transport
    'NONENUSE',      'NEOTHER',    # non-energy use in other

    # Electricity Output
    'ELOUTPUT',      'ELMAINE',   # main activity producer electricity plants
    'ELOUTPUT',      'ELAUTOE',   # autoproducer electricity plants
    'ELOUTPUT',      'ELMAINC',   # main activity producer CHP plants
    'ELOUTPUT',      'ELAUTOC',   # autoproducer CHP plants

    # Heat Output
    'HEATOUT',       'HEMAINC',   # main activity producer CHP plants
    'HEATOUT',       'HEAUTOC',   # autoproducer CHP plants
    'HEATOUT',       'HEMAINH',   # main activity producer heat plants
    'HEATOUT',       'HEAUTOH'    # autoproducer heat plants
  )

  # nolint end quotes_linter


  baseFlows <- unique(ieaFlows$flow)
  summaryFlows <- unique(na.omit(ieaFlows$summary.flow))
  allFlows <- intersect(c(baseFlows, summaryFlows), getNames(data, dim = "FLOW"))

  ### blast furnace flows to be replaced
  # all transformation, energy system and final consumption flows, except for
  # those related to blast furnaces
  flow_BLASTFUR_to_replace <- setdiff(allFlows, c("EBLASTFUR", "TBLASTFUR"))

  ### coke oven flows to be replaced
  # all transformation, energy system and final consumption flows, except for
  # those related to coke ovens
  flow_COKEOVS_to_replace <- setdiff(allFlows, c("ECOKEOVS", "TCOKEOVS"))

  ## 1.3 Replace flows of BF outputs and by inputs into BF ----

  # Example of how replacement routine works:
  # Flows of BF outputs into other sectors: BLFURGAS.MAINELEC = -20
  # BF inputs: OVENCOKE.TBLASTFUR = -90, COKCOAL.TBLASTFUR = -10
  # Flows of BF outputs are attributed to inputs via input shares:
  # New BF output flows
  # OVENCOKE.MAINELEC = -20 * (90 / 100) = -18 # nolint
  # COKCOAL.MAINELEC = -20 * (10 / 100) = -2 # nolint

  # all products in/out of blast furnace transformation and energy demand, except
  # summary flows 'TOTAL' and 'MRENEW'
  data_BLASTFUR <- data[, , c("EBLASTFUR", "TBLASTFUR")][, , c("TOTAL", "MRENEW"), invert = TRUE] %>%
    .cleanData() %>%
    group_by(!!!syms(c("iso3c", "year", "product"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  ### blast furnace inputs
  # inputs into transformation/energy system are negative
  data_BLASTFUR_inputs <- data_BLASTFUR %>%
    filter(0 > .data$value)

  ### blast furnace outputs
  # outputs from transformation are positive
  data_BLASTFUR_outputs <- data_BLASTFUR %>%
    filter(0 < .data$value)

  ### blast furnace output products
  # products blast furnaces supply to other flows
  outputs_BLASTFUR <- data_BLASTFUR_outputs %>%
    select(-"value")

  ### blast furnace product use
  data_BLASTFUR_use <- data[, , flow_BLASTFUR_to_replace] %>%
    .cleanData(keepZeros = TRUE) %>%
    dplyr::right_join(outputs_BLASTFUR, by = c("iso3c", "year", "product"))

  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products
  data_BLASTFUR_replacement <- dplyr::right_join(
    data_BLASTFUR_inputs %>%
      group_by(!!!syms(c("iso3c", "year"))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-"value"),

    data_BLASTFUR_use %>%
      select(-"product"),

    c("iso3c", "year")
  ) %>%
    # assume that countries/years that have no inputs into blast furnaces,
    # also have no outputs and use of blast furnace products (e.g. ISR 1973)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = "Only valid blast furnace replacement data") %>%
    mutate(value = .data$value * .data$factor) %>%
    group_by(!!!syms(c("iso3c", "year", "product", "flow"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  ## 1.4 Replace flwos of CO outputs by inputs to CO ----

  # Example of how replacement routine works:
  # Flow of CO outputs: COKEOVGS.TBLASTFUR = -10 (coke oven gas used in blast furnace)
  # CO inputs: COKCOAL.TCOKEOVS = -180, NATGAS.TCOKEOVS = -20
  # Flows of CO outputs are attributed to inputs via input shares:
  # New Flows CO outputs:
  # COKCOAL.TBLASTFUR = -10 * (180 / 200) = -9 # nolint
  # NATGAS.TBLASTFUR = -10 * (20 / 200) = -1 # nolint

  # all products in/out of coke oven transformation and energy demand, except
  # summary flows 'TOTAL' and 'MRENEW'
  data_COKEOVS <- data[, , c("ECOKEOVS", "TCOKEOVS")][, , c("TOTAL", "MRENEW"), invert = TRUE] %>%
    .cleanData() %>%
    group_by(!!!syms(c("iso3c", "year", "product"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  #### apply blast furnace replacement
  # Coke ovens and blast furnaces can be both inputs and outputs to one another at
  # the same time.  To untangle this, we first replace blast furnace outputs that
  # are inputs into coke ovens by coke oven outputs, which are netted with the
  # direct outputs (here), and then replace coke oven outputs that are blast
  # furnace inputs by coke oven inputs (further below).

  data_COKEOVS <- bind_rows(
    data_COKEOVS %>%
      anti_join(outputs_BLASTFUR,
                by = c("iso3c", "year", "product")),

    data_BLASTFUR_replacement %>%
      filter(.data$flow %in% c("ECOKEOVS", "TCOKEOVS")) %>%
      select(-"flow")
  ) %>%
    group_by(!!!syms(c("iso3c", "year", "product"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  ### coke oven inputs
  # inputs into transformation/energy system are negative
  data_COKEOVS_inputs <- data_COKEOVS %>%
    filter(0 > .data$value)

  ### coke oven outputs
  # outputs from transformation are positive
  data_COKEOVS_outputs <- data_COKEOVS %>%
    filter(0 < .data$value)

  ### coke oven output products
  # products blast furnaces supply to other flows
  outputs_COKEOVS <- data_COKEOVS_outputs %>%
    select(-"value")

  ### coke oven product use
  data_COKEOVS_use <- data[, , flow_COKEOVS_to_replace] %>%
    .cleanData(keepZeros = TRUE) %>%
    dplyr::right_join(outputs_COKEOVS, by = c("iso3c", "year", "product"))

  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use coke oven products
  data_COKEOVS_replacement <- dplyr::right_join(
    data_COKEOVS_inputs %>%
      group_by(!!!syms(c("iso3c", "year"))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-"value"),

    data_COKEOVS_use %>%
      select(-"product"),

    c("iso3c", "year")
  ) %>%
    # assume that countries/years that have no inputs into coke ovens,
    # also have no outputs and use of coke oven products
    # (in reality these products may be imported, but we neglect this case)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = "Only valid coke oven replacement data") %>%
    mutate(value = .data$value * .data$factor) %>%
    select("iso3c", "year", "product", "flow", "value")

  ## 1.5 Calculate CO Losses ----
  # coke oven losses (true losses from ECOKEOVS and transformation energy from
  # TCOKEOVS) are allotted to the IRONSTL sector
  # losses are the difference of inputs and outputs, weighted by input shares
  # right_join() filters out countries/years that do not use coke oven products

  # Example calculation of transformation losses:
  # CO outputs: COKEOVGS.TBLASTFUR = -10 (coke oven gas used in blast furnace)
  # CO inputs: COKCOAL.TCOKEOVS = -180, NATGAS.TCOKEOVS = -20
  # Transformation losses are calculated as difference between
  # inputs and outputs that are attributed to inputs by input shares:
  # Coke oven energy losses:
  # COKCOAL.IRONSTL = 180 - 10 * (180 / 200) = 171 # nolint
  # NATGAS.IRONSTL = 20 - 10 * (20 / 200) = 18 # nolint
  # Note coke oven losses are attributed to IRONSTL flow.

  data_COKEOVS_loss <- dplyr::right_join(
    data_COKEOVS_inputs,

    data_COKEOVS_outputs %>%
      group_by(!!!syms(c("iso3c", "year"))) %>%
      summarise(output = sum(.data$value), .groups = "drop"),

    c("iso3c", "year")
  ) %>%
    # assume that countries/years that have no inputs into coke ovens,
    # also have no transformation losses
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = "Only valid coke oven loss data") %>%
    group_by(!!!syms(c("iso3c", "year"))) %>%
    mutate(value = (sum(-.data$value) - .data$output)
           * .data$value / sum(.data$value),
           flow = "IRONSTL") %>%
    ungroup() %>%
    select("iso3c", "year", "product", "flow", "value")

  ## 1.6 Recalculate BF inputs w/ CO replacements ----

  #### apply coke oven replacement
  # Coke ovens and blast furnaces can be both inputs and outputs to one another at
  # the same time.  To untangle this, we first replace blast furnace outputs that
  # are inputs into coke ovens by coke oven outputs, which are netted with the
  # direct outputs (above), and then replace coke oven outputs that are blast
  # furnace inputs by coke oven inputs (here).
  data_BLASTFUR <- bind_rows(
    data_BLASTFUR %>%
      anti_join(outputs_COKEOVS, by = c("iso3c", "year", "product")),

    data_COKEOVS_replacement %>%
      filter(.data$flow %in% c("EBLASTFUR", "TBLASTFUR")) %>%
      select(-"flow")
  ) %>%
    group_by(!!!syms(c("iso3c", "year", "product"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  ### blast furnace inputs
  # inputs into transformation/energy system are negative
  data_BLASTFUR_inputs <- data_BLASTFUR %>%
    filter(0 > .data$value)

  ### blast furnace replacement data
  # outputs are replaced joule-by-joule with inputs, according to the input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products
  data_BLASTFUR_replacement <- dplyr::right_join(
    data_BLASTFUR_inputs %>%
      group_by(!!!syms(c("iso3c", "year"))) %>%
      mutate(factor = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-"value"),

    data_BLASTFUR_use %>%
      select(-"product"),

    c("iso3c", "year")
  ) %>%
    # assume that countries/years that have no inputs into blast furnaces,
    # also have no outputs and use of blast furnace products (e.g. ISR 1973)
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = "Only valid blast furnace replacement data") %>%
    mutate(value = .data$value * .data$factor) %>%
    group_by(!!!syms(c("iso3c", "year", "product", "flow"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  ## 1.7 Calculate BF Losses ----
  # blast furnace losses (true losses from EBLASTFUR and transformation energy
  # from TBLASTFUR) are allotted to the IRONSTL sector
  # losses are the difference of inputs and outputs, weighted by input shares
  # right_join() filters out countries/years that do not use blast furnace
  # products

  # Example calculation of transformation losses:
  # BF outputs: BLFURGAS.MAINELEC = -20
  # BF inputs: COKCOAL.TBLASTFUR = -90, ELECTR.EBLASTFUR = -10
  # Transformation losses are calculated as difference between
  # inputs and outputs that are attributed to inputs by input shares:
  # Blast furnace energy losses:
  # COKCOAL.IRONSTL = 100 - 20 * (90 / 100) = 82 # nolint
  # ELECTR.IRONSTL = 10 - 20 * (10 / 100) = 8 # nolint
  # Note blast furnace losses are attributed to IRONSTL

  data_BLASTFUR_loss <- dplyr::right_join(
    data_BLASTFUR_inputs,

    data_BLASTFUR_outputs %>%
      group_by(!!!syms(c("iso3c", "year"))) %>%
      summarise(output = sum(.data$value), .groups = "drop"),

    c("iso3c", "year")
  ) %>%
    # assume that countries/years that have no inputs into blast furnaces,
    # also have no transformation losses
    filter(!is.na(.data$product)) %>%
    assert(not_na, everything(),
           description = "Only valid blast furnace loss data") %>%
    group_by(!!!syms(c("iso3c", "year"))) %>%
    mutate(value = (sum(-.data$value) - .data$output)
           * .data$value / sum(.data$value),
           flow = "IRONSTL") %>%
    ungroup() %>%
    select("iso3c", "year", "product", "flow", "value")

  ## 1.8 Replace IEA data with steel sector adjustments ----

  # bind all data of coke oven and blast furnace adjustment routine together
  df_CO_BF_adjustment <-  bind_rows(
    # filter already replaced data
    data_COKEOVS_replacement %>%
      filter(!.data$flow %in% c("EBLASTFUR", "TBLASTFUR")),

    data_BLASTFUR_replacement %>%
      filter(!.data$flow %in% c("ECOKEOVS", "TCOKEOVS")),

    data_COKEOVS_loss %>%
      sum_total_("product", name = "TOTAL"),

    data_BLASTFUR_loss %>%
      sum_total_("product", name = "TOTAL")
  ) %>%
    group_by(!!!syms(c("iso3c", "year", "product", "flow"))) %>%
    summarise(value = sum(.data$value), .groups = "drop")

  # take original IEA data and subtract flows that contain CO or BF outputs
  # these flows are now accounted for in the CO/BF adjusted data df_CO_BF_adjustment

  subtract <- bind_rows(data_BLASTFUR_use, data_COKEOVS_use) %>%
    filter(.data$value != 0) %>%
    as.magpie()
  subtract[is.na(subtract)] <- 0

  data[getItems(subtract, dim = 1), getYears(subtract), getNames(subtract)] <-
    data[getItems(subtract, dim = 1), getYears(subtract), getNames(subtract)] - subtract

  # add flows from CO/BF adjustment routine df_CO_BF_adjustment

  replace <- df_CO_BF_adjustment %>%
    filter(.data$value != 0) %>%
    as.magpie()

  replace[is.na(replace)] <- 0
  newProductFlow <- setdiff(getNames(replace), getNames(data))
  data <- add_columns(data, addnm = newProductFlow, dim = 3, fill = 0)

  data[getItems(replace, dim = 1), getYears(replace), getNames(replace)] <-
    data[getItems(replace, dim = 1), getYears(replace), getNames(replace)] + replace

  # set coke oven and blast furnace flows to zero
  # as the energy is already accounted for in others flows
  data[, , c("ECOKEOVS", "TCOKEOVS", "EBLASTFUR", "TBLASTFUR")] <- 0

  # recalculate summary flows after CO+BF adjustment
  # define additional summary flows to be recalculated

  # nolint start quotes_linter
  additionalSummaryFlows <- tribble(
    ~summary.flow,       ~flow,
    # Manufacturing Industry
    'MANUFACT',          'IRONSTL',    # iron and steel
    'MANUFACT',          'CHEMICAL',   # chemical and petrochemical
    'MANUFACT',          'NONFERR',    # non-ferrous metals
    'MANUFACT',          'NONMET',     # non-metallic minerals
    'MANUFACT',          'TRANSEQ',    # transport equipment
    'MANUFACT',          'MACHINE',    # machinery
    'MANUFACT',          'FOODPRO',    # food production
    'MANUFACT',          'PAPERPRO',   # paper, pulp, and print
    'MANUFACT',          'WOODPRO',    # wood and wood products
    'MANUFACT',          'TEXTILES'    # textiles
  )
  # nolint end quotes_linter

  ieaFlows <- ieaFlows %>%
    filter(!is.na(.data$summary.flow)) %>%
    rbind(additionalSummaryFlows)

  # sum all flows after CO+BF adjustment to get summary flows as defined in
  # ieaFlows table above

  sumFlows <- data %>%
    mselect(FLOW = unique(ieaFlows$flow)) %>%
    .cleanData() %>%
    inner_join(ieaFlows, by = "flow") %>%
    group_by(!!!syms(c("iso3c", "year", "product", "summary.flow"))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    ungroup() %>%
    rename(flow = "summary.flow") %>%
    as.magpie()

  sumFlows <- add_columns(sumFlows,
                          addnm = setdiff(getItems(data, dim = 1), getItems(sumFlows, dim = 1)),
                          dim = 1)
  sumFlows <- add_columns(sumFlows,
                          addnm = setdiff(getYears(data), getYears(sumFlows)),
                          dim = 2)

  # replace summary flows with sums of adjusted CO+BF data
  data <- data[, , intersect(getNames(data, dim = "FLOW"),
                             ieaFlows$summary.flow), invert = TRUE]
  data <- mbind(data, sumFlows)

  data[is.na(data)] <- 0

  # clean up no longer used data to save space
  rm(replace, subtract, sumFlows)
  rm(list = ls(pattern = "^data_"))

  # nolint end object_name_linter

  # 2. Prepare Industry Subsector Time Series ----

  ## 2.1 Define flows and mappings ----

  # all industry subsector flows
  flowsToFix <- c(
    "CHEMICAL", "CONSTRUC", "FOODPRO", "IRONSTL", "MACHINE",
    "MINING", "NONFERR", "NONMET", "PAPERPRO", "TEXTILES",
    "TRANSEQ", "WOODPRO"
  )

  regionMapping <- toolGetMapping(name = "regionmapping_21_EU11.csv",
                                  type = "regional",
                                  where = "mappingfolder") %>%
    as_tibble() %>%
    select("iso3c" = "CountryCode", "region" = "RegionCode")

  ## 2.2 Extend industry subsector time series ----
  # subset of data containing industry subsector products and flows

  dataIndustry <- data[, , c(flowsToFix, "TOTIND", "INONSPEC")] %>%
    .cleanData() %>%
    inner_join(regionMapping, "iso3c")

  # 3. Fix suspicious products in industry ----

  ## 3.1 Prepare data to fix ----

  # all products that use less then 1 % of total energy outside of non-specified
  # industry are 'suspicious' and will be fixed
  dataToFix <- dataIndustry %>%
    filter(.data$flow %in% c("TOTIND", "INONSPEC")) %>%
    spread(.data$flow, .data$value) %>%
    mutate("INONSPEC" = ifelse(is.na(.data$INONSPEC), 0, .data$INONSPEC)) %>%
    filter(1 - .data$INONSPEC / .data$TOTIND < 1e-2) %>%
    select("iso3c", "region", "year", "product", "TOTIND")

  # use all non-suspicious data to calculate regional and global averages
  dataForFixing <- anti_join(
    dataIndustry %>% filter(.data$flow != "TOTIND"),
    dataToFix %>% select(-"TOTIND"),
    c("iso3c", "region", "year", "product")
  ) %>%
    as_tibble()

  dataForFixing <- full_join(

    # compute global averages
    dataForFixing %>%
      group_by(.data$year, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value), .groups = "drop_last") %>%
      mutate("global_share" = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-.data$value) %>%
      # and expand to all regions
      mutate("region" = NA_character_) %>%
      complete(nesting(!!!syms(c("year", "product", "flow", "global_share"))),
               region = unique(regionMapping$region)) %>%
      filter(!is.na(.data$region)),

    # compute regional averages
    dataForFixing %>%
      group_by(.data$year, .data$region, .data$product, .data$flow) %>%
      summarise(value = sum(.data$value), .groups = "drop_last") %>%
      mutate("regional_share" = .data$value / sum(.data$value)) %>%
      ungroup() %>%
      select(-"value"),

    c("year", "region", "product", "flow")
  ) %>%
    group_by(.data$year, .data$region, .data$product) %>%
    mutate("use_global" = all(is.na(.data$regional_share))) %>%
    ungroup() %>%
    # use regional averages if available, global averages otherwise
    mutate(value = ifelse(.data$use_global == TRUE, .data$global_share,
                          ifelse(is.na(.data$regional_share), 0, .data$regional_share))) %>%
    select(-c("regional_share", "global_share", "use_global"))

  # calculated fixed data
  dataIndustryFixed <- left_join(
    dataToFix,
    dataForFixing,
    c("region", "year", "product")
  ) %>%
    # replace "suspicious" data with averages
    mutate("value" = .data$TOTIND * .data$value) %>%
    select("iso3c", "region", "year", "product", "flow", "value") %>%
    # remove data where fixing is not possible, because no data for fixing available
    filter(!is.na(.data$value))

  # replace fixed industry products, make sure that there are no overlaps
  dataIndustryFixed <- rbind(
    # totals in industry data
    dataIndustry %>% filter(.data$flow == "TOTIND"),
    # industry products that did not need fixing
    dataIndustry %>%
      filter(.data$flow != "TOTIND") %>%
      anti_join(dataIndustryFixed, by = c("iso3c", "region", "year", "product")),
    # industry products that were fixed
    dataIndustryFixed
  )


  ## 3.2 Redistribute products to industry-related flows ----
  # redistribute at least 1% of each product into each subsector

  threshold <- 1e-2

  # which flow belongs to which subsector?
  subsectorMapping <- tribble(
    ~subsector,    ~flow,
    "cement",      "NONMET",
    "chemicals",   "CHEMICAL",
    "steel",       "IRONSTL"
  ) %>%
    complete(flow = c(flowsToFix, "INONSPEC"),
             fill = list(subsector = "otherInd"))

  dataIndustryFixed <- dataIndustryFixed %>%
    complete(nesting(!!!syms(c("iso3c", "region", "year", "product"))),
             flow = c(flowsToFix, "INONSPEC"),
             fill = list(value = 0)) %>%
    group_by(.data$iso3c, .data$year, .data$product) %>%
    dplyr::right_join(subsectorMapping, by = "flow") %>%
    # compute subsector totals
    group_by(.data$subsector, .add = TRUE) %>%
    mutate("subsector.total" = sum(.data$value),
           "subsector.count" = dplyr::n()) %>%
    ungroup(.data$subsector) %>%
    mutate(
      # each subsector consumes at least <threshold> of the total consumption of
      # each product (with the exception of heat, which is only consumed in the
      #  otherInd subsector)
      "subsector.min" = ifelse("HEAT" == .data$product, 0,
                               threshold * sum(.data$value)),
      # if total subsector consumption is below the minimum, consumption must be
      # added
      "subsector.add" = pmax(0, .data$subsector.min - .data$subsector.total),
      # each flow gets consumption added according to its share in total
      # subsector consumption
      "flow.add" = ifelse(0 != .data$subsector.total,
                          (.data$subsector.add / .data$subsector.total * .data$value),
                          .data$subsector.add / .data$subsector.count),
      # if the additional flow is zero, consumption has to be subtracted from\
      # this flow, in relation to its share of all flows with
      # more-than-threshold consumption
      "flow.add" = ifelse(0 != .data$flow.add,
                          .data$flow.add,
                          (-sum(.data$flow.add) * .data$value / sum(.data$value[0 == .data$flow.add]))),
      "value.new" = .data$value + .data$flow.add
    ) %>%
    ungroup() %>%
    select("iso3c", "region", "year", "product", "flow", "value" = "value.new")

  ## 3.3 Replace and append fixed data ----
  dataIndustryFixedOverwrite <- dataIndustryFixed %>%
    semi_join(dataIndustry, c("iso3c", "region", "year", "product", "flow")) %>%
    select("iso3c", "year", "product", "flow", "value") %>%
    as.magpie(spatial = 1, temporal = 2, datacol = 5)

  dataIndustryFixedOverwrite[is.na(dataIndustryFixedOverwrite)] <- 0

  dataIndustryFixedAppend <- dataIndustryFixed %>%
    anti_join(dataIndustry, c("iso3c", "region", "year", "product", "flow")) %>%
    select("iso3c", "year", "product", "flow", "value") %>%
    complete(nesting(!!!syms(c("product", "flow"))),
             iso3c = getItems(data, dim = 1),
             year = getYears(data, as.integer = TRUE),
             fill = list(value = 0)) %>%
    select("iso3c", "year", "product", "flow", "value") %>%
    as.magpie(spatial = 1, temporal = 2, datacol = 5)

  data[getItems(dataIndustryFixedOverwrite, dim = 1),
       getYears(dataIndustryFixedOverwrite),
       getNames(dataIndustryFixedOverwrite)] <- dataIndustryFixedOverwrite


  existingNames <- intersect(getNames(data), getNames(dataIndustryFixedAppend))
  addedNames <- setdiff(getNames(dataIndustryFixedAppend), getNames(data))
  tmp <- data[, , existingNames]
  tmp[is.na(tmp)] <- 0
  data[, , existingNames] <- tmp + dataIndustryFixedAppend[, , existingNames]

  if (length(addedNames > 0)) {
    data <- mbind(data, dataIndustryFixedAppend[, , addedNames])
  }

  return(data)
}
