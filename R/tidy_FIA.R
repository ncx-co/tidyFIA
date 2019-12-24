#' @title Create tidy FIA tables
#' @description This function queries the FIA database, by state abbreviation(s) or area of interest,
#' and returns a list of tidy data objects including the TREE, PLOT, COND, and SURVEY tables.
#' @param stateAbb a character vector of state abbreviations, ignored if aoi is supplied
#' @param aoi sf object containing area of interest
#' @return a list object containing tidy FIA data objects
#' @author Henry Rodman, Brian Clough
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export

tidy_fia <- function(stateAbb, aoi=NULL) {

  if(is.null(aoi)){
    targetStates <- stateAbb
  } else{
  # identify states
    targetStates <- us_states %>%
      sf::st_transform(sf::st_crs(aoi)) %>%
      sf::st_intersection(aoi) %>%
      dplyr::pull(ABB)
  }

  # download tables
  files <- purrr::map(
    .x = targetStates,
    .f = ~ tsutils::download_fia_by_state(state = .x)
  )

  # combine tables
  fiaSurvey <- purrr::map(files, "surveyFile") %>%
    vroom::vroom(
      delim = ",",
      col_types = vroom::cols(.default = "?", CN = "c")
    )

  fiaPlots <- purrr::map(files, "plotFile") %>%
    vroom::vroom(
      delim = ",",
      col_types = vroom::cols(
        .default = "?",
        CN = "c",
        PREV_PLT_CN = "c",
        SRV_CN = "c"
      )
    ) %>%
    dplyr::filter(
      KINDCD %in% c(1, 2),
      PLOT_STATUS_CD == 1,
      SRV_CN %in% fiaSurvey$CN,
      INVYR != 9999,
      MEASYEAR > 1998
    )

  fiaCond <- purrr::map(files, "condFile") %>%
    vroom::vroom(
      delim = ",",
      col_types = vroom::cols(
        .default = "?",
        PLT_CN = "c",
        CN = "c"
      )
    ) %>%
    dplyr::filter(
      PLT_CN %in% fiaPlots$CN,
      CONDPROP_UNADJ > 0.99, # single-condition
      DSTRBCD1 == 0, # dropping disturbed plots for now
      TRTCD1 == 0 # no stand treatment
    ) %>%
    dplyr::group_by(PLT_CN) %>%
    dplyr::mutate(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(count == 1)

  fiaTrees <- purrr::map(files, "treeFile") %>%
    vroom::vroom(
      delim = ",",
      col_types = vroom::cols(
        .default = "?",
        PLT_CN = "c",
        CN = "c"
      )
    ) %>%
    dplyr::rename(
      TREE_CN = CN
    ) %>%
    dplyr::filter(
      PLT_CN %in% fiaCond$PLT_CN,
      PLT_CN %in% fiaPlots$CN
    ) %>%
    dplyr::group_by(PLT_CN) %>%
    dplyr::mutate(numSubplots = length(unique(SUBP))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(numSubplots == 4)

  plotFinalFilt <- fiaPlots %>%
    dplyr::mutate(PREV_PLT_CN = as.character(PREV_PLT_CN)) %>%
    dplyr::filter(CN %in% fiaTrees$PLT_CN)

  condFinalFilt <- fiaCond %>%
    dplyr::filter(PLT_CN %in% fiaTrees$PLT_CN)

  # identify remeasured plots
  plotTable <- plotFinalFilt %>%
    dplyr::select(
      CN, PREV_PLT_CN, MEASYEAR, LAT, LON, ELEV, DESIGNCD,
      MACRO_BREAKPOINT_DIA
    ) %>%
    distinct() %>%
    dplyr::filter(!is.na(PREV_PLT_CN)) %>%
    dplyr::mutate(
      MEASYEAR_initial = plotFinalFilt$MEASYEAR[match(PREV_PLT_CN, plotFinalFilt$CN)]
    ) %>%
    dplyr::select(
      CN_initial = PREV_PLT_CN,
      MEASYEAR_initial,
      CN_grown = CN,
      MEASYEAR_grown = MEASYEAR
    ) %>%
    dplyr::filter(!is.na(MEASYEAR_initial), !is.na(MEASYEAR_grown))

  # uniquely identify plots by location
  plotMaster <- plotFinalFilt %>%
    dplyr::select(
      STATECD, UNITCD, COUNTYCD,
      PLOT, CN, MEASYEAR,
      LAT, LON, ELEV
    ) %>%
    unite(
      "fiaPlotID",
      c("STATECD", "UNITCD", "COUNTYCD", "PLOT"),
      sep = "_"
    )

  plotLocs <- plotMaster %>%
    dplyr::select(fiaPlotID, LON, LAT, ELEV) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(
      coords = c("LON", "LAT"),
      crs = 4326,
      remove = FALSE
    ) %>%
    sf::st_intersection(aoi %>% sf::st_transform(4326))

  geography_plot <- ggplot() +
    geom_sf(
      data = aoi,
      size = 0.5,
      color = "black",
      alpha = 0
    ) +
    geom_sf(
      data = plotLocs,
      color = "black",
      alpha = 1
    ) +
    theme_bw() +
    coord_sf() +
    ggtitle("FIA plot distribution")
  names(climateVals)
  # get climate raster values
  climateVals <- get_climate_bio_values(plotLocs) %>%
    dplyr::mutate(fiaPlotID = plotLocs$fiaPlotID)

  # run PCA on climate values
  climatePca <- stats::prcomp(
    na.omit(climateVals %>% dplyr::select(tidyselect::contains("wc2.0"))),
    center = TRUE,
    scale. = TRUE,
    rank = 4
  )

  climatePcaPreds <- predict(climatePca, newdata = climateVals) %>%
    data.frame() %>%
    dplyr::select(PC1:PC4) %>%
    dplyr::mutate(fiaPlotID = climateVals$fiaPlotID) %>%
    dplyr::rename_at(
      .vars = dplyr::vars(-fiaPlotID),
      .funs = ~ paste0("climate", .)
    )

  # compute delta vectors
  deltaTable <- plotTable %>%
    dplyr::left_join(
      plotMaster,
      by = c("CN_initial" = "CN", "MEASYEAR_initial" = "MEASYEAR")
    ) %>%
    dplyr::mutate(
      deltaID = paste(fiaPlotID, MEASYEAR_initial, MEASYEAR_grown, sep = "_")
    ) %>%
    dplyr::filter(fiaPlotID %in% plotLocs$fiaPlotID)

  keepPlots <- c(unique(deltaTable$CN_initial), unique(deltaTable$CN_grown)) %>%
    unique

  nDeltas <- nrow(deltaTable)

  message(
    paste(
      "there are", nDeltas, "delta vectors for this AOI"
    )
  )

  # filter trees down to plots with re-measurement data
  deltaTrees <- fiaTrees %>%
    dplyr::filter(
      PLT_CN %in% keepPlots,
      DIA >= 5,
      STATUSCD == 1,
      !is.na(TPA_UNADJ)
    ) %>%
    dplyr::mutate(
      diameter_meters = tsutils::convert_length(
        DIA,
        from = "in",
        to = "m"
      ),
      expansion_ha = tsutils::convert_hectares_to_acres(TPA_UNADJ),
      st_species = tsutils::fia2st(SPCD)
    ) %>%
    dplyr::select(
      PLT_CN, TREE_CN, SPGRPCD,
      diameter_meters, st_species, expansion_ha
    )

  # compute baph by plot
  baph <- tsutils::calculate_plot_basal_area(
    uniquePlots = keepPlots,
    plotID = deltaTrees$PLT_CN,
    expansion = deltaTrees$expansion_ha,
    diameter = deltaTrees$diameter_meters,
    units = "SI"
  )
  baph$plotID <- as.character(baph$plotID)

  # compute tph by plot
  tph <- tsutils::calculate_plot_stem_density(
    uniquePlots = keepPlots,
    plotID = deltaTrees$PLT_CN,
    expansion = deltaTrees$expansion_ha
  )
  tph$plotID <- as.character(tph$plotID)

  # compute diameter distribution by plot
  diameterProbs <- tsutils::calculate_plot_diameter_probabilities(
    uniquePlots = keepPlots,
    plotID = deltaTrees$PLT_CN,
    expansion = deltaTrees$expansion_ha,
    diameter = deltaTrees$diameter_meters
  )

  diameterProbs$plotID <- as.character(diameterProbs$plotID)

  # compute species importance by plot
  speciesImportance <- tsutils::calculate_plot_species_importance(
    uniquePlots = keepPlots,
    plotID = deltaTrees$PLT_CN,
    species = deltaTrees$st_species,
    expansion = deltaTrees$expansion_ha,
    diameter = deltaTrees$diameter_meters
  )

  speciesImportance$plotID <- as.character(speciesImportance$plotID)

  plotStats <- list(tph, baph, speciesImportance, diameterProbs) %>%
    purrr::reduce(left_join, by = "plotID") %>%
    dplyr::rename(
      CN = plotID,
      tph = stemDensity,
      baph = basal
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        tph, baph, tidyselect::contains("diam"), tidyselect::contains("spp")
      ),
      names_to = "attr",
      values_to = "value"
    )

  # build delta vectors
  deltaVectors <- deltaTable %>%
    dplyr::select(
      deltaID, fiaPlotID,
      CN_initial, CN_grown,
      MEASYEAR_initial, MEASYEAR_grown
    ) %>%
    tidyr::pivot_longer(
      cols = c(CN_initial, CN_grown, MEASYEAR_initial, MEASYEAR_grown),
      names_to = c(".value", "state"),
      names_pattern = "(.*)_(.*)"
    ) %>%
    dplyr::left_join(plotStats) %>%
    dplyr::select(deltaID, fiaPlotID, CN, MEASYEAR, state, attr, value) %>%
    tidyr::pivot_wider(
      names_from = state,
      values_from = c(CN, MEASYEAR, value)
    ) %>%
    dplyr::mutate(
      delta = (value_grown - value_initial) / (MEASYEAR_grown - MEASYEAR_initial)
    ) %>%
    dplyr::left_join(deltaTable)

  initialConditions <- plotStats %>%
    tidyr::pivot_wider(
      names_from = "attr",
      values_from = "value"
    )

  modelDat <- deltaVectors %>%
    dplyr::select(deltaID, fiaPlotID, CN_initial, attr, delta) %>%
    tidyr::pivot_wider(
      names_from = attr,
      values_from = delta,
      names_prefix = "delta_"
    ) %>%
    dplyr::left_join(initialConditions, by = c("CN_initial" = "CN"))

  jenkins <- ref_species %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(fiaID = spcd, genus, species, tidyselect::contains("spgrpcd")) %>%
    dplyr::left_join(st_sp2) %>%
    dplyr::filter(!is.na(stID)) %>%
    dplyr::mutate(sppName = paste0("spp_", stID)) %>%
    dplyr::filter(sppName %in% plotStats$attr) %>%
    dplyr::select(sppName, stID, fiaID, genus, species, jenkins_spgrpcd)

  modSpp <- modelDat %>%
    dplyr::select(tidyselect::contains("spp"), -tidyselect::contains("delta")) %>%
    mutate_at(
      .vars = dplyr::vars(tidyselect::contains("spp")),
      .funs = ~ ifelse(is.na(.), 0, .)
    ) %>%
    dplyr::summarize_all(
      .funs = mean
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "spp",
      values_to = "total"
    ) %>%
    dplyr::arrange(desc(total)) %>%
    dplyr::mutate(cumSum = cumsum(total)) %>%
    dplyr::filter(cumSum <= 0.80) %>%
    dplyr::pull(spp)

  modSppDeltas <- paste0("delta_", modSpp)

  sppDat <- modelDat %>%
    dplyr::select(
      deltaID,
      tidyselect::one_of(modSpp),
      tidyselect::one_of(modSppDeltas)
    )

  # group by Jenkins species group code

  jenkinsScores <- compute_jenkins_score(
    data = modelDat,
    idCol = "deltaID",
    jenkins = jenkins
  )

  jenkinsTypes <- names(jenkinsScores)[names(jenkinsScores) != "deltaID"]

  # group plots by jenkins scores using kmeans
  jenkinsClusters <- kmeans(jenkinsScores[, jenkinsTypes],
                            centers = length(jenkinsTypes)
  )
  jenkinsScores$jenkinsCluster <- factor(jenkinsClusters$cluster)

  jenkinsDeltas <- modelDat %>%
    dplyr::select(deltaID, tidyselect::contains("delta_spp")) %>%
    tidyr::pivot_longer(
      -deltaID,
      names_to = "spp",
      values_to = "delta"
    ) %>%
    dplyr::filter(!spp %in% modSppDeltas) %>% # remove species that are getting modeled already
    dplyr::mutate(stID = as.numeric(gsub("delta_spp_", "", spp))) %>%
    dplyr::left_join(jenkins) %>%
    dplyr::group_by(deltaID, jenkins_spgrpcd) %>%
    dplyr::summarize(delta = sum(delta, na.rm = TRUE)) %>%
    tidyr::pivot_wider(
      names_from = "jenkins_spgrpcd",
      values_from = "delta"
    ) %>%
    dplyr::rename_at(
      .vars = dplyr::vars(-deltaID),
      .funs = ~ paste0("delta_jenkins", .)
    )

  jenkinsNames <- names(jenkinsDeltas)[names(jenkinsDeltas) != "deltaID"]

  # we are going to model those species and the jenkins groups
  sppDeltaVars <- c(modSppDeltas, jenkinsNames)

  # DISTRO #
  modelDiams <- modelDat %>%
    dplyr::select(deltaID, tidyselect::contains("diam"), -tidyselect::contains("delta_")) %>%
    tidyr::pivot_longer(
      -deltaID,
      names_to = "diam",
      values_to = "score",
    ) %>%
    dplyr::mutate(diameter = as.numeric(gsub("diam_", "", diam))) %>%
    dplyr::mutate(nonZero = score > 0) %>%
    dplyr::group_by(diameter) %>%
    dplyr::summarize(nonZeroProp = sum(nonZero) / nrow(modelDat)) %>%
    dplyr::mutate(
      bin5 = diameter - diameter %% 5,
      bin10 = diameter - diameter %% 10
    )

  # individual diameter classes that can't be modeled
  # for now we will not model anything larger than 20"
  noModelClasses <- modelDiams %>%
    dplyr::filter(diameter > 20) %>%
    dplyr::pull(diameter)

  diamScores <- modelDat %>%
    dplyr::select(deltaID, tidyselect::contains("diam"), -tidyselect::contains("delta_"))

  diamDeltas <- modelDat %>%
    dplyr::select(deltaID, tidyselect::contains("delta_diam")) %>%
    tidyr::pivot_longer(
      -deltaID,
      names_to = "diam",
      values_to = "delta",
    ) %>%
    dplyr::mutate(diameter = as.numeric(gsub("delta_diam_", "", diam))) %>%
    dplyr::filter(!diameter %in% noModelClasses) %>%
    dplyr::group_by(deltaID, diam) %>%
    dplyr::summarize(delta = sum(delta, na.rm = TRUE)) %>%
    tidyr::pivot_wider(
      names_from = "diam",
      values_from = "delta"
    )

  diamDeltaModNames <- names(diamDeltas)[!grepl("deltaID", names(diamDeltas))]

  # assemble final training data object
  growthTraining <- modelDat %>%
    dplyr::transmute(
      fiaPlotID,
      deltaID,
      baph,
      tph,
      qmd = dplyr::case_when(
        tph > 0 ~ sqrt(baph / tph / 0.0000785),
        tph == 0 ~ 0
      ),
      delta_baph,
      delta_tph
    ) %>%
    dplyr::left_join(diamScores, by = "deltaID") %>%
    dplyr::left_join(diamDeltas, by = "deltaID") %>%
    dplyr::left_join(sppDat, by = "deltaID") %>%
    dplyr::left_join(jenkinsScores, by = "deltaID") %>%
    dplyr::left_join(jenkinsDeltas, by = "deltaID") %>%
    dplyr::left_join(climatePcaPreds, by = "fiaPlotID") %>%
    dplyr::filter(
      !is.na(climatePC1),
      !is.na(baph),
      !is.na(tph),
      !is.infinite(baph),
      !is.infinite(tph),
      !is.na(delta_baph),
      !is.na(delta_tph),
      !is.infinite(delta_baph),
      !is.infinite(delta_tph)
    )

  trainingCheck <- growthTraining %>%
    dplyr::select(
      deltaID,
      tidyselect::contains("tph"),
      tidyselect::contains("baph"),
      tidyselect::contains("spp"),
      tidyselect::contains("diam"),
      tidyselect::contains("jenkins"),
      tidyselect::contains("climate"),
      -tidyselect::contains("cluster")
    ) %>%
    tidyr::pivot_longer(
      -deltaID,
      names_to = "name",
      values_to = "value",
    ) %>%
    dplyr::filter(is.infinite(value) | is.na(value))

  if (nrow(trainingCheck) > 0) {
    stop(
      paste(
        "there are missing values in these plots:",
        unique(trainingCheck$deltaID),
        sep = "\n"
      )
    )
  }

  out <- list(
    growthTraining = growthTraining,
    jenkinsClusters = jenkinsClusters,
    jenkins = jenkins,
    climatePca = climatePca,
    plotLocs = plotLocs,
    geography_plot = geography_plot
  )

  return(out)

}


#' @title fit growth models from FIA delta vector training data
#' @description This function fits annualized growth models for baph, tph,
#'   species/species group importance, and diameter distribution importance
#' @param growthTraining dataframe containing initial conditions and deltas
#'   for each variable of interest, and a jenkinsCluster assignment for each
#'   plot
#' @return a list of model objects and some diagnostic figures
#' @author Henry Rodman
#' @import foreach
#' @export

fit_growth_models <- function(growthTraining) {
  modCores <- parallel::detectCores() - 2

  doMC::registerDoMC(modCores)

  ### SPECIES ###
  # define predictor variables
  sppPredVars <- c(
    "baph", "tph", "qmd",
    stringr::str_subset(names(growthTraining), "climate")
  )

  sppDeltaVars <- stringr::str_subset(
    names(growthTraining),
    "delta_spp|delta_jenkins"
  )

  # fit the initial model (for compiling)
  initSppMod <- fit_delta_mod(
    deltaVar = sppDeltaVars[1],
    data = growthTraining,
    predVars = sppPredVars,
    sppGroupVar = "jenkinsCluster",
    updateMod = NULL
  )

  sppDeltaMods <- foreach(i = sppDeltaVars) %dopar% {
    fit_delta_mod(
      deltaVar = i,
      data = growthTraining,
      predVars = sppPredVars,
      sppGroupVar = "jenkinsCluster",
      updateMod = initSppMod$mod
    )
  }

  names(sppDeltaMods) <- sppDeltaVars

  sppDiagnostics <- lapply(sppDeltaMods, evaluate_model_resid)

  ### DISTRO ###
  diamDeltaVars <- stringr::str_subset(
    names(growthTraining),
    "delta_diam"
  )

  diamPredVars <- c(
    "baph", "tph", "qmd",
    stringr::str_subset(names(growthTraining), "climate"),
    "diam_smaller", "diam_within2", "diam_larger"
  )

  # fit the initial model (for compiling)
  initDiamMod <- fit_delta_mod(
    deltaVar = diamDeltaVars[2],
    data = growthTraining,
    predVars = diamPredVars,
    sppGroupVar = "jenkinsCluster",
    updateMod = NULL
  )
  initDiamMod$mod

  diamDeltaMods <- foreach(i = diamDeltaVars) %dopar% {
    fit_delta_mod(
      deltaVar = i,
      data = growthTraining,
      predVars = diamPredVars,
      sppGroupVar = "jenkinsCluster",
      updateMod = initDiamMod$mod
    )
  }

  names(diamDeltaMods) <- diamDeltaVars

  diamDiagnostics <- lapply(diamDeltaMods, evaluate_model_resid)

  ### BAPH/TPH ###
  corePredVars <- c(
    "baph", "tph", "qmd",
    stringr::str_subset(names(growthTraining), "climate")
  )

  # fit the baph model
  baphDeltaMod <- fit_delta_mod(
    deltaVar = "delta_baph",
    data = growthTraining,
    predVars = corePredVars,
    sppGroupVar = "jenkinsCluster",
    updateMod = NULL,
    type = "linear"
  )

  baphDiagnostics <- evaluate_model_resid(baphDeltaMod)

  # fit tph model
  tphDeltaMod <- fit_delta_mod(
    deltaVar = "delta_tph",
    data = growthTraining,
    predVars = corePredVars,
    sppGroupVar = "jenkinsCluster",
    updateMod = baphDeltaMod$mod
  )

  tphDiagnostics <- evaluate_model_resid(tphDeltaMod)

  out <- list(
    baphDeltaMod = baphDeltaMod,
    tphDeltaMod = tphDeltaMod,
    diamDeltaMods = diamDeltaMods,
    sppDeltaMods = sppDeltaMods,
    growthTraining = growthTraining,
    diagnostics = list(
      sppDiagnostics = sppDiagnostics,
      diamDiagnostics = diamDiagnostics,
      baphDiagnostics = baphDiagnostics,
      tphDiagnostics = tphDiagnostics
    )
  )

  class(out) <- c("stgrowth", class(out))

  return(out)
}

#' @title compare grown to ungrown training data
#' @description This function compares mean baph, tph, and diameter distribution
#'   before and after growth predictions
#' @param trainingData dataframe containing ungrown observations for each plot
#' @param grownTrainingData dataframe containing ungrown observations for each
#'   plot
#' @param zeroPlots character vector of zeroTree plot_ids
#' @return a pander table and a diameter distribution plot
#' @author Henry Rodman
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export

compare_grown_training_data <- function(trainingData, grownTrainingData,
                                        zeroPlots) {

  growthComp <- list("original" = trainingData, "grown" = grownTrainingData) %>%
    dplyr::bind_rows(.id = "source") %>%
    dplyr::filter(!plot_id %in% zeroPlots) %>%
    dplyr::select(source, baph, tph, tidyselect::contains("diam_")) %>%
    mutate_at(
      .vars = dplyr::vars(tidyselect::contains("diam")),
      .funs = ~ . * tph
    ) %>%
    tidyr::pivot_longer(
      -source,
      names_to = "attr",
      values_to = "value",
    ) %>%
    dplyr::group_by(source, attr) %>%
    dplyr::summarize(
      population_mean = mean(value)
    )

  distroPlot <- growthComp %>%
    dplyr::filter(grepl("diam", attr)) %>%
    dplyr::mutate(diameter = as.numeric(gsub("diam_", "", attr))) %>%
    ggplot(aes(x = diameter, y = population_mean / 2.47, color = source)) +
    geom_line() +
    theme_bw() +
    labs(
      x = "DBH (in)",
      y = "trees per acre",
      title = "ungrown vs grown diameter distribution"
    )

  coreTab <- growthComp %>%
    dplyr::ungroup() %>%
    dplyr::filter(attr %in% c("baph", "tph")) %>%
    dplyr::mutate(
      population_mean = dplyr::case_when(
        attr == "baph" ~ convert_baph_to_bapa(population_mean),
        attr == "tph" ~ population_mean / 2.47
      ),
      attr = dplyr::case_when(
        attr == "baph" ~ "BAPA",
        attr == "tph" ~ "TPA",
        TRUE ~ attr
      )
    ) %>%
    dplyr::transmute(
      source = factor(source, levels = c("original", "grown")),
      attribute = attr,
      population_mean
    ) %>%
    tidyr::pivot_wider(
      names_from = "source",
      values_from = "population_mean"
    )

  coreTab %>%
    pander::pander()

  print(distroPlot)

  return(
    list(
      coreTab = coreTab,
      distroPlot = distroPlot
    )
  )
}
