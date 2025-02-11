#### GOODMAN CIS ####
goodman_cis <- function(counts,
                        alpha = 0.2,
                        chisq = "best",
                        verbose = FALSE){
  if (!is.numeric(counts) | length(counts) < 2) {
    stop("counts must be a numeric vector with at least two values")
  }
  
  if (!(chisq %in% c("A", "B", "best"))) {
    stop("The only valid values for chisq are 'A', 'B', and 'best'.")
  }
  
  # Goodman describes the upper and lower bounds with the equations:
  # Lower estimated pi_i = {A + 2n_i - {A[A + 4n_i(N - n_i) / N]}^0.5} / [2(N + A)]
  # Upper estimated pi_i = {A + 2n_i + {A[A + 4n_i(N - n_i) / N]}^0.5} / [2(N + A)]
  
  # n_i is the "observed cell frequencies in population of size N" (aka count of observations) from a category
  # so that's the incoming argument counts. We'll rename for consistency with the original math (and statistics as a discipline)
  n <- counts
  #names <- names(n)
  #n <- as.vector(n)
 # names(n) <- names
    
  # N is the population those counts make up, or, in lay terms, the total observation count
  N <- sum(n)
  
  # k is the number of categories the population has been sorted into
  # Useful for degrees of freedom
  k <- length(counts)
  
  # "A is the upper alpha * 100-th percentage point of the chi-square distribution with k - 1 degrees of freedom"
  # and B is an alternative which uses alpha / k and one degree of freedom
  # Goodman states that B should be less than A for situations
  # where k > 2 AND alpha is 0.1, 0.05, or 0.01.
  chisq_quantiles <- c("A" = stats::qchisq(p = 1 - alpha,
                                           df = k - 1),
                       "B" = stats::qchisq(p = 1 - (alpha / k),
                                           df = 1))
  
  
  # According to Goodman, A and B are both valid options for the chi-square quantile
  # So the user can specify which they want or just ask for the one that minimizes the confidence intervals
  chisq_quantile <- switch(chisq,
                           "A" = {chisq_quantiles["A"]},
                           "B" = {chisq_quantiles["B"]},
                           "best" = {
                             pick <- which.min(chisq_quantiles)
                             if (verbose){
                               switch(names(chisq_quantiles)[pick],
                                      "A" = message("The chi-square quantile calculation that will provide the tighter confidence intervals is A, the upper alpha X 100-th percentage point of the chi-square distribution with k - 1 degrees of freedom"),
                                      "B" = message("The chi-square quantile calculation that will provide the tighter confidence intervals is B, the upper alpha / k X 100-th percentage point of the chi-square distribution with 1 degree of freedom"))
                             }
                             chisq_quantiles[pick]
                           })
  
  # Calculate the bounds!
  # Note that these ARE symmetrical, just not around the proportions.
  # They're symmetrical around A + 2 * n / (2 * (N + A))
  # The variable A has been replaced with chisq_quantile because it may be A or B, depending
  # Since the only multi-value vector involved here is n, these will be vectors of length k,
  # having one value for each of the values in chisq_quantile <- as.numeric(chisq_quantile)
  lower_bounds <- (chisq_quantile + 2 * n - sqrt(chisq_quantile * (chisq_quantile + 4 * n * (N - n) / N))) / (2 * (N + chisq_quantile))
  upper_bounds <- (chisq_quantile + 2 * n + sqrt(chisq_quantile * (chisq_quantile + 4 * n * (N - n) / N))) / (2 * (N + chisq_quantile))
  
  # A proportion can never be greater than 1 or less than 0 (duh)
  # So we'll add bounds any CIs in case that happens
  # That's definitely a thing that can happen if the magnitude of sqrt(A * (A + 4 * n * (N - n) / N))
  # is large enough
  lower_bounds[lower_bounds < 0] <- 0
  upper_bounds[upper_bounds > 1] <- 1
  
  # Build the output
  output <- data.frame(count = n,
                       proportion = n / N,
                       lower_bound = lower_bounds,
                       upper_bound = upper_bounds,
                       stringsAsFactors = FALSE,
                       row.names = NULL)
  
  # What are the categories called? If anything, that is
  k_names <- names(n)
  
  if (!is.null(k_names)) {
    output[["category"]] <- k_names
    output <- output[, c("category", "count", "proportion", "lower_bound", "upper_bound")]
  }
  
  return(output)
}

# wilson_cis

read_benchmarks <- function(filename = "Terrestrial_benchmarktool_NationalBenchmarks.xlsx",
                            filepath = "//blm.doi.net/dfs/loc/GBP/CA/CASO/projects/vegetation/AIM/GIS_data/Geodatabase/Design_Tracking/Eagle Lake",
                            sheet_name = "Monitoring Objectives",
                            eval_strings = list(c("x", "LL_Relation", "Lower_Limit"),
                                                c("x", "UL_Relation", "Upper_Limit"),
                                                c("x", "Proportion_Relation", "Required_Proportion"))){
  ## Check for the file extension
  file_extension <- toupper(tools::file_ext(filename))
  if (!(file_extension %in% c("CSV", "XLS", "XLSX"))) {
    stop("The benchmark filename needs to have a valid file extension (XLSX, CSV, XLS). The most likely extension is XLSX.")
  }
  
  # Make the full filepath
  if (is.null(filepath)) {
    filepath <- filename
  } else {
    filepath <- file.path(filepath,
                          filename)
  }
  
  # Check to see if it exists
  if (file.exists(filepath)) {
    if (file_extension == "CSV") {
      benchmarks_raw <- read.csv(filepath,
                                 stringsAsFactors = FALSE)
    } else {
      benchmarks_raw <- readxl::read_excel(path = filepath,
                                           sheet = sheet_name)
      # Check to make sure that there's not a weird row up top we need to skip (very possible with the way the workbooks get formatted)
      if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks_raw)))){
        benchmarks_raw <- readxl::read_excel(path = filepath,
                                             sheet = sheet_name,
                                             skip = 1)
      }
    }
    ## Change all the header names to use "_" instead of " " for consistency
    names(benchmarks_raw) <- stringr::str_replace_all(string = names(benchmarks_raw),
                                                      pattern = " ",
                                                      replacement = "_")
    
    if (!all(c("INDICATOR", "UNIT") %in% toupper(names(benchmarks_raw)))){
      stop("Can't find the expected column headers in the provided benchmark file. Check to make sure that there are no non-header rows before the headers.")
    }
  } else {
    stop(paste("Can't find the benchmark file at", filepath))
  }
  
  ## Strip out the extraneous columns and rows, which includes if they left the example in there. The pattern to look for is "e.g"
  benchmarks <- dplyr::select(.data = benchmarks_raw,
                              -tidyselect::matches(match = "__\\d+$")) |>
    dplyr::filter(.data = _,
                  !stringr::str_detect(string = Management_Question,
                                       pattern = "^[Ee].g."),
                  !is.na(Indicator))
  
  ## Create the evaluations strings if asked to!
  if (!is.null(eval_strings)) {
    # Figure out if any are missing (the character "x" is fine though because it's the indicator stand-in)
    varnames <- unlist(eval_strings)[unlist(eval_strings) != "x"]
    missing_varnames <- varnames[!(varnames %in% names(benchmarks))]
    if (length(missing_varnames) > 0) {
      stop(paste("The following expected variables for constructing eval strings are missing:", paste0(missing.varnames, collapse = ", ")))
    }
    # If none were missing, rename each of the vectors with "evalstring" and a suffix number. This will be their variable names in the data frame
    names(eval_strings) <- paste0("evalstring", 1:length(eval_strings))
    # Construct the strings. For each vector in the list
    strings <- lapply(X = eval_strings,
                      benchmarks = benchmarks,
                      FUN = function(X, benchmarks) {
                        # For each character string in the vector, grab the values in the matching variable in the benchmarks (or just return the "x")
                        vectors <- lapply(X = X,
                                          benchmarks = benchmarks,
                                          FUN = function(X, benchmarks) {
                                            if (X %in% names(benchmarks)) {
                                              return(benchmarks[[X]])
                                            } else {
                                              return(X)
                                            }
                                          })
                        # Paste them together. This gnarly eval(parse()) business is to do it regardless of how many components there are
                        output <- eval(parse(text = paste0("paste(", paste0("vectors[[", 1:length(vectors), "]]", collapse = ", "), ")")))
                        return(output)
                      })
    # Add the strings as variables to the benchmarks data frame
    benchmarks <- dplyr::bind_cols(benchmarks,
                                   data.frame(strings,
                                              stringsAsFactors = FALSE))
    
    names(benchmarks)[ncol(benchmarks)] <- "evalstring_threshhold"
  }
  
  return(benchmarks)
}

apply_benchmarks <- function(data,
                             benchmarks,
                             data_id_vars,
                             data_group_var = NULL,
                             # benchmark_idvars = NULL,
                             benchmark_group_var = "Benchmark_Group",
                             benchmark_indicator_var = "Indicator",
                             use_tdat_indicator_lookup = TRUE,
                             indicator_lookup_path = NULL) {
  #### Benchmark sanitization --------------------------------------------------
  if (!("data.frame" %in% class(benchmarks))) {
    stop("Benchmarks must be a data frame.")
  }
  
  required_benchmark_vars <- c(benchmark_indicator_var)
  missing_benchmark_vars <- setdiff(x = required_benchmark_vars,
                                    y = names(benchmarks))
  if (length(missing_benchmark_vars) > 0) {
    stop(paste0("The following variables are missing from benchmarks: ",
                paste(missing_benchmark_vars,
                      collapse = ", ")))
  }
  benchmarks <- dplyr::rename(.data = benchmarks,
                              indicator_name = tidyselect::matches(match = benchmark_indicator_var))
  
  # This needs to be reconsidered for data moving forward.
  if (use_tdat_indicator_lookup) {
    indicator_lut <- indicator.lookup(path = indicator_lookup_path,
                                      tdat_version = 2)
    names(indicator_lut) <- c("indicator_var", "indicator_name")
    benchmarks <- dplyr::left_join(x = benchmarks,
                                   y = indicator_lut,
                                   by = "indicator_name",
                                   relationship = "many-to-one") |>
      dplyr::mutate(.data = _,
                    indicator_var = dplyr::case_when(is.na(indicator_var) ~ indicator_name,
                                                     .default = indicator_var))
  } else {
    benchmarks <- dplyr::rename(.data = benchmarks,
                                indicator_var = indicator_name)
  }
  
  if (is.null(benchmark_group_var)) {
    benchmarks <- dplyr::mutate(.data = benchmarks,
                                benchmark_group = "All")
  } else {
    if (!(benchmark_group_var %in% names(benchmarks))) {
      stop(paste("The benchmark_group_var variable", benchmark_group_var, "does not appear in the benchmarks."))
    }
    benchmarks <- dplyr::rename(.data = benchmarks,
                                benchmark_group = tidyselect::matches(match = benchmark_group_var))
  }
  
  #### Data sanitization -------------------------------------------------------
  if (!("data.frame" %in% class(data))) {
    stop("data must either be a data frame or a spatial data frame.")
  }
  data <- sf::st_drop_geometry(x = data)
  
  required_data_vars <- c(data_id_vars, data_group_var)
  missing_data_vars <- setdiff(x = required_data_vars,
                               y = names(data))
  if (length(missing_data_vars) > 0) {
    stop(paste0("The following variables are missing from data: ",
                paste(missing_data_vars,
                      collapse = ", ")))
  }
  if (is.null(data_group_var)) {
    data <- dplyr::mutate(.data = data,
                          benchmark_group = "All")
  } else {
    data <- dplyr::rename(.data = data,
                          benchmark_group = tidyselect::matches(match = data_group_var))
  }
  
  #### Data checks -------------------------------------------------------------
  if (!any(data$benchmark_group %in% benchmarks$benchmark_group) | !any(benchmarks$benchmark_group %in% data$benchmark_group)) {
    stop("None of these data qualify for any of these benchmarks according to the benchmark groups. Double check your data_group_var and benchmark_group_var values.")
  }
  unrepresented_data_groups <- setdiff(x = unique(data$benchmark_group),
                                       y = unique(benchmarks$benchmark_group))
  if (length(unrepresented_data_groups) > 0) {
    warning("Some data have no applicable benchmarks and will not be evaluted.")
  }
  unrepresented_benchmark_groups <- setdiff(x = unique(benchmarks$benchmark_group),
                                            y = unique(data$benchmark_group))
  if (length(unrepresented_benchmark_groups) > 0) {
    warning("Some benchmarks have no applicable data and will not be evaluted.")
  }
  
  benchmarks <- dplyr::filter(.data = benchmarks,
                              benchmark_group %in% data$benchmark_group)
  
  # if (is.null(benchmark_idvars)) {
  #   benchmarks_list <- list(benchmarks)
  # } else {
  # benchmarks_list <- split(x = benchmarks,
  #                          f = benchmarks$Management_Question)
  # }
  
  data <- dplyr::filter(.data = data,
                        benchmark_group %in% benchmarks$benchmark_group)
  
  indicator_variables_present <- intersect(x = unique(benchmarks$indicator_var),
                                           y = names(data))
  if (length(indicator_variables_present) == 0) {
    stop("None of the indicators in the benchmarks appear as variables in the data. Double check use_tdat_indicator_lut, the indicator names in the benchmarks, and the variable names in the data.")
  } else if (length(indicator_variables_present) != length(unique(benchmarks$indicator_var))) {
    warning("Not all indicators in the benchmarks appear in the data.")
  }
  
  benchmarks <- dplyr::filter(.data = benchmarks,
                              indicator_var %in% indicator_variables_present)
  
  #### Actual benchmarking -----------------------------------------------------
  # Because sometimes the benchmarks are for different classes of values (e.g.,
  # some character and some numeric), we'll do these one at a time.
  results <- list()
  for (current_management_question in unique(benchmarks$Management_Question)) {
    current_benchmarks <- dplyr::filter(.data = benchmarks,
                                        Management_Question == current_management_question)
    for (current_indicator in unique(current_benchmarks$indicator_var)) {
      data_tall <- dplyr::select(.data = data,
                                 tidyselect::all_of(data_id_vars),
                                 benchmark_group,
                                 tidyselect::all_of(current_indicator)) |>
        tidyr::pivot_longer(data = _,
                            cols = tidyselect::all_of(current_indicator),
                            names_to = "indicator_var",
                            values_to = "value") |>
        dplyr::inner_join(x = _,
                          y = current_benchmarks,
                          relationship = "many-to-many",
                          by = c("benchmark_group",
                                 "indicator_var"))
      
      eval_vars <- names(current_benchmarks)[grepl(names(current_benchmarks), pattern = "^evalstring\\d+$")]
      
      benchmark_vector <- sapply(X = 1:nrow(data_tall),
                                 data_tall = data_tall,
                                 eval_vars = eval_vars,
                                 FUN = function(X, data_tall, eval_vars){
                                   all(sapply(X = eval_vars,
                                              current_data = data_tall[X, ],
                                              FUN = function(X, current_data){
                                                current_value <- current_data[["value"]]
                                                # This nonsense is so that strings
                                                # have '' around them so that this
                                                # evaluates correctly
                                                if (class(current_value) == "character") {
                                                  current_value <- paste0("'", current_value, "'")
                                                  evalstring_vector <- stringr::str_split_1(string = current_data[[X]][1],
                                                                                            pattern = "[ ]")
                                                  target_value_index <- setdiff(x = c(1, 3),
                                                                                y = which(evalstring_vector == "x"))
                                                  evalstring_vector[target_value_index] <- paste0("'", evalstring_vector[target_value_index], "'")
                                                  evalstring <- paste(evalstring_vector,
                                                                      collapse = " ")
                                                } else {
                                                  evalstring <- current_data[[X]][1]
                                                }
                                                evalstring <- gsub(evalstring,
                                                                   pattern = "(x){1}",
                                                                   replacement = current_value)
                                                eval(parse(text = evalstring))
                                              }))
                                 })
      
      results[[paste0(current_management_question,
                      "_",
                      current_indicator)]] <- dplyr::select(.data = data_tall,
                                                            tidyselect::all_of(data_id_vars),
                                                            indicator = indicator_var,
                                                            Condition_Category)[benchmark_vector, ] |>
        dplyr::mutate(.data = _,
                      Management_Question = current_management_question)
    }
  }
  dplyr::bind_rows(results) |>
    # dplyr::filter(.data = _,
    #               # Figure out why these are necessary!!!!
    #               !is.na(indicator),
    #               !is.na(Condition_Category))
    dplyr::distinct()
}

indicator.lookup <- function(path = NULL,
                             tdat_version = "2"){
  if (is.null(path)) {
    read.csv(paste0(path.package("aim.analysis"),
                    "/defaults/indicator_lut",
                    tdat_version,
                    ".csv"),
             stringsAsFactors = FALSE)[,c(1,2)]
  } else {
    read.csv(paste0(path),
             stringsAsFactors = FALSE)[,c(1,2)]
  }
}

analyze_cat <- function(data,
                        weights,
                        id_var,
                        cat_var,
                        split_vars,
                        wgt_var,
                        definitions = NULL,
                        conf = 80,
                        verbose = FALSE){
  # Make sure everything is the right class/length
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data frame")
  }
  if (nrow(data) < 1) {
    stop("There are no values in data")
  }
  if (!("data.frame" %in% class(weights))) {
    stop("weights must be a data frame")
  }
  if (nrow(weights) < 1) {
    stop("There are no values in weights")
  }
  
  if (class(id_var) != "character" | length(id_var) != 1) {
    stop("id_var must be a single character string")
  }
  if (class(cat_var) != "character" | length(cat_var) != 1) {
    stop("cat_var must be a single character string")
  }
  if (class(wgt_var) != "character" | length(wgt_var) != 1) {
    stop("wgt_var must be a single character string")
  }
  if (conf <= 0 | conf >= 100) {
    stop("conf must be a value between 0 and 100")
  }
  
  
  # Make sure all the variables are in place
  required_data_vars <- c(id_var,
                          cat_var,
                          split_vars)
  missing_data_vars <- required_data_vars[!(required_data_vars %in% names(data))]
  if (length(missing_data_vars) > 0) {
    stop("The following variables are missing from data: ",
         paste(missing_data_vars,
               collapse = , ", "))
  }
  
  # Just want the bare minimum here.
  data <- dplyr::select(.data = data,
                        tidyselect::all_of(required_data_vars))
  
  # Check to make sure the unique identifiers are, in fact, unique
  non_unique_ids <- any(table(data[[id_var]]) > 1)
  if (non_unique_ids) {
    stop("There are non-unique values in ", id_var, " in data.")
  }
  
  required_weights_vars <- c(id_var,
                             wgt_var)
  missing_weights_vars <- required_weights_vars[!(required_weights_vars %in% names(weights))]
  if (length(missing_weights_vars) > 0) {
    stop("The following variables are missing from weights: ",
         paste(missing_weights_vars,
               collapse = , ", "))
  }
  
  non_unique_ids <- any(table(weights[[id_var]]) > 1)
  if (non_unique_ids) {
    stop("There are non-unique values in ", id_var, " in weights.")
  }
  
  # Paring this down too.
  weights <- dplyr::select(.data = weights,
                           tidyselect::all_of(id_var),
                           weight = tidyselect::matches(match = paste0("^", wgt_var, "$")))
  
  # What categories were observed?
  present_categories <- unique(data[[cat_var]])
  
  # And what if the user provided definitions?
  # This is important for if there are categories that have no data that qualified!
  if (!is.null(definitions)) {
    if (!(class(data[[cat_var]]) %in% class(definitions))) {
      stop("definitions must be the same class as the category values in data")
    }
    if (length(definitions) < 1) {
      stop("There are no values in definitions")
    }
    missing_categories <- !(present_categories %in% definitions)
    if (any(missing_categories)) {
      stop("The following categories appear in data but not in categories: ",
           paste(present_categories[missing_categories], collapse = ", "))
    }
  }
  
  # Make sure the IDs line up
  data_ids_in_weights_indices <- data[[id_var]] %in% weights[[id_var]]
  if (!all(data_ids_in_weights_indices)) {
    stop("Not all unique IDs in data appear in weights")
  }
  weight_ids_in_data_indices <- weights[[id_var]] %in% data[[id_var]]
  if (verbose & !all(weight_ids_in_data_indices)) {
    message("Not all unique IDs in weights appear in data, just so you know.")
  }
  weights <- weights[weight_ids_in_data_indices, ]
  
  
  # Get each observation with just its category and weight
  weighted_categories <- dplyr::left_join(x = dplyr::select(.data = data,
                                                            tidyselect::all_of(c(id_var,
                                                                                 cat_var))),
                                          y = weights,
                                          by = id_var,
                                          relationship = "one-to-one")
  
  # Calculate the sum of the weights for each of the observed categories
  category_weight_summary <- dplyr::summarize(.data = weighted_categories,
                                              .by = tidyselect::matches(match = cat_var),
                                              observation_count = dplyr::n(),
                                              total_observation_weight = sum(weight)) |>
    dplyr::rename(.data = _,
                  setNames(object = cat_var,
                           nm = "category")) |>
    dplyr::mutate(.data = _,
                  observation_weighted_proportion = total_observation_weight / sum(weighted_categories$weight),
                  observation_proportion = observation_count / nrow(weighted_categories),
                  adjusted_count = observation_count * observation_weighted_proportion)
  
  # Okay, so if we have definitions to catch categories with zero observations, add those
  # Because it should matter for calculating confidence intervals
  if (!is.null(definitions)) {
    missing_categories <- definitions[!(definitions %in% present_categories)]
    
    if (length(missing_categories) > 0) {
      empty_category_weight_summary <- lapply(X = missing_categories,
                                              FUN = function(X){
                                                data.frame(category = X,
                                                           count = 0,
                                                           weight_sum = 0,
                                                           weighted_proportion = 0,
                                                           adjusted_count = 0)
                                              }) |>
        dplyr::bind_rows()
      
      category_weight_summary <- dplyr::bind_rows(category_weight_summary,
                                                  empty_category_weight_summary)
    }
  }
  
  # # Calculate the weighted proportions for each category
  # category_weighted_proportions <- category_weight_sums / sum(category_weight_sums)
  # # Get the pure counts of the categories
  # category_counts <- table(weighted_categories[[cat_var]])
  # # And the total number of observations. This should be the same as nrow(weighted_categories)
  # total_observations <- sum(category_counts)
  # # Using the total number of observations and the weighted proportions to calculate "adjusted counts"
  # adjusted_counts <- category_weighted_proportions * total_observations
  #
  # # Okay, so if we have definitions to catch categories with zero observations, add those
  # # Because it should matter for calculating confidence intervals
  # if (!is.null(definitions)) {
  #   defined_categories <- definitions[[cat_var]]
  #   missing_categories <- defined_categories[!(defined_categories %in% present_categories)]
  #   # Looping because it's easy, not because it's the best solution
  #   # But we want to populate the 0s for all of these!
  #   for (category in missing_categories) {
  #     category_weighted_proportions[[category]] <- 0
  #     category_weight_sums[[category]] <- 0
  #     category_counts[[category]] <- 0
  #     adjusted_counts[[category]] <- 0
  #   }
  # }
  
  # Finally ready to calculate confidence intervals!
  # But first we need the alpha value for our confidence level
  alpha <- 1 - (conf / 100)
  
  confidence_intervals <- goodman_cis(counts = setNames(object = category_weight_summary[["adjusted_count"]],
                                                        nm = category_weight_summary[["category"]]),
                                      alpha = alpha,
                                      chisq = "best",
                                      verbose = verbose)
  confidence_interval_vars <- c("category",
                                "weighted_observation_count",
                                "weighted_observation_proportion",
                                "weighted_observation_proportion_lower_bound",
                                "weighted_observation_proportion_upper_bound")
  names(confidence_intervals) <- confidence_interval_vars
  
  # Combine the results and confidence intervals
  output <- dplyr::left_join(x = category_weight_summary,
                             y = dplyr::select(.data = confidence_intervals,
                                               category,
                                               weighted_observation_proportion_lower_bound,
                                               weighted_observation_proportion_upper_bound),
                             by = "category")
  
  # Get the variables restricted to what we care about and ordered properly
  dplyr::select(.data = output,
                category,
                observation_count,
                observation_proportion,
                total_observation_weight,
                weighted_observation_proportion = observation_weighted_proportion,
                weighted_observation_proportion_lower_bound,
                weighted_observation_proportion_upper_bound)
}

analyze_cat_multi <- function(data,
                              weights,
                              id_var,
                              cat_var,
                              wgt_var,
                              split_vars = NULL,
                              definitions = NULL,
                              conf = 80,
                              verbose = FALSE){
  # Make sure everything is the right class/length
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data frame")
  }
  if (nrow(data) < 1) {
    stop("There are no values in data")
  }
  if (!("data.frame" %in% class(weights))) {
    stop("weights must be a data frame")
  }
  if (nrow(weights) < 1) {
    stop("There are no values in weights")
  }
  if (!is.null(definitions)) {
    if (!("data.frame" %in% class(definitions))) {
      stop("definitions must be a data frame")
    }
    if (nrow(definitions) < 1) {
      stop("There are no values in definitions")
    }
  }
  
  if (class(id_var) != "character" | length(id_var) != 1) {
    stop("id_var must be a single character string")
  }
  if (class(cat_var) != "character" | length(cat_var) != 1) {
    stop("cat_var must be a single character string")
  }
  if (class(wgt_var) != "character" | length(wgt_var) != 1) {
    stop("wgt_var must be a single character string")
  }
  if (conf <= 0 | conf >= 100) {
    stop("conf must be a value between 0 and 100")
  }
  
  if (!is.null(split_vars)) {
    if (class(split_vars) != "character" | length(split_vars) < 1) {
      stop("split_vars must be a vector of one or more character strings")
    }
  }
  
  # Make sure all the variables are in place
  required_data_vars <- c(id_var, cat_var, split_vars)
  missing_data_vars <- required_data_vars[!(required_data_vars %in% names(data))]
  if (length(missing_data_vars) > 0) {
    stop("The following variables are missing from data: ", paste(missing_data_vars, collapse = , ", "))
  }
  data <- data[, required_data_vars]
  # Split if necessary!
  if (is.null(split_vars)) {
    data_list <- list("only" = data)
  } else {
    data_list <- split(x = data,
                       f = data[, split_vars],
                       drop = TRUE)
  }
  # This will either be "only" if there are no split variables
  # or all the unique combinations that occur
  list_names <- names(data_list)
  # Check to make sure the unique identifiers are, in fact, unique
  non_unique_ids <- sapply(X = data_list,
                           id_var = id_var,
                           FUN = function(X, id_var){
                             counts <- table(X[[id_var]])
                             any(counts > 1)
                           })
  non_unique_ids_subsets <- list_names[non_unique_ids]
  if (any(non_unique_ids)) {
    if (is.null(split_vars)) {
      stop("There are non-unique values in ", id_var, " in data. Did you intend to subset your data with split_vars?")
    } else {
      stop("There are non-unique values in ", id_var, " in data the following unique combinations of values in ",
           paste(split_vars, collapse = ", "), ": ",
           paste(non_unique_ids_subsets, collapse = ", "))
    }
  }
  
  required_weights_vars <- c(id_var, wgt_var)
  missing_weights_vars <- required_weights_vars[!(required_weights_vars %in% names(weights))]
  if (length(missing_weights_vars) > 0) {
    stop("The following variables are missing from weights: ", paste(missing_weights_vars, collapse = , ", "))
  }
  non_unique_ids <- any(table(weights[[id_var]]) > 1)
  if (non_unique_ids) {
    stop("There are non-unique values in ", id_var, " in weights.")
  }
  weights <- weights[, required_weights_vars]
  
  # And what if the user provided definitions?
  # This is important for if there are categories that have no data that qualified!
  if (is.null(definitions)) {
    definitions_list <- list("only" = NULL)
  } else {
    required_definitions_vars <- c(cat_var, split_vars)
    missing_definitions_vars <- required_definitions_vars[!(required_definitions_vars %in% names(definitions))]
    if (length(missing_definitions_vars) > 0) {
      stop("The following variables are missing from definitions: ", paste(missing_definitions_vars, collapse = , ", "))
    }
    definitions <- definitions[, required_definitions_vars]
    
    if (is.null(split_vars)) {
      # Check for missing categories
      missing_categories <- data[[cat_var]][!(data[[cat_var]] %in% definitions[[cat_var]])]
      if (length(missing_categories) > 0) {
        stop("The following categories appear in data but not definitions: ",
             paste(missing_categories, collapse = ", "))
      }
      # We won't be splitting, but we will be putting it in a list for ease, I guess
      definitions_list <- list("only" = definitions)
    } else {
      # Make sure that the split values line up!
      # It's important that all the values from data appear in definitions
      # but not necessarily the other way around
      data_splitvars_in_def <- sapply(X = split_vars,
                                      data = data,
                                      definitions = definitions,
                                      FUN = function(X, data, definitions){
                                        all(data[[X]] %in% definitions[[X]])
                                      },
                                      USE.NAMES = TRUE)
      if (!all(data_splitvars_in_def)) {
        splitvars_missing_values <- names(data_splitvars_in_def)[!data_splitvars_in_def]
        stop("data has values in the following variables which do not occur in the same variables in definitions: ",
             paste(splitvars_missing_values, collapse = ", "))
      }
      def_splitvars_in_data <- sapply(X = split_vars,
                                      data = data,
                                      definitions = definitions,
                                      FUN = function(X, data, definitions){
                                        all(definitions[[X]] %in% data[[X]])
                                      },
                                      USE.NAMES = TRUE)
      if (verbose & !all(def_splitvars_in_data)) {
        splitvars_missing_values <- names(def_splitvars_in_data)[!def_splitvars_in_data]
        message("Just so you know, definitions has values in the following variables which do not occur in the same variables in data: ",
                paste(splitvars_missing_values, collapse = ", "))
      }
      # At this point, we know that it's safe to split definitions
      definitions_list <- split(definitions, definitions[, split_vars],
                                drop = TRUE)
      # And to restrict them (which also orders things for us later)
      definitions_list <- definitions_list[list_names]
    }
    
    
    # Okay, so do all of the categories from data show up in definitions?
    # This has to be yes!
    # But it's fine (and in fact the whole point) if the other way around isn't true
    # so I'm not even bothering to test that
    missing_categories <- sapply(X = list_names,
                                 data_list = data_list,
                                 definitions_list = definitions_list,
                                 cat_var = cat_var,
                                 FUN = function(X, data_list, definitions_list, cat_var){
                                   current_data <- data_list[[X]]
                                   data_cats <- current_data[[cat_var]]
                                   current_definitions <- definitions_list[[X]]
                                   def_cats <- current_definitions[[cat_var]]
                                   !all(data_cats %in% def_cats)
                                 },
                                 USE.NAMES = TRUE)
    if (any(missing_categories)) {
      stop("For the following unique combinations of values in ",
           paste(split_vars, collapse = ", "),
           "there are categories which occur in data but not in definitions: ",
           paste(names(missing_categories)[missing_categories], collapse = ", "))
    }
  }
  
  # Make sure the IDs line up
  data_ids_in_weights_indices <- data[[id_var]] %in% weights[[id_var]]
  if (!all(data_ids_in_weights_indices)) {
    stop("Not all unique IDs in data appear in weights")
  }
  weight_ids_in_weights_indices <- weights[[id_var]] %in% data[[id_var]]
  if (verbose & !all(weight_ids_in_weights_indices)) {
    message("Not all unique IDs in weights appear in data, just so you know.")
  }
  weights <- weights[weight_ids_in_weights_indices, ]
  
  
  # And now, finally, we can do the calculations!
  results_list <- lapply(X = list_names,
                         data_list = data_list,
                         definitions_list = definitions_list,
                         weights = weights,
                         id_var = id_var,
                         cat_var = cat_var,
                         wgt_var = wgt_var,
                         split_vars = split_vars,
                         conf = conf,
                         verbose = verbose,
                         FUN = function(X,
                                        data_list,
                                        definitions_list,
                                        weights,
                                        id_var,
                                        cat_var,
                                        wgt_var,
                                        split_vars,
                                        conf,
                                        verbose){
                           # Get the data frame for this subset
                           data <- data_list[[X]]
                           definitions <- definitions_list[[X]][[cat_var]]
                           
                           results <- analyze_cat(data = data,
                                                  weights = weights,
                                                  id_var = id_var,
                                                  cat_var = cat_var,
                                                  wgt_var = wgt_var,
                                                  split_vars = split_vars,
                                                  definitions = definitions,
                                                  conf = conf,
                                                  verbose = verbose)
                           
                           # Add in the splitting vars if there are any
                           # I refuse to be ashamed of looping here
                           for (var in split_vars) {
                             var_value <- data[[var]][1]
                             results[[var]] <- var_value
                           }
                           
                           return(results)
                         })
  
  # OKAY. So all those are analyzed and stuff. Time to combine everything into a
  # single output and return it.
  dplyr::bind_rows(results_list)
}

