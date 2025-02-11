data = dplyr::filter(.data = benchmarked_points,
                     # Figure out why these are necessary!!!!
                     !is.na(indicator),
                     !is.na(Condition_Category))
weights = sf::st_drop_geometry(x = dplyr::select(.data = sampled_points,
                                                 unique_id,
                                                 weight))
id_var = "unique_id"
cat_var = "Condition_Category"
wgt_var = "weight"
split_vars = c("indicator",
               "Management_Question")
definitions = dplyr::distinct(dplyr::select(.data = benchmarks,
                                            indicator = Indicator,
                                            Management_Question,
                                            Condition_Category))
conf = confidence
verbose = FALSE

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

