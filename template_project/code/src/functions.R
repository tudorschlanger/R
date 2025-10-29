#'
#' FUNCTIONS 
#'
#'

# xtile like in Stata (thanks to Matthie Gomez)
xtile <- function(.data, nquantiles = 10) {
  .bincode(.data, 
           breaks = quantile(.data, seq(0, 1, length.out = nquantiles + 1), type = 2, na.rm = TRUE),
           include.lowest = TRUE)
}

# tab 
# tab <- function(.data, var) {
#   .data %>% 
#     filter(!is.na({{var}})) %>% 
#     group_by({{var}}) %>% 
#     summarise(n = n()) %>% 
#     mutate(
#       totalN = cumsum(n),
#       percent = round((n/sum(n)), 2),
#       cumuPer = round(cumsum(freq = n /sum(n)), 2)
#     )
# }
tab <- function(df, vars = "") {
  #--------------------------------------------------------
  # Frequency table generator (1-way or 2-way)
  #--------------------------------------------------------
  # Works like Stata's 'tab' command:
  #   - tab(var1)  ->  one-way frequency + share
  #   - tab(var1, var2) ->  cross-tab with shares
  
  # If vars not specified, use all variables in df
  if (length(vars) == 1 && vars == "") {
    vars <- names(df)
  }
  
  # Prevent conflict with variable name "n" (used internally by dplyr::count)
  if ("n" %in% vars) stop("Choose other variable names than n.")
  
  #--------------------------------------------------------
  # 1-WAY TABULATION
  #--------------------------------------------------------
  if (length(vars) == 1) {
    var <- sym(vars[1])
    cat("\nDistribution of ", vars[1], " values:\n")
    
    df %>%
      count(!!var) %>%                                 # count occurrences
      mutate(share = round(n / sum(n), 2))             # compute share (%)
    
    #--------------------------------------------------------
    # 2-WAY TABULATION
    #--------------------------------------------------------
  } else if (length(vars) == 2) {
    var1 <- sym(vars[1])
    var2 <- sym(vars[2])
    cat("\nShare of ", vars[2], " by ", vars[1], ":\n")
    
    df %>%
      drop_na(!!var1, !!var2) %>%                      # drop missing values
      count(!!var1, !!var2) %>%                        # count joint frequencies
      group_by(!!var1) %>%
      mutate(share = n / sum(n)) %>%                   # within-group shares
      pivot_wider(                                     # reshape to wide
        id_cols = !!var1,
        values_from = share,
        names_from = !!var2,
        names_prefix = paste0(vars[2], " = ")
      ) %>%
      ungroup()
    
    #--------------------------------------------------------
    # INVALID INPUT
    #--------------------------------------------------------
  } else {
    stop("Length of vars argument <= 2")
  }
}



share_lost <- function(df_old, df_new, id, threshold = NULL) {
  n_ssn <- length(unique(df_old[[id]]))
  n_ssn_new <- length(unique(df_new[[id]]))
  share <- 1 - n_ssn_new / n_ssn
  cat(sprintf("\n Share of SSN lost = %.3f", share))

  if (!is.null(threshold)) {
    stopifnot(share < threshold)
  }
}


first_nonmiss <- function(x) {
  i <- which(!is.na(x))[1L]
  if (is.na(i)) vctrs::vec_cast(NA, x) else x[i]
}



mdesc <- function(df, vars = "") {
  #--------------------------------------------------------
  # Missing data summary for all or selected variables
  #--------------------------------------------------------
  
  # If vars not specified, default to all columns in df
  if (length(vars) == 1 && vars == "") {
    vars <- names(df)
  }
  
  #--------------------------------------------------------
  # Compute missing values for each variable
  #--------------------------------------------------------
  df %>%
    summarise(across(all_of(vars),
                     ~sum(if (is.character(.)) is.na(.) | . == "" else is.na(.))
    )) %>%
    
    # Add total number of rows
    mutate(total = n()) %>%
    
    # Reshape from wide to long format
    pivot_longer(cols = all_of(vars),
                 names_to = "variables",
                 values_to = "missing") %>%
    
    # Compute share of missing values (rounded to 2 decimals)
    mutate(share = round(missing / total, digits = 2)) %>%
    
    # Reorder columns for readability
    relocate(variables, missing, total, share)
}


# tlag 
tlag <- function(x, n = 1L, time, default = NA) {
  if (!is.numeric(n) | length(n) > 1) stop("n must be a numeric of length one")
  if (dplyr::n_distinct(time) < length(time)) stop()
  
  index <- match(time - n, time, incomparables = NA)
  
  out <- x[index]
  if(!is.na(default)) out[which(is.na(index))] <- default 
  attributes(out) <- attributes(x)
  out
}


# ISID 
isid <- function(df, vars, verbose = TRUE, ignore_error = FALSE) {
  #--------------------------------------------------------
  # Check whether the given variables uniquely identify rows
  # Similar to Stata's `isid` command
  #--------------------------------------------------------
  
  setDT(df)  # ensure df is a data.table
  
  # Count number of unique rows for the provided ID variables
  unique <- nrow(df[!duplicated(df[, ..vars]), ])
  total  <- nrow(df)
  
  vars_concat <- paste(vars, collapse = ", ")
  
  #--------------------------------------------------------
  # Verbose output (if enabled)
  #--------------------------------------------------------
  if (verbose == TRUE) {
    cat(paste0("Do variables ", vars_concat, " uniquely identify the data?\n"))
    print(unique == total)
    cat("Variables define this many unique rows:\n")
    print(unique)
    cat("There are this many total rows in the data:\n")
    print(total)
  }
  
  #--------------------------------------------------------
  # If they do not uniquely identify, show or stop with message
  #--------------------------------------------------------
  vars_pasted <- paste0(vars, collapse = " ")
  message <- paste0("Variable(s) ", vars_pasted, " do not uniquely identify the df.")
  
  if (unique != total) {
    if (ignore_error == FALSE) {
      stop(message)
    }
  }
}

# Save plots 
save_plots <- function(plot, filename, path, width = NA, height = NA, units = "cm") {
  
  extensions <- c(".png")
  
  for (extension in extensions) {
    filename <- paste0(filename, extension)
    ggsave(plot = plot, 
           filename = filename, 
           path = path, 
           dpi = 300,
           width = width,
           height = height, 
           units = units
    )
    message(paste0("Saved ", filename, " to ", path, " folder."))
    
  }
  
}

# Winsorize (based on Matthieu Gomez's statar)
winsorize <- function(x, cutpoints, verbose = TRUE) {
  
  argname_x <- sys.call()[2]
  
  dummy = is.integer(x)
  stopifnot(length(cutpoints) == 2)
  cutpoints <- stats::quantile(x, cutpoints, type = 1, na.rm = TRUE)
  
  if (is.integer(x)) cutpoints <- round(cutpoints)
  
  bottom <- x < cutpoints[1]
  top    <- x > cutpoints[2]
  
  if (verbose) {
    length <- length(x)
    message(paste0("Winsorizing variable: ", argname_x, "..."))
    message(paste0( sprintf("%3.2f", 100*sum(bottom, na.rm = TRUE) / length),
                    "% observations replaced at the bottom"))
    message(paste0( sprintf("%3.2f", 100*sum(top, na.rm = TRUE) / length),
                    "% observations replaced at the top"))
  }
  
  x[bottom] <- cutpoints[1]
  x[top] <- cutpoints[2]
  
  if (dummy) {
    x <- as.integer(x)
  }
  
  x
  
}

share_lost <- function(df_old, df_new, id, threshold = NULL) {
  n_ssn <- length(unique(df_old[[id]]))
  n_ssn_new <- length(unique(df_new[[id]]))
  share <- 1 - n_ssn_new / n_ssn
  cat(sprintf("\n Share of SSN lost = %.3f", share))
  
  if (!is.null(threshold)) {
    stopifnot(share < threshold)
  }
}

first_nonmiss <- function(x) {
  i <- which(!is.na(x))[1L]
  if (is.na(i)) vctrs::vec_cast(NA, x) else x[i]
}

classes <- function(df) {
  data.frame(
    variable = names(df),
    class = sapply(df, function(x) paste(class(x), collapse = ", "))
  )
}


make_wide <- function(df, id_vars, wide_vars, value_vars = NULL) {
  
  # Check that the combination of id_vars and wide_vars uniquely identifies rows
  isid(df, c(id_vars, wide_vars), verbose = FALSE)
  
  # Convert to data.table if not already
  setDT(df)
  
  # Build formula components for dcast
  formula_lhs <- paste(id_vars, collapse = " + ")     # left-hand side (identifiers)
  formula_rhs <- paste(wide_vars, collapse = " + ")   # right-hand side (variables to spread)
  
  # Create the reshaping formula (e.g., id1 + id2 ~ year + variable)
  dcast_formula <- as.formula(paste0(formula_lhs, " ~ ", formula_rhs))
  
  # Determine which columns should be used as value variables
  if (is.null(value_vars)) {
    # If not provided, use all columns except id_vars and wide_vars
    value.var <- setdiff(names(df), c(id_vars, wide_vars))
  } else {
    value.var <- value_vars
  }
  
  cat("\n Reshaping wide...\n")
  
  # Perform the wide transformation
  df_wide <- dcast(
    df,
    dcast_formula,
    value.var = value.var
  )
  
  # Return the reshaped data.table
  return(df_wide)
}

sumup <- function(df, vars = "") {
  #--------------------------------------------------
  # Summary statistics generator for numeric columns
  #--------------------------------------------------
  # If vars not specified, automatically detect numeric columns
  setDT(df)
  if (length(vars) == 1 && vars == "") {
    vars <- names(df)
  }
  
  # Keep only numeric variables from the provided vars
  vars_num <- vars[sapply(df[, ..vars], is.numeric)]
  
  #--------------------------------------------
  # Compute summary statistics for each variable
  #--------------------------------------------
  summary_stats <- df %>%
    summarise(across(all_of(vars_num),
                     list(
                       N    = ~sum(!is.na(.x)),                      # Number of non-missing obs
                       Mean = ~mean(.x, na.rm = TRUE),               # Mean
                       SD   = ~sd(.x, na.rm = TRUE),                 # Standard deviation
                       Min  = ~min(.x, na.rm = TRUE),                # Minimum
                       p10  = ~quantile(.x, probs = 0.1, na.rm = TRUE),  # 10th percentile
                       p50  = ~quantile(.x, probs = 0.5, na.rm = TRUE),  # Median
                       p90  = ~quantile(.x, probs = 0.9, na.rm = TRUE),  # 90th percentile
                       Max  = ~max(.x, na.rm = TRUE)                 # Maximum
                     ),
                     .names = "{.col}--{.fn}"                        # Custom name pattern
    )) %>%
    
    #--------------------------------------------
  # Reshape from wide → long → tidy wide format
  #--------------------------------------------
  pivot_longer(everything(),
               names_to = c("variable", "statistic"),
               names_sep = "--") %>%
    
    pivot_wider(names_from = statistic,
                values_from = value) %>%
    
    # Round numeric results to 3 decimals
    mutate(across(where(is.numeric), ~round(.x, 3)))
  
  # Clean up column names
  colnames(summary_stats) <- c("variable", colnames(summary_stats)[2:ncol(summary_stats)])
  
  return(summary_stats)
}



# 
# 
# # Define a simple function, to collapse the data using weighted mean, weighted sd and count 
# 
# collapse_df <- function(.data, vars, group, weight, bounds = FALSE) { 
#   
#   vars_all <- unique(c(vars, group, weight))
# 
#   df <- .data %>%
#     select({{vars_all}})  %>%
#     group_by(across({{group}}))
# 
#   
#   df_mean <- df %>%
#     summarise(across({{vars}},
#               ~Hmisc::wtd.mean(., weights = df[[weight]], na.rm = TRUE), 
#               .names = "{.col}_mean"),
#                .groups = "drop")
#   
#   df_sd <- df %>%
#     summarise(across({{vars}},
#               ~ sqrt(Hmisc::wtd.var(., weights = df[[weight]], na.rm = TRUE)),
#                    .names = "{.col}_sd"),
#               .groups = "drop")
#   
#   df_n <- df %>%
#     summarise (across({{vars}},
#                       ~ n(),
#                       .names ="{.col}_n"),
#                       .groups ="drop")
#   
#   df_out <- df_mean %>% 
#     left_join(df_sd, by = group) %>% 
#     left_join(df_n,  by = group)
#  
#   # Inherit labels 
#   for (var in vars) {
#     if (!is.null(var_label(df[[var]]))) {
#       label_mean <- var_lbale(df[[var]]) 
#       label_sd   <- paste0("SD ", var_label(df[[var]]))
#       varname_mean <- paste0(var, "_mean")
#       varname_sd <- paste0(var, "_sd")
#       df_out <- df_out %>% 
#         set_variable_labels(
#           {{varname_mean}} := label_mean,
#           {{varname_sd}}   := label_sd
#         )
#     }
#   }
#   
#   # Calculate the lower and upper bounds of confidence interval assuming 2-sided 95%
#   if (bounds == TRUE) {
#     tstat <- 1.96 
#     for (var in vars) {
#      var_lower <- paste0(var, "_mean_lower")
#      var_upper <- paste0(var, "_mean_upper")
#      var_mean <- paste0(var, "_mean")
#      var_sd <- paste0(var, "_sd")
#      var_n <- paste0(var, "_n")
#      
#      df_out <- df_out %>% 
#        mutate(
#         {{var_lower}} := df_out[[var_mean]] - df_out[[var_sd]] / sqrt(df_out[[var_n]]) * tstat,
#         {{var_upper}} := df_out[[var_mean]] + df_out[[var_sd]] / sqrt(df_out[[var_n]]) * tstat
#         )
#     }
#   }
#   
#   # Return data frame 
#   df_out
#   
# }
