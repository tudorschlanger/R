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
tab <- function(.data, var) {
  .data %>%
    filter(!is.na({{var}})) %>% 
    group_by({{var}}) %>% 
    summarise(n = n()) %>% 
    mutate(
      totalN = cumsum(n),
      percent = round((n/sum(n)), 2), 
      cumuPer = round(cumsum(freq=n /sum(n)), 2)
    )
}
    
# mdesc
mdesc <- function(.data, vars, group_var) {
  .data %>% 
    group_by({{group_var}}) %>% 
    summarise(across({{vars}}, ~ sum(is.na(.))), total = n()) %>%
    pivot_longer(cols = {{vars}}, names_to = "variables", values_to = "missing") %>% 
    mutate(pct = round(missing / total, digits = 2)) %>% 
    relocate({{group_var}}, variables, missing, total, pct)
  
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
isid <- function (data, vars, verbose = FALSE, ignore_error = FALSE) {
  unique <- nrow(data[!duplicated(data[, vars]), ])
  total <- nrow(data)
  
  if (verbose == FALSE) {
    cat("Are variables a unique ID?\n")
    print(unique == total)
    cat("Variables define this many unique rows:\n")
    print(unique)
    cat("There are this many total rows in the data:\n")
    print(total)
  }
  vars_pasted = paste0(vars, collapse = " ")
  message <- paste0("Variable(s) ", vars_pasted, " do not uniquely identify the data.")
  if (unique != total) {
    if (ignore_error) {
      print(message)
    }
    else {
      stop(message)
    }
  }
  
  # # Ensure that the identifying variables have no missing data
  # try(na.fail(data[, vars])) 

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
