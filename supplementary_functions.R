# ******************************************************************************
# 
#             All the supplementary functions used in the main code
# 
# ******************************************************************************

install_or_load_pack <- function(packages){
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  invisible(lapply(packages, library, character.only = TRUE))
}


# ---------------   1. Year, Month, Week, Day related functions ----------------

# Function to get the last full week of the quarter
get_last_full_week <- function(year, quarter) {
  # define the end date of each quarter
  quarter_ends <- c("03-31", "06-30", "09-30", "12-31")
  end_date <- as.Date(paste(year, quarter_ends[quarter], sep = "-"))
  
  # if the quarter end is a sunday, return the current week number
  if (format(end_date, "%u") == "7") {
    return(as.numeric(format(end_date, "%V")))
  }
  
  # otherwise, return the week number of the preceding sunday
  last_sunday <- end_date - as.numeric(format(end_date, "%u"))
  return(as.numeric(format(last_sunday, "%V")))
}



# Convert year with quarter to full date of the start of the quarter
quarter_to_date <- function(year_quarter) {
  
  # all inputs must be of 'yyyy_q' format
  parts <- strsplit(year_quarter, "_")
  year <- sapply(parts, function(x) x[1])
  quarter <- sapply(parts, function(x) as.integer(x[2]))
  
  # the starting month of the quarter
  start_month <- (quarter - 1) * 3 + 1
  
  # create the date
  as.Date(paste(year, start_month, "01", sep = "-"))
}


# Return the last date of the given quarter
quarter_to_end_date <- function(year_quarter) {

  # all inputs must be of 'yyyy_q' format
  parts <- strsplit(year_quarter, "_")
  year <- sapply(parts, function(x) x[1])
  quarter <- sapply(parts, function(x) as.integer(x[2]))

  # the ending month of the quarter (March = Q1, June = Q2, September = Q3, December = Q4)
  end_month <- quarter * 3

  # get the last day of the ending month
  end_day <- sapply(end_month, function(m) {
    if (m == 3 || m == 12) return(31)
    else if (m == 6 || m == 9) return(30)
  })

  # create the date for the end of the quarter
  as.Date(paste(year, end_month, end_day, sep = "-"))
}


get_first_day_of_week <- function(year, week) {
  # format the week into "YYYY-Www" format, e.g., "2022-W01"
  iso_week <- sprintf("%d-W%02d-1", year, week)
  
  # convert to date using ISOweek2date
  as.Date(ISOweek2date(iso_week))
}

# Function to get the last day of a given ISO week
get_last_day_of_week <- function(year, week) {
  last_day <- get_first_day_of_week(year, week) + days(6)
  return(last_day)
}


# Function to get the corresponding month for a given week and year
get_month_for_week <- function(year, week) {
  
  first_day_of_week <- get_first_day_of_week(year, week)
  last_day_of_week <- get_last_day_of_week(year, week)
  
  if (month(first_day_of_week) == month(last_day_of_week)) {
    return(month(first_day_of_week))
  } else {
    days_in_first_month <- 7 - as.integer(last_day_of_week - first_day_of_week)
    if (days_in_first_month > 3) {
      return(month(first_day_of_week))
    } else {
      return(month(last_day_of_week))
    }
  }
}


# Priskyrimas: menesinius duomenis atitinkamoms savaitems (jei savaite yra dvejuose menesiuose, 
#              tai imame ta menesi, kur daugiau tos savaites dienu).
mapping_data <- function(im_sar, dsk) {
  dsk <- plyr::rename(dsk, c("JAR_KODAS" = "jar_kodas"))
  dsk <- merge(subset(im_sar, select = c(jar_kodas)), dsk, all.x = TRUE)
  dsk <- dsk[order(dsk$jar_kodas), ]
  
  week_columns <- grep("week", names(im_sar), value = TRUE)
  
  # Create a new data frame for the mapped data
  mapped_data <- im_sar
  
  # Loop through each column with weekly data in 'im_sar'
  #col_name <- "week_2020_40"
  for (col_name in names(im_sar)) {
    # col_name <- "week_2019_01"
    if (grepl("week_", col_name)) {
      # Extract year and week number
      year_week <- strsplit(col_name, "_")[[1]][2:3]
      year <- as.numeric(year_week[1])
      week <- as.numeric(year_week[2])
      
      # Get the corresponding month
      corresponding_month <- get_month_for_week(year, week)
      
      # Create the column name for the monthly data in 'dsk'
      month_col_name <- paste(paste0("m", year), corresponding_month, sep="_")
      
      # Debugging: Print the week, corresponding month, and month column name
      #cat("Week:", col_name, "maps to Month Column:", month_col_name, "\n")
      
      # Check if the monthly column exists in 'dsk'
      if (!is.na(corresponding_month) && month_col_name %in% names(dsk)) {
        # Assign the monthly data from 'dsk' to the corresponding week in 'mapped_data'
        mapped_data[[col_name]] <- dsk[[month_col_name]]
      } else {
        # Handle the case where the monthly data does not exist
        mapped_data[[col_name]] <- NA
      }
    }
  }
  
  return(mapped_data)
}

# ------------------     2. kNN related functions    ---------------------------

# kNN with Gower's distance and weighted mean as a prediction,
# where weight = 1/distance
knn_regression_gower <- function(train_data, train_values, predict_data, k, variables, kernel=NULL) {
  
  train_values <- as.numeric(unlist(train_values))
  
  normalize <- function(x) x / sum(x)
  weighted_avg <- function(values, weights) sum(values * weights)
  predictions <- numeric(nrow(predict_data))
  
  if(is.null(kernel)){
    # Use Gower distances
    distances <- StatMatch::gower.dist(data.x = data.frame(train_data[, variables]),
                                       data.y = data.frame(predict_data[, variables]))
    diag(distances) <- Inf
    
  } else {
    # Use Euclidean distances
    all_data <- rbind(train_data[, variables], predict_data[, variables])
    distances_matrix <- as.matrix(dist(all_data))
    n_train <- nrow(train_data)
    n_predict <- nrow(predict_data)
    distances <- distances_matrix[1:n_train, (n_train + 1):(n_train + n_predict)]
  }
  
  for (i in 1:nrow(predict_data)){
    
    distances_i <- distances[, i]
    nn_indices <- order(distances_i)[1:k]
    
    nn_distances <- distances_i[nn_indices]
    nn_values <- train_values[nn_indices]
    
    if(is.null(kernel)){
      # inverse distance weighting
      weights <- 1 / (nn_distances + 1e-8)
    } else{
      # kernel-based weighting
      bandwidth <- max(nn_distances) + 1e-8
      u <- nn_distances / bandwidth
      weights <- compute_kernel_weights(u, kernel)
    }
    
    weights <- normalize(weights)
    
    predictions[i] <- weighted_avg(nn_values, weights)
  }
  rm(distances)
  return(predictions) 
}


knn_regression_gower_cv <- function(train_data, train_values, variables, k_values, kernel=NULL, cv_folds=5){
  
  train_values <- as.numeric(unlist(train_values))
  
  # function to normalize weights
  normalize <- function(x) x / sum(x)
  # function to compute weighted average
  weighted_avg <- function(values, weights) sum(values * weights)
  
  # store cross-validation errors for each k
  cv_errors <- list("RMSE"=numeric(length(k_values)), "R2"=numeric(length(k_values)))
  
  # create cross-validation folds
  set.seed(123)
  folds <- sample(rep(1:cv_folds, length.out = nrow(train_data)))
  
  for(ki in seq_along(k_values)){
    k <- k_values[ki]
    RMSE_fold_errors <- numeric(cv_folds)
    R2_fold_errors <- numeric(cv_folds)
    
    for (fold in 1:cv_folds) {
      # split data into training and validation sets
      val_indices <- which(folds == fold)
      train_indices <- setdiff(seq_len(nrow(train_data)), val_indices)
      
      train_fold_data <- train_data[train_indices, ]
      val_fold_data <- train_data[val_indices, ]
      train_fold_values <- train_values[train_indices]
      val_fold_values <- train_values[val_indices]
      
      # compute distances
      if(is.null(kernel)){
        # use Gower distances
        distances <- StatMatch::gower.dist(
          data.x = data.frame(train_fold_data[, variables]),
          data.y = data.frame(val_fold_data[, variables])
        )
      } else {
        # use Euclidean distances
        all_data <- rbind(train_fold_data[, variables], val_fold_data[, variables])
        distances_matrix <- as.matrix(dist(all_data))
        n_train <- nrow(train_fold_data)
        n_val <- nrow(val_fold_data)
        distances <- distances_matrix[1:n_train, (n_train + 1):(n_train + n_val)]
      }
      
      # initialize predictions for the validation fold
      predictions <- numeric(nrow(val_fold_data))
      
      for (i in 1:nrow(val_fold_data)){
        distances_i <- distances[, i]
        nn_indices <- order(distances_i)[1:k]
        nn_distances <- distances_i[nn_indices]
        nn_values <- train_fold_values[nn_indices]
        
        if(is.null(kernel)){
          # inverse distance weighting
          weights <- 1 / (nn_distances + 1e-8)
        } else{
          # Kernel-based weighting
          bandwidth <- max(nn_distances) + 1e-8
          u <- nn_distances / bandwidth
          weights <- compute_kernel_weights(u, kernel)
        }
        
        weights <- normalize(weights)
        predictions[i] <- weighted_avg(nn_values, weights)
      }
      
      # compute the error for the fold (Root Mean Squared Error)
      RMSE_fold_errors[fold] <- mean((val_fold_values - predictions)^2)^(1/2)
      R2_fold_errors[fold] <- 1 - sum((val_fold_values - predictions)^2) / sum((val_fold_values - mean(val_fold_values))^2)
      # R2_fold_errors[fold] <- mean(abs(val_fold_values - predictions))
    }
    
    # Average validation error over all folds for current k
    cv_errors[["RMSE"]][ki] <- mean(RMSE_fold_errors)
    cv_errors[["R2"]][ki] <- mean(R2_fold_errors)
  }
  
  # Select the k with the minimum cross-validation error
  optimal_k <- c("RMSE" = k_values[which.min(cv_errors[["RMSE"]])],
                 "R2" = k_values[which.min(1 - cv_errors[["R2"]])]
                 )
  
  # Return the optimal k and the cross-validation errors
  return(list(optimal_k = optimal_k, cv_errors = cv_errors, k_values = k_values))
}



# Kernel weights function
compute_kernel_weights <- function(u, kernel_type){
  
  if(kernel_type == "uniform"){
    # Uniform kernel
    weights <- ifelse(abs(u) <= 1, 0.5, 0)
    
  } else if(kernel_type == "triangular"){
    # Triangular kernel
    weights <- ifelse(abs(u) <= 1, (1 - abs(u)), 0)
    
  } else if(kernel_type == "epanechnikov"){
    # Epanechnikov kernel
    weights <- ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)
    
  } else if(kernel_type == "quartic"){
    # Quartic kernel
    weights <- ifelse(abs(u) <= 1, (15/16)*(1 - u^2)^2, 0)
    
  } else if(kernel_type == "gaussian"){
    # Gaussian kernel
    weights <- (1/sqrt(2*pi)) * exp(-0.5 * u^2)
    
  } else if(kernel_type == "logistic"){
    # Logistic kernel
    weights <- exp(u) / (1 + exp(u))^2
    
  } else {
    stop("Unknown kernel type")
  }
  return(weights)
}


# Function to calculate weighted quantiles
weighted_quantiles <- function(data, weights, probs = c(0.25, 0.5, 0.75)) {
  
  order_index <- order(data)
  data_sorted <- data[order_index]
  weights_sorted <- weights[order_index]
  
  # calculate the cumulative weights and normalize them to sum up to 1
  cum_weights <- cumsum(weights_sorted)
  cum_weights_normalized <- cum_weights / max(cum_weights)
  
  # find the quantile positions
  quantile_values <- sapply(probs, function(p) {
    # find the first occurrence where the cumulative normalized weight exceeds the quantile
    index <- which(cum_weights_normalized >= p)[1]
    return(data_sorted[index])
  })
  names(quantile_values) <- paste(probs * 100, "%", sep="")
  return(quantile_values)
}




# -----------  3. Estimator comparison plots with T_dir  ---------------------

# comparison_df  -- data.frame object with columns of this order: 
#   direct, estimator, direct_cv, estimator_cv

compare_plot <- function(comparison_df, name_estimator = NULL, title_text = NULL){
  
  # first color -- Direct. second -- estimator
  coLors <- c(rgb(0, 0, 220/256), rgb(154/256, 192/265, 205/256))
  
  if(is.null(name_estimator)) name_estimator <- "Model-based"
  colnames(comparison_df) <- c("domain_size", "Direct", name_estimator, "Direct_CV", paste0(name_estimator, "_CV"))
  
  library(patchwork)
  
  # (1)
  p1 <- ggplot(data = comparison_df) +
    geom_point(aes(x = !!sym(name_estimator), y = Direct), cex=2) + 
    
    geom_abline(aes(intercept = 0, slope = 1, color = "Intersection"), lwd=1) +
    geom_smooth(aes(!!sym(name_estimator), Direct, color = "Reg.line"), method = "lm", se=FALSE) +
    scale_colour_manual(name="", values=c(coLors[2], coLors[1])) +
    labs(y="Direct", x=name_estimator) 
  
  
  # (2)
  p2 <- ggplot(data = comparison_df[, 4:5] |> 
                 dplyr::rename(Direct = Direct_CV, !!name_estimator := !!sym(paste0(name_estimator, "_CV"))) |>
                 tidyr::pivot_longer(cols = c(Direct, name_estimator), names_to = "Method", values_to = "CV")) + 
    geom_boxplot(aes(x = CV, y=Method, fill = Method), outlier.size = 2, na.rm = T) + 
    scale_fill_manual(values = c(coLors[1], coLors[2])) +
    guides(fill = "none") + 
    xlim(0, 1) +
    labs(y="")
  
  
  # (3)
  p3 <- ggplot(data = arrange(comparison_df[, 4:5], Direct_CV)) +
    geom_point(aes(x = 1:nrow(comparison_df), y = Direct_CV, color = "Direct"), cex = 2) + 
    
    # geom_rect(ymin = 0, ymax = 0.05, xmin = -Inf, xmax = Inf, fill = "green", alpha = 0.005) +
    # geom_rect(ymin = 0.05, ymax = 1/6, xmin = -Inf, xmax = Inf, fill = "orange", alpha = 0.005) +
    # geom_rect(ymin = 0.16, ymax = 1/3, xmin = -Inf, xmax = Inf, fill = "red", alpha = 0.005) +
    
    geom_hline(yintercept = 1/3, linetype = "dashed", col = "orange") + 
    # annotate("text", x = 55, y = 1/3, label = "cv = 33,3 %", vjust = -0.5) +
    annotate("text", x = Inf, y = 1/3, label = "cv = 33,3 %", hjust = -0.2, size = 4) +
    
    geom_hline(yintercept = 1/6, linetype = "dashed", col = "green") + 
    # annotate("text", x = 55, y = 1/6, label = "cv = 16,5 %", vjust = -0.5) +
    annotate("text", x = Inf, y = 1/6, label = "cv = 16,5 %", hjust = -0.2, size = 4) +
    
    # geom_hline(yintercept = 0.05, linetype = "dashed", col = "green") + 
    # # annotate("text", x = 55, y = 0.05, label = "cv = 5 %", vjust = -0.5) +
    # annotate("text", x = Inf, y = 0.05, label = "cv = 5 %", hjust = -0.2, size = 4) +
    
    geom_point(aes(x = 1:nrow(comparison_df), y = !!sym(paste0(name_estimator, "_CV")), color = name_estimator), cex = 2) + 
    scale_color_manual(name = "Method", values = c(coLors[1], coLors[2])) + 
    ylim(0, 1) +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 10, r = 70, b = 10, l = 10, unit = "pt")  # Add right margin
    ) + 
    labs(y = "CV", x = "Domain (ordered by increasing CV of Direct)") + 
    coord_cartesian(clip = "off") # Ensures text annotations appear outside the plot area
  
  
  p4 <- ggplot(data = arrange(comparison_df[, 4:5], comparison_df["domain_size"])) +
    geom_point(aes(x = 1:nrow(comparison_df), y = Direct_CV, color = "Direct"), cex = 2, pch = 20) + 
    geom_line(aes(x = 1:nrow(comparison_df), y = Direct_CV, color = "Direct")) + 
    
    geom_hline(yintercept = 1/3, linetype = "dashed", col = "orange") + 
    annotate("text", x = Inf, y = 1/3, label = "cv = 33,3 %", hjust = -0.2, size = 4) +
    
    geom_hline(yintercept = 1/6, linetype = "dashed", col = "green") + 
    annotate("text", x = Inf, y = 1/6, label = "cv = 16,5 %", hjust = -0.2, size = 4) +
    
    geom_point(aes(x = 1:nrow(comparison_df), y = !!sym(paste0(name_estimator, "_CV")), color = name_estimator), cex = 2, pch = 20) + 
    geom_line(aes(x = 1:nrow(comparison_df), y = !!sym(paste0(name_estimator, "_CV")), color = name_estimator)) + 
    scale_color_manual(name = "Method", values = c(coLors[1], coLors[2])) + 
    ylim(0, 1) +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 10, r = 70, b = 10, l = 10, unit = "pt")  # Add right margin
    ) +
    labs(y = "CV", x = paste("Domain (ordered by increasing domain size)")) + 
    coord_cartesian(clip = "off")
  
  
  shared_legend <- get_legend(
    ggplot(data = comparison_df) +
      geom_point(aes(x = 1:nrow(comparison_df), y = Direct_CV, color = "Direct")) +
      geom_point(aes(x = 1:nrow(comparison_df), y = !!sym(paste0(name_estimator, "_CV")), color = name_estimator)) +
      scale_color_manual(name = "Estimator", values = c(coLors[1], coLors[2])) +
      theme(legend.position = "bottom")
  )
  
  # combine p3 and p4 in a single row
  combined_plots <- arrangeGrob(
    p3, p4, 
    ncol = 2
  )
  
  # place the shared legend below p3 + p4
  combined_bottom <- arrangeGrob(
    combined_plots,
    shared_legend,
    ncol = 1,
    heights = c(10, 2)  # adjust the relative height for plots vs legend
  )
  
  # arrange everything:
  #  - First row: p1 and p2
  #  - Second row: single column that holds the combined_bottom
  final_grid <- arrangeGrob(
    arrangeGrob(p1, p2, ncol = 2),  # First row, two columns
    combined_bottom,                # Second row, single column
    nrow  = 2,
    heights = c(13, 15)             # Adjust overall row heights as needed
  )
  
  grid.arrange(
    final_grid,
    top = if (!is.null(title_text))
      textGrob(title_text, gp = gpar(fontsize = 16, fontface = "bold"))
    else NULL
  )
}
