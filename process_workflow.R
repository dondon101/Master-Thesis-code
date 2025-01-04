# ******************************************************************************
# 
#              The main functions of the Master thesis code
# 
# ******************************************************************************



#' @title Create bootstrap sample for fixed period
#' 
#' @description
#' This function creates a single bootstrap sample of a given
#' fixed period. The bootstrap is implemented based on Rao and Wu (1992)
#' bootstrap for finite populations with complex survey designs.

bootstrap_sample <- function(p_sample = imt, period, boot){
  
  # fix the sample of given period
  p_sample <- p_sample[[period]]
  
  # split the sample by strata 
  data_by_h <- split(p_sample, f = p_sample$sl2)
  
  # count the sample sizes in each area
  n_h_by_sl <- map(data_by_h, ~ sum(.x$sample))
  
  compute_correction <- function(boot_sample, n_h) {
    correction_h <- data.frame(
      JAR_KODAS = as.numeric(names(table(boot_sample))),
      correction = as.numeric(table(boot_sample)) * n_h / (n_h - 1)
    )
    return(correction_h)
  }
  
  # replicate_observations <- function(data_h, boot_sample_id, n_h) {
  #   freq_table <- table(boot_sample_id)
  #   replicated_data <- data_h[as.numeric(names(freq_table)), ]
  #   replicated_data <- replicated_data[rep(seq_len(nrow(replicated_data)), freq_table), ]
  #   replicated_data$correction <- n_h/(n_h-1)
  #   return(replicated_data)
  # }
  
  
  data_by_h_updated <- map2(data_by_h, names(data_by_h), function(data_h, h) {
    n_h <- nrow(data_h)
    
    if (n_h == 1) {
      data_h["correction"] <- 1
    } else {
      set.seed(as.numeric(h) + boot * 1e6)
      boot_sample_id <- sample(1:n_h, n_h - 1, replace = TRUE)
      
      # data_h <- replicate_observations(data_h, boot_sample_id, n_h)
      
      correction_h <- compute_correction(data_h[boot_sample_id, "JAR_KODAS"], n_h)
      data_h <- left_join(data_h[unique(boot_sample_id), ], correction_h, by = "JAR_KODAS")
    }
    return(data_h)
  })
  
  p_sample <- bind_rows(data_by_h_updated) |> 
    mutate(w_design = w,
           w = w * correction)
  
  return(p_sample)
}



# ******************************************************************************
# ******************************************************************************


#' @title create initial datasets of given period
#'
#' @description
#' This function prepares the data for a single fixed quarter. 
#' It joins NP and P samples, removes duplicated units, excludes 
#' NP units that do not belong to the population of the quarter, 
#' and joins monthly number of employees to the NP data
#'
#' @returns list of following elements: 
#' 1. period (year and quarter combination)
#' 2. population
#' 3. p_sample
#' 4. np_sample (it is actually a union of P and NP samples)

prepare_InitData <- function(
    period, 
    ketvirciai_savaites = week_of_quarter,
    population = pop,
    p_imtis = imt,
    dsk_all = dsk,
    aggreguoti_web = list("scrapped" = aggr_web_scrapped),
    verbose = FALSE
){
  
  # *****************
  #   Population
  # *****************
  
  year_quarter <- substr(ketvirciai_savaites$quarter_id[period], 1, 6)
  
  ldv_t <- population[[year_quarter]] |>
    mutate(w = nn/n1)
  
  d_svoriai <- ldv_t[!is.na(ldv_t$w), c("sl2", "w")] |> unique()
  ldv_t <- left_join(ldv_t, d_svoriai, by = "sl2") |> 
    mutate(w = w.y) %>% 
    select(-w.x, -w.y)
  
  
  # Mikroniuansai:
  if(anyDuplicated(ldv_t$JAR_KODAS) > 0) stop("Exists duplicated legal entities")
  if(period == 12){
    # legal entity changed its name and activity
    ldv_t <- ldv_t |> filter(!(JAR_KODAS == 303686899 & n_dsk < 5))
    if(anyDuplicated(ldv_t$JAR_KODAS) > 0) warning("There remains duplicated legal entities")
  }
  # entity was in population during bankruptcy:
  if(period >= 14) ldv_t <- ldv_t |> filter(!(JAR_KODAS %in% c(123171157, 305151863)))
  
  
  # *****************
  #     Samples
  # *****************
  # nonprob. sample (scrapped)
  np_scrapped <- aggreguoti_web[["scrapped"]] |>
    select("jar_kodas", "sav_kodas", ketvirciai_savaites$week_id[period]) |>
    dplyr::rename(JAR_KODAS = jar_kodas, ldv_scrapped = 3)
  np_scrapped <- np_scrapped[!is.na(np_scrapped[,3]),] #atrenkamos imones be NA
  
  # prob. sample 
  p_sample <- p_imtis[[year_quarter]] |>
    select(JAR_KODAS, sav_kodas, sl2, ldv, w, dsk)
  p_sample$imtis <- 1
  
  
  # *********************************************************
  # PREPARE DATA FOR MODEL-CALIBRATION PART
  # *********************************************************
  # d1 <- np_scrapped
  # d2 <- p_sample
  # ldv_t <- init_data[["populiacija"]]
  
  
  # 1.
  # removing units from NP sample that do not belong to a population
  d1_0 <- np_scrapped |> filter(JAR_KODAS %in% unique(ldv_t$JAR_KODAS))
  d1_0$delta <- 1
  
  # 2.
  # joining NP sample with P sample
  d1_0 <- merge(d1_0 |> dplyr::rename(sav_kodas_B = sav_kodas),
                p_sample |> dplyr::rename(sav_kodas_imtis = sav_kodas), all = TRUE)
  
  # for some units the municipalities in NP sample differ from P sample, the latter is preferred
  d1_0 <- d1_0 |>
    mutate(sav_kodas = ifelse(!is.na(sav_kodas_imtis), sav_kodas_imtis, sav_kodas_B),
           delta = ifelse(is.na(delta), 0, delta)) |>
    select(-sav_kodas_B, -sav_kodas_imtis)
  
  
  # 3.
  # entities activity, activity section and strata information is joined
  d1_0 <- left_join(d1_0, ldv_t |> select(JAR_KODAS, veikla2, sekcija, sl2, w), by = "JAR_KODAS") |>
    mutate(sl2 = ifelse(is.na(sl2.x), sl2.y, sl2.x),
           w = ifelse(is.na(w.x), w.y, w.x)) |> 
    select(-sl2.x, -sl2.y, -w.x, -w.y)
  
  
  # ***************************
  #   Number of employees (NOY)
  # ****************************
  # the monthly number of employees is taken of t-1, t and t+1 periods
  pask_men <- substr((quarter_to_date(year_quarter) + months(2)), 6, 7) |> as.numeric()
  laikotarpiai <- paste0("m", substr(year_quarter, 1, 4), "_", (pask_men-1):(pask_men+1))
  if(any(substr(laikotarpiai, 7, 8) == "13")){
    laikotarpiai[which(substr(laikotarpiai, 7, 8) == "13")] <- paste0("m", as.numeric(substr(year_quarter, 1, 4)) + 1, "_1")
  }
  
  # if after joining the employees from admin. data, there are still NA values,
  # those are filled from population information
  ketvirciai <- paste0(substr(laikotarpiai, 2, 5), "_", quarter(ym(substr(laikotarpiai, 2, 8))))
  stulpeliai_iki_dsk <- ncol(d1_0)
  
  
  # number of employees from last two months are added to the population info
  for(time in 1:2){
    dsk_t <- dsk_all |> dplyr::select(JAR_KODAS, all_of(laikotarpiai[time]))
    names(dsk_t)[2] <- paste0("dsk_t", time-1)
    class(dsk_t$JAR_KODAS) <- "numeric"
    ldv_t <- left_join(ldv_t, dsk_t, by = "JAR_KODAS")
  }
  
  for(time in 1:3){
    # time<-1
    # monthly NOY
    dsk_t <- dsk_all |> dplyr::select(JAR_KODAS, all_of(laikotarpiai[time]))
    names(dsk_t)[2] <- paste0("dsk_t", time-1)
    class(dsk_t$JAR_KODAS) <- "numeric"
    
    # joining montly NOY
    d1_0 <- left_join(d1_0, dsk_t, by = "JAR_KODAS")
    stulpelis <- stulpeliai_iki_dsk + time
    indx <- d1_0[is.na(d1_0[stulpelis]), "JAR_KODAS"]
  
    # ldv_ketv0 <- ldv_visi |> filter(metai_q == ketvirciai[time])
    ldv_ketv <- pop[[ketvirciai[time]]]
    papildymas <- ldv_ketv[ldv_ketv$JAR_KODAS %in% indx, c("JAR_KODAS", "n_dsk")]
    
    # scrapped
    d1_0 <- left_join(d1_0, papildymas, by = "JAR_KODAS")
    d1_0[stulpelis] <- ifelse(is.na(d1_0[, stulpelis]), d1_0[, "n_dsk"], d1_0[, stulpelis])
    d1_0 <- dplyr::select(d1_0, -n_dsk)
  }
  rm(ldv_ketv)
  
  
  if(verbose){
    cat("\n")
    cat("------------------ ", year_quarter, " ------------------\n")
    cat("Population: N=", length(unique(ldv_t$JAR_KODAS)), "\n", sep = "")
    cat("Probability sample: n=", nrow(p_sample), "\n", sep = "")
    cat("Nonprobability sample: N_B=", sum(d1_0$delta), "\n", sep = "")
    cat("Non-zero y-values ratio:", round(100*sum(!(d1_0$ldv_scrapped %in% c(0, NA)))/sum(d1_0$delta), 0), "%", "\n", sep = " ")
  }
  
  return(list("period" = year_quarter,
              "population" = ldv_t,
              "np_scrapped" = d1_0,
              "p_sample" = p_sample)
  )
}






# ******************************************************************************
# ******************************************************************************


#' @title Create area-level information dataset
#'
#' @description
#' This function creates area-level information dataset which
#' contains for each domain the population size, NP and P sample sizes,
#' intersection size of P and NP, and the direct (Hajek) estimates of domain 
#' totals along with Var and CV estimates.
#' 
#' @returns list of following elements: 
#' excluded_areas - areas for which calibration estimation will not be applied
#' data_Area - area-level data

summarize_AreaLevel <- function(
    init_data = init_data_results,
    threshold = pakankamas_N_B,
    direct_estimates = tyr_duom,
    verbose = FALSE
    ){
  
  
  year_quarter <- init_data_results$period
  ldv_t <- init_data_results$population
  d1_0 <- init_data_results$np_scrapped
  
  
  # ****************************
  # A <- subset(d1_0, !is.na(imtis), select=c(JAR_KODAS, sav_kodas, sl2, ldv_web, w, delta))
  B <- d1_0 |> filter(delta==1)
  
  N <- ldv_t |> group_by(sav_kodas) |> count(name = "N") |> ungroup()
  
  N_B <- B |> group_by(sav_kodas) |> dplyr::summarise(N_B = n(), .groups = "drop")
  # papildymas, kai netikimybines imties srityje nera:
  N_B <- merge(subset(N, select=c(sav_kodas)), N_B, all=TRUE)
  N_B[is.na(N_B)] <- 0
  
  
  # prepare dataset that will be returned
  # ****************************
  data_Area <- merge(N, N_B)
  
  # intersection sizes of P and NP samples in domains
  intersection_size <- d1_0 |>
    filter(imtis == 1 & delta == 1) |>
    group_by(sav_kodas) |>
    dplyr::summarise(size_p_inter_np = n(), .groups = "drop")
  
  data_Area <- merge(data_Area, intersection_size, all.x = T)
  data_Area <- data_Area |> 
    mutate(excluded = ifelse(size_p_inter_np < threshold, 1, 0))
  
  excluded_areas <- subset(data_Area, excluded == 1, select=c(sav_kodas))$sav_kodas
  
  if(verbose){
    cat("Ratio of N_B/N in domains:", round(fivenum(N_B$N_B/N$N), 2), "\n", sep = " ")
    cat("Number of areas too small for proper calibration:", length(excluded_areas), "\n", sep = " ")
  }
  
  
  # add the total of y in NP sample:
  T_B <- B |> group_by(sav_kodas) |> summarise(T_B = sum(ldv_scrapped), .groups = "drop")
  data_Area <- merge(data_Area, T_B, all=TRUE)

  # Prijungiame tiesioginius ivercius:
  direct_estimates_t <- subset(direct_estimates, select=c("sav_kodas", year_quarter,
                                       paste(year_quarter, "se", sep="_"), 
                                       paste(year_quarter, "n", sep="_")))
  names(direct_estimates_t)[2:4] <- c("T_dir", "T_dir_se", "n")
  data_Area <- merge(data_Area, direct_estimates_t, all = TRUE)
  data_Area$T_dir_var <- data_Area$T_dir_se^2
  data_Area$T_dir_cv <- data_Area$T_dir_se/data_Area$T_dir
  
  return(list("excluded_areas" = list("scrapped" = excluded_areas), "data_Area" = data_Area))
}



# ******************************************************************************
# ******************************************************************************


#' @title Predictions of kNN model
#'
#' @description
#' This function is used to prepare the data for kNN search and prediction.
#' If k_values is a vector of positive values, then k-fold CV is used to find 
#' optimal k parameter, if k_values is of length 1, then optimal k is the one that
#' was specified. If kernel weighting is used, then Euclidean distances are taken,
#' otherwise by defaul the Gower's distance is used.
#' 
#' @param k_values vector (length > 1) or numeric value, indicating k-neighbors.
#' @param rm_outl if TRUE, outliers of web data are to be removed, default FALSE.
#' @param kernel_type kernel type if Nadarya-Watson regression is used, default NULL.
#' 


prepare_and_predictKNN <- function(
    area_data,
    data,
    variables = c("ldv_web", "dsk_t1", "w", "apskr_kodas"),
    # variables = c("dsk_t1", "w"),
    k_values = NULL,
    rm_outl = FALSE, 
    kernel_type = NULL
    ){
  
  prepare_MC <- function(data){
    data$delta_1 <- 1 - data$delta
    data$imtis <- replace(data$imtis, is.na(data$imtis), 0)
    
    # assign ldv_web=0 to NA values (basically assigning 0 to P\NP part)
    data$ldv_web <- replace(data$ldv_web, is.na(data$ldv_web), 0)
    data$dsk_diff_t1 <- data$dsk_t1 - data$dsk_t0
    data$y_delta <- data$delta * data$ldv_web
    
    data <- left_join(data, 
                      miestai |> 
                        select(sav_kodas, apskrities_kodas) |> 
                        unique(), 
                      by = "sav_kodas") |>
      dplyr::rename(apskr_kodas = apskrities_kodas)
    
    data$apskr_kodas <- as.factor(data$apskr_kodas)
    data$sekcija <- as.factor(data$sekcija)
    
    return(data)
  }
  
  duomMC <- data |> as_tibble() |> prepare_MC()
  
  # remove outliers if specified
  percentile_web <- quantile(duomMC$ldv_web[duomMC$ldv_web > 0], perc_outl)
  if (rm_outl) {
    duomMC$ldv_web <- ifelse(duomMC$ldv_web > percentile_web, percentile_web, duomMC$ldv_web)
  }
  
  train_data <- subset(duomMC, delta==1 & imtis == 1)
  if(normalize) train_data[, variables[-4]] <- scale(train_data[, variables[-4]])
  
  predict_data <- duomMC |> filter(delta==1)
  if(normalize) predict_data[, variables[-4]] <- scale(predict_data[, variables[-4]])
  
  if(length(k_values) > 1){
    # cross-validation to find optimal k
    cv_result <- knn_regression_gower_cv(
      train_data = train_data,
      train_values = train_data[, "ldv"], 
      variables = variables,
      k_values = k_values,
      kernel = kernel_type
    )
    prc_thresh <- 1
    cv_result$optimal_k$R2 <- which(100 * abs(diff(cv_result$cv_errors$R2))/abs(cv_result$cv_errors$R2[-1]) < prc_thresh)[1]
    cv_result$optimal_k$RMSE <- which(100 * abs(diff(cv_result$cv_errors$RMSE))/abs(cv_result$cv_errors$RMSE[-1]) < prc_thresh)[1]
    
    optimal_k <- cv_result$optimal_k$RMSE
    # optimal_k <- cv_result$optimal_k
    
  } else{
    optimal_k <- k_values
  }
  
  
  
  # predictions with optimal k
  predictions_knn <- knn_regression_gower(
    train_data = train_data,
    train_values = train_data[, "ldv"], 
    predict_data = predict_data, 
    k = optimal_k,            
    variables = variables,
    kernel = kernel_type
  )
  
  df_predictions <- data.frame(
    "JAR_KODAS" = predict_data[, "JAR_KODAS"], 
    "predictions_knn" = predictions_knn
  )
  
  # join predictions back into the main data
  duomMC <- left_join(duomMC, unique(df_predictions), by="JAR_KODAS")
  duomMC <- duomMC |> mutate(
    predictions_knn_final = ifelse(is.na(predictions_knn), 0, predictions_knn),
    fit_delta_knn = delta * predictions_knn_final  # zero predictions outside NP sample
  )
  
  return(list(
    data = duomMC,
    cv_results = if(exists("cv_result")) cv_result else NULL
    )
  )
}


# ******************************************************************************
# ******************************************************************************

#' @title Predictions of kNN model
#' 
#' @description
#' Function used to obtain Model-Calibration (MC) estimates in small areas.
#' MC is not applied to those domains that are indicated by a numeric vector 
#' of area IDs (excluded_areas)

get_estimator_MC <- function(
    web_data_predictions,
    area_data, 
    excluded_areas, 
    sav_kodai, 
    verbose = FALSE
    ){
  
  # web_data_predictions <- predictions_of_KNN
  # excluded_areas <- area_data$excluded_areas$scrapped
  # area_data <- area_data$ivert
  # sav_kodai <- sav_kodas$sav_kodas
  
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="certainty")
  
  sav_kodai <- sort(unique(sav_kodai$sav_kodas))
  ivert_sav_knn <- data.frame("sav_kodas" = sav_kodai, "total" = NA, "var" = NA, "cv" = NA)
  ivert_sav_DIR <- ivert_sav_knn
  
  duomMC_knn <- web_data_predictions |> mutate(fpc = 1/w)
  planasMC_sav <- svydesign(ids = ~0, data = subset(duomMC_knn, imtis==1),
                            weights = ~w, strata = ~sl2, fpc = ~fpc
                            )
  
  for(SAV in sav_kodai){
    # SAV <- 12
    duomMC_imtis_sav <- subset(duomMC_knn, subset = imtis==1 & sav_kodas==SAV)
    
    # sums of calibration constraints
    Nb_sav <- sum(duomMC_knn[duomMC_knn$sav_kodas == SAV, "delta"])
    N_sav <- area_data$N[area_data$sav_kodas == SAV]
    T_b_sav <- sum(duomMC_knn[duomMC_knn$sav_kodas == SAV, "predictions_knn_final"])
    
    kalib <- calibrate(subset(planasMC_sav, sav_kodas==SAV),
                       formula = ~1, population = N_sav)
    r <- svytotal(~ldv, kalib)
    ivert_sav_DIR[ivert_sav_DIR$sav_kodas == SAV, -1] <- c(coef(r), SE(r)^2, cv(r))
    
    
    # additional IFs due to bootstrap variance
    # if(nrow(duomMC_imtis_sav) == 0){
    #   ivert_sav_knn[ivert_sav_knn$sav_kodas == SAV, -1] <- c(NA, NA, NA)
    #   next
    # }
    if(nrow(duomMC_imtis_sav) == 1){
      ivert_sav_knn[ivert_sav_knn$sav_kodas == SAV, -1] <- c(duomMC_imtis_sav$ldv * duomMC_imtis_sav$w, 0, 0)
      next
    }
    # if(sum(duomMC_imtis_sav$fit_delta_knn) == 0){
    if(sum(duomMC_imtis_sav$delta)==0 | sum(duomMC_imtis_sav$delta_1) == 0 | sum(duomMC_imtis_sav$fit_delta_knn) == 0){
      ivert_sav_knn[ivert_sav_knn$sav_kodas == SAV, -1] <- c(coef(r), SE(r)^2, cv(r))
      next
    }
    
    if(!(SAV %in% excluded_areas)){
      kalibMC_knn_sav <- calibrate(subset(planasMC_sav, sav_kodas==SAV),
                                   formula= ~ 0 + delta + delta_1 + fit_delta_knn,
                                   population = c(Nb_sav,
                                                  N_sav - Nb_sav,
                                                  T_b_sav), force = T,
                                   calfun = c("linear"), bounds = c(0.01, Inf), bounds.const = T)
      
      
      duomMC_knn[which(duomMC_knn$JAR_KODAS %in% duomMC_imtis_sav$JAR_KODAS), "w_cal"] <- weights(kalibMC_knn_sav)
      duomMC_imtis_sav$wcal_knn <- weights(kalibMC_knn_sav)
      
      # cat("Summary of calibrated weights:\n")
      # print(summary(duomMC_imtis_sav$wcal_knn))
      
      total_sav <- svytotal(~ldv, design = kalibMC_knn_sav)
      ivert_sav_knn[ivert_sav_knn$sav_kodas == SAV, -1] <- c(coef(total_sav), SE(total_sav)^2, cv(total_sav))
      
    } else{
      kalib <- calibrate(subset(planasMC_sav, sav_kodas==SAV), 
                         formula = ~1, population = N_sav)
      r <- svytotal(~ldv, kalib)
      ivert_sav_knn[ivert_sav_knn$sav_kodas == SAV, -1] <- c(coef(r), SE(r)^2, cv(r))
      
    }
  }
  
  cv_calc <- function(pt, st) sqrt(sum(st))/sum(pt)
  
  if(verbose){
    cat("\nComparison of total estimates:", "T_dir =", sum(area_data$T_dir), 
        "// T_knnMC =", sum(ivert_sav_knn$total), "\n")
    cat("Comparison of CVs of total estimates:", "cv(T_dir) =", 
        round(100*cv_calc(area_data$T_dir, area_data$T_dir_var), 2), "%",
        "// cv(T_knnMC) =", round(100*cv_calc(ivert_sav_knn$total, ivert_sav_knn$var), 2),
        "%", "\n")
    
    cat("cv(T_Dir)% distribution in areas:", round(100*fivenum(area_data$T_dir_cv), 2), "\n")
    cat("cv(T_MC)% distribution in areas:", round(100*fivenum(ivert_sav_knn$cv), 2), "\n")
  }
  
  ivert_sav_knn <- ivert_sav_knn |> 
    dplyr::rename("T_knnMC" = total, "T_knnMC_var" = var, "T_knnMC_cv" = cv)
  
  
  ivert_final <- left_join(area_data, ivert_sav_knn, by="sav_kodas")
  ivert_final <- left_join(ivert_final, 
                           ivert_sav_DIR |> select(sav_kodas, "T_dir_boot" = total), 
                           by="sav_kodas")
  
  return(list("area_data" = ivert_final, "unit_data" = duomMC_knn))
  
}



# ******************************************************************************
# ******************************************************************************

#' @title Prepare area-level data for FH model
#' 
#' @description
#' This function aggregates the aux. vector X values to obtain
#' area-level covariates.


prepare_FH_data <- function(
    population = init_data_results$population,
    dsk_all = dsk,
    area_data = estimator_MC
    ){
  
  population$dsk_diff_t1 <- population$dsk_t1 - population$dsk_t0
  
  dsk_t1_sav <- population |> 
    group_by(sav_kodas) |> 
    summarise(
      dsk_t0 = sum(dsk_t0, na.rm = TRUE),
      dsk_t1 = sum(dsk_t1, na.rm = TRUE)
      )
  
  dsk_diff_t1_sav <- population |>
    group_by(sav_kodas) |> 
    summarise(dsk_diff_t1 = sum(dsk_diff_t1, na.rm = TRUE))
  
  ties_iv_t <- area_data
  ties_iv_t <- merge(ties_iv_t, dsk_t1_sav)
  ties_iv_t <- merge(ties_iv_t, dsk_diff_t1_sav)
  
  return(ties_iv_t)  
}







# ******************************************************************************
# ******************************************************************************

#' @title Smooth the variances of direct estimator (Hajek/MC)
#' 
#' @description
#' If for some areas the direct variance estimates via linearization
#' are not stable, lin. reg. smoothing can be applied. 
#' The smoothing is not applied this function can still remain in
#' the whole process as it will return unchanged input data

smooth_variances <- function(
    data = data_FH,
    do_direct = FALSE, 
    do_MC = FALSE
){
  

  f_smooth <- function(data, variable = "T_dir_cv", subset = NULL){
    
    formula_smooth <- formula(paste0("log(", variable, ") ~ log(N)"))
    smooth_model = lm(formula_smooth, data, subset)
    print(summary(smooth_model))
    
    #  Rivest and Belmonte (2000) correction
    adj_coef <- exp(summary(smooth_model)$sigma^2 / 2)
    # adj_coef <- mean(exp(resid(smooth_model)))
    
    data[[paste0(variable, "_smoothed")]] <- adj_coef * exp(coef(smooth_model)[1]) * (data$N ^ coef(smooth_model)[2])
    return(data)
  }
  
  
  # Var(T_dir) smoothing
  # ***************************************
  if (do_direct) {
    cat("\n********************************\n   SMOOTHING MODEL OF DIRECT\n********************************")
    data <- f_smooth(data, variable = "T_dir_cv")
    
    data$T_dir_var_smoothed <- (data$T_dir * data$T_dir_cv_smoothed) ^ 2 # paduoti funkcijai fh
    
    AR_ERROR <- function(actual, predicted) mean(abs((actual-predicted)/(actual + 1e-2)))
    print(sprintf("Relative absolute error of smoothing: %f",
                  AR_ERROR(sqrt(data$T_dir_var), sqrt(data$T_dir_var_smoothed))
    ))
  }
  
  
  
  # Var(T_knnMC) smooting
  # ***********************************
  if (do_MC) {
    cat("\n********************************\n   SMOOTHING MODEL OF MC\n********************************")
    
    neitraukti_i_glodinima <- (filter(data, T_knnMC_cv < 0.0005))[["sav_kodas"]]
    N_bound <- quantile(data$N, 0.1)  # apribojimas srities dydziui
    condition <- data$T_knnMC_cv>0 & data$N > N_bound & !(data$sav_kodas %in% neitraukti_i_glodinima)
    
    data <- f_smooth(data, variable = "T_knnMC_cv", subset = condition)
    data$T_knnMC_var_smoothed <- (data$T_knnMC * data$T_knnMC_cv_smoothed) ^ 2 # paduoti funkcijai fh
    
    #ties_iv_t$T_knnMC_var_sm <- ties_iv_t$T_knnMC_var_sm * (sum(ties_iv_t$T_knnMC_var) / sum(ties_iv_t$T_knnMC_var_sm)) #BANDYMAS!!!
    
    AR_ERROR <- function(actual, predicted) mean(abs((actual-predicted)/(actual + 1e-2)))
    print(sprintf("Relative absolute error of smoothing: %f",
                  AR_ERROR(sqrt(data$T_knnMC_var), sqrt(data$T_knnMC_var_smoothed))
    ))
    
  }
  
  return(list("area_data" = data, "smoothing" = c("direct" = do_direct, "MC" = do_MC)))
}



# ******************************************************************************
# ******************************************************************************

#' @title Fay-Herriot model diagnostics
#' 
#' @description
#' This function is a helper function to carry out FH model diagnostics.
#' It computes Cook's D, multivariate DFFITS, COVRATIO of parameters
#' and can produce diagnostic plot with 4 subplots.

diagnostics_FH <- function(model, plot = TRUE, title_text = NULL, excluded_areas){
  
  # model <- fh_dir
  data <- model$framework$combined_data
  n <- nrow(data)
  
  
  # fix the model inputs for reuse
  fixed_m <- model$fixed
  vardir_m <- get(as.character(model$call)[3], envir = parent.frame())
  domains_m <- model$framework$domains
  transformation_m <- model$transformation$transformation
  backtransformation_m <- model$transformation$backtransformation
  MSE_m <- {if("FH" %in% names(model$MSE)) TRUE else FALSE}
  
  # fix the full models estimates needed for diagnostics
  rank_X <- rankMatrix(model.matrix(model, data)) |> as.numeric()
  logLik_m <- suppressMessages(logLik(model))
  beta_m <- model$model$coefficients[, 1]
  var_beta_m <- model$model$beta_vcov
  
  theta_m <- model$model$variance
  V_m <- theta_m + model$framework$vardir
  var_theta_m <- 2/sum(1/V_m^2)
  
  displacement <- c()
  cooks_d <- list("beta" = NA, "theta" = NA)
  mDFFITS <- list("beta" = NA, "theta" = NA)
  cov_trace <- list("beta" = NA, "theta" = NA)
  cov_ratio <- list("beta" = NA, "theta" = NA)
  p_value <- c()
  
  traceMatrix <- function(matrix) return(sum(diag(matrix)))
  
  for(i in 1:n){
    model_i <- fh(fixed = fixed_m,  
                  vardir = vardir_m,
                  combined_data = data[-i, ],
                  domains = domains_m, 
                  transformation = transformation_m, 
                  backtransformation = backtransformation_m, 
                  MSE = TRUE)
    
    logLik_i <- suppressMessages(logLik(model_i))
    beta_i <- model_i$model$coefficients[, 1]
    var_beta_i <- model_i$model$beta_vcov
    
    theta_i <- model_i$model$variance
    V_i <- theta_i + model_i$framework$vardir
    var_theta_i <- 2/sum(1/V_i^2)
    
    q <- dim(model$model$random_effects)[2]
    
    # 1. Overall influence
    # (restricted) likelihood displacement
    displacement[i] <- 2*(logLik_m - logLik_i)
    
    # 2. Change in parameter estimates
    
    # cooks_d for beta and theta
    cooks_d[["beta"]][i] <- (beta_m - beta_i) %*% solve(var_beta_m) %*% t(t(beta_m - beta_i)) / rank_X
    cooks_d[["theta"]][i] <- (theta_m - theta_i) %*% solve(var_theta_m) %*% t(t(theta_m - theta_i))
    
    # multivariate DFFITS
    mDFFITS[["beta"]][i] <- (beta_m - beta_i) %*% solve(var_beta_i) %*% t(t(beta_m - beta_i)) / rank_X
    mDFFITS[["theta"]][i] <- (theta_m - theta_i) %*% solve(var_theta_i) %*% t(t(theta_m - theta_i))
    
    # 3. Change in precision of parameter estimates
    # trace of covariance matrix
    cov_trace[["beta"]][i] <- abs(traceMatrix(solve(var_beta_m) %*% var_beta_i) - rank_X)
    cov_trace[["theta"]][i] <- abs(traceMatrix(solve(var_theta_m) %*% var_theta_i) - q)
    
    # covariance ratio
    cov_ratio[["beta"]][i] <- det(var_beta_i) / det(var_beta_m)
    cov_ratio[["theta"]][i] <- if(q == 1){ 
      var_theta_i / var_theta_m
    } else {
      det(var_theta_i) / det(var_theta_m)
    }
    
    W_stat_data <- cbind(model_i$ind[, c("Direct", "FH")], "MSE" = model_i$MSE[, c("Direct", "FH")])
    W_stat_data <- W_stat_data %>% filter(!(MSE.Direct == 0 & MSE.FH == 0))
    
    W_stat <- sum((W_stat_data$Direct - W_stat_data$FH)^2 / (W_stat_data$MSE.Direct + W_stat_data$MSE.FH))
    p_value[i] <- pchisq(W_stat, df = nrow(W_stat_data), lower.tail = FALSE)
    
    
  }
  
  check_variances <- data |> 
    select("Domain" = sav_kodas, T_dir_var, T_knnMC_var, N) |>
    mutate(zero_var = ifelse(round(T_dir_var, 0) == 0 | round(T_knnMC_var, 0) == 0,
                             "yes", 
                             "no"))
  
  covratio_interval_thresh <- c("beta" = 3*rank_X/n, "theta" = 3*q/n)
  covratio_interval <- list("beta" = c(1 - 3*rank_X/n, 1 + 3*rank_X/n),
                            "theta" = c(1 - 3*q/n, 1 + 3*q/n))
  
  
  influential <- list("beta" = !between(cov_ratio[["beta"]], 
                                        covratio_interval[["beta"]][1], 
                                        covratio_interval[["beta"]][2]),
                      "theta" = !between(cov_ratio[["theta"]], 
                                         covratio_interval[["theta"]][1], 
                                         covratio_interval[["theta"]][2]))
  
  resid_df <- data.frame("Domain" = model$framework$combined_data$sav_kodas,
                         "fitted" = model$model$fitted,
                         "residuals" = model$model$std_real_residuals) %>% 
    mutate(check_resid = !between(residuals, -1, 1))
  
  
  gamma_df <- data.frame("Domain" = model$framework$combined_data$sav_kodas,
                         "gamma" = model$model$gamma$Gamma)
  
  checks <- cbind(left_join(check_variances[, c("Domain", "zero_var")],
                            resid_df[, c("Domain", "check_resid")], by = "Domain"),
                  "influential_beta" = influential[["beta"]],
                  "influential_theta" = influential[["theta"]])
  
  
  plot_diagnostics_FH <- function(param, title_text = NULL, excluded_areas) {
    # param <- "beta"
    
    palikti <- !(checks$Domain %in% excluded_areas)
    
    cooks_d_df <- data.frame(Domain = resid_df$Domain, Value = cooks_d[[param]])
    cooks_d_df <- cooks_d_df |> filter(!(Domain %in% excluded_areas))
    
    cov_ratio_df <- data.frame(Domain = resid_df$Domain, Value = cov_ratio[[param]])
    cov_ratio_df <- cov_ratio_df |> filter(!(Domain %in% excluded_areas))
    
    mDFFITS_df <- data.frame(Domain = resid_df$Domain, Value = mDFFITS[[param]])
    mDFFITS_df <- mDFFITS_df |> filter(!(Domain %in% excluded_areas))
    
    cov_trace_df <- data.frame(Domain = resid_df$Domain, Value = cov_trace[[param]])
    cov_trace_df <- cov_trace_df |> filter(!(Domain %in% excluded_areas))
    
    check_variances <- check_variances |> filter(!(Domain %in% excluded_areas))
    
    # fitted vs. std.resid (standardized by sqrt(var_dir))
    domains_p1 <- replace(checks$Domain, which(!(checks$zero_var == "yes" | checks$check_resid)), "")
    domains_p1 <- domains_p1[palikti]
    p1 <- ggplot(resid_df[palikti,], aes(x = fitted, y = residuals)) +
      theme_minimal() + 
      ylim(min(extendrange(resid_df$residuals)), max(extendrange(resid_df$residuals))) +
      geom_point(aes(shape = check_resid,
                     colour = check_resid,
                     size = data$N[palikti]), show.legend = FALSE) + 
      scale_shape_manual(values = c("FALSE" = 20, "TRUE" = 8)) +
      geom_hline(yintercept = c(-1, 0, 1),
                 colour = c("red", "blue", "red"),
                 linetype = c(2, 1, 2)) +
      
      geom_text(aes(label = domains_p1,
                    color = check_variances$zero_var),
                show.legend = FALSE,
                vjust = -0.5) +
      scale_color_manual(values = c("no" = "black", "yes" = "red", 
                                    "TRUE" = "red", "FALSE" = "black"))
    
    
    # Cook's D plot
    p2 <- ggplot(cooks_d_df, aes(x = Domain, y = Value)) +
      geom_text(aes(label = Domain, color = check_variances$zero_var), 
                vjust = -0.5, show.legend = FALSE) +
      geom_segment(aes(xend = Domain, yend = 0), color = "blue", size = 1) +
      ylim(min(extendrange(cooks_d[[param]])), max(extendrange(cooks_d[[param]]))) +
      labs(y = sprintf("Cook's D of %s", param)) +
      scale_color_manual(values = c("no" = "black", "yes" = "red")) +
      # scale_shape_manual(values = c("no" = 20, "yes" = 20))
      theme_minimal()
    
    # COVRATIO plot
    domains_p3 <- replace(checks$Domain, which(!(checks$zero_var == "yes" | influential[[param]])), "")
    domains_p3 <- domains_p3[palikti]
    
    p3 <- ggplot(cov_ratio_df, aes(x = Domain, y = Value)) +
      geom_point(aes(shape = influential[[param]][palikti],
                     colour = influential[[param]][palikti],
                     size = data$N[palikti]), show.legend = FALSE) +
      geom_text(aes(label = domains_p3, color = check_variances$zero_var), 
                vjust = -0.5, show.legend = FALSE) +
      scale_color_manual(values = c("no" = "black", "yes" = "red", 
                                    "TRUE" = "red", "FALSE" = "black")) +
      scale_shape_manual(values = c("FALSE" = 20, "TRUE" = 8)) +
      # ylim(0, max(extendrange(cov_ratio[[param]]))) +
      
      # abs(COVRATIO-1) >= 3p/n
      geom_hline(yintercept = 1 + covratio_interval_thresh[param], linetype = "dashed", color = "red") +
      geom_hline(yintercept = 1 - covratio_interval_thresh[param], linetype = "dashed", color = "red") +
      ylim(c(min(extendrange(cov_ratio[[param]])), max(extendrange(cov_ratio[[param]])))) + 
      labs(y = sprintf("COVRATIO of %s", param)) +
      theme_minimal()
    
    # mDFFITS plot
    # p3 <- ggplot(mDFFITS_df, aes(x = Domain, y = Value)) +
    #   geom_segment(aes(xend = Domain, yend = 0), color = "blue", size = 1) +
    #   geom_text(aes(label = Domain, color = check_variances$zero_var), vjust = -0.5) +
    #   scale_color_manual(values = c("no" = "black", "yes" = "red")) +
    #   ylim(0, max(extendrange(mDFFITS[[param]]))) +
    #   labs(y = sprintf("mDFFITS of %s", param)) +
    #   theme_minimal()
    
    
    # ifelse(unlist(checks |> select(contains(param)))
    # GAMMA plot
    # p4 <- ggplot(gamma_df[palikti,], aes(x = Domain, y = gamma)) +
    #   geom_point(aes(shape = resid_df$check_resid[palikti],
    #                  colour = resid_df$check_resid[palikti],
    #                  size = data$N[palikti]), show.legend = FALSE) + 
    #   
    #   geom_text(aes(label = Domain,
    #                 color = check_variances$zero_var,
    #                 alpha = ifelse(unlist(checks |> select(contains(param)))[palikti],
    #                                1.5, 1),
    #                 fontface = ifelse(unlist(checks |> select(contains(param)))[palikti],
    #                                   11, 1)),
    #             vjust = -0.5, show.legend = FALSE) +
    #   
    #   scale_color_manual(values = c("no" = "black", "yes" = "red",
    #                                 "TRUE" = "red", "FALSE" = "black")) +
    #   scale_shape_manual(values = c("FALSE" = 20, "TRUE" = 8)) + 
    #   
    #   geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") +
    #   ylim(c(min(extendrange(gamma_df$gamma)), max(extendrange(gamma_df$gamma)))) + 
    #   theme_minimal()
    
    p4 <- ggplot(gamma_df[palikti,], aes(x = gamma)) +
      geom_histogram(aes(y = ..density..), binwidth = 0.1, color = "black", fill = "gray70") +
      geom_vline(xintercept = 0.5, color = "red", linetype = "dashed") +
      labs(x = "shrinkage factor", y = "Density") +
      theme_minimal()
    
    
    
    # COVTRACE plot
    # p4 <- ggplot(cov_trace_df, aes(x = Domain, y = Value)) +
    #   geom_point() +
    #   geom_text(aes(label = Domain, color = check_variances$zero_var), 
    #             vjust = -0.5, show.legend = FALSE) +
    #   scale_color_manual(values = c("no" = "black", "yes" = "red")) +
    #   ylim(0, max(extendrange(cov_trace[[param]]))) +
    #   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    #   labs(y = sprintf("COVTRACE of %s", param)) +
    #   theme_minimal()
    
    if(!is.null(title_text)){
      main_title <- textGrob(title_text, gp = gpar(fontsize = 16, fontface = "bold"))
    } else {
      main_title <- NULL
    }
    
    # Arrange the plots in a 2x2 grid
    # gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2, top = main_title)
    
    print(ggarrange(
      p1 + theme(
        panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ),
      p2 + theme(
        panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ),
      p3 + theme(
        panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ),
      p4 + theme(
        panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      ),
      ncol = 2, nrow = 2,             
      align = "hv",                   
      # common.legend = TRUE,         
      labels = c("A", "B", "C", "D"), 
      label.x = 0.5, label.y = 1,
      font.label = list(size = 14, color = "#78003F", face = "bold"),
      legend = "bottom"  
    ))
    
    
  }
  
  if(plot){
    plot_diagnostics_FH("beta", excluded_areas = excluded_areas)
    plot_diagnostics_FH("theta", excluded_areas = excluded_areas)
  }
  
  return(list("displacement" = displacement, "cooks_d" = cooks_d,
              "mDFFITS" = mDFFITS, "cov_trace" = cov_trace,
              "cov_ratio" = cov_ratio,
              "checks" = cbind(checks, "pvalue" = p_value)))
  
}


# ******************************************************************************
# ******************************************************************************


#' @title Fay-Herriot model estimates
#' 
#' @description
#' This function returns Fay-Herriot area-level model with EBLUP estimates 
#' and model diagnostics

get_estimator_EBPLUP <- function(
    data, 
    period, 
    excluded_areas, 
    do_direct = FALSE, 
    do_MC = FALSE, 
    bootstrap_var = FALSE,
    verbose = FALSE, 
    plot_diagnostics = FALSE,
    out_of_sample = NULL
    ){
  
  output <- list()
  year_quarter <- period
  
  no_variability_areas <- which(data$area_data$T_dir == 0)
  if(length(no_variability_areas) > 0) data$area_data <- data$area_data[-no_variability_areas, ]
  
  data$area_data <- data$area_data |> filter(!(sav_kodas %in% out_of_sample))
  
  
  if(do_direct){
    
    # FH EBLUPs with direct as Hajek/HT
    # **************************************************
    
    data$area_data$T_dir <- ifelse(data$area_data$T_dir==0, 1, data$area_data$T_dir)
    dir_var <- if(data$smoothing["direct"]) {"T_dir_var_smoothed"} else {"T_dir_var"}
    
    fh_dir <- fh(fixed = T_dir ~ 1 + log(dsk_t1),
                 combined_data = data$area_data, 
                 vardir = dir_var, 
                 domains = "sav_kodas",
                 transformation = "log", 
                 backtransformation = "bc_crude",
                 MSE = TRUE)
    
    W_stat_data <- cbind(fh_dir$ind[, c("Direct", "FH")], 
                         "MSE" = fh_dir$MSE[, c("Direct", "FH")])
    W_stat_data <- W_stat_data |> filter(!(MSE.Direct == 0 & MSE.FH == 0))
    
    W_stat <- sum((W_stat_data$Direct - W_stat_data$FH)^2 / (W_stat_data$MSE.Direct + W_stat_data$MSE.FH))
    p_value_dir <- pchisq(W_stat, df = nrow(W_stat_data), lower.tail = FALSE)
    
    diagnostics_FH_stats <- diagnostics_FH(model = fh_dir, title_text = year_quarter,
                                           plot = plot_diagnostics, excluded_areas = excluded_areas)
    
    data$area_data$FH_T_dir <- fh_dir$ind$FH
    data$area_data$FH_T_dir_mse <- fh_dir$MSE$FH
    data$area_data$FH_T_dir_cv <- sqrt(data$area_data$FH_T_dir_mse)/data$area_data$FH_T_dir
    
    # cat("\ncv(FH_T_dir)% skirstinys srityse:", round(100*fivenum(data$area_data$FH_T_dir_cv), 2), "\n")
    
    stats_dir_fh <- c(round(100*fivenum(data$area_data$FH_T_dir_cv), 2),
                      "AdjR2" = summary(fh_dir)$model$model_select$AdjR2,
                      "FH_R2" = summary(fh_dir)$model$model_select$FH_R2,
                      "Total" = sum(fh_dir$ind$FH))
    names(stats_dir_fh)[1:5] <- c("min", "q1", "median", "q3", "max")
    
    if(verbose){
      cat("\ncv(FH_T_dir)% skirstinys srityse:\n")
      print(round(stats_dir_fh, 3))
    }
    diagnostics_FH_stats[["model_pvalues"]] <- c("beta" = as.numeric(summary(fh_dir)$normality$Shapiro_p),
                                                 "Brown test stat" = W_stat,
                                                 "Brown test (pvalue)" = as.numeric(p_value_dir))
    output[["model_Dir"]] <- fh_dir
    output[["diagnostics_Dir"]] <- diagnostics_FH_stats
  }
  
  
  if(do_MC){
    
    
    # FH EBLUP with direct as MC
    # ***************************
    
    data$area_data$T_knnMC <- ifelse(data$area_data$T_knnMC==0, 1, data$area_data$T_knnMC) 
    
    MC_var <- if(bootstrap_var){
      "var_bootstrap"
    }else{
      if(data$smoothing["MC"]) {"T_knnMC_var_smoothed"} else {"T_knnMC_var"}
    }
    
    
    fh_knn <- fh(fixed = T_knnMC ~ 1 + log(dsk_t1), vardir = MC_var, 
                 combined_data = data$area_data, domains = "sav_kodas",
                 transformation = "log", backtransformation = "bc_crude", MSE = TRUE)
    
    diagnostics_FH_MC_stats <- diagnostics_FH(model = fh_knn, title_text = year_quarter,
                                              plot = plot_diagnostics, excluded_areas = excluded_areas)
    
    
    # H0: EBPLUPS do not differ significantly from direct estimates 
    W_stat_data <- cbind(fh_knn$ind[, c("Direct", "FH")], "MSE" = fh_knn$MSE[, c("Direct", "FH")])
    W_stat_data <- W_stat_data %>% filter(!(MSE.Direct == 0 & MSE.FH == 0))
    
    W_stat <- sum((W_stat_data$Direct - W_stat_data$FH)^2 / (W_stat_data$MSE.Direct + W_stat_data$MSE.FH))
    p_value_kNN <- pchisq(W_stat, df = nrow(W_stat_data), lower.tail = FALSE)
    # if(W_stat > qchisq(0.95, df = nrow(W_stat_data))) print("reject H0")
    
    
    data$area_data$FH_T_knnMC <- fh_knn$ind$FH
    data$area_data$FH_T_knnMC_mse <- fh_knn$MSE$FH
    data$area_data$FH_T_knnMC_cv <- sqrt(data$area_data$FH_T_knnMC_mse)/data$area_data$FH_T_knnMC
    #data$data$FH_T_knnMC_gamma <- fh_knn$model$gamma$Gamma #pasiziurejimui
    
    stats_MC_fh <- c(round(100*fivenum(data$area_data$FH_T_knnMC_cv), 2),
                     "AdjR2" = summary(fh_knn)$model$model_select$AdjR2,
                     "FH_R2" = summary(fh_knn)$model$model_select$FH_R2,
                     "Total" = sum(fh_knn$ind$FH))
    
    names(stats_MC_fh)[1:5] <- c("min", "q1", "median", "q3", "max")
    
    if(verbose){
      cat("\ncv(FH_T_MC)% skirstinys srityse:\n")
      print(round(stats_MC_fh, 3))
    }
    
    diagnostics_FH_MC_stats[["model_pvalues"]] <- c("beta" = as.numeric(summary(fh_knn)$normality$Shapiro_p),
                                                    "Brown test stat" = W_stat,
                                                    "Brown test (pvalue)" = as.numeric(p_value_kNN))
    
    output[["model_MC"]] <- fh_knn
    output[["diagnostics_MC"]] <- diagnostics_FH_MC_stats
  }
  
  output[["area_data"]] <- data$area_data
  
  return(output)
}


