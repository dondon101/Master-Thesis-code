# ******************************************************************************
# 
#                 The main script of the Master Thesis
# 
# ******************************************************************************

rm(list=ls(all=TRUE))
options(scipen=999)


# --------------------------- Path to data and files   ------------------------
# ******************************************************************************

path_to_functions <- "C:/Users/DonatasSl/Desktop/LDV"                         
setwd(path_to_functions)


source(sprintf("%s/Masters/supplementary_functions.R", path_to_functions))

packages <- c("sf", "dplyr", "readxl", "writexl", "survey", "ggplot2", "ggpubr", 
              "tidyverse", "ISOweek", "pscl", "emdi", "openxlsx", "caret", 
              "fastDummies", "FNN", "gridExtra", "patchwork")

install_or_load_pack(packages)

source(sprintf("%s/Masters/process_workflow.R", path_to_functions))


# --------------------------------- Fixed parameteres  -------------------------
# ******************************************************************************

aggr_sm <- 1           # number of last weeks to be aggregated to create OJA
pakankamas_N_B <- 10    # 

ar_Hajek <- 1           # 1 - Hajek, 0 - Horvitz-Thompson

ar_glod_ties_var <- 0   # do variances of DIR need to be smoothed
ar_glod_MC_var <- 0     # do variances of MC need to be smoothed

rm_outl <- FALSE        # TRUE - remove outliers before kNN predictions, FALSE - otherwise
perc_outl <- 0.98       # percentile to detect outliers


# --------------------------------- Import the data ----------------------------
# ******************************************************************************

# shape file for Lithuanian municipalities
shape_SAV <- st_read("C:/Users/DonatasSl/Desktop/Kiti/LTU_SHAPE/SAV/SAV.shp")

if (ar_Hajek==1) {
  # Hajek estimates
  tyr_duom <- read_excel('duomenys2/ldv_savivaldybes.xlsx', sheet = 2)
} else {
  # HT estimates
  tyr_duom <- read_excel('duomenys2/ldv_savivaldybes.xlsx', sheet = 1)
}


# populations with sample data for all quarters
ldv_visi <- readRDS("duomenys3/ldv_visi 1.Rdata")
ldv_visi <- ldv_visi |> filter(JAR_KODAS != 306156176)


# monthly number of employees data
dsk <- readRDS("duomenys3/dsk_men.Rdata")
dsk <- plyr::rename(subset(dsk, select = -c(IM_K)), c("IM_KN" = "JAR_KODAS"))

# Mikroniuansai:
# 304774708 imone iki 09 men veiklos nevykde, bet turejo 5 DSK, 
# po to iki 2024-01 neaisku kas ivyko, nes 01men jau vel priskiriama prie 
# veikianciu ir turi 0 DSK
# nuo 2023-08 iki 2024-01 irasau 0
dsk[dsk$JAR_KODAS == 304774708, paste0("m2023_", 1:8)] <- 5
dsk[dsk$JAR_KODAS == 304774708, paste0("m2023_", 9:12)] <- 0


# IDs of municipalities and cities within
miestai <- read.csv("duomenys2/Miestai savivaldybes apskr.csv", 
                    encoding = "UTF-8", sep = ",")


# NP sample data
path_scrapped <- "duomenys3/imoniu_sarasas_scrapped.Rdata"
if(file.exists(path_scrapped) && file.access(path_scrapped, 4) == 0){
  imoniu_sarasas_scrapped <- readRDS(path_scrapped)
} else{
  imoniu_sarasas_scrapped <- priskirti_nuli_nuskaitytiems(im_sar0_scrapped)
  saveRDS(imoniu_sarasas_scrapped, file = "duomenys3/imoniu_sarasas_scrapped.Rdata")
  rm(im_sar0_scrapped)
}
# ******************************************************************************


# join municipalities (ID, name) with Direct estimates 
sav_kodas <- miestai[, c("sav_kodas", "sav_pavadinimas")] |> unique() |> arrange(sav_kodas)
tyr_duom <- merge(sav_kodas, tyr_duom)
tyr_duom <- tyr_duom |> arrange(sav_kodas)

# Find which week number where the last week of a given month
N_columns <- grep("_N", names(tyr_duom), value = TRUE)

# Vector of quarter identifiers
quarter_ids <- setdiff(N_columns, c("2019_1_N", "2019_2_N", "2019_3_N"))

# Assign week numbers
week_numbers <- sapply(quarter_ids, function(id) {
  parts <- unlist(strsplit(id, "_"))
  year <- as.numeric(parts[1])
  quarter <- as.numeric(parts[2])
  get_last_full_week(year, quarter)
})

# Combine quarter IDs with week numbers
week_of_quarter <- data.frame(quarter_id = quarter_ids, week_number = week_numbers)
week_of_quarter$week_id <- paste("week", substring(week_of_quarter$quarter_id, 1, 4), 
                                 week_of_quarter$week_number, sep = "_")

N_tyr_0 <- subset(tyr_duom, select=c("sav_kodas", quarter_ids))  

# ******************************************************************************
# Create lists of populations and samples
year_q <- unique(ldv_visi$metai_q)

pop <- list()
imt <- list()

for(Q in year_q){
  
  pop[[Q]] <- subset(ldv_visi, metai_q==Q & is.na(maza))
  
  imtis <- subset(pop[[Q]], status1==1)
  imtis$w <- imtis$nn/imtis$n1
  imtis <- subset(imtis, !is.na(w))
  imtis <- plyr::rename(imtis, c("isv_1"="ldv", "n_dsk"="dsk", "status1" = "sample"))
  imtis <- subset(imtis, select=c(JAR_KODAS, sample, sav_kodas, ldv, nn, n1, sl2, w, dsk))
  
  # to ensure that the sum of design weights is equal to population size
  print(sum(imtis$w)/nrow(pop[[Q]]))
  imt[[Q]] <- imtis
}

# ******************************************************************************
# Aggregate micro data for the last few weeks of the quarter

aggr_web_scrapped <- subset(imoniu_sarasas_scrapped, select=c(jar_kodas, sav_kodas))
for(end_week_id in week_of_quarter$week_id){
  # end_week_id <- week_of_quarter$week_id[1]
  
  week <- as.numeric(substr(end_week_id, 11, 12))
  week_set <- c(week)
  for(t in 1:(aggr_sm-1)){
    week <- week - 1
    week_id_test <- paste0(substr(end_week_id, 1, 10), week)
    if (!(week_id_test %in% names(imoniu_sarasas_scrapped))){
      week <- week - 1
    }
    week_set <- c(week_set, week)
  }
  
  week_id_set <- paste0(substr(end_week_id, 1, 10), week_set)
  week_id_set <- gsub("week_(\\d{4})_(\\d{1})$", "week_\\1_0\\2", week_id_set)

  # scrapped
  temp <- data.frame(jar_kodas = imoniu_sarasas_scrapped$jar_kodas)
  temp[[end_week_id]] <- apply(imoniu_sarasas_scrapped[week_id_set], 1, function(x){
    if(all(is.na(x))) NA else sum(x, na.rm = TRUE)
  })
  
  aggr_web_scrapped <-  merge(aggr_web_scrapped, temp)
}




# ******************************************************************************
# ---------------       MODEL CALIBRATION ESTIMATOR             ---------------
# ******************************************************************************

periodai <- c("2019_4",
              paste0(rep(2020:2022, each = 4), "_", rep(1:4, times = 3)),
              "2023_1")


# Loop over the quarters and do the whole procedure for each of the quarter
# Save the bootstrap estimates, FH models (w/ Hajek and MC) in lists

save_bootstrap_estim <- list()
save_eblups_hajek <- list()
save_eblups_MC <- list()

for(quarter_id in 1:14){
  
  # fix the quarter
  fixed_period <- quarter_id

  # prepare initial data for a single period: population, p_sample, np_sample, aux. data
  init_data_results <- prepare_InitData(period = fixed_period,
                                        ketvirciai_savaites = week_of_quarter,
                                        verbose = TRUE)
  
  # summarise initial characteristics of areas
  area_data <- summarize_AreaLevel(init_data = init_data_results,
                                   threshold = pakankamas_N_B,
                                   verbose = TRUE)
  
  # find optimal k-value for kNN using 5-fold cross-validation with RMSE performance metric
  predictions_of_KNN <- prepare_and_predictKNN(
    area_data = area_data$data_Area,
    data = init_data_results$np_scrapped |> dplyr::rename(ldv_web = ldv_scrapped),
    k_values = 1:15,
    rm_outl = TRUE,
    kernel_type = NULL
  )
  
  optimal_k_fix <- predictions_of_KNN$cv_results$optimal_k$RMSE
  
  # plot the RMSE against k values
  ggplot(mapping = aes(x = predictions_of_KNN$cv_results$k_values, 
                       y = predictions_of_KNN$cv_results$cv_errors$RMSE)) +
    geom_line(color = "darkblue", size = 1) +
    geom_point(color = "darkred", size = 3) + 
    geom_vline(xintercept = optimal_k_fix, color = "#78003F", linetype = "dashed", size = 1) + 
    labs(
      x = "k",
      y = "RMSE",
      title = sprintf("t=%i", fixed_period)
    ) +
    theme_minimal(base_size = 14) + 
    theme(
      axis.title = element_text(size = 16), 
      axis.text = element_text(size = 14),  
    )
  
  
  # estimate the total in areas using MC estimator
  estimator_MC <- get_estimator_MC(web_data_predictions = predictions_of_KNN$data,
                                   area_data = area_data$data_Area,
                                   excluded_areas = area_data$excluded_areas$scrapped,
                                   sav_kodai = sav_kodas,
                                   verbose = TRUE
                                   )
  
  
  #       BOOTSTRAP
  # ********************

  # number of bootstrap replications
  B <- 1000
  bootstrap_estimates <- data.frame("sav_kodas" = sav_kodas$sav_kodas)

  for(boot in 1:B){
    cat("Bootstrap =", boot, "\n")

    imt_boot <- imt
    imt_boot[[periodai[fixed_period]]] <- bootstrap_sample(
      p_sample = imt_boot,
      period = periodai[fixed_period],
      boot = boot)

    init_data_results_boot <- prepare_InitData(period = fixed_period,
                                               ketvirciai_savaites = week_of_quarter,
                                               p_imtis = imt_boot)

    area_data_boot <- summarize_AreaLevel(init_data = init_data_results_boot,
                                          threshold = pakankamas_N_B)

    predictions_of_KNN_boot <- prepare_and_predictKNN(
      data = init_data_results_boot$np_scrapped |> dplyr::rename(ldv_web = ldv_scrapped),
      k_values = optimal_k_fix,
      rm_outl = TRUE,
      kernel_type = NULL
    )

    estimator_MC_boot <- get_estimator_MC(
      web_data_predictions = predictions_of_KNN_boot$data,
      area_data = area_data_boot$data_Area,
      excluded_areas = area_data_boot$excluded_areas$scrapped,
      sav_kodai = sav_kodas
    )

    bootstrap_estimates <- left_join(bootstrap_estimates,
                                     estimator_MC_boot$area_data |>
                                       select(!!sym(paste0("estim_boot_", boot)) := T_knnMC, sav_kodas),
                                     by = "sav_kodas")
  }

  # save bootstrap estimates of fixed period
  save_bootstrap_estim[[fixed_period]] <- bootstrap_estimates

  
  # join bootstrap estimates with the rest area-level data
  estimate_bootstrap_all <- left_join(bootstrap_estimates, 
                                      estimator_MC$area_data,
                                      by = "sav_kodas")
  
  
  # bootstrap variance estimate
  B_used <- ncol(bootstrap_estimates) - 1
  estimate_bootstrap_all <- estimate_bootstrap_all |>
    rowwise() |>
    mutate(
      bootstrap_estimate = mean(c_across(starts_with("estim_boot")), na.rm = T),
      var_bootstrap = sum((c_across(starts_with("estim_boot")) - bootstrap_estimate)^2, na.rm = T)/B_used,
    ) |>
    ungroup() |>
    select(-starts_with("estim_boot"))
  
  
  # prepare area-level covariates for FH model
  data_FH <- prepare_FH_data(population = init_data_results$population,
                             dsk_all = dsk,
                             area_data = estimate_bootstrap_all
                             )
  
  # variance smoothing if needed;
  # if both do_direct and do_MC are zero, the output is the same as the input
  data_smoothed <- smooth_variances(data = data_FH, do_direct = 0, do_MC = 0)
  
  
  # fit FH model with Hajek/MC as direct estimate
  estimator_FH <- get_estimator_EBPLUP(data = data_smoothed,
                                       period = init_data_results$period,
                                       do_MC = TRUE,
                                       do_direct = FALSE,
                                       bootstrap_var = FALSE,
                                       verbose = TRUE,
                                       plot_diagnostics = TRUE,
                                       excluded_areas = area_data$excluded_areas$scrapped
                                       )
  
  save_eblups_hajek[[fixed_period]] <- estimator_FH
  
  estimator_FH_boot <- get_estimator_EBPLUP(data = data_smoothed,
                                            period = init_data_results$period,
                                            do_MC = TRUE,
                                            do_direct = FALSE,
                                            bootstrap_var = TRUE,
                                            verbose = TRUE,
                                            plot_diagnostics = TRUE,
                                            excluded_areas = area_data$excluded_areas$scrapped
                                            )
  
  save_eblups_MC[[fixed_period]] <- estimator_FH_boot
  
}

saveRDS(save_bootstrap_estim, file = "MASTERS/bootstrap_all_quarters.Rdata")
saveRDS(save_eblups_hajek, file = "MASTERS/eblup-hajek_all_quarters.Rdata")
saveRDS(save_eblups_MC, file = "MASTERS/eblup-MC_all_quarters.Rdata")




# ---------- 1. Overall comparison of estimators of period 2023 Q1 ----------
fixed_period <- 14

# prepare initial data for a single period: population, p_sample, np_sample, aux. data
init_data_results <- prepare_InitData(period = fixed_period, 
                                      ketvirciai_savaites = week_of_quarter,
                                      verbose = TRUE)

# summarise initial characteristics of areas
area_data <- summarize_AreaLevel(init_data = init_data_results,
                                 threshold = pakankamas_N_B,
                                 verbose = TRUE)

# find optimal k-value for kNN using 5-fold cross-validation with RMSE performance metric
predictions_of_KNN <- prepare_and_predictKNN(
  area_data = area_data$data_Area,
  data = init_data_results$np_scrapped |> dplyr::rename(ldv_web = ldv_scrapped),
  k_values = 1:20,
  rm_outl = TRUE,
  kernel_type = NULL
)

optimal_k_fix <- predictions_of_KNN$cv_results$optimal_k$RMSE

# plot the RMSE against k values
ggplot(mapping = aes(x = predictions_of_KNN$cv_results$k_values, 
           y = predictions_of_KNN$cv_results$cv_errors$RMSE)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 3) + 
  geom_vline(xintercept = optimal_k_fix, color = "#78003F", linetype = "dashed", size = 1) + 
  labs(
    x = "k",
    y = "RMSE"
  ) +
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 18), 
    axis.text = element_text(size = 16),  
  )



summary(save_eblups_hajek[[fixed_period]]$model_Dir)
save_eblups_hajek[[fixed_period]]$diagnostics_Dir$model_pvalues

summary(save_eblups_hajek[[fixed_period]]$model_MC)
save_eblups_hajek[[fixed_period]]$diagnostics_MC$model_pvalues

summary(save_eblups_MC[[fixed_period]]$model_MC)
save_eblups_MC[[fixed_period]]$diagnostics_MC$model_pvalues


compare_plot(save_eblups_MC[[fixed_period]]$area_data |>
               filter(!(sav_kodas %in% c(13, 19, 21, 29))) |>
               filter(excluded == 0) |>
               select(N, T_dir, FH_T_knnMC, T_dir_cv, FH_T_knnMC_cv),
               # select(N, T_dir, FH_T_knnMC, FH_T_dir_cv, FH_T_knnMC_cv),
             # select(N, T_knnMC, FH_T_knnMC, T_knnMC_cv, FH_T_knnMC_cv),
             name_estimator = "EBLUP(MC*)"
)

area_data_df <- data.frame(
  sav_kodas = save_eblups_hajek[[fixed_period]]$area_data$sav_kodas,
  
  # hajek
  T_dir_cv = save_eblups_hajek[[fixed_period]]$area_data$T_dir_cv,
  FH_T_dir_cv = save_eblups_hajek[[fixed_period]]$area_data$FH_T_dir_cv,
  
  # MC
  T_knnMC_cv = save_eblups_hajek[[fixed_period]]$area_data$T_knnMC_cv,
  FH_T_knnMC_cv = save_eblups_hajek[[fixed_period]]$area_data$FH_T_knnMC_cv,
  
  # MC*
  T_knnMC_cv_BOOT = apply(save_bootstrap_estim[[fixed_period]][, -1], 1, sd) / save_eblups_hajek[[fixed_period]]$area_data$T_knnMC,
  FH_T_knnMC_cv_BOOT = save_eblups_MC[[fixed_period]]$area_data$FH_T_knnMC_cv
)


filtered_data <- area_data_df %>%
  filter(save_eblups_hajek[[fixed_period]]$area_data$excluded == 0)

round(apply(filtered_data[, -1], 2, function(x) summary(x)) * 100, 2) |> t()

quality_ranges <- function(x){
  x <- na.omit(x)
  return(c(
    "good" = mean(x <= 1/6)*100,
    "sufficient" = mean(x > 1/6 & x <= 1/3)*100,
    "unreliable" = mean(x > 1/3)*100
  ))
}

round(apply(filtered_data[, -1], 2, quality_ranges), 1) |> t()

long_data <- filtered_data |> 
  pivot_longer(
    cols = c(T_dir_cv, T_knnMC_cv, FH_T_dir_cv, FH_T_knnMC_cv, T_knnMC_cv_BOOT, FH_T_knnMC_cv_BOOT),
    names_to = "Method",
    values_to = "Value"
  )

method_levels <- c("T_dir_cv", "FH_T_dir_cv", "T_knnMC_cv", "FH_T_knnMC_cv", "T_knnMC_cv_BOOT", "FH_T_knnMC_cv_BOOT")
method_labels <- c("Hajek", "EBLUP (Hajek)", "MC", "EBLUP (MC)", "MC*", "EBLUP (MC*)")
long_data$Method <- factor(long_data$Method, levels = method_levels, labels = method_labels)

ggplot(long_data, aes(x = Method, y = Value, fill = Method)) +
  geom_boxplot(outliers = FALSE) +
  labs(x = NULL, y = "CV") +
  # labs(x = NULL, y = "estimates (log scale)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Blues")







# ---------- 2. Comparison of estimators based on quality groups ----------

CV_all <- c()
for(t in 10:length(periodai)){
  temp_hajek <- save_eblups_hajek[[t]]$area_data |>
    filter(excluded == 0)
  
  temp_MC <- save_eblups_MC[[t]]$area_data |>
    filter(excluded == 0)
  
  temp_bootstrap <- save_bootstrap_estim[[t]] |> 
    filter(sav_kodas %in% temp_MC$sav_kodas)
  
  
  median_CV <- data.frame(
    sav_kodas = temp_hajek$sav_kodas,
    inter_size_rel_to_sample = temp_hajek$size_p_inter_np / temp_hajek$n,
    period = quarter_to_date(periodai[t]),
    n_size = temp_hajek$n,
    N_size = temp_hajek$N,
    
    # hajek
    T_dir_cv = temp_hajek$T_dir_cv,
    FH_T_dir_cv = temp_hajek$FH_T_dir_cv,
    
    # MC*
    FH_T_knnMC_cv_BOOT = temp_MC$FH_T_knnMC_cv
  )
  
  if(t == 11) median_CV <- mutate(median_CV, FH_T_knnMC_cv_BOOT = ifelse(sav_kodas == 33, NaN, FH_T_knnMC_cv_BOOT))
  
  CV_all <- bind_rows(CV_all, median_CV)
}

# add groups of quality labels to each estimate 
CV_all <- CV_all |> 
  mutate(
    quality_dir = case_when(
      T_dir_cv <= 1/6 ~ "good",
      T_dir_cv > 1/6 & T_dir_cv <= 1/3 ~ "sufficient",
      T_dir_cv > 1/3 ~ "unreliable"),
    quality_MC = case_when(
      FH_T_knnMC_cv_BOOT <= 1/6 ~ "good",
      FH_T_knnMC_cv_BOOT > 1/6 & FH_T_knnMC_cv_BOOT <= 1/3 ~ "sufficient",
      FH_T_knnMC_cv_BOOT > 1/3 ~ "unreliable")
  )


# addmargins(100 * prop.table(xtabs(~ quality_dir + quality_MC, data = median_CV_all, subset = period == "2023-01-01"))) |> round(digits = 1)
# prop.table(xtabs(~ sav_kodas + quality_MC, data = median_CV_all), margin = 1) * 100 - 
#   prop.table(xtabs(~ sav_kodas + quality_dir, data = median_CV_all), margin = 1) * 100


plot_data <- CV_all |>
  filter(period == "2023-01-01") |>
  pivot_longer(cols = c(T_dir_cv, FH_T_knnMC_cv_BOOT),
               names_to = "metric",
               values_to = "value")

# create faceted boxplots
ggplot(plot_data, aes(x = metric, y = value, fill = metric)) +
  geom_boxplot() +
  facet_wrap(~quality_dir) +
  labs(x = "", y = "CV") + 
  scale_x_discrete(labels = c("T_dir_cv" = "Hájek", 
                              "FH_T_knnMC_cv_BOOT" = "EBLUP(MC*)")) +
  scale_fill_manual(values = c("T_dir_cv" = "deepskyblue3", 
                               "FH_T_knnMC_cv_BOOT" = "lightblue")) + 
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 15),
    strip.text = element_text(size = 18),
    legend.position = "none"
  )



# ---------- 3. Comparison of estimators over multiple quarters ----------
# CV distribution for two estimators (for LaTeX table)
CV_all |>
  group_by(period) |>
  summarize(
    # Summaries for DIR
    T_dir_cv_min   = 100 * min(T_dir_cv, na.rm = TRUE),
    T_dir_cv_q1    = 100 * quantile(T_dir_cv, probs = 0.25, na.rm = TRUE),
    T_dir_cv_med   = 100 * median(T_dir_cv, na.rm = TRUE),
    T_dir_cv_q3    = 100 * quantile(T_dir_cv, probs = 0.75, na.rm = TRUE),
    T_dir_cv_max   = 100 * max(T_dir_cv, na.rm = TRUE),
    
    # Summaries for FH(MC*)
    MC_cv_min   = 100 * min(FH_T_knnMC_cv_BOOT, na.rm = TRUE),
    MC_cv_q1    = 100 * quantile(FH_T_knnMC_cv_BOOT, probs = 0.25, na.rm = TRUE),
    MC_cv_med   = 100 * median(FH_T_knnMC_cv_BOOT, na.rm = TRUE),
    MC_cv_q3    = 100 * quantile(FH_T_knnMC_cv_BOOT, probs = 0.75, na.rm = TRUE),
    MC_cv_max   = 100 * max(FH_T_knnMC_cv_BOOT, na.rm = TRUE)
  )



# boxplot of estimators: Hajek, EBLUP(Hajek) and EBLUP(MC)
# ************************************************************
CV_all_long <- pivot_longer(
  CV_all, 
  cols = contains("cv"),
  # cols = -c(period, sav_kodas, inter_size_rel_to_sample, quality_dir, quality_MC, n_size),
  names_to = "estimator", values_to = "median_CV"
)

CV_all_long <- CV_all_long |> 
  mutate(estimator = recode(estimator, 
                            "FH_T_dir_cv" = "EBLUP (Hájek)",
                            "FH_T_knnMC_cv_BOOT" = "EBLUP (MC*)",
                            "T_dir_cv" = "Hájek"),
         estimator = fct_relevel(estimator, "Hájek", "EBLUP (Hájek)", "EBLUP (MC*)"))

ggplot(CV_all_long, 
       aes(x = paste0(substr(period, 1, 4), "-Q", quarter(period)),
           y = median_CV,
           colour = factor(estimator))) + 
  geom_boxplot(outliers = T) + 
  scale_color_manual(values = c("Hájek" = "navyblue",
                                "EBLUP (Hájek)" = "slateblue1",
                                "EBLUP (MC*)" = "deepskyblue")) +
  labs(x = "", y = "CV", colour = "Estimator:") + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    )


# quality proportions of two estimators over analyzed quarters
# ************************************************************
quality_compare <- CV_all |>
  group_by(period) |>
  reframe(across(.cols = c("T_dir_cv", "FH_T_knnMC_cv_BOOT"), 
                 .fns  = ~ quality_ranges(.x)))


quality_compare$quality <- rep(c("good", "sufficient", "unreliable"), 
                               length(10:length(periodai)))

plot_data <- pivot_longer(quality_compare, 
                          cols = c(T_dir_cv, FH_T_knnMC_cv_BOOT),
                          names_to = "metric",
                          values_to = "value")

plot_data <- mutate(plot_data, 
                    metric = recode(metric, 
                           "FH_T_knnMC_cv_BOOT" = "EBLUP (MC*)",
                           "T_dir_cv" = "Hájek"),
                    metric = fct_relevel(metric, "Hájek", "EBLUP (MC*)"))

ggplot(plot_data, 
       aes(x = paste0(substr(period, 1, 4), "-Q", quarter(period)), 
           y = value, fill = quality)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_brewer(palette = "BuPu") +
  facet_wrap(~metric) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 15),
    strip.text = element_text(size = 12)
    ) +
  labs(x = "", y = "proportion of CV", fill = "Quality:")


tapply_result <- tapply(
  CV_all$inter_size_rel_to_sample,
  # CV_all$n/CV_all$N,
  list(CV_all$period, CV_all$quality_MC),
  mean)

df_plot <- as.data.frame.table(tapply_result, responseName = "mean_value") %>%
  rename(
    period = Var1,
    quality_MC = Var2
  )

ggplot(df_plot, aes(x = paste0(substr(period, 1, 4), "-Q", quarter(period)), 
                    y = mean_value, group = quality_MC, color = quality_MC)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("navyblue", "slateblue1", "deepskyblue")) +
  labs(
    x = "",
    y = "Avg. proportion of intersection size",
    color = "Quality:"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 14),  
    legend.title = element_text(size = 16),
    legend.position = "bottom"
  )

