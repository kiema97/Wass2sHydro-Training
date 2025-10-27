################################################################################
# Script:   extract_forecast_results.R
# Author:   Arsène Wend-denda KIEMA
# Purpose:  Extract forecast, performance metrics, and leaderboard
#           results from machine learning and stats forecast outputs and save them to CSV.
# Date:     2025-10-27
################################################################################

# ---- Load Required Packages ----
library(purrr)
library(dplyr)
library(readr)
library(stringr)

# ---- Define Input Path ----
input_path <- "D:/CCR_AOS/Wass2sHydro-Training_base/outputs/CIV_SST_seasonal_forecast_stat_rf_20251019_220251.rds"

# ---- Create Output Folder ----
output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# ---- Safety Check ----
if (!file.exists(input_path)) {
  stop(glue::glue("File not found: {input_path}"))
} else {
  message(glue::glue("Loading results from: {input_path}"))
}

# ---- Load Machine Learning Results ----
frcst_results <- readRDS(input_path)

# ---- Extract Fused Results ----
message("Extracting fused predictions by model...")
fused_all <- imap(frcst_results, ~ purrr::pluck(.x, 1, "fused_by_model", .default = NULL)) %>%
  discard(is.null) %>%
  imap_dfr(~ mutate(.x, HYBAS_ID = .y, .before = 1)) %>%
  arrange(HYBAS_ID)

message(glue::glue("Extracted fused results for {n_distinct(fused_all$HYBAS_ID)} basins."))

# ---- Extract Performance Scores ----
message("Extracting performance metrics...")
performances <- imap(frcst_results, ~ purrr::pluck(.x, 1, "scores", .default = NULL)) %>%
  discard(is.null) %>%
  imap_dfr(~ mutate(.x, HYBAS_ID = .y, .before = 1)) %>%
  arrange(HYBAS_ID)

message(glue::glue("Extracted performance metrics for {n_distinct(performances$HYBAS_ID)} basins."))

# ---- Extract Leaderboard Results by Product ----
message("Extracting leaderboard results...")
performance_by_product <- map(names(frcst_results), function(.x) {
  ml_result <- frcst_results[[.x]][[1]]

  pdf <- map(names(ml_result$leaderboards), function(.y) {
    leaderboard <- ml_result$leaderboards[[.y]] %>%
      mutate(ML_MODEL = .y) %>%
      dplyr::select(-any_of(c("weight")))
  }) %>% bind_rows()

  pdf %>% mutate(HYBAS_ID = .x)
}) %>%
  bind_rows() %>%
  arrange(HYBAS_ID, ML_MODEL)

message(glue::glue("Extracted leaderboard data for {n_distinct(performance_by_product$HYBAS_ID)} basins."))


# ---- Define Timestamp for Output Filenames ----
timestamp <- ""
# ---- Save Results to CSV ----
write_csv(fused_all, file.path(output_dir, glue::glue("fused_all_{timestamp}.csv")))
write_csv(performances, file.path(output_dir, glue::glue("performances_{timestamp}.csv")))
write_csv(performance_by_product, file.path(output_dir, glue::glue("performance_by_product_{timestamp}.csv")))

