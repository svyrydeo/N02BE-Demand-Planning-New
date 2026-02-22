#!/usr/bin/env Rscript

# Pharma sales analysis: monthly N02BE

required_packages <- c("readr", "dplyr", "tidyr", "ggplot2", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
})

get_project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  script_arg <- args[grepl(file_flag, args)]

  if (length(script_arg) > 0) {
    script_path <- normalizePath(
      sub(file_flag, "", script_arg[1]),
      winslash = "/",
      mustWork = TRUE
    )
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE))
  }

  if (file.exists(file.path(getwd(), "data", "salesmonthly.csv"))) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }

  stop(
    "Could not locate project root. Run from project root or use: Rscript scripts/01_analysis.R",
    call. = FALSE
  )
}

project_root <- get_project_root()
data_dir <- file.path(project_root, "data")
plots_dir <- file.path(project_root, "outputs", "plots")
tables_dir <- file.path(project_root, "outputs", "tables")

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

data_paths <- c(
  daily = file.path(data_dir, "salesdaily.csv"),
  hourly = file.path(data_dir, "saleshourly.csv"),
  weekly = file.path(data_dir, "salesweekly.csv"),
  monthly = file.path(data_dir, "salesmonthly.csv")
)

missing_files <- data_paths[!file.exists(data_paths)]
if (length(missing_files) > 0) {
  stop("Missing required input file(s): ", paste(missing_files, collapse = ", "), call. = FALSE)
}

sales_daily <- read_csv(data_paths[["daily"]], show_col_types = FALSE)
sales_hourly <- read_csv(data_paths[["hourly"]], show_col_types = FALSE)
sales_weekly <- read_csv(data_paths[["weekly"]], show_col_types = FALSE)
sales_monthly <- read_csv(data_paths[["monthly"]], show_col_types = FALSE) %>%
  mutate(datum = as.Date(datum)) %>%
  arrange(datum)

# Target metric.
target_metric <- "N02BE"
if (!target_metric %in% names(sales_monthly)) {
  stop("Expected column 'N02BE' not found in salesmonthly.csv.", call. = FALSE)
}

dataset_inventory <- tibble(
  dataset = c("salesdaily.csv", "saleshourly.csv", "salesweekly.csv", "salesmonthly.csv"),
  rows = c(nrow(sales_daily), nrow(sales_hourly), nrow(sales_weekly), nrow(sales_monthly)),
  cols = c(ncol(sales_daily), ncol(sales_hourly), ncol(sales_weekly), ncol(sales_monthly))
)
print(dataset_inventory)

# Data understanding.
print(tibble(rows = nrow(sales_monthly), cols = ncol(sales_monthly)))

column_types <- tibble(
  column = names(sales_monthly),
  type = vapply(sales_monthly, function(x) paste(class(x), collapse = ","), character(1))
)
print(column_types)

print(slice_head(sales_monthly, n = 5))

# Data preparation checks.

missing_values <- sales_monthly %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count")

print(missing_values)
write_csv(missing_values, file.path(tables_dir, "missing_values_monthly.csv"))

target_series <- sales_monthly %>%
  transmute(datum, value = .data[[target_metric]])

target_summary_stats <- target_series %>%
  summarise(
    min = min(value, na.rm = TRUE),
    q1 = as.numeric(quantile(value, 0.25, na.rm = TRUE)),
    median = median(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    q3 = as.numeric(quantile(value, 0.75, na.rm = TRUE)),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

print(target_summary_stats)
write_csv(target_summary_stats, file.path(tables_dir, "target_summary_stats.csv"))

q1 <- as.numeric(quantile(target_series$value, 0.25, na.rm = TRUE))
q3 <- as.numeric(quantile(target_series$value, 0.75, na.rm = TRUE))
iqr_value <- q3 - q1
lower_bound <- q1 - 1.5 * iqr_value
upper_bound <- q3 + 1.5 * iqr_value

outliers <- target_series %>%
  filter(value < lower_bound | value > upper_bound)

print(tibble(
  metric = target_metric,
  lower_bound = round(lower_bound, 2),
  upper_bound = round(upper_bound, 2),
  outlier_rows = nrow(outliers)
))
if (nrow(outliers) > 0) {
  print(outliers)
}
write_csv(outliers, file.path(tables_dir, "outliers_monthly_target.csv"))

hist_plot <- ggplot(target_series, aes(x = value)) +
  geom_histogram(bins = 20, fill = "#1f77b4", color = "white") +
  labs(
    title = paste("Distribution of", target_metric, "(monthly)"),
    x = target_metric,
    y = "Count"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(plots_dir, "monthly_target_histogram.png"),
  plot = hist_plot,
  width = 8,
  height = 5,
  dpi = 300
)

box_plot <- ggplot(target_series, aes(y = value)) +
  geom_boxplot(fill = "#ff7f0e", color = "#333333", outlier.color = "red") +
  labs(
    title = paste("Outlier check (boxplot) for", target_metric),
    y = target_metric,
    x = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(plots_dir, "monthly_target_boxplot.png"),
  plot = box_plot,
  width = 7,
  height = 4,
  dpi = 300
)

# Core analysis.

monthly_long <- sales_monthly %>%
  pivot_longer(cols = -datum, names_to = "category", values_to = "sales")

top_categories <- monthly_long %>%
  group_by(category) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_sales))

top_categories_top10 <- top_categories %>%
  slice_head(n = 10)

print(top_categories_top10)
write_csv(top_categories_top10, file.path(tables_dir, "top_categories.csv"))

trend_plot <- ggplot(target_series, aes(x = datum, y = value)) +
  geom_line(color = "#2ca02c", linewidth = 0.9) +
  labs(
    title = paste("Monthly trend for", target_metric),
    x = "Date",
    y = target_metric
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(plots_dir, "monthly_target_trend.png"),
  plot = trend_plot,
  width = 9,
  height = 5,
  dpi = 300
)

top_categories_plot <- ggplot(top_categories_top10, aes(x = reorder(category, total_sales), y = total_sales)) +
  geom_col(fill = "#9467bd") +
  coord_flip() +
  labs(
    title = "Top 10 categories by total monthly sales",
    x = "Category",
    y = "Total sales"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(plots_dir, "monthly_top_categories.png"),
  plot = top_categories_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# Evaluation and benchmark.

series_complete <- target_series %>%
  filter(!is.na(value))

n_obs <- nrow(series_complete)
if (n_obs < 10) {
  stop("Not enough observations for a stable train/test split.", call. = FALSE)
}

test_size <- max(1, ceiling(n_obs * 0.20))
train_size <- n_obs - test_size

if (train_size < 3) {
  stop("Training split too small for moving-average benchmark.", call. = FALSE)
}

train <- series_complete %>% slice_head(n = train_size)
test <- series_complete %>% slice_tail(n = test_size)

naive_forecast <- rep(tail(train$value, 1), nrow(test))

moving_average_forecast <- function(train_values, horizon, window_size = 3) {
  history <- as.numeric(train_values)
  preds <- numeric(horizon)

  for (i in seq_len(horizon)) {
    k <- min(window_size, length(history))
    preds[i] <- mean(tail(history, k), na.rm = TRUE)
    history <- c(history, preds[i])
  }

  preds
}

ma_forecast <- moving_average_forecast(train$value, nrow(test), window_size = 3)

calc_metrics <- function(actual, predicted) {
  tibble(
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    MAPE = mean(abs((actual - predicted) / ifelse(actual == 0, NA_real_, actual)), na.rm = TRUE) * 100
  )
}

metrics_table <- bind_rows(
  calc_metrics(test$value, naive_forecast) %>% mutate(model = "Naive (last value)"),
  calc_metrics(test$value, ma_forecast) %>% mutate(model = "Moving average (k=3)")
) %>%
  select(model, MAE, MAPE)

print(metrics_table)
write_csv(metrics_table, file.path(tables_dir, "forecast_metrics.csv"))

forecast_plot_data <- bind_rows(
  train %>% mutate(series = "Train actual"),
  test %>% mutate(series = "Test actual"),
  tibble(datum = test$datum, value = naive_forecast, series = "Naive forecast"),
  tibble(datum = test$datum, value = ma_forecast, series = "Moving average forecast")
)

split_date <- min(test$datum)

evaluation_plot <- ggplot(forecast_plot_data, aes(x = datum, y = value, color = series)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = split_date, linetype = "dashed", color = "#555555") +
  labs(
    title = paste("Forecast evaluation for", target_metric),
    subtitle = "Dashed line marks start of test split (last 20%)",
    x = "Date",
    y = target_metric,
    color = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(plots_dir, "forecast_evaluation.png"),
  plot = evaluation_plot,
  width = 10,
  height = 5,
  dpi = 300
)

# Output paths.
print(tibble(output_type = c("plots", "tables"), path = c(plots_dir, tables_dir)))
