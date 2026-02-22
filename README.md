# N02BE-Demand-Planning (R)

This repository contains a reproducible analytics workflow using four provided sales datasets at different granularities (hourly, daily, weekly, monthly).



## Project structure

- `data/`
  - `saleshourly.csv`
  - `salesdaily.csv`
  - `salesweekly.csv`
  - `salesmonthly.csv` (main dataset used for detailed analysis)
  
  - `01_analysis.R` (main reproducible pipeline)
  - `legacy_pharma_final_session_info.txt` (archived original file)
- `outputs/`
  - `plots/` (saved figures)
  - `tables/` (saved CSV summary tables)
- `report/`
  - `report.Rmd` (optional report skeleton aligned to assignment rubric)
- `.gitignore`
- `README.md`

## What the analysis does

`01_analysis.R` covers the rubric end-to-end:

1. Data understanding
   - Prints dimensions, column types, and 5-row sample for monthly data.
2. Data preparation checks
   - Missing values per column.
   - Outlier detection (IQR rule) + boxplot.
   - Distribution histogram for target metric (`N02BE`).
3. Core analysis
   - Trend over time plot.
   - Top category/products summary and bar chart.
4. Evaluation + benchmark
   - Time-series split: last 20% as test.
   - Naive benchmark (last observed value).
   - Simple model: recursive moving average (`k = 3`).
   - Metrics: MAE and MAPE.
5. Outputs
   - Plots saved to `outputs/plots/`.
   - Tables saved to `outputs/tables/` (including `top_categories.csv` and `forecast_metrics.csv`).

## How to run end-to-end

From repository root:

```bash
01_analysis.R
```

If packages are missing, install:

```r
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "tibble"))
```

## Optional report knitting

A report skeleton is provided in `report/report.Rmd`.

To render:

```r
rmarkdown::render("report/report.Rmd")
```

Note: PDF rendering may require a LaTeX installation.
