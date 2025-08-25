# --- Setup Test Script for R + VS Code ---
# Reads /data/test-data.csv, inspects it, and generates quick EDA plots.

# 1) Helper: install + load packages -----------------------------------------
install_if_missing <- function(pkgs) {
  installed <- rownames(installed.packages())
  to_install <- pkgs[!pkgs %in% installed]
  if (length(to_install)) {
    install.packages(to_install, repos = "https://cloud.r-project.org")
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

install_if_missing(c(
  "tidyverse", # ggplot2, dplyr, readr, tibble, etc.
  "data.table", # fast CSV read (fread)
  "janitor", # clean names, tabyl
  "skimr", # rich summary
  "naniar", # missingness viz
  "GGally", # correlation/pairs plots
  "lubridate", # dates, if present
  "patchwork", # simple plot layouts if needed
  "styler" # code formatting
))

# 2) Basic environment info ---------------------------------------------------
cat("\n=== R SESSION INFO ===\n")
print(R.version.string)
cat("\nLibraries loaded:\n")
print(.packages())

# 3) I/O paths ----------------------------------------------------------------
setwd("playground")
csv_path <- "data/test-data.csv"
stopifnot(file.exists(csv_path))

out_dir <- "output"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# 4) Read data ----------------------------------------------------------------
# fread is resilient to odd CSVs; convert to tibble for nice printing
DT <- data.table::fread(csv_path, na.strings = c("", "NA", "NaN", "NULL"))
df <- tibble::as_tibble(DT) |> janitor::clean_names()

cat("\n=== FILE LOADED OK ===\n")
cat("Rows:", nrow(df), "  Cols:", ncol(df), "\n\n")

# 5) Peek & schema ------------------------------------------------------------
cat("=== HEAD (first 10 rows) ===\n")
print(utils::head(df, 10))

cat("\n=== COLUMN CLASSES ===\n")
print(sapply(df, class))

# 6) Rich summary -------------------------------------------------------------
cat("\n=== SKIM SUMMARY ===\n")
print(skimr::skim(df))

# 7) Basic missingness check --------------------------------------------------
miss_any <- any(is.na(df))
cat("\nAny missing values? ", miss_any, "\n", sep = "")
if (miss_any) {
  p_miss <- naniar::gg_miss_var(df) +
    ggplot2::labs(title = "Missingness by Column")
  print(p_miss)
  ggplot2::ggsave(
    filename = file.path(plot_dir, "missingness_by_column.png"),
    p_miss,
    width = 8,
    height = 5,
    dpi = 120
  )
}

# 8) Auto-detect types for plotting ------------------------------------------
is_num <- sapply(df, is.numeric)
is_chr <- sapply(df, is.character)
is_fac <- sapply(df, is.factor)
# Treat low-cardinality character as categorical
cat_cols <- names(df)[is_fac | (is_chr & sapply(df, function(x) dplyr::n_distinct(x, na.rm = TRUE) <= 30))]
num_cols <- names(df)[is_num]

# 9) Univariate numeric hist/density (up to 3 vars) ---------------------------
if (length(num_cols) > 0) {
  for (nm in head(num_cols, 3)) {
    p_hist <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[nm]])) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::geom_density(ggplot2::aes(y = after_stat(count)), alpha = 0.2) +
      ggplot2::labs(title = paste("Distribution:", nm), x = nm, y = "Count")
    print(p_hist)
    ggplot2::ggsave(file.path(plot_dir, paste0("hist_", nm, ".png")), p_hist, width = 7, height = 4.5, dpi = 120)
  }
}

# 10) Categorical bar charts (up to 3 vars) -----------------------------------
if (length(cat_cols) > 0) {
  for (cm in head(cat_cols, 3)) {
    # Top 20 levels to avoid unreadable bars
    tab <- df |>
      dplyr::count(.data[[cm]], sort = TRUE, name = "n") |>
      dplyr::mutate(.level = as.character(.data[[cm]])) |>
      dplyr::slice_head(n = 20)

    p_bar <- ggplot2::ggplot(tab, ggplot2::aes(x = reorder(.level, n), y = n)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Top levels:", cm), x = cm, y = "Count")
    print(p_bar)
    ggplot2::ggsave(file.path(plot_dir, paste0("bar_top_levels_", cm, ".png")), p_bar, width = 7, height = 5, dpi = 120)
  }
}

# 11) Quick pairs/correlation (if ≥2 numeric cols) ----------------------------
if (length(num_cols) >= 2) {
  # Correlation heatmap (pairwise complete obs)
  num_df <- dplyr::select(df, dplyr::all_of(num_cols))
  cor_mat <- suppressWarnings(stats::cor(num_df, use = "pairwise.complete.obs"))
  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "value")

  p_cor <- ggplot2::ggplot(cor_df, ggplot2::aes(Var1, Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
    ggplot2::labs(title = "Correlation Heatmap (numeric columns)", x = NULL, y = NULL)
  print(p_cor)
  ggplot2::ggsave(file.path(plot_dir, "correlation_heatmap.png"), p_cor, width = 7, height = 6, dpi = 120)

  # Pairs plot with GGally (cap at 6 vars to keep fast)
  cols_for_pairs <- head(num_cols, 6)
  p_pairs <- GGally::ggpairs(dplyr::select(df, dplyr::all_of(cols_for_pairs)))
  print(p_pairs)
  ggplot2::ggsave(file.path(plot_dir, "pairs_plot.png"), p_pairs, width = 8, height = 8, dpi = 120)
}

# 12) Simple scatter if we have ≥2 numeric vars -------------------------------
if (length(num_cols) >= 2) {
  xcol <- num_cols[1]
  ycol <- num_cols[2]
  p_scatter <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::labs(title = paste("Scatter:", xcol, "vs", ycol), x = xcol, y = ycol)
  print(p_scatter)
  ggplot2::ggsave(file.path(plot_dir, paste0("scatter_", xcol, "_vs_", ycol, ".png")),
    p_scatter,
    width = 7, height = 5, dpi = 120
  )
}
