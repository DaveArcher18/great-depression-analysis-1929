# compute_landscapes.R
# Compute persistence landscape L^p norms following Gidea & Katz (2017)
#
# Methodology:
#   1. Sliding window over d-dimensional log-returns → point clouds
#   2. Vietoris-Rips filtration → persistence diagrams (H1 loops)
#   3. Persistence diagrams → persistence landscapes
#   4. Landscapes → L^1 and L^2 norms (instability scores)
#
# Input:  data/processed/1929_log_returns.csv
# Output: data/processed/tda_norms.csv
#         reports/figures/lp_norms_timeseries.png
#
# Usage: source("src/features/compute_landscapes.R")

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

if (!file.exists("README.md")) {
    stop("Please run this script from the project root directory")
}

suppressPackageStartupMessages({
    library(TDA)
    library(dplyr)
    library(readr)
    library(ggplot2)
    library(scales)
})

# Logging
log_info <- function(...) message(sprintf("[%s] %s", Sys.time(), sprintf(...)))

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

# Input/output paths
INPUT_FILE  <- "data/processed/1929_log_returns.csv"
OUTPUT_FILE <- "data/processed/tda_norms.csv"
PLOT_FILE   <- "reports/figures/lp_norms_timeseries.png"

# TDA parameters (following Gidea & Katz 2017)
WINDOW_SIZE <- 50          # Sliding window size (paper uses w=50)
MAX_DIMENSION <- 1         # We care about H1 (1-dimensional loops)
MAX_SCALE <- 0.25          # Maximum filtration radius (covers full point cloud diameter)
LANDSCAPE_KK <- 1:5        # Number of landscape functions to compute
LANDSCAPE_RESOLUTION <- 200 # Grid resolution for landscape evaluation

# Key dates for annotation
KEY_DATES <- list(
    crash_start = as.Date("1929-10-24"),   # Black Thursday
    crash_peak = as.Date("1929-10-29"),    # Black Tuesday
    recovery_start = as.Date("1930-04-17"), # Post-crash high
    bottom = as.Date("1932-07-08")          # Market bottom
)

# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------

#' Compute persistence metrics for a single point cloud
#'
#' @param cloud Matrix (w x d) representing point cloud
#' @param max_scale Maximum filtration radius
#' @return Named numeric vector with various persistence metrics
compute_persistence_metrics <- function(cloud, max_scale) {
    
    # Compute Vietoris-Rips persistence diagram
    diag_out <- ripsDiag(
        X = cloud,
        maxdimension = 1,
        maxscale = max_scale,
        library = "GUDHI",
        printProgress = FALSE
    )
    
    diagram <- diag_out$diagram
    
    # Filter for dimension 1 features only (loops)
    diag_h1 <- diagram[diagram[, 1] == 1, , drop = FALSE]
    
    # Handle case with no H1 features
    if (nrow(diag_h1) == 0) {
        return(c(
            l1_norm = 0, 
            l2_norm = 0, 
            total_persistence = 0,
            max_persistence = 0,
            n_loops = 0,
            mean_birth = NA,
            mean_death = NA
        ))
    }
    
    # Extract birth-death pairs
    births <- diag_h1[, 2]
    deaths <- diag_h1[, 3]
    persistence <- deaths - births
    
    # Method 1: Direct persistence-based metrics (more robust)
    # Total persistence (sum of lifetimes) - this is the L^1 norm of the diagram
    total_persistence <- sum(persistence)
    
    # Max persistence
    max_persistence <- max(persistence)
    
    # Method 2: Landscape-based norms
    # Create adaptive grid based on actual death values
    tseq <- seq(0, max(deaths) * 1.1, length.out = LANDSCAPE_RESOLUTION)
    
    # Compute landscape
    L <- landscape(
        Diag = diag_h1,
        dimension = 1,
        KK = LANDSCAPE_KK,
        tseq = tseq
    )
    
    # L^p norms via numerical integration
    dx <- tseq[2] - tseq[1]
    l1_norm <- sum(abs(L)) * dx
    l2_norm <- sqrt(sum(L^2) * dx)
    
    return(c(
        l1_norm = l1_norm,
        l2_norm = l2_norm,
        total_persistence = total_persistence,
        max_persistence = max_persistence,
        n_loops = nrow(diag_h1),
        mean_birth = mean(births),
        mean_death = mean(deaths)
    ))
}

#' Process all sliding windows and compute metrics
#'
#' @param data Data frame with log-returns (columns: DJI, DJT, GSPC)
#' @param dates Vector of dates
#' @param window_size Size of sliding window
#' @param max_scale Maximum filtration radius
#' @return Data frame with dates and computed metrics
compute_all_metrics <- function(data, dates, window_size, max_scale) {
    
    n_windows <- nrow(data) - window_size + 1
    
    log_info("Computing TDA metrics for %d windows (w=%d)...", n_windows, window_size)
    
    # Pre-allocate results
    results <- vector("list", n_windows)
    
    # Progress tracking
    progress_interval <- max(1, n_windows %/% 20)
    start_time <- Sys.time()
    
    for (i in seq_len(n_windows)) {
        # Extract point cloud for this window
        cloud <- as.matrix(data[i:(i + window_size - 1), ])
        
        # Compute metrics
        metrics <- compute_persistence_metrics(cloud, max_scale)
        
        # Store with corresponding date (end of window)
        results[[i]] <- c(
            date = as.character(dates[i + window_size - 1]),
            window_start = as.character(dates[i]),
            metrics
        )
        
        # Progress reporting
        if (i %% progress_interval == 0 || i == n_windows) {
            pct <- round(100 * i / n_windows)
            elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
            eta <- elapsed / i * (n_windows - i)
            log_info("  Progress: %d%% (%d/%d) - ETA: %.0f sec", 
                     pct, i, n_windows, eta)
        }
    }
    
    # Combine into data frame
    results_df <- bind_rows(lapply(results, function(x) {
        data.frame(
            date = as.Date(x["date"]),
            window_start = as.Date(x["window_start"]),
            l1_norm = as.numeric(x["l1_norm"]),
            l2_norm = as.numeric(x["l2_norm"]),
            total_persistence = as.numeric(x["total_persistence"]),
            max_persistence = as.numeric(x["max_persistence"]),
            n_loops = as.integer(x["n_loops"]),
            mean_birth = as.numeric(x["mean_birth"]),
            mean_death = as.numeric(x["mean_death"]),
            stringsAsFactors = FALSE
        )
    }))
    
    return(results_df)
}

#' Create visualization of persistence metrics time series
#'
#' @param norms_df Data frame with computed metrics
#' @param key_dates List of key dates for annotations
#' @param output_file Path to save the plot
create_norms_plot <- function(norms_df, key_dates, output_file) {
    
    log_info("Creating visualization...")
    
    # Ensure output directory exists
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    
    # Scale norms for visualization (multiply by 10^6 for readability)
    scale_factor <- 1e6
    
    # Create multi-panel plot
    p1 <- ggplot(norms_df, aes(x = date)) +
        # Total persistence (most interpretable metric)
        geom_line(aes(y = total_persistence * 1e3), 
                  color = "#2E86AB", linewidth = 0.4, alpha = 0.9) +
        geom_smooth(aes(y = total_persistence * 1e3), 
                    method = "loess", span = 0.1, 
                    color = "#1a5276", linewidth = 1, se = FALSE) +
        # Crash markers
        geom_vline(xintercept = key_dates$crash_start, 
                   linetype = "dashed", color = "red", alpha = 0.7) +
        geom_vline(xintercept = key_dates$bottom, 
                   linetype = "dotted", color = "darkgreen", alpha = 0.7) +
        labs(
            title = "Total Persistence (Sum of H₁ Lifetimes)",
            subtitle = "Higher values indicate more complex topological structure",
            y = "Total Persistence (×10⁻³)"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.title.x = element_blank())
    
    p2 <- ggplot(norms_df, aes(x = date)) +
        # L1 and L2 landscape norms
        geom_line(aes(y = l1_norm * scale_factor, color = "L¹ Norm"), 
                  linewidth = 0.4, alpha = 0.8) +
        geom_line(aes(y = l2_norm * scale_factor * 10, color = "L² Norm (×10)"), 
                  linewidth = 0.4, alpha = 0.8) +
        # Crash markers
        geom_vline(xintercept = key_dates$crash_start, 
                   linetype = "dashed", color = "red", alpha = 0.7) +
        geom_vline(xintercept = key_dates$bottom, 
                   linetype = "dotted", color = "darkgreen", alpha = 0.7) +
        scale_color_manual(
            values = c("L¹ Norm" = "#A23B72", "L² Norm (×10)" = "#F18F01"),
            name = NULL
        ) +
        labs(
            title = "Persistence Landscape Norms",
            y = "Norm Value (×10⁻⁶)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
            legend.position = "bottom",
            axis.title.x = element_blank()
        )
    
    p3 <- ggplot(norms_df, aes(x = date)) +
        geom_line(aes(y = n_loops), color = "#6B4C9A", linewidth = 0.4, alpha = 0.8) +
        geom_smooth(aes(y = n_loops), method = "loess", span = 0.1,
                    color = "#4a235a", linewidth = 1, se = FALSE) +
        geom_vline(xintercept = key_dates$crash_start, 
                   linetype = "dashed", color = "red", alpha = 0.7) +
        geom_vline(xintercept = key_dates$bottom, 
                   linetype = "dotted", color = "darkgreen", alpha = 0.7) +
        labs(
            title = "Number of H₁ Features (Loops)",
            x = "Date",
            y = "Count"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Combine plots
    library(patchwork)
    
    combined <- p1 / p2 / p3 +
        plot_annotation(
            title = "TDA Persistence Metrics: 1929 Stock Market",
            subtitle = sprintf("Sliding window w=%d days | Indices: DJIA, DJTA, S&P | Red: Crash Start | Green: Market Bottom", 
                               WINDOW_SIZE),
            caption = "Methodology: Gidea & Katz (2017) - Topological Data Analysis of Financial Time Series",
            theme = theme(
                plot.title = element_text(size = 14, face = "bold"),
                plot.subtitle = element_text(size = 10, color = "gray40")
            )
        )
    
    # Save
    ggsave(output_file, combined, width = 14, height = 10, dpi = 150)
    
    log_info("Plot saved to: %s", output_file)
    
    return(combined)
}

# ------------------------------------------------------------------------------
# MAIN PIPELINE
# ------------------------------------------------------------------------------

main <- function() {
    log_info("=== TDA Persistence Landscape Analysis ===")
    log_info("Following Gidea & Katz (2017) methodology")
    
    # -------------------------------------------------------------------------
    # Step 1: Load data
    # -------------------------------------------------------------------------
    log_info("Step 1: Loading log-returns data")
    
    data <- read_csv(INPUT_FILE, show_col_types = FALSE)
    
    log_info("  Loaded %d observations", nrow(data))
    log_info("  Date range: %s to %s", min(data$date), max(data$date))
    
    # Extract return columns and dates
    dates <- data$date
    returns <- data %>% select(DJI, DJT, GSPC)
    
    # Data quality check
    returns_sd <- sapply(returns, sd)
    log_info("  Return std devs: DJI=%.4f, DJT=%.4f, GSPC=%.4f", 
             returns_sd[1], returns_sd[2], returns_sd[3])
    
    # -------------------------------------------------------------------------
    # Step 2: Configure TDA parameters
    # -------------------------------------------------------------------------
    log_info("Step 2: TDA configuration")
    log_info("  Window size: %d days", WINDOW_SIZE)
    log_info("  Max scale: %.3f (covers ~%.1fx typical return magnitude)", 
             MAX_SCALE, MAX_SCALE / mean(returns_sd))
    
    # -------------------------------------------------------------------------
    # Step 3: Compute metrics for all windows
    # -------------------------------------------------------------------------
    log_info("Step 3: Computing persistence metrics")
    
    metrics_df <- compute_all_metrics(
        data = returns,
        dates = dates,
        window_size = WINDOW_SIZE,
        max_scale = MAX_SCALE
    )
    
    log_info("  Computed metrics for %d windows", nrow(metrics_df))
    
    # -------------------------------------------------------------------------
    # Step 4: Save results
    # -------------------------------------------------------------------------
    log_info("Step 4: Saving results")
    
    write_csv(metrics_df, OUTPUT_FILE)
    log_info("  Saved to: %s", OUTPUT_FILE)
    
    # -------------------------------------------------------------------------
    # Step 5: Summary statistics
    # -------------------------------------------------------------------------
    log_info("=== Summary Statistics ===")
    
    log_info("Total Persistence: mean=%.2e, sd=%.2e, max=%.2e",
             mean(metrics_df$total_persistence), 
             sd(metrics_df$total_persistence), 
             max(metrics_df$total_persistence))
    log_info("L¹ Norm: mean=%.2e, sd=%.2e, max=%.2e",
             mean(metrics_df$l1_norm), 
             sd(metrics_df$l1_norm), 
             max(metrics_df$l1_norm))
    log_info("L² Norm: mean=%.2e, sd=%.2e, max=%.2e",
             mean(metrics_df$l2_norm), 
             sd(metrics_df$l2_norm), 
             max(metrics_df$l2_norm))
    log_info("Loops: mean=%.1f, max=%d",
             mean(metrics_df$n_loops), max(metrics_df$n_loops))
    
    # Find peak dates
    total_peak_idx <- which.max(metrics_df$total_persistence)
    log_info("Peak total persistence: %s (value=%.2e)", 
             metrics_df$date[total_peak_idx], 
             metrics_df$total_persistence[total_peak_idx])
    
    # Analyze pre-crash behavior (250 days before Oct 29, 1929)
    crash_date <- as.Date("1929-10-29")
    pre_crash <- metrics_df %>% 
        filter(date >= crash_date - 250, date < crash_date)
    
    if (nrow(pre_crash) > 0) {
        log_info("Pre-crash period (250 days before Oct 29, 1929):")
        log_info("  Total persistence: start=%.2e, end=%.2e, ratio=%.2f",
                 pre_crash$total_persistence[1],
                 tail(pre_crash$total_persistence, 1),
                 tail(pre_crash$total_persistence, 1) / pre_crash$total_persistence[1])
    }
    
    # -------------------------------------------------------------------------
    # Step 6: Create visualization
    # -------------------------------------------------------------------------
    log_info("Step 6: Creating visualization")
    
    p <- create_norms_plot(metrics_df, KEY_DATES, PLOT_FILE)
    
    log_info("=== TDA Analysis Complete ===")
    
    invisible(list(metrics = metrics_df, plot = p))
}

# ------------------------------------------------------------------------------
# EXECUTION
# ------------------------------------------------------------------------------

result <- main()

message("\n", paste(rep("=", 60), collapse = ""))
message("✓ TDA analysis completed successfully")
message("  Output: ", OUTPUT_FILE)
message("  Plot: ", PLOT_FILE)
message(paste(rep("=", 60), collapse = ""))
