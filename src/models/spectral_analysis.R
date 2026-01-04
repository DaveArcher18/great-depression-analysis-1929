# spectral_analysis.R
# Spectral density analysis and Mann-Kendall trend tests
# Following Gidea & Katz (2017) methodology for early warning signals
#
# The paper finds that:
#   1. Average spectral density at low frequencies shows rising trend 250 days before crashes
#   2. Rolling variance (volatility of volatility) also shows upward trend
#   3. Mann-Kendall test confirms statistically significant monotonic trends
#
# Input:  data/processed/tda_norms.csv
# Output: data/processed/spectral_indicators.csv
#         reports/figures/spectral_analysis.png
#         reports/figures/precrash_trends.png
#
# Usage: source("src/models/spectral_analysis.R")

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

if (!file.exists("README.md")) {
    stop("Please run this script from the project root directory")
}

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(zoo)
    library(Kendall)
    library(ggplot2)
    library(patchwork)
})

log_info <- function(...) message(sprintf("[%s] %s", Sys.time(), sprintf(...)))

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

INPUT_FILE <- "data/processed/tda_norms.csv"
OUTPUT_FILE <- "data/processed/spectral_indicators.csv"
PLOT_FILE_1 <- "reports/figures/spectral_analysis.png"
PLOT_FILE_2 <- "reports/figures/precrash_trends.png"

# Rolling window for spectral density (paper uses ~50-100 days)
ROLLING_WINDOW <- 50

# Low frequency cutoff for spectral power (paper focuses on low frequencies)
# We'll use the lowest 10% of frequencies
LOW_FREQ_PERCENTILE <- 0.1

# Pre-crash analysis period (paper uses 250 trading days)
PRECRASH_DAYS <- 250

# Key crash date
CRASH_DATE <- as.Date("1929-10-29")  # Black Tuesday

# Additional analysis points
ANALYSIS_DATES <- list(
    crash_1929 = as.Date("1929-10-29"),
    bottom_1932 = as.Date("1932-07-08")
)

# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------

#' Compute power spectral density at low frequencies
#'
#' Uses periodogram-based spectral estimation
#'
#' @param x Numeric vector (time series)
#' @param low_freq_pct Fraction of frequencies to consider as "low"
#' @return Average power at low frequencies
compute_low_freq_power <- function(x, low_freq_pct = 0.1) {
    # Remove NA and detrend
    x <- x[!is.na(x)]
    if (length(x) < 10) return(NA)
    
    x <- x - mean(x)  # Center
    
    # Compute periodogram (raw spectral estimate)
    n <- length(x)
    spec <- abs(fft(x))^2 / n
    
    # Only use first half (positive frequencies)
    n_freq <- floor(n / 2)
    spec <- spec[2:(n_freq + 1)]  # Exclude DC component
    
    # Low frequency power (average of lowest frequencies)
    n_low <- max(1, floor(n_freq * low_freq_pct))
    low_freq_power <- mean(spec[1:n_low])
    
    return(low_freq_power)
}

#' Compute rolling spectral density
#'
#' @param x Numeric vector (time series)
#' @param window Rolling window size
#' @param low_freq_pct Low frequency percentile
#' @return Numeric vector of rolling spectral power
rolling_spectral_power <- function(x, window, low_freq_pct = 0.1) {
    n <- length(x)
    result <- rep(NA, n)
    
    for (i in window:n) {
        segment <- x[(i - window + 1):i]
        result[i] <- compute_low_freq_power(segment, low_freq_pct)
    }
    
    return(result)
}

#' Compute rolling variance
#'
#' @param x Numeric vector
#' @param window Rolling window size
#' @return Numeric vector of rolling variance
rolling_variance <- function(x, window) {
    rollapply(x, width = window, FUN = var, fill = NA, align = "right")
}

#' Run Mann-Kendall trend test
#'
#' @param x Numeric vector (time series to test)
#' @param name Name for reporting
#' @return List with test results
run_mk_test <- function(x, name = "series") {
    # Remove NAs
    x_clean <- x[!is.na(x)]
    
    if (length(x_clean) < 10) {
        return(list(
            name = name,
            tau = NA,
            p_value = NA,
            n = length(x_clean),
            significant = NA,
            trend = NA
        ))
    }
    
    # Run Mann-Kendall test
    mk_result <- MannKendall(x_clean)
    
    # Interpret
    tau <- as.numeric(mk_result$tau)
    p_value <- as.numeric(mk_result$sl)
    significant <- p_value < 0.05
    
    if (significant) {
        trend <- ifelse(tau > 0, "increasing", "decreasing")
    } else {
        trend <- "no significant trend"
    }
    
    return(list(
        name = name,
        tau = tau,
        p_value = p_value,
        n = length(x_clean),
        significant = significant,
        trend = trend
    ))
}

#' Analyze pre-event period
#'
#' @param data Data frame with date and indicator columns
#' @param event_date Date of the event
#' @param lookback Number of days to look back
#' @param indicator_cols Names of columns to analyze
#' @return Data frame with analysis results
analyze_precrash_period <- function(data, event_date, lookback, indicator_cols) {
    # Filter to pre-event period
    pre_event <- data %>%
        filter(date >= (event_date - lookback), date < event_date) %>%
        arrange(date)
    
    if (nrow(pre_event) < 50) {
        log_info("  Warning: Only %d observations in pre-event period", nrow(pre_event))
    }
    
    # Run Mann-Kendall tests on each indicator
    results <- lapply(indicator_cols, function(col) {
        mk <- run_mk_test(pre_event[[col]], col)
        data.frame(
            indicator = col,
            tau = mk$tau,
            p_value = mk$p_value,
            n = mk$n,
            significant = mk$significant,
            trend = mk$trend,
            stringsAsFactors = FALSE
        )
    })
    
    bind_rows(results)
}

#' Create spectral analysis visualization
create_spectral_plot <- function(data, output_file) {
    log_info("Creating spectral analysis plot...")
    
    # Panel 1: L1 norm with rolling mean
    p1 <- ggplot(data, aes(x = date)) +
        geom_line(aes(y = total_persistence * 1e3), 
                  color = "gray70", linewidth = 0.3, alpha = 0.7) +
        geom_line(aes(y = rolling_mean_persistence * 1e3), 
                  color = "#2E86AB", linewidth = 0.8) +
        geom_vline(xintercept = CRASH_DATE, linetype = "dashed", color = "red") +
        geom_vline(xintercept = as.Date("1932-07-08"), linetype = "dotted", color = "darkgreen") +
        labs(
            title = "Total Persistence (Raw & Smoothed)",
            y = "Persistence (×10⁻³)"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.title.x = element_blank())
    
    # Panel 2: Rolling Variance (volatility of volatility)
    p2 <- ggplot(data, aes(x = date)) +
        geom_line(aes(y = rolling_var * 1e6), 
                  color = "#A23B72", linewidth = 0.5) +
        geom_vline(xintercept = CRASH_DATE, linetype = "dashed", color = "red") +
        geom_vline(xintercept = as.Date("1932-07-08"), linetype = "dotted", color = "darkgreen") +
        labs(
            title = "Rolling Variance of Persistence (Vol-of-Vol)",
            subtitle = sprintf("Window = %d days", ROLLING_WINDOW),
            y = "Variance (×10⁻⁶)"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.title.x = element_blank())
    
    # Panel 3: Rolling Spectral Power (low frequencies)
    p3 <- ggplot(data, aes(x = date)) +
        geom_line(aes(y = log10(spectral_power + 1e-20)), 
                  color = "#F18F01", linewidth = 0.5) +
        geom_vline(xintercept = CRASH_DATE, linetype = "dashed", color = "red") +
        geom_vline(xintercept = as.Date("1932-07-08"), linetype = "dotted", color = "darkgreen") +
        labs(
            title = "Low-Frequency Spectral Power",
            subtitle = sprintf("Bottom %d%% of frequencies | Window = %d days", 
                              LOW_FREQ_PERCENTILE * 100, ROLLING_WINDOW),
            x = "Date",
            y = "log₁₀(Power)"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Combine
    combined <- p1 / p2 / p3 +
        plot_annotation(
            title = "Spectral Analysis of Persistence Landscape Norms",
            subtitle = "Red: Oct 1929 Crash | Green: July 1932 Market Bottom",
            caption = "Methodology: Gidea & Katz (2017)",
            theme = theme(plot.title = element_text(size = 14, face = "bold"))
        )
    
    ggsave(output_file, combined, width = 14, height = 10, dpi = 150)
    log_info("  Saved to: %s", output_file)
    
    return(combined)
}

#' Create pre-crash trend visualization
create_precrash_plot <- function(data, crash_date, lookback, output_file) {
    log_info("Creating pre-crash trend plot...")
    
    # Define periods
    pre_crash_start <- crash_date - lookback
    
    # Filter and add normalized time
    plot_data <- data %>%
        filter(date >= pre_crash_start, date <= crash_date + 30) %>%
        mutate(
            days_to_crash = as.numeric(date - crash_date),
            period = case_when(
                days_to_crash < 0 ~ "Pre-Crash",
                TRUE ~ "Post-Crash"
            )
        )
    
    # Normalize indicators to [0,1] for comparison
    pre_only <- plot_data %>% filter(period == "Pre-Crash")
    
    normalize <- function(x) {
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }
    
    # Panel 1: Rolling variance trend
    p1 <- ggplot(pre_only, aes(x = days_to_crash, y = rolling_var * 1e6)) +
        geom_line(color = "gray60", linewidth = 0.5) +
        geom_smooth(method = "lm", color = "#A23B72", linewidth = 1, se = TRUE, alpha = 0.2) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(
            title = "Rolling Variance: Pre-Crash Trend",
            subtitle = "Linear fit shows trend direction",
            x = "Days to Crash (Oct 29, 1929)",
            y = "Variance (×10⁻⁶)"
        ) +
        theme_minimal(base_size = 11)
    
    # Panel 2: Spectral power trend  
    p2 <- ggplot(pre_only, aes(x = days_to_crash, y = log10(spectral_power + 1e-20))) +
        geom_line(color = "gray60", linewidth = 0.5) +
        geom_smooth(method = "lm", color = "#F18F01", linewidth = 1, se = TRUE, alpha = 0.2) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(
            title = "Low-Frequency Spectral Power: Pre-Crash Trend",
            x = "Days to Crash (Oct 29, 1929)",
            y = "log₁₀(Power)"
        ) +
        theme_minimal(base_size = 11)
    
    # Panel 3: Total persistence trend
    p3 <- ggplot(pre_only, aes(x = days_to_crash, y = total_persistence * 1e3)) +
        geom_line(color = "gray60", linewidth = 0.5) +
        geom_smooth(method = "lm", color = "#2E86AB", linewidth = 1, se = TRUE, alpha = 0.2) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(
            title = "Total Persistence: Pre-Crash Trend",
            x = "Days to Crash (Oct 29, 1929)",
            y = "Persistence (×10⁻³)"
        ) +
        theme_minimal(base_size = 11)
    
    # Combine
    combined <- p1 / p2 / p3 +
        plot_annotation(
            title = sprintf("Early Warning Signals: %d Days Before Oct 1929 Crash", lookback),
            subtitle = "Mann-Kendall test quantifies monotonic trend significance",
            caption = "Methodology: Gidea & Katz (2017)",
            theme = theme(plot.title = element_text(size = 14, face = "bold"))
        )
    
    ggsave(output_file, combined, width = 12, height = 10, dpi = 150)
    log_info("  Saved to: %s", output_file)
    
    return(combined)
}

# ------------------------------------------------------------------------------
# MAIN PIPELINE
# ------------------------------------------------------------------------------

main <- function() {
    log_info("=== Spectral Density Analysis & Mann-Kendall Tests ===")
    log_info("Following Gidea & Katz (2017) methodology")
    
    # -------------------------------------------------------------------------
    # Step 1: Load TDA norms data
    # -------------------------------------------------------------------------
    log_info("Step 1: Loading TDA norms data")
    
    data <- read_csv(INPUT_FILE, show_col_types = FALSE)
    log_info("  Loaded %d observations", nrow(data))
    
    # -------------------------------------------------------------------------
    # Step 2: Compute rolling indicators
    # -------------------------------------------------------------------------
    log_info("Step 2: Computing rolling indicators (window=%d)", ROLLING_WINDOW)
    
    data <- data %>%
        arrange(date) %>%
        mutate(
            # Rolling mean of total persistence
            rolling_mean_persistence = rollmean(total_persistence, ROLLING_WINDOW, 
                                                 fill = NA, align = "right"),
            
            # Rolling variance (volatility of the topological signal)
            rolling_var = rolling_variance(total_persistence, ROLLING_WINDOW),
            
            # Rolling spectral power at low frequencies
            spectral_power = rolling_spectral_power(total_persistence, ROLLING_WINDOW, 
                                                     LOW_FREQ_PERCENTILE)
        )
    
    log_info("  Rolling indicators computed")
    
    # -------------------------------------------------------------------------
    # Step 3: Save processed data
    # -------------------------------------------------------------------------
    log_info("Step 3: Saving spectral indicators")
    
    write_csv(data, OUTPUT_FILE)
    log_info("  Saved to: %s", OUTPUT_FILE)
    
    # -------------------------------------------------------------------------
    # Step 4: Mann-Kendall trend tests for pre-crash period
    # -------------------------------------------------------------------------
    log_info("Step 4: Mann-Kendall trend tests")
    log_info("  Analyzing %d days before Oct 29, 1929 crash", PRECRASH_DAYS)
    
    indicator_cols <- c("total_persistence", "rolling_var", "spectral_power", 
                        "l1_norm", "l2_norm", "n_loops")
    
    mk_results <- analyze_precrash_period(
        data = data,
        event_date = CRASH_DATE,
        lookback = PRECRASH_DAYS,
        indicator_cols = indicator_cols
    )
    
    log_info("\n=== Mann-Kendall Test Results (Pre-Crash Period) ===")
    log_info("Period: %s to %s (%d days before crash)\n",
             CRASH_DATE - PRECRASH_DAYS, CRASH_DATE, PRECRASH_DAYS)
    
    for (i in 1:nrow(mk_results)) {
        row <- mk_results[i, ]
        sig_marker <- ifelse(row$significant, "***" , "   ")
        log_info("  %s: τ = %+.4f, p = %.4f %s (%s)",
                 sprintf("%-20s", row$indicator),
                 row$tau, row$p_value, sig_marker, row$trend)
    }
    
    log_info("\n  *** = significant at p < 0.05")
    
    # -------------------------------------------------------------------------
    # Step 5: Additional analysis - 1932 bottom
    # -------------------------------------------------------------------------
    log_info("\nStep 5: Additional analysis - 1932 market bottom")
    
    bottom_date <- as.Date("1932-07-08")
    mk_results_bottom <- analyze_precrash_period(
        data = data,
        event_date = bottom_date,
        lookback = PRECRASH_DAYS,
        indicator_cols = indicator_cols
    )
    
    log_info("=== Mann-Kendall Test Results (Pre-Bottom Period) ===")
    log_info("Period: %s to %s\n", bottom_date - PRECRASH_DAYS, bottom_date)
    
    for (i in 1:nrow(mk_results_bottom)) {
        row <- mk_results_bottom[i, ]
        sig_marker <- ifelse(row$significant, "***" , "   ")
        log_info("  %s: τ = %+.4f, p = %.4f %s (%s)",
                 sprintf("%-20s", row$indicator),
                 row$tau, row$p_value, sig_marker, row$trend)
    }
    
    # -------------------------------------------------------------------------
    # Step 6: Create visualizations
    # -------------------------------------------------------------------------
    log_info("\nStep 6: Creating visualizations")
    
    create_spectral_plot(data, PLOT_FILE_1)
    create_precrash_plot(data, CRASH_DATE, PRECRASH_DAYS, PLOT_FILE_2)
    
    # -------------------------------------------------------------------------
    # Summary
    # -------------------------------------------------------------------------
    log_info("\n=== Analysis Complete ===")
    
    # Key finding summary
    log_info("\nKey Findings:")
    sig_precrash <- mk_results %>% filter(significant == TRUE)
    if (nrow(sig_precrash) > 0) {
        log_info("  Significant trends before 1929 crash:")
        for (i in 1:nrow(sig_precrash)) {
            log_info("    - %s: %s (τ=%.3f)", 
                     sig_precrash$indicator[i], 
                     sig_precrash$trend[i],
                     sig_precrash$tau[i])
        }
    } else {
        log_info("  No significant monotonic trends detected in pre-crash period")
    }
    
    invisible(list(
        data = data,
        mk_precrash = mk_results,
        mk_bottom = mk_results_bottom
    ))
}

# ------------------------------------------------------------------------------
# EXECUTION
# ------------------------------------------------------------------------------

result <- main()

message("\n", paste(rep("=", 70), collapse = ""))
message("✓ Spectral analysis completed successfully")
message("  Indicators: ", OUTPUT_FILE)
message("  Spectral plot: ", PLOT_FILE_1)
message("  Pre-crash trends: ", PLOT_FILE_2)
message(paste(rep("=", 70), collapse = ""))

