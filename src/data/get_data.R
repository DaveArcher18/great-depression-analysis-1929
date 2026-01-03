# get_data.R
# Fetch historical stock index data and compute log-returns for 1929 crash analysis
#
# Data Sources (all from Stooq.com - free historical data):
#   - ^DJI:  Dow Jones Industrial Average (daily since 1896)
#   - ^DJT:  Dow Jones Transportation Average (daily since 1896)
#   - ^SPX:  S&P 500 / S&P Composite (daily since 1928)
#
# Outputs: 
#   - data/raw/{dji,djt,spx}_raw.csv  (original prices)
#   - data/processed/1929_log_returns.csv (aligned log-returns)
#
# Usage: source("src/data/get_data.R")

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

# Ensure we're running from project root
if (!file.exists("README.md")) {
    stop("Please run this script from the project root directory")
}

# Load required libraries
suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(xts)
    library(zoo)
})

# Configure logging
log_info <- function(...) message(sprintf("[%s] %s", Sys.time(), sprintf(...)))
log_warn <- function(...) warning(sprintf("[%s] %s", Sys.time(), sprintf(...)), call. = FALSE)
log_error <- function(...) stop(sprintf("[%s] %s", Sys.time(), sprintf(...)), call. = FALSE)

# ------------------------------------------------------------------------------
# CONFIGURATION
# ------------------------------------------------------------------------------

# Analysis period
ANALYSIS_START <- as.Date("1928-01-01")
ANALYSIS_END   <- as.Date("1933-12-31")

# Data sources: Stooq.com (free, no API key required)
# Format: https://stooq.com/q/d/l/?s=<symbol>&d1=YYYYMMDD&d2=YYYYMMDD&i=d
DATA_SOURCES <- list(
    DJI = list(
        name = "Dow Jones Industrial Average",
        symbol = "^dji",
        source = "Stooq.com"
    ),
    DJT = list(
        name = "Dow Jones Transportation Average", 
        symbol = "^djt",
        source = "Stooq.com"
    ),
    SPX = list(
        name = "S&P 500 / S&P Composite",
        symbol = "^spx",
        source = "Stooq.com"
    )
)

# Output paths
OUTPUT_DIR  <- "data/processed"
RAW_DIR     <- "data/raw"
OUTPUT_FILE <- file.path(OUTPUT_DIR, "1929_log_returns.csv")

# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------

#' Download historical index data from Stooq.com
#'
#' @param symbol Character. Stooq symbol (e.g., "^dji")
#' @param start_date Date. Start date for data
#' @param end_date Date. End date for data
#' @return data.frame with date and close columns, or NULL on failure
download_stooq <- function(symbol, start_date, end_date) {
    # Format dates as YYYYMMDD
    d1 <- format(start_date, "%Y%m%d")
    d2 <- format(end_date, "%Y%m%d")
    
    url <- sprintf(
        "https://stooq.com/q/d/l/?s=%s&d1=%s&d2=%s&i=d",
        symbol, d1, d2
    )
    
    log_info("  Downloading from: %s", url)
    
    tryCatch({
        temp_file <- tempfile(fileext = ".csv")
        download.file(url, temp_file, mode = "wb", quiet = TRUE)
        
        # Read Stooq CSV format: Date,Open,High,Low,Close[,Volume]
        data <- read_csv(temp_file, show_col_types = FALSE)
        
        # Check for valid data
        if (nrow(data) == 0 || !"Close" %in% names(data)) {
            log_warn("  No valid data returned for %s", symbol)
            unlink(temp_file)
            return(NULL)
        }
        
        # Standardize column names and select only date/close
        result <- data %>%
            select(date = Date, close = Close) %>%
            mutate(date = as.Date(date)) %>%
            filter(!is.na(close)) %>%
            arrange(date)
        
        log_info("  Retrieved %d observations", nrow(result))
        
        unlink(temp_file)
        return(result)
        
    }, error = function(e) {
        log_warn("  Download failed: %s", e$message)
        return(NULL)
    })
}

#' Compute daily log-returns from price series
#'
#' @param prices Numeric vector of prices
#' @return Numeric vector of log-returns (length = length(prices) - 1)
compute_log_returns <- function(prices) {
    log(prices[-1] / prices[-length(prices)])
}

#' Align multiple data frames to common dates
#'
#' @param df_list Named list of data frames, each with 'date' and 'close' columns
#' @return data.frame with aligned dates and prices for all indices
align_series <- function(df_list) {
    # Start with first data frame
    result <- df_list[[1]] %>% select(date, close)
    names(result)[2] <- names(df_list)[1]
    
    # Merge each additional series
    for (i in 2:length(df_list)) {
        name <- names(df_list)[i]
        df <- df_list[[i]] %>% select(date, close)
        names(df)[2] <- name
        
        result <- result %>%
            inner_join(df, by = "date")
    }
    
    # Remove any remaining NAs
    result <- result[complete.cases(result), ]
    
    return(result)
}

# ------------------------------------------------------------------------------
# MAIN PIPELINE
# ------------------------------------------------------------------------------

main <- function() {
    log_info("=== Starting Data Ingestion Pipeline ===")
    log_info("Analysis period: %s to %s", ANALYSIS_START, ANALYSIS_END)
    log_info("Data sources: %d indices from Stooq.com", length(DATA_SOURCES))
    
    # Ensure directories exist
    if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
    if (!dir.exists(RAW_DIR)) dir.create(RAW_DIR, recursive = TRUE)
    
    # -------------------------------------------------------------------------
    # Step 1: Download all index data
    # -------------------------------------------------------------------------
    log_info("Step 1: Downloading index data from Stooq.com")
    
    raw_data <- list()
    
    for (code in names(DATA_SOURCES)) {
        info <- DATA_SOURCES[[code]]
        log_info("Fetching %s (%s)...", info$name, info$symbol)
        
        data <- download_stooq(info$symbol, ANALYSIS_START, ANALYSIS_END)
        
        if (is.null(data) || nrow(data) == 0) {
            log_error("Failed to download %s - aborting", code)
        }
        
        # Save raw data
        raw_file <- file.path(RAW_DIR, sprintf("%s_raw.csv", tolower(code)))
        write_csv(data, raw_file)
        log_info("  Saved to %s", raw_file)
        
        raw_data[[code]] <- data
    }
    
    # -------------------------------------------------------------------------
    # Step 2: Align all series to common trading days
    # -------------------------------------------------------------------------
    log_info("Step 2: Aligning series to common trading days")
    
    aligned_prices <- align_series(raw_data)
    
    log_info("Aligned data: %d common trading days", nrow(aligned_prices))
    log_info("Date range: %s to %s", 
             min(aligned_prices$date), max(aligned_prices$date))
    
    if (nrow(aligned_prices) < 100) {
        log_error("Insufficient aligned data points: %d (need >= 100)", 
                  nrow(aligned_prices))
    }
    
    # -------------------------------------------------------------------------
    # Step 3: Compute log-returns for all indices
    # -------------------------------------------------------------------------
    log_info("Step 3: Computing daily log-returns")
    
    # Extract price columns (excluding date)
    price_cols <- setdiff(names(aligned_prices), "date")
    
    # Compute log-returns for each series
    returns_list <- lapply(price_cols, function(col) {
        compute_log_returns(aligned_prices[[col]])
    })
    names(returns_list) <- price_cols
    
    # Create output dataframe
    output_df <- data.frame(
        date = aligned_prices$date[-1],  # Drop first date (no return)
        DJI = returns_list$DJI,
        DJT = returns_list$DJT,
        GSPC = returns_list$SPX  # Rename SPX to GSPC for consistency
    )
    
    log_info("Log-returns computed: %d observations", nrow(output_df))
    
    # -------------------------------------------------------------------------
    # Step 4: Save processed data
    # -------------------------------------------------------------------------
    log_info("Step 4: Saving processed data to %s", OUTPUT_FILE)
    
    write_csv(output_df, OUTPUT_FILE)
    
    log_info("Output saved: %d rows, %d columns", nrow(output_df), ncol(output_df))
    
    # -------------------------------------------------------------------------
    # Step 5: Data quality report
    # -------------------------------------------------------------------------
    log_info("=== Data Quality Report ===")
    
    # Summary statistics
    log_info("Summary Statistics:")
    for (col in c("DJI", "DJT", "GSPC")) {
        vals <- output_df[[col]]
        log_info("  %s: mean=%.6f, sd=%.6f, min=%.4f, max=%.4f",
                 col, mean(vals), sd(vals), min(vals), max(vals))
    }
    
    # Correlation matrix
    log_info("Correlation Matrix:")
    cor_matrix <- cor(output_df[, c("DJI", "DJT", "GSPC")])
    log_info("  DJI-DJT: %.4f", cor_matrix["DJI", "DJT"])
    log_info("  DJI-GSPC: %.4f", cor_matrix["DJI", "GSPC"])
    log_info("  DJT-GSPC: %.4f", cor_matrix["DJT", "GSPC"])
    
    # Key crash dates
    log_info("Key Crash Dates:")
    crash_dates <- c(
        "1929-10-24" = "Black Thursday",
        "1929-10-28" = "Black Monday",
        "1929-10-29" = "Black Tuesday"
    )
    
    for (d in names(crash_dates)) {
        d_date <- as.Date(d)
        if (d_date %in% output_df$date) {
            row <- output_df[output_df$date == d_date, ]
            log_info("  %s (%s): DJI=%.2f%%, DJT=%.2f%%, GSPC=%.2f%%",
                     d, crash_dates[d],
                     row$DJI * 100, row$DJT * 100, row$GSPC * 100)
        }
    }
    
    # Data sources attribution
    log_info("=== Data Sources ===")
    for (code in names(DATA_SOURCES)) {
        info <- DATA_SOURCES[[code]]
        log_info("  %s: %s (%s)", code, info$name, info$source)
    }
    
    log_info("=== Data Ingestion Complete ===")
    
    invisible(output_df)
}

# ------------------------------------------------------------------------------
# EXECUTION
# ------------------------------------------------------------------------------

result <- main()

# Print confirmation
message("\n", paste(rep("=", 60), collapse = ""))
message("✓ Log-returns saved to: ", OUTPUT_FILE)
message("  Observations: ", nrow(result))
message("  Date range: ", min(result$date), " to ", max(result$date))
message("  Columns: date, DJI, DJT, GSPC")
message("\n  Data Sources (all real historical data):")
message("  • DJI  - Dow Jones Industrial Average (Stooq.com)")
message("  • DJT  - Dow Jones Transportation Average (Stooq.com)")
message("  • GSPC - S&P 500 / S&P Composite (Stooq.com)")
message(paste(rep("=", 60), collapse = ""))
