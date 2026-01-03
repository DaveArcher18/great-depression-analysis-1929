# Topological Data Analysis of the 1929 Market Crash

**Replicating the Gidea & Katz (2017) methodology to detect early warning signals of the Great Depression.**

## Project Overview

This project applies Topological Data Analysis (TDA) to daily log-returns of major U.S. stock indices during 1928–1933 to detect precursors of the October 1929 crash. We replicate the methodology from:

> Gidea, M., & Katz, Y. (2017). *Topological Data Analysis of Financial Time Series: Landscapes of Crashes.* arXiv:1703.04385.

### Hypothesis

Persistence landscape norms (L¹ and L²) computed from sliding windows of log-returns exhibit significant growth prior to major market crashes, providing early warning signals.

## Data

| Index | Symbol | Description |
|-------|--------|-------------|
| DJIA | ^DJI | Dow Jones Industrial Average |
| DJTA | ^DJT | Dow Jones Transportation Average |
| S&P 90 | ^GSPC | S&P Composite (proxy for historical S&P 90) |

**Time Period:** 1928-01-01 to 1933-12-31

## Methodology

1. **Data Ingestion:** Fetch daily prices, compute log-returns
2. **Point Cloud Construction:** Sliding window (w=50 days) over d=3 time series
3. **Filtration:** Vietoris-Rips complex
4. **Persistence:** Compute persistence diagrams (H₁ homology)
5. **Landscapes:** Transform diagrams to persistence landscapes
6. **Norms:** Calculate L¹ and L² norms as crash indicators

## Installation

```r
# Install required packages
source("install_packages.R")
```

## Usage

```r
# Step 1: Fetch and process data
source("src/data/get_data.R")

# Data output: data/processed/1929_log_returns.csv
```

## Project Structure

```
great-depression-analysis-1929/
├── README.md
├── install_packages.R      # Dependency management
├── data/
│   ├── raw/                # Immutable original data
│   ├── interim/            # Intermediate transformations
│   └── processed/          # Canonical log-returns
├── src/
│   ├── data/               # Data ingestion scripts
│   ├── features/           # Point cloud & window construction
│   ├── models/             # TDA pipeline (persistence, landscapes)
│   └── visualization/      # Plotting scripts
├── notebooks/              # Exploratory analysis
├── models/                 # Saved persistence diagrams
├── reports/
│   └── figures/            # Generated visualizations
├── tests/                  # Unit tests
└── docs/                   # Reference materials
```

## Key Dates

| Date | Event |
|------|-------|
| 1929-10-24 | Black Thursday |
| 1929-10-28 | Black Monday |
| 1929-10-29 | Black Tuesday |
| 1932-07-08 | Market bottom |

## References

- Gidea, M., & Katz, Y. (2017). Topological Data Analysis of Financial Time Series: Landscapes of Crashes.
- Bubenik, P. (2015). Statistical Topological Data Analysis using Persistence Landscapes.

## License

MIT

