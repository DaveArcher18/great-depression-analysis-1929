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
6. **Norms:** Calculate L¹ and L² norms
7. **Volatility Analysis:** Compute rolling variance and low-frequency spectral power of L^p norms
8. **Trend Detection:** Mann-Kendall tests to identify statistically significant trends in volatility measures

**Key Insight:** This approach appears to detect *instability* in the market rather than predicting crashes directly. The early warning signal lies in the rising volatility of topological complexity measures, not in the norms themselves.

## Installation

```r
# Install required packages
source("install_packages.R")
```

## Usage

```r
# Step 1: Fetch and process data
source("src/data/get_data.R")

# Step 2: Compute persistence landscape norms
source("src/features/compute_landscapes.R")

# Step 3: Spectral analysis and Mann-Kendall trend tests
source("src/models/spectral_analysis.R")

# Step 4: Generate final visualization
source("src/visualization/plot_tda_results.R")
```

## Results

Applying the Gidea & Katz methodology to the 1929 crash period, we observe statistically significant upward trends in the volatility of L^p norms during the 250 days preceding Black Tuesday:

| Metric | Kendall τ | p-value |
|--------|-----------|---------|
| Rolling Variance of Total Persistence | +0.341 | < 0.0001 |
| Low-Frequency Spectral Power | +0.398 | < 0.0001 |

These findings provide additional empirical support for the hypothesis that rising volatility in topological complexity measures may serve as an early warning indicator of market instability.

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

