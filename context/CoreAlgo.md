### Algorithm: TDA Landscapes of Crashes

This workflow outlines the exact procedure described in Gidea & Katz (2017) to detecting market instability using topological signals.

#### 1. Data Construction & Sliding Window

**Objective:** Transform raw financial data into a sequence of geometric shapes (point clouds).

* **Input:**  time series of daily log-returns (e.g.,  for DJIA, DJTA, S&P).
* 
**Procedure:** Apply a sliding window of size  (Paper uses  or ).


* **Output:** A sequence of point clouds . Each cloud contains  points in .


* **R Implementation:**
```r
# Create window of size w at index i
point_cloud <- log_returns[i:(i+w-1), ] 

```



#### 2. Rips Filtration & Persistence Diagrams

**Objective:** Detect the "birth" and "death" of 1-dimensional loops (holes) in the point cloud.

* **Procedure:** Construct the Vietoris-Rips complex  for growing radius . Track 1-dimensional homology () features (loops).


* 
**Output:** A Persistence Diagram  consisting of birth-death pairs  for each loop.


* **R Function:** `TDA::ripsDiag`
```r
# maxdimension=1 focuses on loops; maxscale needs tuning (e.g., 5-10x standard dev)
diag <- TDA::ripsDiag(X = point_cloud, maxdimension = 1, maxscale = ...)$diagram

```



#### 3. Persistence Landscapes

**Objective:** Convert the persistence diagram (which is difficult to analyze statistically) into a stable function in a Banach space.

* **Procedure:** Transform the diagram into a sequence of continuous, piecewise linear functions . The function  represents the "height" of the -th most persistent loop at parameter .


* **Output:** A landscape function .
* **R Function:** `TDA::landscape`
```r
# tseq is the domain grid (resolution) over which to evaluate the landscape
land <- TDA::landscape(Diag = diag, dimension = 1, KK = 1, tseq = grid_points)

```



#### 4.  Norm Calculation

**Objective:** Compress the complex landscape into a single scalar "instability score" for that day.

* **Procedure:** Compute the  or  norm of the landscape functions.


The paper notes that both  and  norms exhibit strong growth prior to crashes.


* **Output:** A single scalar value for the window . Repeating this for all  creates a univariate "Time Series of Norms".


* **R Implementation:**
```r
# Numerical integration (trapezoidal rule) over the grid points
l1_norm <- sum(abs(land)) * (grid_points[2] - grid_points[1])

```



#### 5. Statistical Indicators & Mann-Kendall Test

**Objective:** Quantify the rising trend in topological volatility before the crash.

* **Procedure:**
1. Take the generated "Time Series of Norms".
2. Calculate rolling statistics over a *different* lookback window (Paper uses 250 days prior to crash events):


* **Variance** (Vol of Vol)
* **Spectral Density** (Power at low frequencies)


3. Apply the **Mann-Kendall Test** to these rolling statistics to confirm a statistically significant monotonic upward trend.




* 
**Output:** A Kendall  coefficient (close to 1.0 indicates a strong rising trend).


* **R Function:** `Kendall::MannKendall` or `trend::mk.test`
```r
# Test for monotonic trend in the spectral density of the L-norms
mk_test <- Kendall::MannKendall(rolling_spectral_density)

```



---

### Suggested `src/features/compute_landscapes.R` Outline

This script implements steps 1 through 4.

```r
library(TDA)
library(dplyr)
library(parallel) # TDA is computationally expensive; parallelize windows

compute_tda_norms <- function(data, window_size = 50, resolution = 100) {
  
  # Grid for landscape evaluation (needs to cover expected birth-death range)
  # A safe range for log-returns is usually [0, 0.2] depending on volatility
  tseq <- seq(0, 0.1, length.out = resolution) 
  
  results <- mclapply(1:(nrow(data) - window_size), function(i) {
    # 1. Point Cloud
    cloud <- data[i:(i + window_size - 1), ]
    
    # 2. Rips Diagram (Dimension 1 for loops)
    # library 'TDA' computes Rips filtration
    diag_out <- ripsDiag(X = cloud, maxdimension = 1, maxscale = 0.05, 
                         library = "GUDHI", printProgress = FALSE)
    
    # Filter for dimension 1 features only
    diag_loops <- diag_out$diagram[diag_out$diagram[, 1] == 1, , drop = FALSE]
    
    if (nrow(diag_loops) == 0) {
      return(c(l1 = 0, l2 = 0))
    }
    
    # 3. Landscape
    # We sum the first k=5 landscapes (or just k=1 as per primary signal)
    # The paper often implies the total norm of the landscape sequence.
    L <- landscape(Diag = diag_loops, dimension = 1, KK = 1:5, tseq = tseq)
    
    # 4. Norms (Integration via Riemann Sum)
    dx <- tseq[2] - tseq[1]
    # Landscape output is a matrix: rows = tseq points, cols = KK depth
    l1 <- sum(colSums(abs(L)) * dx) 
    l2 <- sqrt(sum(colSums(L^2) * dx))
    
    return(c(l1 = l1, l2 = l2))
    
  }, mc.cores = detectCores() - 1)
  
  # Combine results into dataframe
  results_df <- do.call(rbind, results)
  return(as.data.frame(results_df))
}

```