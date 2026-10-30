# Spatial Dependence in Used-Car Prices: Australia

Do nearby Australian cities have similar used-car prices? This R project aggregates about 16,000 listings to city level, stabilises noisy city means with Empirical Bayes shrinkage, and tests global and local spatial autocorrelation with Moran's I.

## Method

1. **Data preparation.** Cleaning of the listings, extraction of city and state, geocoding of unique city-state pairs with OpenStreetMap (`tidygeocoder`), merging of duplicate place names and checks for failed or identical coordinates.
2. **Empirical Bayes shrinkage.** City mean prices are pulled towards the national mean in proportion to their unreliability (few listings), using the within-city and between-city variance. Cities with fewer than 4 listings are treated as noisy.
3. **Spatial structure.** Cities are nodes in a k-nearest-neighbour graph with row-standardised weights. The number of neighbours k is chosen by comparing Moran's I, its z-score and whether the graph is connected for k between 3 and 21.
4. **Global Moran's I.** Tested analytically and with 999 Monte Carlo permutations.
5. **Local Moran's I (LISA).** Classification into High-High, Low-Low, High-Low and Low-High cities, with analytical and permutation-based significance, overall and by state.

## Results

- The final sample has 478 cities (407 reliable, 71 noisy); the national mean price is 32,601 AUD.
- With k = 13, a fully connected neighbour graph with one of the largest z-scores among the connected configurations, global Moran's I is 0.112 (z = 7.56, analytical p = 2.0e-14, Monte Carlo p = 0.001): a moderate but statistically significant positive spatial autocorrelation of prices.
- Smaller k gives larger Moran's I (0.186 at k = 3), but the neighbour graph is then disconnected.
- Local clusters are concentrated in New South Wales.

## Reproduce

1. Download the Australian Vehicle Prices dataset from Kaggle (listings from 2023) and save it as `Australian Vehicle Prices.csv` in the project root.
2. Install the R packages used by the script:

```r
install.packages(c("tidyverse", "tidygeocoder", "sf", "spdep", "geosphere", "maps",
                   "data.table", "lattice", "forcats", "scales"))
```

3. Run from the project root: `Rscript R/spatial_autocorrelation_australia.R`.

The script writes tables (`table_*.csv`) and figures (`figure_*.png`) to the working directory. Geocoding queries a public service and takes some time; the geocoded coordinates are saved to `cities_coords_raw.rds` so that it does not need to be repeated.

## Data

The data are not included. Check the licence on the Kaggle dataset page before reuse.

## Context and author

Student research project (Research Task in Statistics) in the MSc Transportation Economics programme at TU Dresden, 2026. Author: Ali Mirza. Released under the MIT licence.
