library(maps)
library(geosphere)

library(lattice)

library(forcats)
library(tidyverse)
library(tidygeocoder)
library(sf)
library(maps)
library(spdep)
library(data.table)



################################################################################
#load data
getwd()
list.files()

data <- read.csv("Australian Vehicle Prices.csv")
head(data)
data %>% dim() 


################################################################################
#                           --- DATA CLEANING ---
################################################################################


data[data == "-"] <- NA

data <- data %>%
  mutate(
    # numeric FuelConsumption
    FuelConsumptionNum = FuelConsumption %>%
      str_extract("\\d+\\.?\\d*") %>%
      as.numeric(),
    # Title → ModelYear + ModelName
    ModelYear = Title %>%
      str_extract("^\\d{4}") %>%
      as.numeric(),
    ModelName = Title %>%
      str_replace("^\\d{4}\\s+", "") %>%
      str_trim(),
    # Location → City + State
    City = Location %>%
      str_extract("^[^,]+") %>%
      str_trim(),
    State = Location %>%
      str_replace("^[^,]+,", "") %>%
      str_trim(),
    # Doors + Seats numeric
    DoorNum = Doors %>%
      str_extract("\\d+") %>%
      as.numeric(),
    SeatsNum = Seats %>%
      str_extract("\\d+") %>%
      as.numeric()
  )

data <- data %>%
  mutate(
    EngineCyl_raw = str_extract(Engine, "\\d+\\s*cyl"),
    EngineVol_raw = str_extract(Engine, "\\d+\\.?\\d*\\s*L"),
    
    EngineCyl = EngineCyl_raw %>%
      str_extract("\\d+") %>%
      as.numeric(),
    EngineVol = EngineVol_raw %>%
      str_extract("\\d+\\.?\\d*") %>%
      as.numeric()
  )

names(data)

data <- data %>% select(-Engine, 
                        -FuelConsumption, 
                        -Location, 
                        -CylindersinEngine, 
                        -Doors, -Seats, 
                        -ModelYear, 
                        -EngineCyl_raw, -EngineVol_raw) #remove dople and unprocessed variables

data <- data %>%
  mutate(
    Price = as.numeric(gsub(",", "", Price)),
    Kilometres = as.numeric(gsub(",", "", Kilometres))
  )

numeric_vars <- names(data)[sapply(data, is.numeric)]
categorical_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
numeric_vars
categorical_vars
summary(data[numeric_vars])

order <- c(
  "Brand", "Model", "Car.Suv", "Title", "UsedOrNew", "Transmission",
  "DriveType", "FuelType", "ColourExtInt", "BodyType", "ModelName",
  "City", "State", "Year", "Kilometres", "FuelConsumptionNum",
  "DoorNum", "SeatsNum", "EngineCyl", "EngineVol", "Price"
) #logical order

data <- data[, c(order)]

dim(data)

#dealing with NA values
data[1]

data[1]  <- data[[1]]
data[2]  <- data[[2]]
data[3]  <- data[[3]]
data[4]  <- data[[4]]
data[5]  <- data[[5]]
data[6]  <- data[[6]]
data[7]  <- data[[7]]
data[8]  <- data[[8]]
data[9]  <- data[[9]]
data[10] <- data[[10]]
data[11] <- data[[11]]
data[12] <- data[[12]]
data[13] <- data[[13]]
data[14] <- data[[14]]
data[15] <- data[[15]]
data[16] <- data[[16]]
data[17] <- data[[17]]
data[18] <- data[[18]]
data[19] <- data[[19]]
data[20] <- data[[20]]
data[21] <- data[[21]]

nrow(data)
data <- data[1:16734,]

sum(is.na(data))
sum(is.na(data[,1]))
sum(is.na(data[,2]))
sum(is.na(data[,3]))
sum(is.na(data[,4])) 
sum(is.na(data[,5]))
sum(is.na(data[,6])) #251
sum(is.na(data[,7]))
sum(is.na(data[,8])) #637
sum(is.na(data[,9])) 
sum(is.na(data[,10]))
sum(is.na(data[,11])) 
sum(is.na(data[,12])) #450
sum(is.na(data[,13]))
sum(is.na(data[,14])) #1
sum(is.na(data[,15])) #592
sum(is.na(data[,16])) #1698
sum(is.na(data[,17])) #1604
sum(is.na(data[,18])) #1705
sum(is.na(data[,19])) #1782
sum(is.na(data[,20])) #1676
sum(is.na(data[,21])) #53 NA values in the variable

colSums(is.na(data)) #the sum of NAs for every column at once
data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  glimpse()

obs_wth_NA_vals <- data %>%
  filter(if_any(everything(), is.na))
obs_wth_NA_vals %>% dim #2537 observations in total with at least 1 NA value

data <- na.omit(data)
data %>% dim #14197 observations in total with no NA values
sum(is.na(data)) #2537 observations in total very deleted 

length(unique(data$Title)) #6946 unique car line-ups (Year included)

###used cars
sum(data$UsedOrNew == "USED") #out of 14197 observations in total, 13401 cars are used

data <- data %>% 
  filter(UsedOrNew == "USED")
dim(data)
# we will stick to 'USED' cars only
data <- data %>%
  select(-c(Car.Suv, UsedOrNew))
data %>% dim()
colnames(data)





data_log1p <- data %>%
  mutate(
    log1p_Kilometres = log1p(Kilometres),
    log1p_FuelConsumptionNum = log1p(FuelConsumptionNum),
    log1p_EngineVol = log1p(EngineVol),       
    log1p_Price = log1p(Price)
  )
log1p_vars <- c("log1p_Kilometres",
                "log1p_FuelConsumptionNum",
                "log1p_EngineVol",
                "log1p_Price",
                "Year")





################################################################################
#                   --- SPATIAL AUTOCORRELATION ANALYSIS ---
################################################################################
# =========================================================
# 0. LOAD LIBRARIES
# =========================================================
library(dplyr)
library(tidygeocoder)
library(maps)
library(stringr)

# =========================================================
# 1. PREPARE DATA
# =========================================================
table(data_log1p$State)

data_sydney <- data_log1p %>%
  filter(!State %in% c("WA", "NT", "AU-VIC"))

data <- data_sydney

# =========================================================
# 2. BUILD UNIQUE CITY-STATE LIST FOR GEOCODING
#    IMPORTANT: DO NOT STANDARDIZE CITY CASE YET
#    because you want to geocode first and merge later
# =========================================================
cities_df <- data %>%
  distinct(City, State) %>%
  mutate(address = paste(City, State, "Australia", sep = ", "))

# =========================================================
# 3. GEOCODE
# =========================================================
cities_coords_raw <- cities_df %>%
  geocode(
    address = address,
    method = "osm",
    lat = lat,
    long = long
  )

# =========================================================
# 4. CHECK FAILED GEOCODES
# =========================================================
failed_geocodes <- cities_coords_raw %>%
  filter(is.na(lat) | is.na(long))

if (nrow(failed_geocodes) > 0) {
  cat("Geocoding failed for these City-State pairs:\n")
  print(failed_geocodes)
} else {
  cat("No geocoding failures detected.\n")
}

# =========================================================
# 5. SAVE RAW GEOCODES SO YOU DON'T HAVE TO RE-RUN
# =========================================================
saveRDS(cities_coords_raw, "cities_coords_raw.rds")
write.csv(cities_coords_raw, "cities_coords_raw.csv", row.names = FALSE)

# =========================================================
# 6. MERGE DUPLICATES AFTER GEOCODING
#    Standardize names NOW, then collapse duplicates
# =========================================================
cities_coords_clean <- cities_coords_raw %>%
  mutate(
    City_std = str_to_title(str_trim(City)),
    State_std = str_to_upper(str_trim(State))
  )

# =========================================================
# 7. CHECK WHETHER STANDARDIZED DUPLICATES DISAGREE
#    If they do, do NOT collapse blindly
# =========================================================
duplicate_conflicts <- cities_coords_clean %>%
  group_by(City_std, State_std) %>%
  summarise(
    n_rows = n(),
    n_lat = n_distinct(lat),
    n_long = n_distinct(long),
    .groups = "drop"
  ) %>%
  filter(n_rows > 1 & (n_lat > 1 | n_long > 1))

if (nrow(duplicate_conflicts) > 0) {
  cat("WARNING: Some standardized duplicates have conflicting coordinates:\n")
  print(duplicate_conflicts)
} else {
  cat("No conflicting coordinates among standardized duplicates.\n")
}

# =========================================================
# 8. COLLAPSE STANDARDIZED DUPLICATES
#    This is safe if duplicate_conflicts is empty
# =========================================================
cities_coords_dedup <- cities_coords_clean %>%
  group_by(City_std, State_std) %>%
  summarise(
    lat = first(lat),
    long = first(long),
    .groups = "drop"
  ) %>%
  rename(
    City = City_std,
    State = State_std
  )

# =========================================================
# 9. SEE WHICH RAW ENTRIES WERE MERGED
# =========================================================
merged_groups <- cities_coords_clean %>%
  group_by(City_std, State_std) %>%
  summarise(
    original_entries = paste(sort(unique(City)), collapse = " | "),
    n_versions = n(),
    lat_values = n_distinct(lat),
    long_values = n_distinct(long),
    .groups = "drop"
  ) %>%
  filter(n_versions > 1)

if (nrow(merged_groups) > 0) {
  cat("These city/state names were merged after standardization:\n")
  print(merged_groups)
} else {
  cat("No duplicate city/state names needed merging.\n")
}

# =========================================================
# 10. STANDARDIZE MAIN DATA THE SAME WAY
# =========================================================
data_clean <- data %>%
  mutate(
    City = str_to_title(str_trim(City)),
    State = str_to_upper(str_trim(State))
  )

# =========================================================
# 11. REMOVE OLD COORDINATE COLUMNS IF THEY EXIST
# =========================================================
existing_coord_cols <- intersect(names(data_clean), c("lat", "long"))

if (length(existing_coord_cols) > 0) {
  data_clean <- data_clean %>%
    select(-all_of(existing_coord_cols), everything())
}

# =========================================================
# 12. JOIN CLEAN COORDINATES BACK
# =========================================================
data_clean <- data_clean %>%
  left_join(cities_coords_dedup, by = c("City", "State"))

# =========================================================
# 13. CHECK MISSING COORDINATES AFTER JOIN
# =========================================================
missing_after_join <- data_clean %>%
  filter(is.na(lat) | is.na(long)) %>%
  distinct(City, State)

if (nrow(missing_after_join) > 0) {
  cat("These City-State pairs still have missing coordinates after join:\n")
  print(missing_after_join)
} else {
  cat("All City-State pairs successfully received coordinates.\n")
}

# =========================================================
# 14. BUILD FINAL COORDINATE REFERENCE TABLE
# =========================================================
coords_full_and_cities <- data_clean %>%
  select(City, State, long, lat) %>%
  distinct()

# =========================================================
# 15. CHECK FOR IDENTICAL COORDINATES AFTER MERGING
# =========================================================
duplicate_coords_after_merge <- coords_full_and_cities %>%
  count(long, lat, sort = TRUE) %>%
  filter(n > 1)

if (nrow(duplicate_coords_after_merge) > 0) {
  cat("Some distinct City-State pairs still share identical coordinates:\n")
  print(duplicate_coords_after_merge)
  
  cat("\nAffected City-State pairs:\n")
  print(
    coords_full_and_cities %>%
      inner_join(duplicate_coords_after_merge, by = c("long", "lat")) %>%
      arrange(long, lat, State, City)
  )
} else {
  cat("No duplicate coordinates remain after merging standardized duplicates.\n")
}

# =========================================================
# 16. OPTIONAL QUICK MAP CHECK
# =========================================================
plot(
  coords_full_and_cities$long,
  coords_full_and_cities$lat,
  xlab = "Longitude",
  ylab = "Latitude",
  main = "Australian Cities (Cleaned Coordinates)"
)

maps::map(
  "world",
  regions = "Australia",
  xlim = range(coords_full_and_cities$long, na.rm = TRUE),
  ylim = range(coords_full_and_cities$lat, na.rm = TRUE),
  fill = TRUE,
  col = "lightgray",
  bg = "white"
)

points(
  coords_full_and_cities$long,
  coords_full_and_cities$lat,
  pch = 19,
  cex = 0.5
)

# =========================================================
# 17. SAVE CLEAN OUTPUTS
# =========================================================
saveRDS(cities_coords_dedup, "cities_coords_dedup.rds")
write.csv(cities_coords_dedup, "cities_coords_dedup.csv", row.names = FALSE)

saveRDS(data_clean, "data_with_coords_clean.rds")
write.csv(data_clean, "data_with_coords_clean.csv", row.names = FALSE)

saveRDS(coords_full_and_cities, "coords_full_and_cities.rds")
write.csv(coords_full_and_cities, "coords_full_and_cities.csv", row.names = FALSE)

# =========================================================
# 18. FINAL OBJECTS TO USE NEXT
# =========================================================
# Use these going forward:
# - data_clean
# - cities_coords_dedup
# - coords_full_and_cities
# =========================================================

print(failed_geocodes)
print(duplicate_conflicts)
print(merged_groups)
print(duplicate_coords_after_merge)













# =========================================================
# 0. LOAD LIBRARIES
# =========================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# =========================================================
# 1. USE CLEAN DATA
# =========================================================
data <- data_clean

# =========================================================
# 2. CITY-LEVEL SUMMARY
# =========================================================
city_stats <- data %>%
  group_by(City, State) %>%
  summarise(
    count = n(),
    mean_price = mean(Price, na.rm = TRUE),
    median_price = median(Price, na.rm = TRUE),
    var_price = var(Price, na.rm = TRUE),
    long = first(long),
    lat = first(lat),
    .groups = "drop"
  )

# =========================================================
# 3. GLOBAL STATS
# =========================================================
global_mean <- mean(data$Price, na.rm = TRUE)

# =========================================================
# 4. POOLED WITHIN-VARIANCE (sigma^2)
# =========================================================
sigma_sq <- sum(city_stats$var_price * (city_stats$count - 1), na.rm = TRUE) /
  (sum(city_stats$count) - nrow(city_stats))

# =========================================================
# 5. BETWEEN-VARIANCE (tau^2)
# =========================================================
weights <- city_stats$count
mu_hat <- weighted.mean(city_stats$mean_price, w = weights)

tau_sq <- max(
  0,
  sum(weights * (city_stats$mean_price - mu_hat)^2) / sum(weights) -
    sigma_sq / mean(weights)
)

# =========================================================
# 6. EMPIRICAL BAYES ESTIMATE
# =========================================================
city_stats <- city_stats %>%
  mutate(
    se_sq = sigma_sq / count,
    B = se_sq / (se_sq + tau_sq),
    Technique_EB = B * global_mean + (1 - B) * mean_price
  )

# =========================================================
# 7. DIAGNOSTICS
# =========================================================
cat("----- EB Diagnostics -----\n")
cat("Sigma^2 (within):", round(sigma_sq, 4), "\n")
cat("Tau^2 (between):", round(tau_sq, 4), "\n")
cat("Shrinkage (B) summary:\n")
print(summary(city_stats$B))

# =========================================================
# 8. FILTER VALID ROWS
# =========================================================
city_stats <- city_stats %>%
  filter(!is.na(long), !is.na(lat), count >= 2)

# =========================================================
# 9. RELIABILITY THRESHOLD
# =========================================================
n_star <- ceiling(sigma_sq / tau_sq)

city_stats <- city_stats %>%
  mutate(
    Reliability = ifelse(
      count >= n_star,
      paste0("Reliable (n >= ", n_star, ")"),
      paste0("Noisy (n < ", n_star, ")")
    )
  ) %>%
  mutate(
    Reliability = factor(
      Reliability,
      levels = c(
        paste0("Reliable (n >= ", n_star, ")"),
        paste0("Noisy (n < ", n_star, ")")
      )
    )
  )

cat("n* (reliability threshold):", n_star, "\n")

# =========================================================
# 10. PREPARE PLOT DATA
# =========================================================
plot_data_optimal <- city_stats %>%
  pivot_longer(
    cols = c(mean_price, Technique_EB),
    names_to = "Estimate_Type",
    values_to = "Price"
  ) %>%
  mutate(
    Estimate_Type = factor(
      Estimate_Type,
      levels = c("mean_price", "Technique_EB"),
      labels = c("Raw Mean", "EB Estimate")
    )
  )

# =========================================================
# 11. STATE-LEVEL SUMMARY FOR FACET LABELS
#     INCLUDE STATE ID IN FACET LABEL
# =========================================================
state_counts <- city_stats %>%
  group_by(State) %>%
  summarise(
    total = n(),
    noisy = sum(count < n_star),
    reliable = sum(count >= n_star),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      State, "\n",
      "Cities: ", total, " (", reliable, " R | ", noisy, " N)"
    )
  )

plot_data_final <- plot_data_optimal %>%
  left_join(
    state_counts %>% select(State, facet_label = label),
    by = "State"
  )

# =========================================================
# 12. PLOT 1 — STATE-LEVEL FACETED EB VS RAW
# =========================================================
p_state <- ggplot(
  plot_data_final,
  aes(
    x = Estimate_Type,
    y = Price,
    group = interaction(City, State),
    color = Reliability
  )
) +
  geom_line(aes(alpha = Reliability, linewidth = Reliability)) +
  geom_point(aes(alpha = Reliability), size = 0.8) +
  geom_hline(
    yintercept = global_mean,
    linetype = "dashed",
    color = "black",
    linewidth = 0.4
  ) +
  scale_y_continuous(
    breaks = sort(c(pretty(plot_data_final$Price), global_mean)),
    labels = scales::label_dollar(prefix = "$", big.mark = ",")
  ) +
  scale_color_manual(values = c("grey20", "#E63946")) +
  scale_alpha_manual(values = c(0.15, 0.8)) +
  scale_linewidth_manual(values = c(0.3, 0.8)) +
  facet_wrap(~facet_label, scales = "free_y", ncol = 3) +
  labs(
    title = "State-Level Empirical Bayes Analysis",
    subtitle = paste0(
      "Shrinkage toward Global Mean ($",
      format(round(global_mean, 2), big.mark = ","),
      ")"
    ),
    x = NULL,
    y = "Price (AUD)",
    caption = "R = Reliable (n >= n*) | N = Noisy (n < n*)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    aspect.ratio = 1.2,
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 11)
  )

print(p_state)

# =========================================================
# 13. PLOT 2 — NATIONAL EB VS RAW
# =========================================================
p_national <- ggplot(
  plot_data_optimal,
  aes(
    x = Estimate_Type,
    y = Price,
    group = interaction(City, State),
    color = Reliability
  )
) +
  geom_line(aes(alpha = Reliability, linewidth = Reliability)) +
  geom_point(aes(alpha = Reliability), size = 1) +
  geom_hline(
    yintercept = global_mean,
    linetype = "dashed",
    color = "black"
  ) +
  scale_y_continuous(
    breaks = sort(c(pretty(plot_data_optimal$Price), global_mean)),
    labels = scales::label_dollar(prefix = "$", big.mark = ",")
  ) +
  scale_color_manual(values = c("grey40", "#E63946")) +
  scale_alpha_manual(values = c(0.08, 0.9)) +
  scale_linewidth_manual(values = c(0.2, 0.7)) +
  labs(
    title = "National Empirical Bayes Shrinkage Overview",
    subtitle = paste(
      "Comparison of Raw vs. EB prices for",
      nrow(city_stats),
      "cities"
    ),
    x = NULL,
    y = "Price (AUD)",
    caption = paste0(
      "Reliable: ", sum(city_stats$count >= n_star),
      " | Noisy: ", sum(city_stats$count < n_star)
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    aspect.ratio = 1.5,
    plot.title = element_text(face = "bold")
  )

print(p_national)

# =========================================================
# 14. OPTIONAL OBJECTS FOR LATER SPATIAL ANALYSIS
# =========================================================
city_stats <- city_stats %>%
  arrange(State, City)

coords <- as.matrix(city_stats[, c("long", "lat")])
x <- as.numeric(city_stats$Technique_EB)


























# =========================================================
# OPTIMAL k SELECTION FOR MORAN'S I
# Based on final cleaned Empirical Bayes city estimates
# =========================================================

# -------------------------------
# 1. LOAD LIBRARIES
# -------------------------------
library(spdep)
library(ggplot2)
library(dplyr)

# -------------------------------
# 2. PRE-FLIGHT CHECKS
# -------------------------------
required_cols <- c("long", "lat", "Technique_EB", "City", "State", "count")
missing_cols <- setdiff(required_cols, names(city_stats))

if (length(missing_cols) > 0) {
  stop(
    paste(
      "city_stats is missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

city_stats_spatial <- city_stats %>%
  filter(
    !is.na(long),
    !is.na(lat),
    !is.na(Technique_EB),
    count >= 2
  ) %>%
  arrange(State, City)

if (nrow(city_stats_spatial) < 10) {
  stop("Too few valid cities after filtering. Check long, lat, Technique_EB, and count.")
}

coords <- as.matrix(city_stats_spatial[, c("long", "lat")])
x <- as.numeric(city_stats_spatial$Technique_EB)

if (nrow(coords) != length(x)) {
  stop("coords and x are not aligned.")
}

if (anyDuplicated(as.data.frame(coords)) > 0) {
  warning("Some cities still share identical coordinates. kNN may be sensitive to ties.")
}

n <- nrow(coords)

cat("Spatial optimization setup\n")
cat("--------------------------\n")
cat("Number of cities:", n, "\n")
cat("Variable used: Technique_EB\n\n")

# -------------------------------
# 3. DEFINE SEARCH RANGE FOR k
# -------------------------------
k_min <- 3
k_max <- floor(sqrt(n))

k_max <- max(k_max, k_min + 2)
k_max <- min(k_max, n - 1)

k_range <- k_min:k_max

cat("k range:", min(k_range), "to", max(k_range), "\n\n")

# -------------------------------
# 4. HELPER FUNCTIONS
# -------------------------------
find_first_local_max <- function(y) {
  if (length(y) < 3) return(NA_integer_)
  for (i in 2:(length(y) - 1)) {
    if (is.finite(y[i - 1]) && is.finite(y[i]) && is.finite(y[i + 1])) {
      if (y[i] >= y[i - 1] && y[i] > y[i + 1]) {
        return(i)
      }
    }
  }
  return(NA_integer_)
}

# -------------------------------
# 5. RUN MORAN'S I OVER k
# -------------------------------
results <- data.frame(
  k = k_range,
  moran_i = NA_real_,
  expectation = NA_real_,
  variance = NA_real_,
  z_score = NA_real_,
  p_value = NA_real_,
  n_components = NA_integer_,
  stringsAsFactors = FALSE
)

start_time <- Sys.time()
cat("Starting optimization...\n")

for (i in seq_along(k_range)) {
  curr_k <- k_range[i]
  
  nb_k <- knn2nb(knearneigh(coords, k = curr_k))
  lw_k <- nb2listw(nb_k, style = "W")
  
  m_test <- moran.test(x, lw_k, alternative = "greater")
  comp_k <- spdep::n.comp.nb(nb_k)$nc
  
  est <- m_test$estimate
  
  results$moran_i[i] <- unname(est["Moran I statistic"])
  results$expectation[i] <- unname(est["Expectation"])
  results$variance[i] <- unname(est["Variance"])
  results$z_score[i] <- as.numeric(m_test$statistic)
  results$p_value[i] <- as.numeric(m_test$p.value)
  results$n_components[i] <- comp_k
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  avg_per_k <- elapsed / i
  remaining <- avg_per_k * (length(k_range) - i)
  
  cat(sprintf(
    "\rProcessing k = %2d [%d/%d] | Components: %d | Elapsed: %.1fs | Est. Remaining: %.1fs",
    curr_k, i, length(k_range), comp_k, elapsed, remaining
  ))
  utils::flush.console()
}

cat("\nOptimization complete.\n\n")

# -------------------------------
# 6. SUMMARIZE CANDIDATES
# -------------------------------
global_max_idx <- which.max(results$z_score)
global_max_k <- results$k[global_max_idx]
global_max_z <- results$z_score[global_max_idx]

first_local_idx <- find_first_local_max(results$z_score)

connected_results <- results %>%
  filter(n_components == 1)

if (nrow(connected_results) > 0) {
  best_connected_idx <- which.max(connected_results$z_score)
  best_connected_k <- connected_results$k[best_connected_idx]
  best_connected_z <- connected_results$z_score[best_connected_idx]
} else {
  best_connected_k <- NA_integer_
  best_connected_z <- NA_real_
}

# -------------------------------
# 7. SELECT FINAL k
# -------------------------------
# Rule:
# 1. Prefer first local max if it is fully connected
# 2. Otherwise prefer best connected k
# 3. Otherwise fallback to first local max
# 4. Otherwise fallback to global max

if (!is.na(first_local_idx) && results$n_components[first_local_idx] == 1) {
  optimal_idx <- first_local_idx
  selection_method <- "First local maximum of Z-score among connected graphs"
} else if (!is.na(best_connected_k)) {
  optimal_idx <- which(results$k == best_connected_k)[1]
  selection_method <- "Best fully connected graph by Z-score"
} else if (!is.na(first_local_idx)) {
  optimal_idx <- first_local_idx
  selection_method <- "First local maximum of Z-score (graph not fully connected)"
} else {
  optimal_idx <- global_max_idx
  selection_method <- "Global maximum of Z-score (fallback)"
}

optimal_k <- results$k[optimal_idx]
optimal_moran_i <- results$moran_i[optimal_idx]
optimal_p_value <- results$p_value[optimal_idx]
optimal_z <- results$z_score[optimal_idx]
optimal_components <- results$n_components[optimal_idx]

cat("Optimal k selection\n")
cat("-------------------\n")
cat("Selection method:", selection_method, "\n")
cat("Optimal k:", optimal_k, "\n")
cat("Moran's I at optimal k:", round(optimal_moran_i, 4), "\n")
cat("Z-score at optimal k:", round(optimal_z, 4), "\n")
cat("p-value at optimal k:", signif(optimal_p_value, 4), "\n")
cat("Connected components at optimal k:", optimal_components, "\n\n")

cat("Reference candidates\n")
cat("--------------------\n")
cat("Global max k:", global_max_k, "| Z-score:", round(global_max_z, 4), "\n")

if (!is.na(first_local_idx)) {
  cat(
    "First local max k:", results$k[first_local_idx],
    "| Z-score:", round(results$z_score[first_local_idx], 4),
    "| Components:", results$n_components[first_local_idx], "\n"
  )
}

if (!is.na(best_connected_k)) {
  cat(
    "Best connected k:", best_connected_k,
    "| Z-score:", round(best_connected_z, 4), "\n\n"
  )
} else {
  cat("No fully connected k found in tested range.\n\n")
}

# -------------------------------
# 8. FINAL WEIGHTS USING optimal_k
# -------------------------------
nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
lw_final <- nb2listw(nb_final, style = "W")

# Analytical Moran's I
final_moran <- moran.test(x, lw_final, alternative = "greater")

# Monte Carlo Moran's I
set.seed(123)
final_moran_mc <- moran.mc(x, lw_final, nsim = 999, alternative = "greater")

cat("Final spatial significance report\n")
cat("---------------------------------\n")
cat(sprintf("Optimal k used: %d\n", optimal_k))
cat(sprintf("Connected components: %d\n", spdep::n.comp.nb(nb_final)$nc))
cat(sprintf("Moran's I Statistic: %.4f\n", final_moran$estimate[1]))
cat(sprintf("Expected Moran's I: %.4f\n", final_moran$estimate[2]))
cat(sprintf("Variance: %.6f\n", final_moran$estimate[3]))
cat(sprintf("Analytical Z-score: %.4f\n", final_moran$statistic))
cat(sprintf("Analytical p-value: %.8f\n", final_moran$p.value))
cat(sprintf("Monte Carlo p-value: %.4f\n", final_moran_mc$p.value))

if (final_moran_mc$p.value < 0.05) {
  cat("Verdict: Significant positive spatial autocorrelation detected.\n\n")
} else {
  cat("Verdict: No statistically significant spatial autocorrelation detected.\n\n")
}

# -------------------------------
# 9. PLOT 1: Z-SCORE BY k
# -------------------------------
p1 <- ggplot(results, aes(x = k, y = z_score)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = n_components == 1), size = 2) +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
  annotate(
    "label",
    x = optimal_k,
    y = optimal_z,
    label = paste0(
      "Optimal k = ", optimal_k,
      "\n", selection_method,
      "\nComponents = ", optimal_components
    ),
    vjust = -0.4
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  labs(
    title = "Spatial Scale Optimization via Moran's I",
    subtitle = "Z-score profile across k-nearest-neighbor graphs",
    x = "Number of Neighbors (k)",
    y = "Moran's I Z-score",
    caption = paste("n =", n, "| Optimal k =", optimal_k)
  ) +
  theme_minimal()

print(p1)

# -------------------------------
# 10. PLOT 2: MORAN'S I BY k
# -------------------------------
p2 <- ggplot(results, aes(x = k, y = moran_i)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = n_components == 1), size = 2) +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  labs(
    title = "Moran's I Across Neighborhood Sizes",
    subtitle = "Observed spatial autocorrelation as k varies",
    x = "Number of Neighbors (k)",
    y = "Moran's I"
  ) +
  theme_minimal()

print(p2)

# -------------------------------
# 11. OPTIONAL: INSPECT RESULTS TABLE
# -------------------------------
print(results)

# -------------------------------
# 12. FINAL OBJECTS FOR LATER USE
# -------------------------------
# Neighbor structure
# - nb_final
# - lw_final
#
# Spatial data used
# - city_stats_spatial
# - coords
# - x
#
# Optimization table
# - results
# -------------------------------


# =========================================================
# FINAL NEIGHBOR STRUCTURE FOR SPATIAL ANALYSIS
# =========================================================
optimal_k <- 13

nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
lw_final <- nb2listw(nb_final, style = "W")

# Analytical Moran's I
final_moran <- moran.test(x, lw_final, alternative = "greater")

# Monte Carlo Moran's I
set.seed(123)
final_moran_mc <- moran.mc(x, lw_final, nsim = 999, alternative = "greater")

cat("Final spatial significance report\n")
cat("---------------------------------\n")
cat(sprintf("Optimal k used: %d\n", optimal_k))
cat(sprintf("Connected components: %d\n", spdep::n.comp.nb(nb_final)$nc))
cat(sprintf("Moran's I Statistic: %.4f\n", final_moran$estimate[1]))
cat(sprintf("Expected Moran's I: %.4f\n", final_moran$estimate[2]))
cat(sprintf("Variance: %.6f\n", final_moran$estimate[3]))
cat(sprintf("Analytical Z-score: %.4f\n", final_moran$statistic))
cat(sprintf("Analytical p-value: %.8f\n", final_moran$p.value))
cat(sprintf("Monte Carlo p-value: %.4f\n", final_moran_mc$p.value))





# =========================================================
# LOCAL MORAN'S I ONLY
# Analytical + Permutation
# No multiple-testing correction
# =========================================================

# -------------------------------
# 1. LOAD LIBRARIES
# -------------------------------
library(spdep)
library(dplyr)
library(ggplot2)

# -------------------------------
# 2. PRE-FLIGHT CHECKS
# -------------------------------
required_cols <- c("long", "lat", "Technique_EB", "City", "State", "count")
missing_cols <- setdiff(required_cols, names(city_stats))

if (length(missing_cols) > 0) {
  stop(
    paste(
      "city_stats is missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

city_stats_local <- city_stats %>%
  filter(
    !is.na(long),
    !is.na(lat),
    !is.na(Technique_EB),
    count >= 2
  ) %>%
  arrange(State, City)

if (nrow(city_stats_local) < 10) {
  stop("Too few valid cities after filtering.")
}

coords <- as.matrix(city_stats_local[, c("long", "lat")])
x <- as.numeric(city_stats_local$Technique_EB)

if (nrow(coords) != length(x)) {
  stop("coords and x are not aligned.")
}

if (anyNA(x)) {
  stop("x contains NA values.")
}

if (sd(x) == 0) {
  stop("x has zero variance.")
}

# -------------------------------
# 3. SET FINAL k AND WEIGHTS
# -------------------------------
optimal_k <- 13

nb_final <- knn2nb(knearneigh(coords, k = optimal_k))
lw_final <- nb2listw(nb_final, style = "W")

cat("Local Moran setup\n")
cat("-----------------\n")
cat("Number of cities:", nrow(city_stats_local), "\n")
cat("Variable used: Technique_EB\n")
cat("k used:", optimal_k, "\n")
cat("Connected components:", spdep::n.comp.nb(nb_final)$nc, "\n\n")

# -------------------------------
# 4. STANDARDIZE VARIABLE + SPATIAL LAG
# -------------------------------
z <- as.numeric(scale(x))
lag_z <- lag.listw(lw_final, z)

# -------------------------------
# 5. ANALYTICAL LOCAL MORAN
# -------------------------------
local_moran_analytical <- localmoran(
  x,
  lw_final,
  alternative = "two.sided"
)

cat("Analytical Local Moran column names:\n")
print(colnames(local_moran_analytical))
cat("\n")

p_col_analytical <- grep("^Pr", colnames(local_moran_analytical), value = TRUE)[1]

if (is.na(p_col_analytical)) {
  stop("Could not find analytical p-value column.")
}

# -------------------------------
# 6. PERMUTATION LOCAL MORAN
# -------------------------------
set.seed(123)

local_moran_perm <- localmoran_perm(
  x,
  lw_final,
  nsim = 999,
  alternative = "two.sided"
)

cat("Permutation Local Moran column names:\n")
print(colnames(local_moran_perm))
cat("\n")

p_col_perm_sim <- grep("Sim$", colnames(local_moran_perm), value = TRUE)[1]

if (is.na(p_col_perm_sim)) {
  stop("Could not find permutation simulation p-value column.")
}

# -------------------------------
# 7. MERGE RESULTS
# -------------------------------
city_stats_local <- city_stats_local %>%
  mutate(
    z_value = z,
    lag_z = lag_z,
    
    # Analytical results
    local_I_analytical = local_moran_analytical[, "Ii"],
    local_EI_analytical = local_moran_analytical[, "E.Ii"],
    local_VI_analytical = local_moran_analytical[, "Var.Ii"],
    local_Z_analytical = local_moran_analytical[, "Z.Ii"],
    local_p_analytical = local_moran_analytical[, p_col_analytical],
    
    # Permutation results
    local_I_perm = local_moran_perm[, "Ii"],
    local_EI_perm = local_moran_perm[, "E.Ii"],
    local_VI_perm = local_moran_perm[, "Var.Ii"],
    local_Z_perm = local_moran_perm[, "Z.Ii"],
    local_p_perm = local_moran_perm[, p_col_perm_sim]
  )

# -------------------------------
# 8. CLUSTER CLASSIFICATION
# NO CORRECTION
# -------------------------------
city_stats_local <- city_stats_local %>%
  mutate(
    lisa_cluster_analytical = case_when(
      z_value > 0 & lag_z > 0 & local_p_analytical < 0.05 ~ "High-High",
      z_value < 0 & lag_z < 0 & local_p_analytical < 0.05 ~ "Low-Low",
      z_value > 0 & lag_z < 0 & local_p_analytical < 0.05 ~ "High-Low",
      z_value < 0 & lag_z > 0 & local_p_analytical < 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    ),
    lisa_cluster_perm = case_when(
      z_value > 0 & lag_z > 0 & local_p_perm < 0.05 ~ "High-High",
      z_value < 0 & lag_z < 0 & local_p_perm < 0.05 ~ "Low-Low",
      z_value > 0 & lag_z < 0 & local_p_perm < 0.05 ~ "High-Low",
      z_value < 0 & lag_z > 0 & local_p_perm < 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  )

# -------------------------------
# 9. SUMMARY OUTPUTS
# -------------------------------
cat("====================================\n")
cat("ANALYTICAL LOCAL MORAN SUMMARY\n")
cat("====================================\n")
cat("Minimum analytical p-value:", signif(min(city_stats_local$local_p_analytical, na.rm = TRUE), 6), "\n")
cat("Number significant at p < 0.05:", sum(city_stats_local$local_p_analytical < 0.05, na.rm = TRUE), "\n")
print(table(city_stats_local$lisa_cluster_analytical))
cat("\n")

cat("====================================\n")
cat("PERMUTATION LOCAL MORAN SUMMARY\n")
cat("====================================\n")
cat("Minimum permutation p-value:", signif(min(city_stats_local$local_p_perm, na.rm = TRUE), 6), "\n")
cat("Number significant at p < 0.05:", sum(city_stats_local$local_p_perm < 0.05, na.rm = TRUE), "\n")
print(table(city_stats_local$lisa_cluster_perm))
cat("\n")

# -------------------------------
# 10. TOP TABLES
# -------------------------------
top_analytical <- city_stats_local %>%
  select(
    City, State, Technique_EB,
    z_value, lag_z,
    local_I_analytical, local_Z_analytical, local_p_analytical,
    lisa_cluster_analytical
  ) %>%
  arrange(local_p_analytical)

top_perm <- city_stats_local %>%
  select(
    City, State, Technique_EB,
    z_value, lag_z,
    local_I_perm, local_Z_perm, local_p_perm,
    lisa_cluster_perm
  ) %>%
  arrange(local_p_perm)

cat("====================================\n")
cat("TOP 30 ANALYTICAL\n")
cat("====================================\n")
print(top_analytical, n = 30)
cat("\n")

cat("====================================\n")
cat("TOP 30 PERMUTATION\n")
cat("====================================\n")
print(top_perm, n = 30)
cat("\n")

# -------------------------------
# 11. MAP 1: OVERALL PERMUTATION CLUSTER MAP
# Grey non-significant below, significant above
# -------------------------------
p_perm_overall <- ggplot() +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm == "Not significant"),
    aes(x = long, y = lat),
    color = "grey80",
    size = 2.0,
    alpha = 0.55
  ) +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm != "Not significant"),
    aes(x = long, y = lat, fill = lisa_cluster_perm),
    shape = 21,
    color = "black",
    stroke = 0.3,
    size = 3.3,
    alpha = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    ),
    breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
  ) +
  labs(
    title = "Local Moran's I Cluster Map (Permutation, raw p-values)",
    subtitle = paste("k =", optimal_k, "| Technique_EB | Significant points shown above non-significant"),
    x = "Longitude",
    y = "Latitude",
    fill = "Cluster type"
  ) +
  theme_minimal()

print(p_perm_overall)

# -------------------------------
# 12. MAP 2: FACET MAP BY STATE
# -------------------------------
p_perm_by_state <- ggplot() +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm == "Not significant"),
    aes(x = long, y = lat),
    color = "grey85",
    size = 1.8,
    alpha = 0.5
  ) +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm != "Not significant"),
    aes(x = long, y = lat, fill = lisa_cluster_perm),
    shape = 21,
    color = "black",
    stroke = 0.25,
    size = 3.0,
    alpha = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    ),
    breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
  ) +
  facet_wrap(~ State, scales = "free") +
  labs(
    title = "Local Moran's I by State (Permutation, raw p-values)",
    subtitle = paste("k =", optimal_k, "| Faceted by state for clearer local patterns"),
    x = "Longitude",
    y = "Latitude",
    fill = "Cluster type"
  ) +
  theme_minimal()

print(p_perm_by_state)

# -------------------------------
# 13. OPTIONAL: ANALYTICAL OVERALL MAP
# If you also want analytical visualization
# -------------------------------
p_analytical_overall <- ggplot() +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_analytical == "Not significant"),
    aes(x = long, y = lat),
    color = "grey80",
    size = 2.0,
    alpha = 0.55
  ) +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_analytical != "Not significant"),
    aes(x = long, y = lat, fill = lisa_cluster_analytical),
    shape = 21,
    color = "black",
    stroke = 0.3,
    size = 3.3,
    alpha = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    ),
    breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
  ) +
  labs(
    title = "Local Moran's I Cluster Map (Analytical, raw p-values)",
    subtitle = paste("k =", optimal_k, "| Technique_EB"),
    x = "Longitude",
    y = "Latitude",
    fill = "Cluster type"
  ) +
  theme_minimal()

print(p_analytical_overall)

# -------------------------------
# 14. SAVE MAIN OBJECTS
# -------------------------------
# Main final dataset:
# - city_stats_local
#
# Maps:
# - p_perm_overall
# - p_perm_by_state
# - p_analytical_overall
#
# Tables:
# - top_analytical
# - top_perm
# -------------------------------
table(city_stats_local$lisa_cluster_analytical)
table(city_stats_local$lisa_cluster_perm)
city_stats_local %>%
  filter(local_p_perm < 0.05) %>%
  count(State, lisa_cluster_perm, sort = TRUE)
library(ggplot2)
library(dplyr)

plot_state_zoom_tight <- function(data, state_name, xlim_vec, ylim_vec) {
  ggplot() +
    geom_point(
      data = data %>% filter(State == state_name, lisa_cluster_perm == "Not significant"),
      aes(x = long, y = lat),
      color = "grey88",
      size = 1.8,
      alpha = 0.45
    ) +
    geom_point(
      data = data %>% filter(State == state_name, lisa_cluster_perm != "Not significant"),
      aes(x = long, y = lat, fill = lisa_cluster_perm),
      shape = 21,
      color = "black",
      stroke = 0.35,
      size = 4.2,
      alpha = 0.98
    ) +
    geom_text(
      data = data %>% filter(State == state_name, lisa_cluster_perm != "Not significant"),
      aes(x = long, y = lat, label = City),
      size = 3.1,
      nudge_y = 0.02,
      check_overlap = TRUE
    ) +
    scale_fill_manual(
      values = c(
        "High-High" = "red",
        "Low-Low" = "blue",
        "High-Low" = "orange",
        "Low-High" = "skyblue"
      ),
      breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
    ) +
    coord_cartesian(xlim = xlim_vec, ylim = ylim_vec) +
    labs(
      title = paste("Local Moran's I:", state_name),
      subtitle = "Tighter zoom around significant cities",
      x = "Longitude",
      y = "Latitude",
      fill = "Cluster type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )
}
p_nsw_zoom <- plot_state_zoom(
  city_stats_local,
  state_name = "NSW",
  xlim_vec = c(150.4, 152.2),
  ylim_vec = c(-35.2, -32.8)
)

print(p_nsw_zoom)
p_vic_zoom <- plot_state_zoom(
  city_stats_local,
  state_name = "VIC",
  xlim_vec = c(144.2, 145.4),
  ylim_vec = c(-38.35, -37.55)
)

print(p_vic_zoom)
p_qld_zoom <- plot_state_zoom(
  city_stats_local,
  state_name = "QLD",
  xlim_vec = c(152.2, 153.2),
  ylim_vec = c(-28.1, -27.1)
)

print(p_qld_zoom)
p_sa_zoom <- plot_state_zoom(
  city_stats_local,
  state_name = "SA",
  xlim_vec = c(138.3, 139.6),
  ylim_vec = c(-35.4, -34.2)
)

print(p_sa_zoom)






















################################################################################
# RESULTS PACKAGE: TABLES + PLOTS + MAPS
################################################################################

# =========================================================
# 0. LOAD LIBRARIES
# =========================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(spdep)
library(maps)
library(stringr)

# =========================================================
# 1. BASIC COUNTS FOR RESULTS
# =========================================================
table_data_prep <- tibble(
  metric = c(
    "Unique city-state pairs before deduplication",
    "Successful geocoded city-state pairs",
    "Failed geocodes",
    "Merged duplicate city/state names after standardization",
    "Final unique city-state coordinate pairs",
    "Final cities in EB/spatial analysis"
  ),
  value = c(
    nrow(cities_df),
    nrow(cities_coords_raw) - nrow(failed_geocodes),
    nrow(failed_geocodes),
    nrow(merged_groups),
    nrow(coords_full_and_cities),
    nrow(city_stats)
  )
)

print(table_data_prep)
write.csv(table_data_prep, "table_1_data_preparation.csv", row.names = FALSE)

# =========================================================
# 2. EB DIAGNOSTICS TABLE
# =========================================================
table_eb_diag <- tibble(
  metric = c(
    "Global mean price",
    "Within-city variance (sigma^2)",
    "Between-city variance (tau^2)",
    "Reliability threshold (n*)",
    "Reliable cities",
    "Noisy cities",
    "Min shrinkage B",
    "Q1 shrinkage B",
    "Median shrinkage B",
    "Mean shrinkage B",
    "Q3 shrinkage B",
    "Max shrinkage B"
  ),
  value = c(
    global_mean,
    sigma_sq,
    tau_sq,
    n_star,
    sum(city_stats$count >= n_star, na.rm = TRUE),
    sum(city_stats$count < n_star, na.rm = TRUE),
    min(city_stats$B, na.rm = TRUE),
    quantile(city_stats$B, 0.25, na.rm = TRUE),
    median(city_stats$B, na.rm = TRUE),
    mean(city_stats$B, na.rm = TRUE),
    quantile(city_stats$B, 0.75, na.rm = TRUE),
    max(city_stats$B, na.rm = TRUE)
  )
)

print(table_eb_diag)
write.csv(table_eb_diag, "table_2_eb_diagnostics.csv", row.names = FALSE)

# =========================================================
# 3. EB SUMMARY BY STATE
# =========================================================
table_eb_state <- city_stats %>%
  group_by(State) %>%
  summarise(
    n_cities = n(),
    reliable = sum(count >= n_star, na.rm = TRUE),
    noisy = sum(count < n_star, na.rm = TRUE),
    mean_raw = mean(mean_price, na.rm = TRUE),
    mean_eb = mean(Technique_EB, na.rm = TRUE),
    mean_B = mean(B, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_cities))

print(table_eb_state)
write.csv(table_eb_state, "table_3_eb_by_state.csv", row.names = FALSE)

# =========================================================
# 4. GLOBAL MORAN OPTIMIZATION TABLE
# =========================================================
table_k_selection <- results %>%
  mutate(
    connected = ifelse(n_components == 1, "Yes", "No")
  )

print(table_k_selection)
write.csv(table_k_selection, "table_4_k_selection.csv", row.names = FALSE)

# Small main-text version
table_k_main <- results %>%
  filter(k %in% c(3, 5, 7, 10, 11, 13, 15, 21)) %>%
  mutate(
    connected = ifelse(n_components == 1, "Yes", "No")
  )

print(table_k_main)
write.csv(table_k_main, "table_4b_k_selection_main_text.csv", row.names = FALSE)

# =========================================================
# 5. FINAL GLOBAL MORAN TABLE
# =========================================================
table_global_moran <- tibble(
  metric = c(
    "Optimal k",
    "Connected components",
    "Moran's I",
    "Expectation",
    "Variance",
    "Analytical Z-score",
    "Analytical p-value",
    "Monte Carlo p-value"
  ),
  value = c(
    optimal_k,
    spdep::n.comp.nb(nb_final)$nc,
    unname(final_moran$estimate[1]),
    unname(final_moran$estimate[2]),
    unname(final_moran$estimate[3]),
    as.numeric(final_moran$statistic),
    as.numeric(final_moran$p.value),
    as.numeric(final_moran_mc$p.value)
  )
)

print(table_global_moran)
write.csv(table_global_moran, "table_5_global_moran_final.csv", row.names = FALSE)

# =========================================================
# 6. LOCAL MORAN CLUSTER COUNT TABLE
# =========================================================
table_local_counts <- tibble(
  cluster = c("High-High", "Low-Low", "High-Low", "Low-High", "Not significant"),
  analytical = c(
    sum(city_stats_local$lisa_cluster_analytical == "High-High"),
    sum(city_stats_local$lisa_cluster_analytical == "Low-Low"),
    sum(city_stats_local$lisa_cluster_analytical == "High-Low"),
    sum(city_stats_local$lisa_cluster_analytical == "Low-High"),
    sum(city_stats_local$lisa_cluster_analytical == "Not significant")
  ),
  permutation = c(
    sum(city_stats_local$lisa_cluster_perm == "High-High"),
    sum(city_stats_local$lisa_cluster_perm == "Low-Low"),
    sum(city_stats_local$lisa_cluster_perm == "High-Low"),
    sum(city_stats_local$lisa_cluster_perm == "Low-High"),
    sum(city_stats_local$lisa_cluster_perm == "Not significant")
  )
)

print(table_local_counts)
write.csv(table_local_counts, "table_6_local_cluster_counts.csv", row.names = FALSE)

# =========================================================
# 7. LOCAL MORAN BY STATE (PERMUTATION)
# =========================================================
table_local_state_perm <- city_stats_local %>%
  filter(local_p_perm < 0.05) %>%
  count(State, lisa_cluster_perm, sort = TRUE) %>%
  pivot_wider(
    names_from = lisa_cluster_perm,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(State)

print(table_local_state_perm)
write.csv(table_local_state_perm, "table_7_local_clusters_by_state_permutation.csv", row.names = FALSE)

# =========================================================
# 8. TOP LOCAL OBSERVATIONS
# =========================================================
table_top_local_perm <- top_perm %>%
  filter(lisa_cluster_perm != "Not significant") %>%
  select(
    City, State, Technique_EB,
    z_value, lag_z,
    local_I_perm, local_Z_perm, local_p_perm,
    lisa_cluster_perm
  ) %>%
  slice_head(n = 20)

print(table_top_local_perm, n = 20)
write.csv(table_top_local_perm, "table_8_top_local_permutation.csv", row.names = FALSE)

table_top_local_analytical <- top_analytical %>%
  filter(lisa_cluster_analytical != "Not significant") %>%
  select(
    City, State, Technique_EB,
    z_value, lag_z,
    local_I_analytical, local_Z_analytical, local_p_analytical,
    lisa_cluster_analytical
  ) %>%
  slice_head(n = 20)

print(table_top_local_analytical, n = 20)
write.csv(table_top_local_analytical, "table_9_top_local_analytical.csv", row.names = FALSE)

# =========================================================
# 9. PLOT: CLEANED CITY COORDINATES
# =========================================================
p_coords <- ggplot(coords_full_and_cities, aes(x = long, y = lat)) +
  borders("world", regions = "Australia", fill = "grey90", color = "grey60") +
  geom_point(size = 1.2, alpha = 0.8) +
  labs(
    title = "Cleaned City Coordinates",
    subtitle = paste("Final spatial reference:", nrow(coords_full_and_cities), "city-state pairs"),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

print(p_coords)
ggsave("figure_1_cleaned_coordinates.png", p_coords, width = 9, height = 7, dpi = 300)

# =========================================================
# 10. PLOT: NATIONAL RAW VS EB
# =========================================================
p_national_results <- ggplot(
  plot_data_optimal,
  aes(
    x = Estimate_Type,
    y = Price,
    group = interaction(City, State),
    color = Reliability
  )
) +
  geom_line(aes(alpha = Reliability, linewidth = Reliability)) +
  geom_point(aes(alpha = Reliability), size = 1) +
  geom_hline(yintercept = global_mean, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ",")) +
  scale_color_manual(values = c("grey40", "#E63946")) +
  scale_alpha_manual(values = c(0.08, 0.9)) +
  scale_linewidth_manual(values = c(0.2, 0.7)) +
  labs(
    title = "National Empirical Bayes Shrinkage Overview",
    subtitle = paste("Comparison of raw mean and EB estimate across", nrow(city_stats), "cities"),
    x = NULL,
    y = "Price (AUD)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(p_national_results)
ggsave("figure_2_national_raw_vs_eb.png", p_national_results, width = 11, height = 7, dpi = 300)

# =========================================================
# 11. PLOT: STATE RAW VS EB
# =========================================================
p_state_results <- ggplot(
  plot_data_final,
  aes(
    x = Estimate_Type,
    y = Price,
    group = interaction(City, State),
    color = Reliability
  )
) +
  geom_line(aes(alpha = Reliability, linewidth = Reliability)) +
  geom_point(aes(alpha = Reliability), size = 0.8) +
  geom_hline(yintercept = global_mean, linetype = "dashed", color = "black", linewidth = 0.4) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ",")) +
  scale_color_manual(values = c("grey20", "#E63946")) +
  scale_alpha_manual(values = c(0.15, 0.8)) +
  scale_linewidth_manual(values = c(0.3, 0.8)) +
  facet_wrap(~facet_label, scales = "free_y", ncol = 3) +
  labs(
    title = "State-Level Empirical Bayes Analysis",
    subtitle = paste0("Global mean = $", format(round(global_mean, 2), big.mark = ",")),
    x = NULL,
    y = "Price (AUD)",
    caption = "R = Reliable | N = Noisy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 11)
  )

print(p_state_results)
ggsave("figure_3_state_raw_vs_eb.png", p_state_results, width = 14, height = 10, dpi = 300)

# =========================================================
# 12. PLOT: Z-SCORE BY k
# =========================================================
p_z_by_k <- ggplot(results, aes(x = k, y = z_score)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = n_components == 1), size = 2) +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
  annotate(
    "label",
    x = optimal_k,
    y = optimal_z,
    label = paste0(
      "Optimal k = ", optimal_k,
      "\n", selection_method,
      "\nComponents = ", optimal_components
    ),
    vjust = -0.4
  ) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  labs(
    title = "Z-score Profile Across k-nearest-neighbor Graphs",
    subtitle = "Global Moran's I optimization",
    x = "Number of neighbors (k)",
    y = "Z-score"
  ) +
  theme_minimal()

print(p_z_by_k)
ggsave("figure_4_zscore_by_k.png", p_z_by_k, width = 10, height = 6, dpi = 300)

# =========================================================
# 13. PLOT: MORAN'S I BY k
# =========================================================
p_moran_by_k <- ggplot(results, aes(x = k, y = moran_i)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = n_components == 1), size = 2) +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  labs(
    title = "Global Moran's I Across Neighborhood Sizes",
    subtitle = "Observed spatial autocorrelation as k changes",
    x = "Number of neighbors (k)",
    y = "Moran's I"
  ) +
  theme_minimal()

print(p_moran_by_k)
ggsave("figure_5_moran_by_k.png", p_moran_by_k, width = 10, height = 6, dpi = 300)

# =========================================================
# 14. PLOT: OVERALL PERMUTATION LISA MAP
# =========================================================
p_lisa_perm_overall <- ggplot() +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm == "Not significant"),
    aes(x = long, y = lat),
    color = "grey80",
    size = 2.0,
    alpha = 0.55
  ) +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm != "Not significant"),
    aes(x = long, y = lat, fill = lisa_cluster_perm),
    shape = 21,
    color = "black",
    stroke = 0.3,
    size = 3.3,
    alpha = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    ),
    breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
  ) +
  labs(
    title = "Local Moran's I Cluster Map",
    subtitle = paste("Permutation approach, raw p-values, k =", optimal_k),
    x = "Longitude",
    y = "Latitude",
    fill = "Cluster type"
  ) +
  theme_minimal()

print(p_lisa_perm_overall)
ggsave("figure_6_lisa_perm_overall.png", p_lisa_perm_overall, width = 10, height = 7, dpi = 300)

# =========================================================
# 15. PLOT: PERMUTATION LISA BY STATE
# =========================================================
p_lisa_perm_by_state <- ggplot() +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm == "Not significant"),
    aes(x = long, y = lat),
    color = "grey85",
    size = 1.8,
    alpha = 0.5
  ) +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_perm != "Not significant"),
    aes(x = long, y = lat, fill = lisa_cluster_perm),
    shape = 21,
    color = "black",
    stroke = 0.25,
    size = 3.0,
    alpha = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    ),
    breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
  ) +
  facet_wrap(~ State, scales = "free") +
  labs(
    title = "Local Moran's I by State",
    subtitle = paste("Permutation approach, raw p-values, k =", optimal_k),
    x = "Longitude",
    y = "Latitude",
    fill = "Cluster type"
  ) +
  theme_minimal()

print(p_lisa_perm_by_state)
ggsave("figure_7_lisa_perm_by_state.png", p_lisa_perm_by_state, width = 14, height = 10, dpi = 300)

# =========================================================
# 16. ZOOM MAP FUNCTION
# =========================================================
plot_state_zoom <- function(data, state_name, xlim_vec, ylim_vec) {
  ggplot() +
    geom_point(
      data = data %>% filter(State == state_name, lisa_cluster_perm == "Not significant"),
      aes(x = long, y = lat),
      color = "grey88",
      size = 1.8,
      alpha = 0.45
    ) +
    geom_point(
      data = data %>% filter(State == state_name, lisa_cluster_perm != "Not significant"),
      aes(x = long, y = lat, fill = lisa_cluster_perm),
      shape = 21,
      color = "black",
      stroke = 0.35,
      size = 4.2,
      alpha = 0.98
    ) +
    geom_text(
      data = data %>% filter(State == state_name, lisa_cluster_perm != "Not significant"),
      aes(x = long, y = lat, label = City),
      size = 3.1,
      nudge_y = 0.02,
      check_overlap = TRUE
    ) +
    scale_fill_manual(
      values = c(
        "High-High" = "red",
        "Low-Low" = "blue",
        "High-Low" = "orange",
        "Low-High" = "skyblue"
      ),
      breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
    ) +
    coord_cartesian(xlim = xlim_vec, ylim = ylim_vec) +
    labs(
      title = paste("Local Moran's I:", state_name),
      subtitle = "Zoomed view with significant cities labeled",
      x = "Longitude",
      y = "Latitude",
      fill = "Cluster type"
    ) +
    theme_minimal()
}

# =========================================================
# 17. ZOOM MAPS FOR MAIN STATES
# =========================================================
p_nsw_zoom <- plot_state_zoom(
  city_stats_local, "NSW",
  xlim_vec = c(150.7, 151.9),
  ylim_vec = c(-34.95, -33.55)
)
print(p_nsw_zoom)
ggsave("figure_8_nsw_zoom.png", p_nsw_zoom, width = 10, height = 7, dpi = 300)

p_vic_zoom <- plot_state_zoom(
  city_stats_local, "VIC",
  xlim_vec = c(144.45, 145.15),
  ylim_vec = c(-38.15, -37.70)
)
print(p_vic_zoom)
ggsave("figure_9_vic_zoom.png", p_vic_zoom, width = 10, height = 7, dpi = 300)

p_qld_zoom <- plot_state_zoom(
  city_stats_local, "QLD",
  xlim_vec = c(152.85, 153.20),
  ylim_vec = c(-27.70, -27.20)
)
print(p_qld_zoom)
ggsave("figure_10_qld_zoom.png", p_qld_zoom, width = 10, height = 7, dpi = 300)

p_sa_zoom <- plot_state_zoom(
  city_stats_local, "SA",
  xlim_vec = c(138.45, 139.15),
  ylim_vec = c(-35.20, -34.55)
)
print(p_sa_zoom)
ggsave("figure_11_sa_zoom.png", p_sa_zoom, width = 10, height = 7, dpi = 300)

# =========================================================
# 18. OPTIONAL: ANALYTICAL LISA MAP
# =========================================================
p_lisa_analytical_overall <- ggplot() +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_analytical == "Not significant"),
    aes(x = long, y = lat),
    color = "grey80",
    size = 2.0,
    alpha = 0.55
  ) +
  geom_point(
    data = city_stats_local %>% filter(lisa_cluster_analytical != "Not significant"),
    aes(x = long, y = lat, fill = lisa_cluster_analytical),
    shape = 21,
    color = "black",
    stroke = 0.3,
    size = 3.3,
    alpha = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    ),
    breaks = c("High-High", "Low-Low", "High-Low", "Low-High")
  ) +
  labs(
    title = "Local Moran's I Cluster Map",
    subtitle = paste("Analytical approach, raw p-values, k =", optimal_k),
    x = "Longitude",
    y = "Latitude",
    fill = "Cluster type"
  ) +
  theme_minimal()

print(p_lisa_analytical_overall)
ggsave("figure_12_lisa_analytical_overall.png", p_lisa_analytical_overall, width = 10, height = 7, dpi = 300)

# =========================================================
# 19. CONSOLE OUTPUTS TO SEND BACK
# =========================================================
cat("\n============================\n")
cat("SEND THESE OUTPUTS BACK\n")
cat("============================\n\n")

cat("Table 1: data preparation\n")
print(table_data_prep)

cat("\nTable 2: EB diagnostics\n")
print(table_eb_diag)

cat("\nTable 4 main: k selection\n")
print(table_k_main)

cat("\nTable 5: final global Moran\n")
print(table_global_moran)

cat("\nTable 6: local cluster counts\n")
print(table_local_counts)

cat("\nTable 7: local clusters by state (permutation)\n")
print(table_local_state_perm)

cat("\nTable 8: top local permutation\n")
print(table_top_local_perm, n = 20)

cat("\nGenerated figures saved as PNG files.\n")







table_local_state_share <- city_stats_local %>%
  filter(local_p_perm < 0.05) %>%
  count(State, lisa_cluster_perm) %>%
  group_by(State) %>%
  mutate(
    total_state = sum(n),
    share = n / total_state
  ) %>%
  ungroup()

print(table_local_state_share)



p_clusters_state <- city_stats_local %>%
  filter(local_p_perm < 0.05) %>%
  count(State, lisa_cluster_perm) %>%
  ggplot(aes(x = State, y = n, fill = lisa_cluster_perm)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    )
  ) +
  labs(
    title = "Distribution of Local Moran’s I Clusters by State",
    subtitle = "Permutation-based significant clusters (p < 0.05)",
    x = "State",
    y = "Number of significant cities",
    fill = "Cluster type"
  ) +
  theme_minimal(base_size = 13)

print(p_clusters_state)

ggsave("figure_clusters_by_state.png", p_clusters_state,
       width = 10, height = 6, dpi = 300)








city_stats_local %>%
  filter(local_p_analytical < 0.05) %>%
  count(State, lisa_cluster_analytical, sort = TRUE) %>%
  tidyr::pivot_wider(
    names_from = lisa_cluster_analytical,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(State)

city_stats_local %>%
  filter(local_p_perm < 0.05) %>%
  select(
    City, State, Technique_EB, z_value, lag_z,
    local_I_perm, local_Z_perm, local_p_perm, lisa_cluster_perm
  ) %>%
  arrange(State, local_p_perm)

city_stats_local %>%
  filter(local_p_analytical < 0.05) %>%
  select(
    City, State, Technique_EB, z_value, lag_z,
    local_I_analytical, local_Z_analytical, local_p_analytical, lisa_cluster_analytical
  ) %>%
  arrange(State, local_p_analytical)

city_stats_local %>%
  count(State, sort = TRUE)

p_clusters_state <- city_stats_local %>%
  filter(local_p_perm < 0.05) %>%
  count(State, lisa_cluster_perm) %>%
  ggplot(aes(x = State, y = n, fill = lisa_cluster_perm)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "skyblue"
    )
  ) +
  labs(
    title = "Distribution of Local Moran's I Clusters by State",
    subtitle = "Permutation-based significant clusters (p < 0.05)",
    x = "State",
    y = "Number of significant cities",
    fill = "Cluster type"
  ) +
  theme_minimal()

print(p_clusters_state)
ggsave("figure_13_clusters_by_state.png", p_clusters_state, width = 10, height = 6, dpi = 300)













































#results
#Descriptive statistics
print(table_data_prep)
write.csv(table_data_prep, "table_1_data_preparation.csv", row.names = FALSE)
summary(data$Price)
ggplot(data, aes(x = Price)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Car Prices", x = "Price", y = "Frequency") +
  theme_minimal()
#Empirical Bayes results
print(table_eb_diag)
print(table_eb_state)
print(p_national_results)
print(p_state_results)
#Spatial structure
print(p_coords)
spdep::n.comp.nb(nb_final)
#Global spatial autocorrelation
print(table_global_moran)
cat(sprintf("Moran's I: %.4f\n", final_moran$estimate[1]))
cat(sprintf("Z-score: %.4f\n", final_moran$statistic))
cat(sprintf("p-value: %.8f\n", final_moran$p.value))
#Sensitivity to k
print(table_k_main)
print(p_z_by_k)
print(p_moran_by_k)
#Local spatial autocorrelation
print(table_local_counts)
print(table_local_state_perm)
print(table_top_local_perm, n = 20)
print(p_lisa_perm_overall)
print(p_lisa_perm_by_state)
#Detailed local patterns
print(p_nsw_zoom)
print(p_vic_zoom)
print(p_qld_zoom)
print(p_sa_zoom)
print(p_clusters_state)
print(table_local_state_share)



# ================================
# 0. LIBRARIES
# ================================
library(dplyr)
library(ggplot2)
library(scales)
library(spdep)
library(sf)

# ================================
# 1. DESCRIPTIVE STATISTICS
# ================================

# Table 1
write.csv(table_data_prep, "table_1_data_preparation.csv", row.names = FALSE)

# Summary (optional export)
capture.output(summary(data$Price), file = "summary_price.txt")

# Figure 1 — Price distribution
p_hist <- ggplot(data, aes(x = Price)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of Car Prices",
    x = "Price (AUD)",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave("figure_1_price_distribution.png", p_hist, width = 8, height = 5)

# ================================
# 2. EMPIRICAL BAYES
# ================================

# Table 2
write.csv(table_eb_diag, "table_2_eb_diagnostics.csv", row.names = FALSE)

# Table 3
write.csv(table_eb_state, "table_3_eb_state.csv", row.names = FALSE)

# Figure 2 — National shrinkage
ggsave("figure_2_national_shrinkage.png", p_national_results, width = 8, height = 6)

# Figure 3 — State shrinkage
ggsave("figure_3_state_shrinkage.png", p_state_results, width = 10, height = 6)

# ================================
# 3. SPATIAL STRUCTURE
# ================================

# Figure 4 — City coordinates
ggsave("figure_4_city_coordinates.png", p_coords, width = 8, height = 6)

# Connectivity check
capture.output(spdep::n.comp.nb(nb_final), file = "spatial_connectivity.txt")

# ================================
# 4. GLOBAL MORAN'S I
# ================================

# Table 5
write.csv(table_global_moran, "table_5_global_moran.csv", row.names = FALSE)

# Save Moran test results
moran_results <- data.frame(
  Moran_I = final_moran$estimate[1],
  Z_score = final_moran$statistic,
  p_value = final_moran$p.value
)
write.csv(moran_results, "global_moran_test_values.csv", row.names = FALSE)

# ================================
# 5. SENSITIVITY TO k
# ================================

# Table 4
write.csv(table_k_main, "table_4_k_sensitivity.csv", row.names = FALSE)

# Figure 5 — Z-score vs k
ggsave("figure_5_zscore_by_k.png", p_z_by_k, width = 8, height = 5)

# Figure 6 — Moran’s I vs k
ggsave("figure_6_moran_by_k.png", p_moran_by_k, width = 8, height = 5)

# ================================
# 6. LOCAL MORAN'S I
# ================================

# Table 6
write.csv(table_local_counts, "table_6_local_counts.csv", row.names = FALSE)

# Table 7
write.csv(table_local_state_perm, "table_7_local_by_state.csv", row.names = FALSE)

# Table 8
write.csv(table_top_local_perm, "table_8_top_local_clusters.csv", row.names = FALSE)

# Table 9
write.csv(table_local_state_share, "table_9_state_cluster_share.csv", row.names = FALSE)

# ================================
# 7. LOCAL MAPS
# ================================

# Figure 7 — Overall LISA
ggsave("figure_7_lisa_overall.png", p_lisa_perm_overall, width = 8, height = 6)

# Figure 8 — LISA by state
ggsave("figure_8_lisa_by_state.png", p_lisa_perm_by_state, width = 10, height = 6)

# Figure 9 — NSW zoom
ggsave("figure_9_nsw_clusters.png", p_nsw_zoom, width = 8, height = 6)

# Figure 10 — VIC zoom
ggsave("figure_10_vic_clusters.png", p_vic_zoom, width = 8, height = 6)

# Figure 11 — QLD zoom
ggsave("figure_11_qld_clusters.png", p_qld_zoom, width = 8, height = 6)

# Figure 12 — SA zoom
ggsave("figure_12_sa_clusters.png", p_sa_zoom, width = 8, height = 6)

# Figure 13 — Cluster distribution by state
ggsave("figure_13_clusters_by_state.png", p_clusters_state, width = 8, height = 5)
