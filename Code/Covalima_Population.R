# =============================================================================
# Suai-Loro Population Projections Comparison (Official vs Alternative Data)
# =============================================================================
# This script compares population projections for Suai-Loro, Timor-Leste from 2022-2050
# using two different sources of Cova-Lima municipal population projections
#
# References:
# 1. Timor-Leste Census 2022: Suai-Loro population of 4,113
# 2. Two datasets for Cova-Lima municipality projections:
#    - Official Table: From the statistical table (2022-2035) - Timor-Leste Population and Housing Census 2022, Thematic Report – Population Projection
#    - Alternative Dataset: Extended projections (2022-2050) with anomalous growth - https://inetl-ip.gov.tl/2024/05/14/population-projection-2022-2050/
# =============================================================================

# Load required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("writexl")) install.packages("writexl")

library(tidyverse)
library(writexl)

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# =============================================================================
# STEP 1: Define both sets of population projections for Cova-Lima municipality
# =============================================================================

# First dataset: From the official statistical table (2022-2035)
covalima_official_table <- tibble(
  Year = 2022:2035,
  Population_Official = c(
    74787,  # 2022
    76086,  # 2023
    77335,  # 2024
    78476,  # 2025
    79531,  # 2026
    80525,  # 2027
    81472,  # 2028
    82390,  # 2029
    83280,  # 2030
    84152,  # 2031
    85021,  # 2032
    85892,  # 2033
    86770,  # 2034
    87653   # 2035
  ),
  Growth_Rate_Official = c(
    NA,     # 2022 (base year)
    1.72,   # 2023
    1.63,   # 2024
    1.46,   # 2025
    1.34,   # 2026
    1.24,   # 2027
    1.17,   # 2028
    1.12,   # 2029
    1.07,   # 2030
    1.04,   # 2031
    1.03,   # 2032
    1.02,   # 2033
    1.02,   # 2034
    1.01    # 2035
  )
)

# Second dataset: Alternative projections (2022-2050)
covalima_alternative <- tibble(
  Year = 2022:2050,
  Population_Alternative = c(
    74787,  # 2022
    110620, # 2023
    112149, # 2024
    112542, # 2025
    115961, # 2026
    117568, # 2027
    119124, # 2028
    120632, # 2029
    122087, # 2030
    123516, # 2031
    124937, # 2032
    126346, # 2033
    127736, # 2034
    129102, # 2035
    130454, # 2036
    131802, # 2037
    133136, # 2038
    134449, # 2039
    135731, # 2040
    136992, # 2041
    138254, # 2042
    139476, # 2043
    140691, # 2044
    142000, # 2045
    142838, # 2046
    144261, # 2047
    145419, # 2048
    146533, # 2049
    148241  # 2050
  )
)

# Calculate growth rates for the alternative dataset
covalima_alternative <- covalima_alternative %>%
  mutate(
    Previous_Pop = lag(Population_Alternative),
    Growth_Rate_Alternative = ((Population_Alternative / Previous_Pop) - 1) * 100
  ) %>%
  select(-Previous_Pop)

# =============================================================================
# STEP 2: Extrapolate the official table projections to 2050
# =============================================================================

# For the official table, we need to extend the projections from 2035 to 2050
# We'll use a simple linear model based on the observed growth rates

# Fit a linear model to the official growth rates (excluding NA for 2022)
official_growth_data <- covalima_official_table %>%
  filter(!is.na(Growth_Rate_Official))

official_growth_model <- lm(Growth_Rate_Official ~ Year, data = official_growth_data)

# Create a data frame for future years (2036-2050)
future_years <- tibble(Year = 2036:2050)

# Predict growth rates for future years (ensure they don't go below 0.5%)
future_growth_rates <- predict(official_growth_model, future_years)
future_growth_rates <- pmax(future_growth_rates, 0.5)

# Start with the 2035 population
extended_pop <- covalima_official_table$Population_Official[14]  # 2035 population

# Calculate future populations using the predicted growth rates
future_populations <- numeric(length(future_years$Year))

for (i in 1:length(future_years$Year)) {
  extended_pop <- extended_pop * (1 + future_growth_rates[i]/100)
  future_populations[i] <- extended_pop
}

# Create extended official dataset
covalima_official_extended <- bind_rows(
  covalima_official_table,
  tibble(
    Year = future_years$Year,
    Population_Official = future_populations,
    Growth_Rate_Official = future_growth_rates
  )
)

# =============================================================================
# STEP 3: Compare the two datasets
# =============================================================================

# Merge the datasets for comparison (2022-2050)
covalima_comparison <- covalima_official_extended %>%
  left_join(covalima_alternative, by = "Year") %>%
  mutate(
    Population_Difference = Population_Alternative - Population_Official,
    Population_Pct_Diff = (Population_Difference / Population_Official) * 100,
    Growth_Rate_Difference = Growth_Rate_Alternative - Growth_Rate_Official
  )

# Visualize population comparison
population_comparison_plot <- covalima_comparison %>%
  pivot_longer(cols = c(Population_Official, Population_Alternative),
               names_to = "Source", 
               values_to = "Population") %>%
  mutate(Source = case_when(
    Source == "Population_Official" ~ "Official Table (Extended)",
    Source == "Population_Alternative" ~ "Alternative Dataset"
  )) %>%
  ggplot(aes(x = Year, y = Population, color = Source, group = Source)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 2035.5, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  labs(
    title = "Comparison of Cova-Lima Population Projections",
    subtitle = "Official Table (Extended) vs. Alternative Dataset",
    y = "Population",
    x = "Year"
  )
population_comparison_plot

# Save the plot
ggsave("output/covalima_population_comparison.png", population_comparison_plot, width = 10, height = 6)

# =============================================================================
# STEP 4: Project Suai-Loro population using both datasets
# =============================================================================

# Define Suai-Loro base population from 2022 census
suailoro_base_population <- 4113  # 2022 census figure

# Create two datasets for Suai-Loro projections

# 1. Official-based projection
suailoro_official <- tibble(
  Year = 2022:2050,
  Growth_Rate = covalima_official_extended$Growth_Rate_Official,
  Population = NA_real_
)

# Set the initial population
suailoro_official$Population[1] <- suailoro_base_population

# Calculate population year by year
for (i in 2:nrow(suailoro_official)) {
  prev_pop <- suailoro_official$Population[i-1]
  growth_rate <- suailoro_official$Growth_Rate[i]
  
  suailoro_official$Population[i] <- prev_pop * (1 + growth_rate/100)
}

# 2. Alternative-based projection
suailoro_alternative <- tibble(
  Year = 2022:2050,
  Growth_Rate = covalima_alternative$Growth_Rate_Alternative,
  Population = NA_real_
)

# Set the initial population
suailoro_alternative$Population[1] <- suailoro_base_population

# Calculate population year by year
for (i in 2:nrow(suailoro_alternative)) {
  prev_pop <- suailoro_alternative$Population[i-1]
  growth_rate <- suailoro_alternative$Growth_Rate[i]
  
  suailoro_alternative$Population[i] <- prev_pop * (1 + growth_rate/100)
}

# Round population figures
suailoro_official <- suailoro_official %>%
  mutate(Population = round(Population))

suailoro_alternative <- suailoro_alternative %>%
  mutate(Population = round(Population))

# Create combined dataset for visualization
suailoro_comparison <- bind_rows(
  suailoro_official %>% 
    select(Year, Growth_Rate, Population) %>%
    mutate(Source = "Based on Official Data"),
  
  suailoro_alternative %>% 
    select(Year, Growth_Rate, Population) %>%
    mutate(Source = "Based on Alternative Data")
)

# Visualize Suai-Loro projection comparison
suailoro_comparison_plot <- suailoro_comparison %>%
  ggplot(aes(x = Year, y = Population, color = Source, group = Source)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 2035.5, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Suai-Loro Population Projections (2022-2050)",
    subtitle = "Comparison of Projections Based on Different Cova-Lima Datasets",
    y = "Population",
    x = "Year"
  )
suailoro_comparison_plot

# Save the plot
ggsave("output/suailoro_projection_comparison.png", suailoro_comparison_plot, width = 10, height = 6)

# =============================================================================
# STEP 5: Calculate key statistics and save results
# =============================================================================

# Calculate total growth and percentages for different periods
periods <- tibble(
  Period = c("2022-2030", "2030-2040", "2040-2050", "2022-2050"),
  Start_Year = c(2022, 2030, 2040, 2022),
  End_Year = c(2030, 2040, 2050, 2050)
)

# Function to calculate growth statistics
calc_growth_stats <- function(data, periods) {
  result <- tibble(
    Period = character(),
    Start_Population = numeric(),
    End_Population = numeric(),
    Absolute_Growth = numeric(),
    Percent_Growth = numeric(),
    Avg_Annual_Growth_Rate = numeric()
  )
  
  for (i in 1:nrow(periods)) {
    start_pop <- data$Population[data$Year == periods$Start_Year[i]]
    end_pop <- data$Population[data$Year == periods$End_Year[i]]
    years_diff <- periods$End_Year[i] - periods$Start_Year[i]
    
    abs_growth <- end_pop - start_pop
    pct_growth <- (abs_growth / start_pop) * 100
    avg_rate <- (((end_pop / start_pop)^(1/years_diff)) - 1) * 100
    
    result <- result %>%
      add_row(
        Period = periods$Period[i],
        Start_Population = start_pop,
        End_Population = end_pop,
        Absolute_Growth = abs_growth,
        Percent_Growth = pct_growth,
        Avg_Annual_Growth_Rate = avg_rate
      )
  }
  
  return(result)
}

# Calculate statistics for both projections
official_stats <- calc_growth_stats(suailoro_official, periods) %>% 
  mutate(Source = "Based on Official Data")

alternative_stats <- calc_growth_stats(suailoro_alternative, periods) %>% 
  mutate(Source = "Based on Alternative Data")

all_stats <- bind_rows(official_stats, alternative_stats)

# Format statistics for display/export
all_stats_formatted <- all_stats %>%
  mutate(
    Start_Population = round(Start_Population),
    End_Population = round(End_Population),
    Absolute_Growth = round(Absolute_Growth),
    Percent_Growth = round(Percent_Growth, 1),
    Avg_Annual_Growth_Rate = round(Avg_Annual_Growth_Rate, 2)
  )

# Print summary statistics
cat("\nSuai-Loro Population Growth Statistics (Based on Official Data):\n")
print(official_stats %>% 
        mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>% 
        select(-Source))

cat("\nSuai-Loro Population Growth Statistics (Based on Alternative Data):\n")
print(alternative_stats %>% 
        mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>% 
        select(-Source))

# Print key years for reference
key_years <- c(2022, 2025, 2030, 2035, 2040, 2045, 2050)

cat("\nSuai-Loro Population Projections - Key Years (Based on Official Data):\n")
print(suailoro_official %>% 
        filter(Year %in% key_years) %>% 
        select(Year, Population, Growth_Rate))

cat("\nSuai-Loro Population Projections - Key Years (Based on Alternative Data):\n")
print(suailoro_alternative %>% 
        filter(Year %in% key_years) %>% 
        select(Year, Population, Growth_Rate))

# Save all data to files
write_csv(covalima_comparison, "output/covalima_data_comparison.csv")
write_csv(suailoro_comparison, "output/suailoro_projection_comparison.csv")
write_csv(all_stats_formatted, "output/suailoro_growth_statistics_comparison.csv")

# Also save as Excel for easier use
write_xlsx(list(
  "Cova-Lima Data Comparison" = covalima_comparison,
  "Suai-Loro Projections" = suailoro_comparison,
  "Growth Statistics" = all_stats_formatted
), "output/suailoro_population_comparison.xlsx")

# =============================================================================
# STEP 6: Create a formatted report summarizing the findings
# =============================================================================

# Create a text report
report_file <- file("output/suailoro_population_comparison_report.txt", "w")

cat("=============================================================================\n", file = report_file)
cat("         SUAI-LORO (TIMOR-LESTE) POPULATION PROJECTIONS COMPARISON           \n", file = report_file)
cat("                   OFFICIAL DATA VS. ALTERNATIVE DATASET                      \n", file = report_file)
cat("=============================================================================\n\n", file = report_file)

cat("DATA SOURCES:\n", file = report_file)
cat("1. 2022 Census: Suai-Loro population of 4,113\n", file = report_file)
cat("2. Official Table: Population projections from the official statistical table (2022-2035)\n", file = report_file)
cat("3. Alternative Dataset: Extended population projections (2022-2050) with anomalous growth\n\n", file = report_file)

cat("METHODOLOGY:\n", file = report_file)
cat("- Starting with the census population of 4,113 for Suai-Loro (2022)\n", file = report_file)
cat("- Using growth rates from both the official table and alternative dataset\n", file = report_file)
cat("- Extending official projections from 2035 to 2050 using trend analysis\n", file = report_file)
cat("- Comparing resulting population trajectories and growth statistics\n\n", file = report_file)

cat("KEY FINDINGS - COVA-LIMA DATA COMPARISON:\n", file = report_file)
cat("- The alternative dataset shows an extreme 47.9% population increase between 2022-2023\n", file = report_file)
cat("  while the official table shows a more realistic 1.72% increase\n", file = report_file)
cat("- By 2035, the alternative dataset projects a population of 129,102 for Cova-Lima,\n", file = report_file)
cat("  which is 47.3% higher than the official projection of 87,653\n", file = report_file)
cat("- The anomalous growth in the alternative dataset significantly impacts all projections\n\n", file = report_file)

cat("SUAI-LORO POPULATION PROJECTIONS - KEY YEARS:\n", file = report_file)
cat(sprintf("%-20s %10s %10s %14s\n", 
            "Year", "Official", "Alternative", "Difference (%)"), 
    file = report_file)
cat("--------------------  ----------  ----------  --------------\n", file = report_file)

for (year in key_years) {
  off_pop <- suailoro_official$Population[suailoro_official$Year == year]
  alt_pop <- suailoro_alternative$Population[suailoro_alternative$Year == year]
  diff_pct <- ((alt_pop - off_pop) / off_pop) * 100
  
  cat(sprintf("%-20d %10d %10d %14.1f%%\n", 
              year, off_pop, alt_pop, diff_pct), 
      file = report_file)
}
cat("\n", file = report_file)

cat("GROWTH STATISTICS COMPARISON (2022-2050):\n", file = report_file)
cat(sprintf("%-20s %10s %10s %14s %12s %12s\n", 
            "Source", "2022 Pop", "2050 Pop", "Total Growth", "Growth %", "Avg Rate/Yr"), 
    file = report_file)
cat("--------------------  ----------  ----------  --------------  ------------  ------------\n", file = report_file)

off_stats <- official_stats %>% filter(Period == "2022-2050")
alt_stats <- alternative_stats %>% filter(Period == "2022-2050")

cat(sprintf("%-20s %10d %10d %14d %12.1f%% %12.2f%%\n", 
            "Official Projection", 
            round(off_stats$Start_Population), 
            round(off_stats$End_Population),
            round(off_stats$Absolute_Growth),
            round(off_stats$Percent_Growth, 1),
            round(off_stats$Avg_Annual_Growth_Rate, 2)), 
    file = report_file)

cat(sprintf("%-20s %10d %10d %14d %12.1f%% %12.2f%%\n", 
            "Alternative Projection", 
            round(alt_stats$Start_Population), 
            round(alt_stats$End_Population),
            round(alt_stats$Absolute_Growth),
            round(alt_stats$Percent_Growth, 1),
            round(alt_stats$Avg_Annual_Growth_Rate, 2)), 
    file = report_file)
cat("\n", file = report_file)

cat("ASSESSMENT OF DATA RELIABILITY:\n", file = report_file)
cat("- The official table shows a consistent pattern of gradually declining growth rates\n", file = report_file)
cat("  (from 1.72% to 1.01%) which aligns with demographic transition patterns\n", file = report_file)
cat("- The alternative dataset shows a highly questionable 47.9% increase in a single year,\n", file = report_file)
cat("  followed by more moderate growth rates\n", file = report_file)
cat("- The magnitude of difference between the two datasets cannot be explained by normal\n", file = report_file)
cat("  demographic factors or methodological variations\n\n", file = report_file)

cat("RECOMMENDATION:\n", file = report_file)
cat("- The official table-based projections should be considered more reliable for planning purposes\n", file = report_file)
cat("- The alternative dataset may include boundary changes, methodological variations,\n", file = report_file)
cat("  or data errors that make it unsuitable for reliable population projection\n", file = report_file)
cat("- If using the alternative dataset, it would be advisable to exclude the 2022-2023 period\n", file = report_file)
cat("  and possibly recalibrate using only post-2023 growth patterns\n\n", file = report_file)

cat("NEXT STEPS:\n", file = report_file)
cat("- Investigate the source of the discrepancy between the two datasets\n", file = report_file)
cat("- Consider creating a third projection that excludes the anomalous growth in the alternative dataset\n", file = report_file)
cat("- Incorporate local factors that might affect Suai-Loro's growth relative to the municipal average\n", file = report_file)
cat("- Update projections when new census or survey data becomes available\n\n", file = report_file)

cat("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = report_file)
cat("=============================================================================\n", file = report_file)

close(report_file)

cat("\nComparison analysis complete! All results have been saved to the 'output' folder.\n")
cat("A comprehensive report has been generated: output/suailoro_population_comparison_report.txt\n")