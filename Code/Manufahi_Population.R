# =============================================================================
# Betano Population Projections using Official Census Data and Municipal Projections
# =============================================================================
# This script projects the population of Betano, Timor-Leste from 2022-2050
# using official census data and municipal population projections
#
# References:
# 1. Timor-Leste Census 2022: Betano population of 7,442
# 2. Municipal population projections: 
#    a) Official Table (2022-2035)
#    b) Alternative Extended Dataset (2022-2050)
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
# STEP 1: Define both sets of population projections for Manufahi municipality
# =============================================================================

# First dataset: From the official statistical table (2022-2035)
manufahi_official_table <- tibble(
  Year = 2022:2035,
  Population_Official = c(
    61280, # 2022
    62501, # 2023
    63697, # 2024
    64833, # 2025
    65936, # 2026
    67028, # 2027
    68102, # 2028
    69149, # 2029
    70168, # 2030
    71168, # 2031
    72157, # 2032
    73136, # 2033
    74106, # 2034
    75068  # 2035
  ),
  Growth_Rate_Official = c(
    NA,    # 2022 (base year)
    1.97,  # 2023
    1.90,  # 2024
    1.77,  # 2025
    1.69,  # 2026
    1.64,  # 2027
    1.59,  # 2028
    1.53,  # 2029
    1.46,  # 2030
    1.42,  # 2031
    1.38,  # 2032
    1.35,  # 2033
    1.32,  # 2034
    1.29   # 2035
  )
)

# Second dataset: Alternative extended projections (2022-2050)
manufahi_alternative <- tibble(
  Year = 2022:2050,
  Population_Alternative = c(
    61280, # 2022
    62501, # 2023
    63697, # 2024
    64825, # 2025 (slight difference from official table)
    65936, # 2026
    67028, # 2027
    68102, # 2028
    69149, # 2029
    70168, # 2030
    71168, # 2031
    72157, # 2032
    73136, # 2033
    74106, # 2034
    75068, # 2035
    76023, # 2036
    76974, # 2037
    77920, # 2038
    78859, # 2039
    79791, # 2040
    80723, # 2041
    81664, # 2042
    82613, # 2043
    83571, # 2044
    84525, # 2045
    85471, # 2046
    86408, # 2047
    87338, # 2048
    88272, # 2049
    89480  # 2050
  )
)

# Calculate growth rates for the alternative dataset
manufahi_alternative <- manufahi_alternative %>%
  mutate(
    Previous_Pop = lag(Population_Alternative),
    Growth_Rate_Alternative = ((Population_Alternative / Previous_Pop) - 1) * 100
  ) %>%
  select(-Previous_Pop)

# =============================================================================
# STEP 2: Compare the two datasets for the overlapping period (2022-2035)
# =============================================================================

# Merge the datasets for comparison
manufahi_comparison <- manufahi_official_table %>%
  left_join(manufahi_alternative %>% select(Year, Population_Alternative, Growth_Rate_Alternative), 
            by = "Year") %>%
  mutate(
    Population_Difference = Population_Alternative - Population_Official,
    Population_Pct_Diff = (Population_Difference / Population_Official) * 100,
    Growth_Rate_Difference = Growth_Rate_Alternative - Growth_Rate_Official
  )

# Print comparison results for the overlapping period
cat("Comparison of Manufahi Population Projections (2022-2035):\n")
print(manufahi_comparison %>% 
        select(Year, Population_Official, Population_Alternative, 
               Population_Difference, Population_Pct_Diff) %>%
        mutate(across(where(is.numeric), round, digits = 2)))

cat("\nComparison of Growth Rates (2022-2035):\n")
print(manufahi_comparison %>% 
        select(Year, Growth_Rate_Official, Growth_Rate_Alternative, Growth_Rate_Difference) %>%
        filter(!is.na(Growth_Rate_Official)) %>%
        mutate(across(where(is.numeric), round, digits = 2)))

# Calculate average differences
avg_pop_diff <- mean(abs(manufahi_comparison$Population_Difference), na.rm = TRUE)
avg_pop_pct_diff <- mean(abs(manufahi_comparison$Population_Pct_Diff), na.rm = TRUE)
avg_growth_diff <- mean(abs(manufahi_comparison$Growth_Rate_Difference), na.rm = TRUE)

cat("\nSummary Statistics for Comparison Period (2022-2035):\n")
cat(sprintf("Average absolute population difference: %.2f people\n", avg_pop_diff))
cat(sprintf("Average percentage difference: %.2f%%\n", avg_pop_pct_diff))
cat(sprintf("Average absolute growth rate difference: %.2f percentage points\n\n", avg_growth_diff))

# Visualize population comparison
population_comparison_plot <- manufahi_comparison %>%
  pivot_longer(cols = c(Population_Official, Population_Alternative),
               names_to = "Source", 
               values_to = "Population") %>%
  mutate(Source = case_when(
    Source == "Population_Official" ~ "Official Table (2022-2035)",
    Source == "Population_Alternative" ~ "Alternative Dataset (2022-2050)"
  )) %>%
  ggplot(aes(x = Year, y = Population, color = Source, group = Source)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Comparison of Manufahi Population Projections",
    subtitle = "Official Table vs. Alternative Dataset (2022-2035)",
    y = "Population",
    x = "Year"
  )
population_comparison_plot

# Save the plot
ggsave("output/manufahi_projection_comparison.png", population_comparison_plot, width = 10, height = 6)

# =============================================================================
# STEP 3: Extrapolate the official table projections to 2050
# =============================================================================

# For the official table, we need to extend the projections from 2035 to 2050
# We'll use a simple linear model based on the observed growth rates

# Fit a linear model to the official growth rates (excluding NA for 2022)
official_growth_data <- manufahi_official_table %>%
  filter(!is.na(Growth_Rate_Official))

official_growth_model <- lm(Growth_Rate_Official ~ Year, data = official_growth_data)

# Create a data frame for future years (2036-2050)
future_years <- tibble(Year = 2036:2050)

# Predict growth rates for future years (ensure they don't go below 0.5%)
future_growth_rates <- predict(official_growth_model, future_years)
future_growth_rates <- pmax(future_growth_rates, 0.5)

# Start with the 2035 population
extended_pop <- manufahi_official_table$Population_Official[14]  # 2035 population

# Calculate future populations using the predicted growth rates
future_populations <- numeric(length(future_years$Year))

for (i in 1:length(future_years$Year)) {
  extended_pop <- extended_pop * (1 + future_growth_rates[i]/100)
  future_populations[i] <- extended_pop
}

# Create extended official dataset
manufahi_official_extended <- bind_rows(
  manufahi_official_table,
  tibble(
    Year = future_years$Year,
    Population_Official = future_populations,
    Growth_Rate_Official = future_growth_rates
  )
)

# Merge the extended official dataset with the alternative dataset
manufahi_full_comparison <- manufahi_official_extended %>%
  left_join(manufahi_alternative, by = "Year") %>%
  mutate(
    Population_Difference = Population_Alternative - Population_Official,
    Population_Pct_Diff = (Population_Difference / Population_Official) * 100,
    Growth_Rate_Difference = Growth_Rate_Alternative - Growth_Rate_Official
  )

# Visualize full comparison (2022-2050)
full_comparison_plot <- manufahi_full_comparison %>%
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
  annotate("text", x = 2036, y = min(manufahi_full_comparison$Population_Official), 
           label = "Extrapolated", hjust = 0, vjust = 0) +
  theme_minimal() +
  labs(
    title = "Comparison of Manufahi Population Projections (2022-2050)",
    subtitle = "Official Table (Extended) vs. Alternative Dataset",
    y = "Population",
    x = "Year"
  )
full_comparison_plot

# Save the plot
ggsave("output/manufahi_full_projection_comparison.png", full_comparison_plot, width = 10, height = 6)

# =============================================================================
# STEP 4: Project Betano's population using the official growth rates
# =============================================================================

# Define the base population
betano_base_population <- 7442  # From 2022 census

# Create a data frame to store the projections
betano_projection <- tibble(
  Year = 2022:2050,
  Growth_Rate = manufahi_official_extended$Growth_Rate_Official,
  Population = NA_real_,
  Annual_Increase = NA_real_
)

# Set the initial population
betano_projection$Population[1] <- betano_base_population

# Calculate population year by year
for (i in 2:nrow(betano_projection)) {
  # Get previous year's population
  prev_pop <- betano_projection$Population[i-1]
  
  # Get current year's growth rate
  growth_rate <- betano_projection$Growth_Rate[i]
  
  # Calculate this year's population
  betano_projection$Population[i] <- prev_pop * (1 + growth_rate/100)
  
  # Calculate annual increase
  betano_projection$Annual_Increase[i] <- betano_projection$Population[i] - prev_pop
}

# Round the population figures to whole numbers (people)
betano_projection <- betano_projection %>%
  mutate(
    Population = round(Population),
    Annual_Increase = round(Annual_Increase)
  )

# Print key years for reference
key_years <- c(2022, 2025, 2030, 2035, 2040, 2045, 2050)
cat("\nBetano Population Projections (Key Years):\n")
print(betano_projection %>% filter(Year %in% key_years) %>% select(Year, Population, Growth_Rate))

# =============================================================================
# STEP 5: Create alternative scenarios (high, low, medium growth)
# =============================================================================
# We'll create high and low scenarios by adjusting the growth rates

# High scenario: Add 0.5 percentage points to the growth rate
betano_high <- betano_projection %>%
  mutate(
    Growth_Rate_High = if_else(!is.na(Growth_Rate), Growth_Rate + 0.5, NA_real_),
    Population_High = betano_base_population,  # Initialize with base population
    Annual_Increase_High = NA_real_
  )

# Calculate high scenario population
for (i in 2:nrow(betano_high)) {
  prev_pop <- betano_high$Population_High[i-1]
  growth_rate <- betano_high$Growth_Rate_High[i]
  
  betano_high$Population_High[i] <- prev_pop * (1 + growth_rate/100)
  betano_high$Annual_Increase_High[i] <- betano_high$Population_High[i] - prev_pop
}

# Low scenario: Subtract 0.5 percentage points from the growth rate (minimum 0.1%)
betano_low <- betano_projection %>%
  mutate(
    Growth_Rate_Low = if_else(!is.na(Growth_Rate), pmax(Growth_Rate - 0.5, 0.1), NA_real_),
    Population_Low = betano_base_population,  # Initialize with base population
    Annual_Increase_Low = NA_real_
  )

# Calculate low scenario population
for (i in 2:nrow(betano_low)) {
  prev_pop <- betano_low$Population_Low[i-1]
  growth_rate <- betano_low$Growth_Rate_Low[i]
  
  betano_low$Population_Low[i] <- prev_pop * (1 + growth_rate/100)
  betano_low$Annual_Increase_Low[i] <- betano_low$Population_Low[i] - prev_pop
}

# Round high and low scenario populations
betano_high <- betano_high %>%
  mutate(
    Population_High = round(Population_High),
    Annual_Increase_High = round(Annual_Increase_High)
  )

betano_low <- betano_low %>%
  mutate(
    Population_Low = round(Population_Low),
    Annual_Increase_Low = round(Annual_Increase_Low)
  )

# Prepare data for combined scenarios visualization
scenarios <- bind_rows(
  betano_projection %>% 
    select(Year, Growth_Rate, Population) %>%
    rename(Value = Population) %>%
    mutate(Scenario = "Medium (Official)"),
  
  betano_high %>% 
    select(Year, Growth_Rate_High, Population_High) %>%
    rename(Growth_Rate = Growth_Rate_High, Value = Population_High) %>%
    mutate(Scenario = "High"),
  
  betano_low %>% 
    select(Year, Growth_Rate_Low, Population_Low) %>%
    rename(Growth_Rate = Growth_Rate_Low, Value = Population_Low) %>%
    mutate(Scenario = "Low")
)

# Visualize all scenarios
scenarios_plot <- scenarios %>%
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2035.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2036, y = min(scenarios$Value, na.rm = TRUE), 
           label = "Beyond 2035", hjust = 0, vjust = 0) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Betano Population Projections (2022-2050)",
    subtitle = "Based on Official Manufahi Municipality Growth Rates",
    y = "Population",
    x = "Year"
  )
scenarios_plot

# Save the plot
ggsave("output/betano_population_scenarios.png", scenarios_plot, width = 10, height = 6)

# =============================================================================
# STEP 6: Calculate key statistics and save results
# =============================================================================

# Calculate total growth and percentages for different periods
periods <- tibble(
  Period = c("2022-2030", "2030-2040", "2040-2050", "2022-2050"),
  Start_Year = c(2022, 2030, 2040, 2022),
  End_Year = c(2030, 2040, 2050, 2050)
)

# Function to calculate growth statistics
calc_growth_stats <- function(data, pop_col, periods) {
  result <- tibble(
    Period = character(),
    Start_Population = numeric(),
    End_Population = numeric(),
    Absolute_Growth = numeric(),
    Percent_Growth = numeric(),
    Avg_Annual_Growth_Rate = numeric()
  )
  
  for (i in 1:nrow(periods)) {
    start_pop <- data[[pop_col]][data$Year == periods$Start_Year[i]]
    end_pop <- data[[pop_col]][data$Year == periods$End_Year[i]]
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

# Calculate statistics for all scenarios
medium_stats <- calc_growth_stats(betano_projection, "Population", periods) %>% 
  mutate(Scenario = "Medium (Official)")

high_stats <- calc_growth_stats(betano_high, "Population_High", periods) %>% 
  mutate(Scenario = "High")

low_stats <- calc_growth_stats(betano_low, "Population_Low", periods) %>% 
  mutate(Scenario = "Low")

all_stats <- bind_rows(medium_stats, high_stats, low_stats)

# Format statistics for display/export
all_stats_formatted <- all_stats %>%
  mutate(
    Start_Population = round(Start_Population),
    End_Population = round(End_Population),
    Absolute_Growth = round(Absolute_Growth),
    Percent_Growth = round(Percent_Growth, 1),
    Avg_Annual_Growth_Rate = round(Avg_Annual_Growth_Rate, 2)
  )

# Print summary statistics for medium scenario
cat("\nBetano Population Growth Statistics (Medium Scenario):\n")
print(medium_stats %>% 
        mutate(across(where(is.numeric), \(x) round(x, digits = 1))) %>% 
        select(-Scenario))

# Prepare a comprehensive dataset with all projections
all_projections <- betano_projection %>%
  left_join(
    betano_high %>% select(Year, Population_High, Growth_Rate_High),
    by = "Year"
  ) %>%
  left_join(
    betano_low %>% select(Year, Population_Low, Growth_Rate_Low),
    by = "Year"
  ) %>%
  # Add original Manufahi projections for reference
  left_join(
    manufahi_official_extended %>% 
      select(Year, Population_Official, Growth_Rate_Official),
    by = "Year"
  ) %>%
  left_join(
    manufahi_alternative %>% 
      select(Year, Population_Alternative, Growth_Rate_Alternative),
    by = "Year"
  )

# Save all data to files
write_csv(manufahi_comparison, "output/manufahi_data_comparison.csv")
write_csv(all_projections, "output/betano_all_projections.csv")
write_csv(all_stats_formatted, "output/betano_growth_statistics.csv")

# Also save as Excel for easier use
write_xlsx(list(
  "Manufahi Data Comparison" = manufahi_comparison,
  "Detailed Projections" = all_projections,
  "Growth Statistics" = all_stats_formatted,
  "Manufahi Official" = manufahi_official_table,
  "Manufahi Alternative" = manufahi_alternative
), "output/betano_population_projections.xlsx")

# =============================================================================
# STEP 7: Create a formatted report summarizing the findings
# =============================================================================

# Create a text report
report_file <- file("output/betano_population_report.txt", "w")

cat("=============================================================\n", file = report_file)
cat("         BETANO (TIMOR-LESTE) POPULATION PROJECTIONS         \n", file = report_file)
cat("      BASED ON OFFICIAL MANUFAHI POPULATION PROJECTIONS      \n", file = report_file)
cat("=============================================================\n\n", file = report_file)

cat("DATA SOURCES:\n", file = report_file)
cat("- 2022 Census: Betano population of 7,442\n", file = report_file)
cat("- Official Table: Manufahi Municipality population projections (2022-2035)\n", file = report_file)
cat("- Alternative Dataset: Extended Manufahi population projections (2022-2050)\n\n", file = report_file)

cat("COMPARISON OF MANUFAHI DATA SOURCES:\n", file = report_file)
cat(sprintf("- Average population difference: %.2f people (%.2f%%)\n", 
            avg_pop_diff, avg_pop_pct_diff), file = report_file)
cat(sprintf("- Average growth rate difference: %.2f percentage points\n", 
            avg_growth_diff), file = report_file)
cat("- The two datasets are highly consistent, with the main difference being\n", file = report_file)
cat("  a slight discrepancy in the 2025 population (64,833 vs. 64,825)\n\n", file = report_file)

cat("METHODOLOGY:\n", file = report_file)
cat("- Starting with the official 2022 census population of 7,442 for Betano\n", file = report_file)
cat("- Using growth rates from the official table (2022-2035)\n", file = report_file)
cat("- Extrapolating growth rates to 2050 using trend analysis\n", file = report_file)
cat("- Creating high/low scenarios by adjusting growth rates by ±0.5 percentage points\n\n", file = report_file)

cat("KEY POPULATION PROJECTIONS (MEDIUM/OFFICIAL SCENARIO):\n", file = report_file)
cat(sprintf("%-6s  %12s  %17s\n", "Year", "Population", "Growth Rate (%)"), file = report_file)
cat("------  ------------  -----------------\n", file = report_file)
for (year in key_years) {
  row_data <- betano_projection %>% filter(Year == year)
  cat(sprintf("%-6d  %12d  %17.2f\n", 
              row_data$Year, 
              row_data$Population, 
              row_data$Growth_Rate), 
      file = report_file)
}
cat("\n", file = report_file)

# Print scenario comparisons for 2050
cat("2050 POPULATION PROJECTIONS BY SCENARIO:\n", file = report_file)
cat(sprintf("Low scenario:        %d people\n", 
            round(betano_low$Population_Low[betano_low$Year == 2050])), 
    file = report_file)
cat(sprintf("Medium scenario:     %d people\n", 
            round(betano_projection$Population[betano_projection$Year == 2050])), 
    file = report_file)
cat(sprintf("High scenario:       %d people\n", 
            round(betano_high$Population_High[betano_high$Year == 2050])), 
    file = report_file)
cat("\n", file = report_file)

# Print growth statistics
cat("GROWTH STATISTICS (MEDIUM/OFFICIAL SCENARIO):\n", file = report_file)
cat(sprintf("%-12s  %8s  %8s  %8s  %8s  %8s\n", 
            "Period", "Start Pop", "End Pop", "Growth", "Growth %", "Avg Rate"), 
    file = report_file)
cat("------------  --------  --------  --------  --------  --------\n", file = report_file)

for (i in 1:nrow(medium_stats)) {
  cat(sprintf("%-12s  %8d  %8d  %8d  %8.1f%%  %8.2f%%\n", 
              medium_stats$Period[i],
              round(medium_stats$Start_Population[i]),
              round(medium_stats$End_Population[i]),
              round(medium_stats$Absolute_Growth[i]),
              round(medium_stats$Percent_Growth[i], 1),
              round(medium_stats$Avg_Annual_Growth_Rate[i], 2)), 
      file = report_file)
}
cat("\n", file = report_file)

cat("COMPARISON WITH MANUFAHI MUNICIPALITY:\n", file = report_file)
cat(sprintf("In 2022, Betano's population (%d) represented %.1f%% of Manufahi's total population (%d).\n", 
            betano_base_population, 
            (betano_base_population / manufahi_official_table$Population_Official[1]) * 100,
            manufahi_official_table$Population_Official[1]), 
    file = report_file)
cat(sprintf("By 2050, Betano's projected population (%d) will represent %.1f%% of Manufahi's projected population (%d).\n\n", 
            round(betano_projection$Population[betano_projection$Year == 2050]),
            (round(betano_projection$Population[betano_projection$Year == 2050]) / 
               manufahi_official_extended$Population_Official[manufahi_official_extended$Year == 2050]) * 100,
            round(manufahi_official_extended$Population_Official[manufahi_official_extended$Year == 2050])), 
    file = report_file)

cat("NOTES AND LIMITATIONS:\n", file = report_file)
cat("- These projections assume Betano will follow the same growth pattern as the wider Manufahi municipality\n", file = report_file)
cat("- Local factors specific to Betano (e.g., economic development projects) may cause deviations\n", file = report_file)
cat("- Projections beyond 2035 involve greater uncertainty as they are extrapolated from trends\n", file = report_file)
cat("- The high/low scenarios provide a reasonable range of potential outcomes\n\n", file = report_file)

cat("NEXT STEPS FOR ANALYSIS:\n", file = report_file)
cat("- Incorporate local development plans specific to Betano\n", file = report_file)
cat("- Analyze age structure and demographic composition\n", file = report_file)
cat("- Consider spatial distribution of population within the Betano area\n", file = report_file)
cat("- Update projections when new census or survey data becomes available\n\n", file = report_file)

cat("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = report_file)
cat("=============================================================\n", file = report_file)

close(report_file)

cat("\nAnalysis complete! All results have been saved to the 'output' folder.\n")
cat("A comprehensive report has been generated: output/betano_population_report.txt\n")
cat("Population projections are available in both CSV and Excel formats.\n")