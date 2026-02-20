# ============================================================================
# MASTER SIMULATION SCRIPT FOR AIM 2
# Generates all datasets for method development and validation
# ============================================================================

library(MASS)
library(tidyverse)

# Set master seed for reproducibility
set.seed(2024)

# ----------------------------------------------------------------------------
# SHARED PARAMETERS
# ----------------------------------------------------------------------------

# Correlation structure for dental measurements (realistic)
# Based on Gómez-Robles et al. 2015, PNAS
cor_matrix <- matrix(c(
  1.00, 0.75, 0.70, 0.65, 0.60,
  0.75, 1.00, 0.65, 0.70, 0.55,
  0.70, 0.65, 1.00, 0.75, 0.60,
  0.65, 0.70, 0.75, 1.00, 0.55,
  0.60, 0.55, 0.60, 0.55, 1.00
), nrow = 5)

# Measurement names
measurement_names <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")

# Helper function to generate species data
generate_species <- function(n, mean_vec, sd_vec, cor_mat, species_name) {
  cov_mat <- diag(sd_vec) %*% cor_mat %*% diag(sd_vec)
  data <- mvrnorm(n, mean_vec, cov_mat)
  colnames(data) <- measurement_names
  
  df <- as.data.frame(data)
  df$specimen_id <- paste0(species_name, "_", 1:n)
  df$taxon <- species_name
  
  return(df)
}

# ----------------------------------------------------------------------------
# SIMULATION 1: EASY SEPARATION (Positive Control)
# ----------------------------------------------------------------------------

sim1 <- rbind(
  generate_species(
    n = 25,
    mean_vec = c(20, 18, 21, 19, 12),
    sd_vec = c(0.7, 0.6, 0.8, 0.7, 0.5),
    cor_mat = cor_matrix,
    species_name = "Species_A"
  ),
  generate_species(
    n = 25,
    mean_vec = c(15, 14, 16, 15, 10),
    sd_vec = c(0.6, 0.5, 0.7, 0.6, 0.5),
    cor_mat = cor_matrix,
    species_name = "Species_B"
  ),
  generate_species(
    n = 25,
    mean_vec = c(12, 11, 13, 12, 8),
    sd_vec = c(0.5, 0.4, 0.6, 0.5, 0.4),
    cor_mat = cor_matrix,
    species_name = "Species_C"
  )
)

sim1$simulation <- "SIM1_Easy_Separation"
sim1$true_n_species <- 3
sim1$expected_accuracy <- ">95%"

write.csv(sim1, "simulation1_easy_separation.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIMULATION 2: MODERATE SEPARATION (Realistic Test)
# ----------------------------------------------------------------------------

sim2 <- rbind(
  generate_species(
    n = 20,
    mean_vec = c(16.5, 15.0, 17.0, 16.0, 11.0),
    sd_vec = c(0.9, 0.8, 1.0, 0.9, 0.7),
    cor_mat = cor_matrix,
    species_name = "Species_A"
  ),
  generate_species(
    n = 20,
    mean_vec = c(14.0, 12.5, 14.5, 13.5, 9.5),
    sd_vec = c(0.9, 0.8, 1.0, 0.9, 0.7),
    cor_mat = cor_matrix,
    species_name = "Species_B"
  ),
  generate_species(
    n = 20,
    mean_vec = c(15.0, 13.5, 15.5, 14.5, 10.0),
    sd_vec = c(0.9, 0.8, 1.0, 0.9, 0.7),
    cor_mat = cor_matrix,
    species_name = "Species_C"
  )
)

sim2$simulation <- "SIM2_Moderate_Separation"
sim2$true_n_species <- 3
sim2$expected_accuracy <- "75-85%"

write.csv(sim2, "simulation2_moderate_separation.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIMULATION 3: OVERSPLIT (Critical Negative Control)
# ----------------------------------------------------------------------------

# Generate ONE species
true_species <- generate_species(
  n = 60,
  mean_vec = c(15, 14, 16, 15, 10),
  sd_vec = c(1.2, 1.1, 1.3, 1.2, 0.9),  # High variance
  cor_mat = cor_matrix,
  species_name = "True_Species"
)

# Artificially "split" into 3 taxa
sim3 <- true_species
sim3$taxon <- rep(c("Taxon_A", "Taxon_B", "Taxon_C"), each = 20)

# Add subtle site effects (makes it harder to detect it's one species)
sim3$M1_BL[1:20] <- sim3$M1_BL[1:20] + rnorm(20, 0.3, 0.2)
sim3$M1_BL[41:60] <- sim3$M1_BL[41:60] - rnorm(20, 0.3, 0.2)

sim3$simulation <- "SIM3_Oversplit"
sim3$true_n_species <- 1  # Actually just ONE species
sim3$expected_result <- "Should_NOT_distinguish_taxa"

write.csv(sim3, "simulation3_oversplit.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIMULATION 4: CHRONOSPECIES (Temporal Variation)
# ----------------------------------------------------------------------------

n_time_points <- 5
n_per_time <- 15
evolutionary_rate <- 0.4

sim4 <- data.frame()

for (time in 1:n_time_points) {
  
  time_specimens <- generate_species(
    n = n_per_time,
    mean_vec = c(14, 13, 15, 14, 9) + evolutionary_rate * time,
    sd_vec = c(0.8, 0.7, 0.9, 0.8, 0.6),
    cor_mat = cor_matrix,
    species_name = paste0("Time_", time)
  )
  
  time_specimens$time_ma <- time
  time_specimens$true_lineage <- "Chronospecies_A"
  
  sim4 <- rbind(sim4, time_specimens)
}

sim4$simulation <- "SIM4_Chronospecies"
sim4$true_n_species <- 1  # ONE evolving lineage
sim4$expected_result <- "Temporal_model_should_recognize_single_lineage"

write.csv(sim4, "simulation4_chronospecies.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIMULATION 5: GEOGRAPHIC VARIATION
# ----------------------------------------------------------------------------

n_regions <- 3
n_per_region <- 20

base_mean <- c(15, 14, 16, 15, 10)
regional_offsets <- list(
  c(0, 0, 0, 0, 0),
  c(0.4, 0.3, 0.5, 0.4, 0.2),
  c(-0.3, -0.2, -0.4, -0.3, -0.1)
)

sim5 <- data.frame()

for (region in 1:n_regions) {
  
  region_specimens <- generate_species(
    n = n_per_region,
    mean_vec = base_mean + regional_offsets[[region]],
    sd_vec = c(0.8, 0.7, 0.9, 0.8, 0.6),
    cor_mat = cor_matrix,
    species_name = paste0("Region_", region)
  )
  
  region_specimens$region <- paste0("Region_", region)
  region_specimens$latitude <- c(0, 15, 30)[region]
  region_specimens$true_species <- "Widespread_Species"
  
  sim5 <- rbind(sim5, region_specimens)
}

sim5$simulation <- "SIM5_Geographic_Variation"
sim5$true_n_species <- 1
sim5$expected_result <- "Geographic_model_should_recognize_single_species"

write.csv(sim5, "simulation5_geographic_variation.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# METADATA SUMMARY
# ----------------------------------------------------------------------------

metadata <- data.frame(
  Simulation = c("SIM1", "SIM2", "SIM3", "SIM4", "SIM5"),
  Description = c(
    "Easy separation - positive control",
    "Moderate separation - realistic test",
    "Oversplit taxa - negative control",
    "Chronospecies - temporal variation",
    "Geographic variation - spatial structure"
  ),
  True_N_Species = c(3, 3, 1, 1, 1),
  N_Specimens = c(75, 60, 60, 75, 60),
  Expected_Accuracy = c(">95%", "75-85%", "<60%", "N/A", "N/A"),
  Purpose = c(
    "Verify method works when it should",
    "Test realistic Australopithecus scenario",
    "Test ability to detect oversplitting",
    "Test temporal modeling",
    "Test geographic modeling"
  )
)

write.csv(metadata, "simulation_metadata.csv", row.names = FALSE)

print("All simulations generated successfully!")
print(metadata)