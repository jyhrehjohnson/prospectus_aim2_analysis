# ============================================================================
# ANALYSIS FUNCTIONS FOR AIM 2 SIMULATIONS
# Author: [Your Name]
# Date: [Date]
# Purpose: Analyze simulated datasets to validate species delimitation methods
# ============================================================================

# Required packages
required_packages <- c(
  "MASS",        # For multivariate stats
  "mclust",      # Model-based clustering
  "cluster",     # Silhouette analysis
  "lme4",        # Mixed models
  "brms",        # Bayesian models
  "tidyverse",   # Data manipulation
  "ggplot2",     # Plotting
  "cowplot",     # Multi-panel plots
  "viridis",     # Color palettes
  "factoextra",  # PCA visualization
  "caret"        # Cross-validation
)

# Install if needed
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------------------------------------------------------
# FUNCTION 1: Calculate Mahalanobis Distance Matrix
# ----------------------------------------------------------------------------

calc_mahalanobis_matrix <- function(data) {
  #' Calculate pairwise Mahalanobis distances
  #' 
  #' @param data Matrix or data.frame of measurements (rows=specimens, cols=variables)
  #' @return Distance matrix
  
  # Center data
  centered <- scale(data, center = TRUE, scale = FALSE)
  
  # Calculate pooled covariance matrix
  S <- cov(centered)
  
  # Invert (with check for singularity)
  if (det(S) == 0) {
    warning("Covariance matrix is singular. Adding small ridge.")
    S <- S + diag(0.001, ncol(S))
  }
  S_inv <- solve(S)
  
  # Calculate pairwise distances
  n <- nrow(data)
  D <- matrix(0, n, n)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      diff <- as.numeric(data[i,] - data[j,])
      D[i,j] <- sqrt(t(diff) %*% S_inv %*% diff)
      D[j,i] <- D[i,j]
    }
  }
  
  return(as.dist(D))
}

# ----------------------------------------------------------------------------
# FUNCTION 2: Calculate Euclidean Distance Matrix
# ----------------------------------------------------------------------------

calc_euclidean_matrix <- function(data) {
  #' Calculate pairwise Euclidean distances
  #' 
  #' @param data Matrix or data.frame of measurements
  #' @return Distance matrix
  
  return(dist(data, method = "euclidean"))
}

# ----------------------------------------------------------------------------
# FUNCTION 3: Perform Classification with Cross-Validation
# ----------------------------------------------------------------------------

classify_with_cv <- function(data, true_labels, metric = "mahalanobis", k_folds = 5) {
  #' Cross-validated classification
  #' 
  #' @param data Measurement data
  #' @param true_labels True taxon assignments
  #' @param metric Distance metric to use
  #' @param k_folds Number of CV folds
  #' @return List with accuracy, confusion matrix, and predictions
  
  library(caret)
  
  # Combine data and labels
  df <- data.frame(data)
  df$taxon <- true_labels
  
  # Set up cross-validation
  train_control <- trainControl(
    method = "cv",
    number = k_folds,
    savePredictions = "final"
  )
  
  # Train Linear Discriminant Analysis model
  if (metric == "mahalanobis") {
    model <- train(
      taxon ~ .,
      data = df,
      method = "lda",
      trControl = train_control
    )
  } else {
    # For Euclidean, use k-NN
    model <- train(
      taxon ~ .,
      data = df,
      method = "knn",
      trControl = train_control,
      tuneGrid = data.frame(k = 3)
    )
  }
  
  # Extract results
  accuracy <- max(model$results$Accuracy)
  conf_matrix <- confusionMatrix(model$pred$pred, model$pred$obs)
  
  return(list(
    accuracy = accuracy,
    confusion_matrix = conf_matrix,
    predictions = model$pred,
    model = model
  ))
}

# ----------------------------------------------------------------------------
# FUNCTION 4: Model-Based Clustering Analysis
# ----------------------------------------------------------------------------

analyze_clustering <- function(data, true_k = NULL, max_k = 6) {
  #' Perform model-based clustering and evaluate fit
  #' 
  #' @param data Measurement data
  #' @param true_k True number of clusters (if known)
  #' @param max_k Maximum number of clusters to test
  #' @return List with clustering results and diagnostics
  
  library(mclust)
  
  # Run model-based clustering
  mc <- Mclust(data, G = 1:max_k)
  
  # Extract results
  optimal_k <- mc$G
  bic_values <- mc$BIC
  classification <- mc$classification
  uncertainty <- mc$uncertainty
  
  # Silhouette analysis (if k > 1)
  if (optimal_k > 1) {
    dist_mat <- dist(data)
    sil <- silhouette(classification, dist_mat)
    avg_silhouette <- mean(sil[, 3])
  } else {
    avg_silhouette <- NA
  }
  
  # Calculate separation metrics
  within_ss <- sum(mc$uncertainty)
  between_ss <- sum(mc$z * log(mc$z + 1e-10))  # Entropy
  
  results <- list(
    optimal_k = optimal_k,
    true_k = true_k,
    bic = bic_values,
    classification = classification,
    uncertainty = uncertainty,
    silhouette = avg_silhouette,
    within_ss = within_ss,
    model = mc
  )
  
  return(results)
}

# ----------------------------------------------------------------------------
# FUNCTION 5: Temporal Variation Analysis
# ----------------------------------------------------------------------------

analyze_temporal_variation <- function(data, time_var, taxon_var) {
  #' Analyze temporal structure and variance partitioning
  #' 
  #' @param data Data frame with measurements
  #' @param time_var Name of time variable
  #' @param taxon_var Name of taxon variable
  #' @return List with model results and variance components
  
  library(lme4)
  
  # Get measurement columns (exclude metadata)
  measurement_cols <- setdiff(names(data), c("specimen_id", "taxon", "time_ma", 
                                             "region", "latitude", "true_species",
                                             "simulation", "true_n_species", 
                                             "expected_accuracy", "expected_result",
                                             "true_lineage"))
  
  results_list <- list()
  
  for (measure in measurement_cols) {
    
    # Model 1: No temporal effect (naive)
    formula_1 <- as.formula(paste(measure, "~ taxon"))
    model_1 <- lm(formula_1, data = data)
    
    # Model 2: Linear temporal effect
    formula_2 <- as.formula(paste(measure, "~ time_ma"))
    model_2 <- lm(formula_2, data = data)
    
    # Model 3: Temporal + taxon effects
    formula_3 <- as.formula(paste(measure, "~ time_ma + taxon"))
    model_3 <- lm(formula_3, data = data)
    
    # Model 4: Mixed model with random taxon effect
    formula_4 <- as.formula(paste(measure, "~ time_ma + (1|taxon)"))
    model_4 <- lmer(formula_4, data = data, REML = TRUE)
    
    # Compare models
    aic_comparison <- AIC(model_1, model_2, model_3, model_4)
    
    # Variance partitioning (from Model 4)
    var_comps <- as.data.frame(VarCorr(model_4))
    temporal_effect <- summary(model_4)$coefficients["time_ma", "Estimate"]
    
    results_list[[measure]] <- list(
      models = list(model_1, model_2, model_3, model_4),
      aic = aic_comparison,
      variance_components = var_comps,
      temporal_effect = temporal_effect
    )
  }
  
  return(results_list)
}

# ----------------------------------------------------------------------------
# FUNCTION 6: Geographic Variation Analysis
# ----------------------------------------------------------------------------

analyze_geographic_variation <- function(data, region_var) {
  #' Analyze geographic structure and variance partitioning
  #' 
  #' @param data Data frame with measurements
  #' @param region_var Name of region/location variable
  #' @return List with model results and variance components
  
  library(lme4)
  
  measurement_cols <- setdiff(names(data), c("specimen_id", "taxon", "time_ma", 
                                             "region", "latitude", "true_species",
                                             "simulation", "true_n_species", 
                                             "expected_accuracy", "expected_result"))
  
  results_list <- list()
  
  for (measure in measurement_cols) {
    
    # Model 1: No geographic effect
    formula_1 <- as.formula(paste(measure, "~ 1"))
    model_1 <- lm(formula_1, data = data)
    
    # Model 2: Fixed geographic effect
    formula_2 <- as.formula(paste(measure, "~ region"))
    model_2 <- lm(formula_2, data = data)
    
    # Model 3: Random geographic effect
    formula_3 <- as.formula(paste(measure, "~ (1|region)"))
    model_3 <- lmer(formula_3, data = data, REML = TRUE)
    
    # Compare models
    aic_comparison <- AIC(model_1, model_2, model_3)
    
    # Variance components
    var_comps <- as.data.frame(VarCorr(model_3))
    
    # Intraclass correlation (proportion of variance due to geography)
    icc <- var_comps$vcov[1] / (var_comps$vcov[1] + var_comps$vcov[2])
    
    results_list[[measure]] <- list(
      models = list(model_1, model_2, model_3),
      aic = aic_comparison,
      variance_components = var_comps,
      icc = icc
    )
  }
  
  return(results_list)
}

# ----------------------------------------------------------------------------
# FUNCTION 7: Comprehensive Simulation Analysis
# ----------------------------------------------------------------------------

analyze_simulation <- function(sim_data, sim_name) {
  #' Run complete analysis pipeline on a simulation
  #' 
  #' @param sim_data Simulated dataset
  #' @param sim_name Name of simulation (e.g., "SIM1")
  #' @return Comprehensive results object
  
  cat(paste0("\n========== Analyzing ", sim_name, " ==========\n"))
  
  # Extract measurements
  measurement_cols <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")
  measurements <- sim_data[, measurement_cols]
  true_taxa <- sim_data$taxon
  
  # 1. Distance metric comparison
  cat("1. Comparing distance metrics...\n")
  
  # Euclidean
  euclidean_result <- classify_with_cv(
    measurements, 
    true_taxa, 
    metric = "euclidean"
  )
  
  # Mahalanobis
  mahalanobis_result <- classify_with_cv(
    measurements,
    true_taxa,
    metric = "mahalanobis"
  )
  
  # 2. Clustering analysis
  cat("2. Running clustering analysis...\n")
  
  true_k <- unique(sim_data$true_n_species)[1]
  clustering_result <- analyze_clustering(
    measurements,
    true_k = true_k
  )
  
  # 3. PCA for visualization
  cat("3. Performing PCA...\n")
  
  pca_result <- prcomp(measurements, scale. = TRUE)
  pca_df <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    taxon = true_taxa,
    specimen_id = sim_data$specimen_id
  )
  
  # 4. Specific analyses based on simulation type
  temporal_result <- NULL
  geographic_result <- NULL
  
  if ("time_ma" %in% names(sim_data)) {
    cat("4. Analyzing temporal variation...\n")
    temporal_result <- analyze_temporal_variation(
      sim_data,
      time_var = "time_ma",
      taxon_var = "taxon"
    )
  }
  
  if ("region" %in% names(sim_data)) {
    cat("4. Analyzing geographic variation...\n")
    geographic_result <- analyze_geographic_variation(
      sim_data,
      region_var = "region"
    )
  }
  
  # Compile results
  results <- list(
    simulation = sim_name,
    true_k = true_k,
    n_specimens = nrow(sim_data),
    euclidean = euclidean_result,
    mahalanobis = mahalanobis_result,
    clustering = clustering_result,
    pca = list(model = pca_result, data = pca_df),
    temporal = temporal_result,
    geographic = geographic_result
  )
  
  cat(paste0("\n========== ", sim_name, " Complete ==========\n\n"))
  
  return(results)
}

# ----------------------------------------------------------------------------
# FUNCTION 8: Sensitivity Analysis - Sample Size
# ----------------------------------------------------------------------------

sensitivity_sample_size <- function(sim_data, sample_sizes = seq(5, 30, 5), 
                                    n_replicates = 50) {
  #' Test effect of sample size on classification accuracy
  #' 
  #' @param sim_data Full simulated dataset
  #' @param sample_sizes Vector of sample sizes to test
  #' @param n_replicates Number of replicates per sample size
  #' @return Data frame with results
  
  measurement_cols <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")
  results <- data.frame()
  
  for (n in sample_sizes) {
    cat(paste0("Testing n = ", n, "...\n"))
    
    for (rep in 1:n_replicates) {
      
      # Subsample
      subsample <- sim_data %>%
        group_by(taxon) %>%
        sample_n(min(n, n())) %>%  # Handle cases where taxon has < n specimens
        ungroup()
      
      # Run classification
      tryCatch({
        result <- classify_with_cv(
          subsample[, measurement_cols],
          subsample$taxon,
          metric = "mahalanobis",
          k_folds = min(5, nrow(subsample) / length(unique(subsample$taxon)))
        )
        
        results <- rbind(results, data.frame(
          sample_size = n,
          replicate = rep,
          accuracy = result$accuracy,
          n_taxa = length(unique(subsample$taxon))
        ))
      }, error = function(e) {
        # Skip if error (e.g., too few samples)
        NULL
      })
    }
  }
  
  return(results)
}

# ----------------------------------------------------------------------------
# FUNCTION 9: Sensitivity Analysis - Missing Data
# ----------------------------------------------------------------------------

sensitivity_missing_data <- function(sim_data, missing_pcts = seq(0, 50, 10),
                                     n_replicates = 50) {
  #' Test effect of missing data on classification accuracy
  #' 
  #' @param sim_data Full simulated dataset
  #' @param missing_pcts Vector of missing data percentages to test
  #' @param n_replicates Number of replicates per percentage
  #' @return Data frame with results
  
  measurement_cols <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")
  results <- data.frame()
  
  for (pct in missing_pcts) {
    cat(paste0("Testing ", pct, "% missing...\n"))
    
    for (rep in 1:n_replicates) {
      
      # Create copy
      sim_missing <- sim_data
      
      # Randomly delete data
      if (pct > 0) {
        n_cells <- nrow(sim_missing) * length(measurement_cols)
        n_to_delete <- round(n_cells * pct / 100)
        
        cells_to_delete <- sample(1:n_cells, n_to_delete)
        
        for (cell in cells_to_delete) {
          row <- ceiling(cell / length(measurement_cols))
          col <- ((cell - 1) %% length(measurement_cols)) + 1
          sim_missing[row, measurement_cols[col]] <- NA
        }
      }
      
      # Remove specimens with all NA
      sim_missing <- sim_missing[rowSums(is.na(sim_missing[, measurement_cols])) < 
                                   length(measurement_cols), ]
      
      # Run classification (with imputation)
      tryCatch({
        # Simple mean imputation
        for (col in measurement_cols) {
          sim_missing[[col]][is.na(sim_missing[[col]])] <- 
            mean(sim_missing[[col]], na.rm = TRUE)
        }
        
        result <- classify_with_cv(
          sim_missing[, measurement_cols],
          sim_missing$taxon,
          metric = "mahalanobis"
        )
        
        results <- rbind(results, data.frame(
          pct_missing = pct,
          replicate = rep,
          accuracy = result$accuracy
        ))
      }, error = function(e) {
        NULL
      })
    }
  }
  
  return(results)
}

# ----------------------------------------------------------------------------
# FUNCTION 10: Sensitivity Analysis - Measurement Error
# ----------------------------------------------------------------------------

sensitivity_measurement_error <- function(sim_data, error_sds = seq(0, 1, 0.1),
                                          n_replicates = 50) {
  #' Test effect of measurement error on classification accuracy
  #' 
  #' @param sim_data Full simulated dataset
  #' @param error_sds Vector of error standard deviations to test
  #' @param n_replicates Number of replicates per error level
  #' @return Data frame with results
  
  measurement_cols <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")
  results <- data.frame()
  
  for (error_sd in error_sds) {
    cat(paste0("Testing error SD = ", error_sd, "...\n"))
    
    for (rep in 1:n_replicates) {
      
      # Add measurement error
      sim_error <- sim_data
      
      if (error_sd > 0) {
        for (col in measurement_cols) {
          sim_error[[col]] <- sim_error[[col]] + rnorm(nrow(sim_error), 0, error_sd)
        }
      }
      
      # Run classification
      tryCatch({
        result <- classify_with_cv(
          sim_error[, measurement_cols],
          sim_error$taxon,
          metric = "mahalanobis"
        )
        
        results <- rbind(results, data.frame(
          error_sd = error_sd,
          replicate = rep,
          accuracy = result$accuracy
        ))
      }, error = function(e) {
        NULL
      })
    }
  }
  
  return(results)
}

# ----------------------------------------------------------------------------
# FUNCTION 11: Generate Summary Statistics
# ----------------------------------------------------------------------------

summarize_results <- function(all_results) {
  #' Create summary table of all simulation results
  #' 
  #' @param all_results List of results from analyze_simulation()
  #' @return Data frame with summary statistics
  
  summary_df <- data.frame()
  
  for (sim_name in names(all_results)) {
    result <- all_results[[sim_name]]
    
    summary_df <- rbind(summary_df, data.frame(
      Simulation = sim_name,
      True_K = result$true_k,
      Estimated_K = result$clustering$optimal_k,
      N_Specimens = result$n_specimens,
      Euclidean_Accuracy = result$euclidean$accuracy,
      Mahalanobis_Accuracy = result$mahalanobis$accuracy,
      Silhouette_Score = result$clustering$silhouette,
      PC1_Variance = summary(result$pca$model)$importance[2, 1] * 100,
      PC2_Variance = summary(result$pca$model)$importance[2, 2] * 100
    ))
  }
  
  return(summary_df)
}

cat("\n=== Analysis functions loaded successfully ===\n")
cat("Available functions:\n")
cat("  - calc_mahalanobis_matrix()\n")
cat("  - classify_with_cv()\n")
cat("  - analyze_clustering()\n")
cat("  - analyze_temporal_variation()\n")
cat("  - analyze_geographic_variation()\n")
cat("  - analyze_simulation() [Main function]\n")
cat("  - sensitivity_sample_size()\n")
cat("  - sensitivity_missing_data()\n")
cat("  - sensitivity_measurement_error()\n")
cat("  - summarize_results()\n")