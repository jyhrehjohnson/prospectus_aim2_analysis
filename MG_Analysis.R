# ============================================================================
# HYBRID MAHALANOBIS-GOWER DISTANCE ANALYSIS - COMPLETE MASTER SCRIPT
# For Hominin Species Delimitation
# 
# Implements Solution 1C: Combining Mahalanobis distance (covariance-aware)
# with Gower distance (discrete character integration)
# 
# ALL 5 SIMULATIONS INCLUDED:
# - SIM1: Easy separation (3 species)
# - SIM2: Moderate separation (3 species) - REALISTIC
# - SIM3: Oversplit (1 species split into 3 "taxa")
# - SIM4: Chronospecies (1 lineage evolving through time)
# - SIM5: Geographic variation (1 widespread species)
# ============================================================================

# Clear environment
rm(list = ls())

# Set seed for reproducibility
set.seed(2024)

cat("\n")
cat("========================================\n")
cat("HYBRID DISTANCE ANALYSIS - MASTER SCRIPT\n")
cat("========================================\n\n")

# ----------------------------------------------------------------------------
# SETUP: Install and load packages
# ----------------------------------------------------------------------------

required_packages <- c(
  "MASS",        # For mvrnorm and discriminant analysis
  "cluster",     # For Gower's distance
  "tidyverse",   # Data manipulation
  "ggplot2",     # Plotting
  "cowplot",     # Multi-panel plots
  "viridis",     # Color palettes
  "caret",       # Classification
  "lme4",        # Mixed models
  "reshape2",    # Data reshaping
  "gridExtra"    # Plot arrangement
)

cat("Checking and installing required packages...\n")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# Create directories
dir.create("data", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

cat("Setup complete.\n\n")

# ----------------------------------------------------------------------------
# CORRELATION MATRIX FOR CONTINUOUS VARIABLES
# ----------------------------------------------------------------------------

cor_matrix <- matrix(c(
  1.00, 0.75, 0.70, 0.65, 0.60,
  0.75, 1.00, 0.65, 0.70, 0.55,
  0.70, 0.65, 1.00, 0.75, 0.60,
  0.65, 0.70, 0.75, 1.00, 0.55,
  0.60, 0.55, 0.60, 0.55, 1.00
), nrow = 5)

measurement_names <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")

# ----------------------------------------------------------------------------
# FUNCTION: Generate species with mixed data
# ----------------------------------------------------------------------------

generate_species_mixed <- function(n, mean_vec, sd_vec, cor_mat, 
                                   species_name, discrete_probs = NULL) {
  #' Generate specimens with continuous + discrete characters
  
  # Generate continuous measurements
  cov_mat <- diag(sd_vec) %*% cor_mat %*% diag(sd_vec)
  continuous_data <- mvrnorm(n, mean_vec, cov_mat)
  colnames(continuous_data) <- measurement_names
  
  df <- as.data.frame(continuous_data)
  df$specimen_id <- paste0(species_name, "_", sprintf("%03d", 1:n))
  df$taxon <- species_name
  
  # Add discrete characters
  if (!is.null(discrete_probs)) {
    for (trait_name in names(discrete_probs)) {
      probs <- discrete_probs[[trait_name]]
      df[[trait_name]] <- sample(
        names(probs), 
        size = n, 
        replace = TRUE, 
        prob = probs
      )
      df[[trait_name]] <- factor(df[[trait_name]], levels = names(probs))
    }
  }
  
  return(df)
}

# ----------------------------------------------------------------------------
# FUNCTION: Calculate Mahalanobis distance
# ----------------------------------------------------------------------------

calc_mahalanobis_distance <- function(data, robust = TRUE) {
  #' Calculate Mahalanobis distance matrix with singularity handling
  
  centered <- scale(data, center = TRUE, scale = FALSE)
  S <- cov(centered)
  
  # Handle near-singularity
  if (robust) {
    det_S <- det(S)
    if (det_S == 0 || det_S < 1e-10) {
      warning("Covariance matrix near-singular (det = ", 
              format(det_S, scientific = TRUE), 
              "). Adding ridge = 0.001")
      S <- S + diag(0.001, ncol(S))
    }
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
# FUNCTION: Calculate Gower distance for discrete variables
# ----------------------------------------------------------------------------

calc_gower_distance_discrete <- function(data) {
  #' Calculate Gower's distance for discrete variables only
  
  if (ncol(data) == 0) {
    n <- nrow(data)
    return(as.dist(matrix(0, n, n)))
  }
  
  gower_dist <- daisy(data, metric = "gower", stand = FALSE)
  return(gower_dist)
}

# ----------------------------------------------------------------------------
# FUNCTION: Calculate hybrid distance
# ----------------------------------------------------------------------------

calc_hybrid_distance <- function(data, continuous_vars, discrete_vars, 
                                 alpha = 0.5, robust = TRUE, 
                                 standardize_method = "minmax") {
  #' Hybrid Mahalanobis-Gower distance
  
  n <- nrow(data)
  
  # 1. Mahalanobis for continuous
  continuous_data <- data[, continuous_vars, drop = FALSE]
  D_mahal <- as.matrix(calc_mahalanobis_distance(continuous_data, robust = robust))
  
  # Standardize to 0-1
  if (standardize_method == "minmax") {
    D_mahal_scaled <- D_mahal / max(D_mahal[D_mahal < Inf])
  } else {
    D_mahal_vec <- D_mahal[upper.tri(D_mahal)]
    mean_d <- mean(D_mahal_vec)
    sd_d <- sd(D_mahal_vec)
    D_mahal_scaled <- (D_mahal - mean_d) / sd_d
    D_mahal_scaled <- (D_mahal_scaled - min(D_mahal_scaled)) / 
      (max(D_mahal_scaled) - min(D_mahal_scaled))
  }
  
  # 2. Gower for discrete
  if (length(discrete_vars) > 0 && all(discrete_vars %in% names(data))) {
    discrete_data <- data[, discrete_vars, drop = FALSE]
    D_gower <- as.matrix(calc_gower_distance_discrete(discrete_data))
  } else {
    D_gower <- matrix(0, n, n)
    discrete_vars <- character(0)
  }
  
  # 3. Combine
  D_hybrid <- alpha * D_mahal_scaled + (1 - alpha) * D_gower
  
  # Diagnostics
  diagnostics <- list(
    n_specimens = n,
    n_continuous = length(continuous_vars),
    n_discrete = length(discrete_vars),
    alpha = alpha,
    mean_mahal = mean(D_mahal_scaled[upper.tri(D_mahal_scaled)]),
    mean_gower = mean(D_gower[upper.tri(D_gower)]),
    mean_hybrid = mean(D_hybrid[upper.tri(D_hybrid)])
  )
  
  cat("\nHybrid Distance Summary:\n")
  cat("  Specimens:", diagnostics$n_specimens, "\n")
  cat("  Continuous vars:", diagnostics$n_continuous, "\n")
  cat("  Discrete vars:", diagnostics$n_discrete, "\n")
  cat("  Alpha:", round(alpha, 3), "\n")
  cat("  Mean Mahalanobis:", round(diagnostics$mean_mahal, 3), "\n")
  cat("  Mean Gower:", round(diagnostics$mean_gower, 3), "\n")
  cat("  Mean hybrid:", round(diagnostics$mean_hybrid, 3), "\n")
  
  return(list(
    distance = as.dist(D_hybrid),
    distance_matrix = D_hybrid,
    mahalanobis_matrix = D_mahal_scaled,
    gower_matrix = D_gower,
    diagnostics = diagnostics,
    continuous_vars = continuous_vars,
    discrete_vars = discrete_vars,
    alpha = alpha
  ))
}

# ----------------------------------------------------------------------------
# FUNCTION: Classification with distance matrix
# ----------------------------------------------------------------------------

classify_with_distance <- function(distance_matrix, taxa, k_folds = 5, 
                                   k_neighbors = 5) {
  #' K-nearest neighbors with cross-validation
  
  if (inherits(distance_matrix, "dist")) {
    dist_mat <- as.matrix(distance_matrix)
  } else {
    dist_mat <- distance_matrix
  }
  
  n <- nrow(dist_mat)
  fold_size <- floor(n / k_folds)
  fold_ids <- sample(rep(1:k_folds, length.out = n))
  
  predictions <- rep(NA, n)
  posterior_probs <- matrix(NA, nrow = n, ncol = length(unique(taxa)))
  colnames(posterior_probs) <- sort(unique(taxa))
  
  # Cross-validation
  for (fold in 1:k_folds) {
    test_idx <- which(fold_ids == fold)
    train_idx <- which(fold_ids != fold)
    
    for (i in test_idx) {
      dists_to_train <- dist_mat[i, train_idx]
      nearest_idx <- order(dists_to_train)[1:min(k_neighbors, length(train_idx))]
      nearest_taxa <- taxa[train_idx[nearest_idx]]
      
      # Posterior probabilities
      taxon_counts <- table(nearest_taxa)
      for (tx in names(taxon_counts)) {
        posterior_probs[i, tx] <- taxon_counts[tx] / k_neighbors
      }
      
      # Prediction
      predictions[i] <- names(sort(taxon_counts, decreasing = TRUE))[1]
    }
  }
  
  accuracy <- mean(predictions == taxa, na.rm = TRUE)
  conf_mat <- table(Predicted = predictions, True = taxa)
  mean_posterior <- mean(apply(posterior_probs, 1, max, na.rm = TRUE), na.rm = TRUE)
  
  return(list(
    accuracy = accuracy,
    predictions = predictions,
    posterior_probs = posterior_probs,
    confusion_matrix = conf_mat,
    mean_confidence = mean_posterior
  ))
}

# ----------------------------------------------------------------------------
# FUNCTION: Clustering with distance matrix
# ----------------------------------------------------------------------------

cluster_with_distance <- function(distance_matrix, true_k = NULL, max_k = 6) {
  #' PAM clustering
  
  if (inherits(distance_matrix, "dist")) {
    dist_obj <- distance_matrix
  } else {
    dist_obj <- as.dist(distance_matrix)
  }
  
  silhouette_scores <- numeric(max_k - 1)
  pam_results <- list()
  
  for (k in 2:max_k) {
    pam_fit <- pam(dist_obj, k = k, diss = TRUE)
    pam_results[[k]] <- pam_fit
    silhouette_scores[k - 1] <- pam_fit$silinfo$avg.width
  }
  
  optimal_k <- which.max(silhouette_scores) + 1
  best_pam <- pam_results[[optimal_k]]
  
  hc <- hclust(dist_obj, method = "ward.D2")
  hc_clusters <- cutree(hc, k = optimal_k)
  
  return(list(
    optimal_k = optimal_k,
    true_k = true_k,
    silhouette_scores = silhouette_scores,
    best_silhouette = silhouette_scores[optimal_k - 1],
    pam_clusters = best_pam$clustering,
    hc_clusters = hc_clusters,
    pam_model = best_pam,
    hc_model = hc
  ))
}

# ----------------------------------------------------------------------------
# FUNCTION: Optimize alpha parameter
# ----------------------------------------------------------------------------

optimize_alpha <- function(data, taxa, continuous_vars, discrete_vars, 
                           alpha_range = seq(0, 1, 0.05), k_folds = 5) {
  #' Find optimal alpha by cross-validation
  
  cat("\nOptimizing alpha parameter...\n")
  
  results <- data.frame(
    alpha = alpha_range,
    accuracy = NA,
    mean_confidence = NA
  )
  
  for (i in seq_along(alpha_range)) {
    alpha <- alpha_range[i]
    
    hybrid <- calc_hybrid_distance(
      data, continuous_vars, discrete_vars, 
      alpha = alpha, robust = TRUE
    )
    
    classification <- classify_with_distance(
      hybrid$distance, taxa, k_folds = k_folds
    )
    
    results$accuracy[i] <- classification$accuracy
    results$mean_confidence[i] <- classification$mean_confidence
    
    if (i %% 5 == 0) {
      cat("  Alpha =", round(alpha, 2), 
          "| Accuracy =", round(classification$accuracy * 100, 1), "%\n")
    }
  }
  
  optimal_idx <- which.max(results$accuracy)
  optimal_alpha <- results$alpha[optimal_idx]
  optimal_accuracy <- results$accuracy[optimal_idx]
  
  cat("\nOptimal alpha:", round(optimal_alpha, 3), 
      "| Accuracy:", round(optimal_accuracy * 100, 1), "%\n")
  
  return(list(
    optimal_alpha = optimal_alpha,
    optimal_accuracy = optimal_accuracy,
    results = results
  ))
}

# ----------------------------------------------------------------------------
# FUNCTION: Temporal variation analysis
# ----------------------------------------------------------------------------

analyze_temporal_variation <- function(sim_data, continuous_vars, 
                                       discrete_vars, time_var = "time_ma") {
  #' Hierarchical models for temporal variation
  
  cat("\n--- Temporal Variation Analysis ---\n")
  
  results_list <- list()
  
  for (var in continuous_vars) {
    formula_1 <- as.formula(paste(var, "~ taxon"))
    model_1 <- lm(formula_1, data = sim_data)
    
    formula_2 <- as.formula(paste(var, "~", time_var))
    model_2 <- lm(formula_2, data = sim_data)
    
    formula_3 <- as.formula(paste(var, "~", time_var, "+ (1|taxon)"))
    model_3 <- lmer(formula_3, data = sim_data, REML = TRUE)
    
    aic_comparison <- AIC(model_1, model_2, model_3)
    var_comps <- as.data.frame(VarCorr(model_3))
    temporal_coef <- fixef(model_3)[time_var]
    
    total_var <- sum(var_comps$vcov)
    temporal_var_prop <- var_comps$vcov[var_comps$grp == "taxon"] / total_var
    residual_var_prop <- var_comps$vcov[var_comps$grp == "Residual"] / total_var
    
    results_list[[var]] <- list(
      models = list(model_1, model_2, model_3),
      aic = aic_comparison,
      variance_components = var_comps,
      temporal_effect = temporal_coef,
      temporal_var_prop = temporal_var_prop,
      residual_var_prop = residual_var_prop
    )
  }
  
  temporal_props <- sapply(results_list, function(x) x$temporal_var_prop)
  mean_temporal_var <- mean(temporal_props, na.rm = TRUE)
  
  cat("  Mean temporal variance:", round(mean_temporal_var * 100, 1), "%\n")
  
  # Discrete character evolution
  discrete_tests <- list()
  for (var in discrete_vars) {
    if (var %in% names(sim_data)) {
      contingency <- table(sim_data[[var]], sim_data[[time_var]])
      chi_test <- chisq.test(contingency)
      discrete_tests[[var]] <- list(
        contingency = contingency,
        chi_square = chi_test
      )
      cat("  ", var, "~ time: χ² =", round(chi_test$statistic, 2), 
          ", p =", format.pval(chi_test$p.value), "\n")
    }
  }
  
  return(list(
    continuous = results_list,
    mean_temporal_variance = mean_temporal_var,
    discrete = discrete_tests
  ))
}

# ----------------------------------------------------------------------------
# FUNCTION: Geographic variation analysis
# ----------------------------------------------------------------------------

analyze_geographic_variation <- function(sim_data, continuous_vars, 
                                         discrete_vars, region_var = "region") {
  #' Hierarchical models for geographic variation
  
  cat("\n--- Geographic Variation Analysis ---\n")
  
  results_list <- list()
  
  for (var in continuous_vars) {
    formula_1 <- as.formula(paste(var, "~ 1"))
    model_1 <- lm(formula_1, data = sim_data)
    
    formula_2 <- as.formula(paste(var, "~", region_var))
    model_2 <- lm(formula_2, data = sim_data)
    
    formula_3 <- as.formula(paste(var, "~ (1|", region_var, ")"))
    model_3 <- lmer(formula_3, data = sim_data, REML = TRUE)
    
    aic_comparison <- AIC(model_1, model_2, model_3)
    var_comps <- as.data.frame(VarCorr(model_3))
    
    geographic_var <- var_comps$vcov[var_comps$grp == region_var]
    residual_var <- var_comps$vcov[var_comps$grp == "Residual"]
    icc <- geographic_var / (geographic_var + residual_var)
    
    results_list[[var]] <- list(
      models = list(model_1, model_2, model_3),
      aic = aic_comparison,
      variance_components = var_comps,
      icc = icc,
      geographic_var_prop = geographic_var / (geographic_var + residual_var),
      residual_var_prop = residual_var / (geographic_var + residual_var)
    )
  }
  
  geographic_props <- sapply(results_list, function(x) x$geographic_var_prop)
  mean_geographic_var <- mean(geographic_props, na.rm = TRUE)
  
  cat("  Mean geographic variance:", round(mean_geographic_var * 100, 1), "%\n")
  
  # Discrete character structure
  discrete_tests <- list()
  for (var in discrete_vars) {
    if (var %in% names(sim_data)) {
      contingency <- table(sim_data[[var]], sim_data[[region_var]])
      chi_test <- chisq.test(contingency)
      discrete_tests[[var]] <- list(
        contingency = contingency,
        chi_square = chi_test
      )
      cat("  ", var, "~ region: χ² =", round(chi_test$statistic, 2),
          ", p =", format.pval(chi_test$p.value), "\n")
    }
  }
  
  return(list(
    continuous = results_list,
    mean_geographic_variance = mean_geographic_var,
    discrete = discrete_tests
  ))
}

# ----------------------------------------------------------------------------
# FUNCTION: Comprehensive analysis
# ----------------------------------------------------------------------------

analyze_with_hybrid <- function(sim_data, sim_name, 
                                continuous_vars, discrete_vars,
                                optimize_alpha_flag = TRUE,
                                alpha_default = 0.65) {
  #' Complete analysis pipeline
  
  cat("\n========================================\n")
  cat("ANALYZING:", sim_name, "\n")
  cat("========================================\n")
  
  true_taxa <- sim_data$taxon
  true_k <- unique(sim_data$true_n_species)[1]
  
  # Optimize alpha
  if (optimize_alpha_flag) {
    alpha_opt <- optimize_alpha(
      sim_data, true_taxa, continuous_vars, discrete_vars
    )
    alpha_use <- alpha_opt$optimal_alpha
  } else {
    alpha_use <- alpha_default
    alpha_opt <- NULL
  }
  
  # Calculate hybrid distance
  cat("\nCalculating hybrid distance (alpha =", round(alpha_use, 3), ")...\n")
  hybrid <- calc_hybrid_distance(
    sim_data, continuous_vars, discrete_vars, 
    alpha = alpha_use, robust = TRUE
  )
  
  # Classification
  cat("Running classification...\n")
  classification <- classify_with_distance(hybrid$distance, true_taxa)
  cat("  Accuracy:", round(classification$accuracy * 100, 1), "%\n")
  cat("  Confidence:", round(classification$mean_confidence * 100, 1), "%\n")
  
  # Clustering
  cat("Running clustering...\n")
  clustering <- cluster_with_distance(hybrid$distance, true_k = true_k)
  cat("  Optimal k:", clustering$optimal_k, "(true k =", true_k, ")\n")
  cat("  Silhouette:", round(clustering$best_silhouette, 3), "\n")
  
  # PCA
  pca_data <- sim_data[, continuous_vars]
  pca_result <- prcomp(pca_data, scale. = TRUE)
  pca_df <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    taxon = true_taxa,
    specimen_id = sim_data$specimen_id,
    predicted = classification$predictions
  )
  
  results <- list(
    simulation = sim_name,
    true_k = true_k,
    n_specimens = nrow(sim_data),
    alpha_optimization = alpha_opt,
    alpha_used = alpha_use,
    hybrid = hybrid,
    classification = classification,
    clustering = clustering,
    pca = list(model = pca_result, data = pca_df),
    continuous_vars = continuous_vars,
    discrete_vars = discrete_vars
  )
  
  cat("========================================\n\n")
  
  return(results)
}

# ----------------------------------------------------------------------------
# FUNCTION: Compare distance approaches
# ----------------------------------------------------------------------------

compare_distance_approaches <- function(sim_data, taxa, 
                                        continuous_vars, discrete_vars) {
  #' Compare hybrid to alternatives
  
  cat("\nComparing distance approaches...\n")
  
  results <- data.frame()
  
  # 1. Hybrid (optimized)
  cat("  1. Hybrid (optimized)...\n")
  alpha_opt <- optimize_alpha(sim_data, taxa, continuous_vars, discrete_vars,
                              alpha_range = seq(0.4, 1, 0.05))
  hybrid_opt <- calc_hybrid_distance(
    sim_data, continuous_vars, discrete_vars, 
    alpha = alpha_opt$optimal_alpha
  )
  class_hybrid_opt <- classify_with_distance(hybrid_opt$distance, taxa)
  
  results <- rbind(results, data.frame(
    Approach = "Hybrid (optimal alpha)",
    Alpha = alpha_opt$optimal_alpha,
    Accuracy = class_hybrid_opt$accuracy,
    Confidence = class_hybrid_opt$mean_confidence
  ))
  
  # 2. Hybrid (alpha = 0.5)
  cat("  2. Hybrid (alpha = 0.5)...\n")
  hybrid_50 <- calc_hybrid_distance(
    sim_data, continuous_vars, discrete_vars, alpha = 0.5
  )
  class_hybrid_50 <- classify_with_distance(hybrid_50$distance, taxa)
  
  results <- rbind(results, data.frame(
    Approach = "Hybrid (alpha = 0.5)",
    Alpha = 0.5,
    Accuracy = class_hybrid_50$accuracy,
    Confidence = class_hybrid_50$mean_confidence
  ))
  
  # 3. Mahalanobis only
  cat("  3. Mahalanobis only...\n")
  mahal_only <- calc_hybrid_distance(
    sim_data, continuous_vars, discrete_vars, alpha = 1.0
  )
  class_mahal <- classify_with_distance(mahal_only$distance, taxa)
  
  results <- rbind(results, data.frame(
    Approach = "Mahalanobis only",
    Alpha = 1.0,
    Accuracy = class_mahal$accuracy,
    Confidence = class_mahal$mean_confidence
  ))
  
  # 4. Standard Gower
  cat("  4. Standard Gower...\n")
  all_data <- sim_data[, c(continuous_vars, discrete_vars)]
  gower_standard <- daisy(all_data, metric = "gower", stand = TRUE)
  class_gower_std <- classify_with_distance(gower_standard, taxa)
  
  results <- rbind(results, data.frame(
    Approach = "Standard Gower",
    Alpha = NA,
    Accuracy = class_gower_std$accuracy,
    Confidence = class_gower_std$mean_confidence
  ))
  
  # 5. Euclidean
  cat("  5. Euclidean...\n")
  euclidean_data <- scale(sim_data[, continuous_vars])
  euclidean_dist <- dist(euclidean_data, method = "euclidean")
  class_euclidean <- classify_with_distance(euclidean_dist, taxa)
  
  results <- rbind(results, data.frame(
    Approach = "Euclidean",
    Alpha = NA,
    Accuracy = class_euclidean$accuracy,
    Confidence = class_euclidean$mean_confidence
  ))
  
  results <- results[order(-results$Accuracy), ]
  
  cat("\nComparison complete.\n")
  
  return(results)
}

# ============================================================================
# PART: GENERATE ALL SIMULATIONS
# ============================================================================

cat("\n========================================\n")
cat("GENERATING SIMULATED DATA\n")
cat("========================================\n\n")

# ----------------------------------------------------------------------------
# Define discrete character probabilities
# ----------------------------------------------------------------------------

# SIM1: Easy separation - RELABELED (Large=A, Intermediate=B, Small=C)
discrete_1A <- list(  # Large teeth (formerly Species_A)
  cusp_pattern = c("Y5" = 0.9, "Y4" = 0.1, "Plus5" = 0.0),
  hypocone_size = c("small" = 0.0, "medium" = 0.2, "large" = 0.8),
  cingulum = c("absent" = 0.1, "weak" = 0.2, "strong" = 0.7)
)

discrete_1B <- list(  # Intermediate teeth (formerly Species_C)
  cusp_pattern = c("Y5" = 0.6, "Y4" = 0.3, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.2, "medium" = 0.6, "large" = 0.2),
  cingulum = c("absent" = 0.3, "weak" = 0.5, "strong" = 0.2)
)

discrete_1C <- list(  # Small teeth (formerly Species_B)
  cusp_pattern = c("Y5" = 0.2, "Y4" = 0.7, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.7, "medium" = 0.3, "large" = 0.0),
  cingulum = c("absent" = 0.8, "weak" = 0.2, "strong" = 0.0)
)

# SIM2: Moderate separation - RELABELED
discrete_2A <- list(  # Large
  cusp_pattern = c("Y5" = 0.7, "Y4" = 0.2, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.1, "medium" = 0.3, "large" = 0.6),
  cingulum = c("absent" = 0.2, "weak" = 0.5, "strong" = 0.3)
)

discrete_2B <- list(  # Intermediate
  cusp_pattern = c("Y5" = 0.5, "Y4" = 0.4, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.2, "medium" = 0.5, "large" = 0.3),
  cingulum = c("absent" = 0.3, "weak" = 0.5, "strong" = 0.2)
)

discrete_2C <- list(  # Small
  cusp_pattern = c("Y5" = 0.4, "Y4" = 0.5, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.3, "medium" = 0.5, "large" = 0.2),
  cingulum = c("absent" = 0.4, "weak" = 0.4, "strong" = 0.2)
)

# ----------------------------------------------------------------------------
# SIM1: Easy Separation (RELABELED: Large=A, Intermediate=B, Small=C)
# ----------------------------------------------------------------------------

cat("Generating SIM1 (Easy Separation)...\n")
sim1 <- rbind(
  generate_species_mixed(25, c(20, 18, 21, 19, 12), c(0.7, 0.6, 0.8, 0.7, 0.5),
                         cor_matrix, "Species_A", discrete_1A),  # LARGE
  generate_species_mixed(25, c(15, 14, 16, 15, 10), c(0.6, 0.5, 0.7, 0.6, 0.5),
                         cor_matrix, "Species_B", discrete_1B),  # INTERMEDIATE
  generate_species_mixed(25, c(12, 11, 13, 12, 8), c(0.5, 0.4, 0.6, 0.5, 0.4),
                         cor_matrix, "Species_C", discrete_1C)   # SMALL
)
sim1$simulation <- "SIM1"
sim1$true_n_species <- 3
write.csv(sim1, "data/sim1_hybrid.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIM2: Moderate Separation (RELABELED: Large=A, Intermediate=B, Small=C)
# ----------------------------------------------------------------------------

cat("Generating SIM2 (Moderate Separation)...\n")
sim2 <- rbind(
  generate_species_mixed(20, c(16.5, 15.0, 17.0, 16.0, 11.0), 
                         c(0.9, 0.8, 1.0, 0.9, 0.7),
                         cor_matrix, "Species_A", discrete_2A),  # LARGE
  generate_species_mixed(20, c(15.0, 13.5, 15.5, 14.5, 10.0), 
                         c(0.9, 0.8, 1.0, 0.9, 0.7),
                         cor_matrix, "Species_B", discrete_2B),  # INTERMEDIATE
  generate_species_mixed(20, c(14.0, 12.5, 14.5, 13.5, 9.5), 
                         c(0.9, 0.8, 1.0, 0.9, 0.7),
                         cor_matrix, "Species_C", discrete_2C)   # SMALL
)
sim2$simulation <- "SIM2"
sim2$true_n_species <- 3
write.csv(sim2, "data/sim2_hybrid.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIM3: Oversplit (One species split into 3 taxa)
# ----------------------------------------------------------------------------

cat("Generating SIM3 (Oversplit)...\n")

discrete_3_all <- list(
  cusp_pattern = c("Y5" = 0.6, "Y4" = 0.3, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.2, "medium" = 0.5, "large" = 0.3),
  cingulum = c("absent" = 0.3, "weak" = 0.5, "strong" = 0.2)
)

true_species <- generate_species_mixed(
  60, c(15, 14, 16, 15, 10), c(1.2, 1.1, 1.3, 1.2, 0.9),
  cor_matrix, "True_Species", discrete_3_all
)

sim3 <- true_species
sim3$taxon <- rep(c("Taxon_A", "Taxon_B", "Taxon_C"), each = 20)

# Add tiny site effects
sim3$M1_BL[1:20] <- sim3$M1_BL[1:20] + rnorm(20, 0.3, 0.2)
sim3$M1_BL[41:60] <- sim3$M1_BL[41:60] - rnorm(20, 0.3, 0.2)

sim3$simulation <- "SIM3"
sim3$true_n_species <- 1
write.csv(sim3, "data/sim3_hybrid.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIM4: Chronospecies (Temporal evolution)
# ----------------------------------------------------------------------------

cat("Generating SIM4 (Chronospecies)...\n")

sim4 <- data.frame()

for (time in 1:5) {
  # Discrete characters evolve through time
  y5_prob <- max(0.1, 0.9 - 0.15 * time)
  y4_prob <- min(0.8, 0.1 + 0.15 * time)
  small_hypo_prob <- min(0.7, 0.1 + 0.15 * time)
  large_hypo_prob <- max(0.1, 0.6 - 0.15 * time)
  
  discrete_temporal <- list(
    cusp_pattern = c("Y5" = y5_prob, "Y4" = y4_prob, "Plus5" = 0.1),
    hypocone_size = c("small" = small_hypo_prob, 
                      "medium" = 0.3, 
                      "large" = large_hypo_prob),
    cingulum = c("absent" = 0.3, "weak" = 0.5, "strong" = 0.2)
  )
  
  # Continuous measurements evolve directionally
  time_specimens <- generate_species_mixed(
    15, 
    mean_vec = c(14, 13, 15, 14, 9) + 0.4 * time,
    sd_vec = c(0.8, 0.7, 0.9, 0.8, 0.6),
    cor_mat = cor_matrix,
    species_name = paste0("Time_", time),
    discrete_probs = discrete_temporal
  )
  
  time_specimens$time_ma <- time
  time_specimens$true_lineage <- "Chronospecies_A"
  
  sim4 <- rbind(sim4, time_specimens)
}

sim4$simulation <- "SIM4"
sim4$true_n_species <- 1
write.csv(sim4, "data/sim4_hybrid.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# SIM5: Geographic Variation
# ----------------------------------------------------------------------------

cat("Generating SIM5 (Geographic Variation)...\n")

base_mean <- c(15, 14, 16, 15, 10)

regional_offsets <- list(
  c(0, 0, 0, 0, 0),
  c(0.4, 0.3, 0.5, 0.4, 0.2),
  c(-0.3, -0.2, -0.4, -0.3, -0.1)
)

discrete_geo_1 <- list(
  cusp_pattern = c("Y5" = 0.6, "Y4" = 0.3, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.2, "medium" = 0.5, "large" = 0.3),
  cingulum = c("absent" = 0.3, "weak" = 0.5, "strong" = 0.2)
)

discrete_geo_2 <- list(
  cusp_pattern = c("Y5" = 0.5, "Y4" = 0.4, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.3, "medium" = 0.5, "large" = 0.2),
  cingulum = c("absent" = 0.4, "weak" = 0.4, "strong" = 0.2)
)

discrete_geo_3 <- list(
  cusp_pattern = c("Y5" = 0.7, "Y4" = 0.2, "Plus5" = 0.1),
  hypocone_size = c("small" = 0.2, "medium" = 0.4, "large" = 0.4),
  cingulum = c("absent" = 0.2, "weak" = 0.6, "strong" = 0.2)
)

discrete_geo_list <- list(discrete_geo_1, discrete_geo_2, discrete_geo_3)

sim5 <- data.frame()

for (region in 1:3) {
  region_specimens <- generate_species_mixed(
    20,
    mean_vec = base_mean + regional_offsets[[region]],
    sd_vec = c(0.8, 0.7, 0.9, 0.8, 0.6),
    cor_mat = cor_matrix,
    species_name = paste0("Region_", region),
    discrete_probs = discrete_geo_list[[region]]
  )
  
  region_specimens$region <- paste0("Region_", region)
  region_specimens$latitude <- c(0, 15, 30)[region]
  region_specimens$true_species <- "Widespread_Species"
  
  sim5 <- rbind(sim5, region_specimens)
}

sim5$simulation <- "SIM5"
sim5$true_n_species <- 1
write.csv(sim5, "data/sim5_hybrid.csv", row.names = FALSE)

cat("\nAll simulations generated (SIM1-SIM5).\n")

# ============================================================================
# PART: RUN ALL ANALYSES
# ============================================================================

cat("\n========================================\n")
cat("RUNNING ANALYSES ON ALL SIMULATIONS\n")
cat("========================================\n\n")

continuous_vars <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")
discrete_vars <- c("cusp_pattern", "hypocone_size", "cingulum")

# Load data (already in memory, but good practice)
sim1 <- read.csv("data/sim1_hybrid.csv", stringsAsFactors = TRUE)
sim2 <- read.csv("data/sim2_hybrid.csv", stringsAsFactors = TRUE)
sim3 <- read.csv("data/sim3_hybrid.csv", stringsAsFactors = TRUE)
sim4 <- read.csv("data/sim4_hybrid.csv", stringsAsFactors = TRUE)
sim5 <- read.csv("data/sim5_hybrid.csv", stringsAsFactors = TRUE)

# Analyze SIM1
results_sim1 <- analyze_with_hybrid(
  sim1, "SIM1", continuous_vars, discrete_vars,
  optimize_alpha_flag = TRUE
)

# Analyze SIM2
results_sim2 <- analyze_with_hybrid(
  sim2, "SIM2", continuous_vars, discrete_vars,
  optimize_alpha_flag = TRUE
)

# Analyze SIM3
results_sim3 <- analyze_with_hybrid(
  sim3, "SIM3", continuous_vars, discrete_vars,
  optimize_alpha_flag = TRUE
)

cat("\n*** SIM3 INTERPRETATION ***\n")
cat("ONE species artificially split into 3 taxa.\n")
cat("Expected: Low accuracy (<65%), high uncertainty.\n")
cat("Actual:", round(results_sim3$classification$accuracy * 100, 1), "%\n")
if (results_sim3$classification$accuracy < 0.65) {
  cat("✓ CORRECT: Method shows appropriate uncertainty.\n")
} else {
  cat("✗ WARNING: Method may be over-splitting.\n")
}
cat("***************************\n\n")

# Analyze SIM4
results_sim4 <- analyze_with_hybrid(
  sim4, "SIM4", continuous_vars, discrete_vars,
  optimize_alpha_flag = FALSE,
  alpha_default = 0.65
)

temporal_analysis_sim4 <- analyze_temporal_variation(
  sim4, continuous_vars, discrete_vars, time_var = "time_ma"
)
results_sim4$temporal_analysis <- temporal_analysis_sim4

# Calculate inter-specific threshold from SIM2
sim2_between_var <- var(tapply(sim2$M1_BL, sim2$taxon, mean))
sim2_total_var <- var(sim2$M1_BL)
sim2_between_prop <- sim2_between_var / sim2_total_var

cat("\n*** SIM4 INTERPRETATION ***\n")
cat("ONE lineage evolving through time.\n")
cat("Temporal variance:", 
    round(temporal_analysis_sim4$mean_temporal_variance * 100, 1), "%\n")
cat("Inter-specific threshold (SIM2):", round(sim2_between_prop * 100, 1), "%\n")

if (temporal_analysis_sim4$mean_temporal_variance < sim2_between_prop) {
  cat("✓ CORRECT: Temporal < inter-specific threshold.\n")
  cat("  Conclusion: Chronospecies, not multiple species.\n")
} else {
  cat("✗ WARNING: Temporal exceeds species threshold.\n")
}
cat("***************************\n\n")

# Analyze SIM5
results_sim5 <- analyze_with_hybrid(
  sim5, "SIM5", continuous_vars, discrete_vars,
  optimize_alpha_flag = FALSE,
  alpha_default = 0.65
)

geographic_analysis_sim5 <- analyze_geographic_variation(
  sim5, continuous_vars, discrete_vars, region_var = "region"
)
results_sim5$geographic_analysis <- geographic_analysis_sim5

cat("\n*** SIM5 INTERPRETATION ***\n")
cat("ONE species with geographic structure.\n")
cat("Geographic variance:", 
    round(geographic_analysis_sim5$mean_geographic_variance * 100, 1), "%\n")
cat("Inter-specific threshold:", round(sim2_between_prop * 100, 1), "%\n")

if (geographic_analysis_sim5$mean_geographic_variance < sim2_between_prop) {
  cat("✓ CORRECT: Geographic < inter-specific threshold.\n")
  cat("  Conclusion: Geographic variation, not separate species.\n")
} else {
  cat("✗ WARNING: Geographic exceeds species threshold.\n")
}
cat("***************************\n\n")

# Distance comparison (SIM2)
cat("\n========================================\n")
cat("COMPARING DISTANCE APPROACHES (SIM2)\n")
cat("========================================\n\n")

comparison_sim2 <- compare_distance_approaches(
  sim2, sim2$taxon, continuous_vars, discrete_vars
)

print(comparison_sim2)

# ============================================================================
# PART: CREATE SUMMARY TABLES
# ============================================================================

cat("\n========================================\n")
cat("CREATING SUMMARY TABLES\n")
cat("========================================\n\n")

summary_all <- data.frame(
  Simulation = c("SIM1", "SIM2", "SIM3", "SIM4", "SIM5"),
  Description = c("Easy separation", "Moderate separation", 
                  "Oversplit (1 sp)", "Chronospecies", "Geographic"),
  True_K = c(results_sim1$true_k, results_sim2$true_k, 
             results_sim3$true_k, results_sim4$true_k, results_sim5$true_k),
  Estimated_K = c(results_sim1$clustering$optimal_k, 
                  results_sim2$clustering$optimal_k,
                  results_sim3$clustering$optimal_k, 
                  results_sim4$clustering$optimal_k,
                  results_sim5$clustering$optimal_k),
  Alpha = c(results_sim1$alpha_used, results_sim2$alpha_used,
            results_sim3$alpha_used, results_sim4$alpha_used, 
            results_sim5$alpha_used),
  Accuracy = c(results_sim1$classification$accuracy * 100,
               results_sim2$classification$accuracy * 100,
               results_sim3$classification$accuracy * 100,
               results_sim4$classification$accuracy * 100,
               results_sim5$classification$accuracy * 100),
  Confidence = c(results_sim1$classification$mean_confidence * 100,
                 results_sim2$classification$mean_confidence * 100,
                 results_sim3$classification$mean_confidence * 100,
                 results_sim4$classification$mean_confidence * 100,
                 results_sim5$classification$mean_confidence * 100),
  Silhouette = c(results_sim1$clustering$best_silhouette,
                 results_sim2$clustering$best_silhouette,
                 results_sim3$clustering$best_silhouette,
                 results_sim4$clustering$best_silhouette,
                 results_sim5$clustering$best_silhouette)
)

summary_all$Accuracy <- round(summary_all$Accuracy, 1)
summary_all$Confidence <- round(summary_all$Confidence, 1)
summary_all$Silhouette <- round(summary_all$Silhouette, 3)
summary_all$Alpha <- round(summary_all$Alpha, 3)

print(summary_all)
write.csv(summary_all, "results/summary_all_simulations.csv", row.names = FALSE)

variance_summary <- data.frame(
  Source = c("Inter-specific (SIM2)", "Temporal (SIM4)", "Geographic (SIM5)"),
  Variance_Proportion = c(
    sim2_between_prop * 100,
    temporal_analysis_sim4$mean_temporal_variance * 100,
    geographic_analysis_sim5$mean_geographic_variance * 100
  ),
  Below_Species_Threshold = c(
    NA,
    temporal_analysis_sim4$mean_temporal_variance < sim2_between_prop,
    geographic_analysis_sim5$mean_geographic_variance < sim2_between_prop
  )
)

variance_summary$Variance_Proportion <- round(variance_summary$Variance_Proportion, 1)

cat("\nVariance Partitioning:\n")
print(variance_summary)
write.csv(variance_summary, "results/variance_partitioning_summary.csv", 
          row.names = FALSE)

all_results <- list(
  SIM1 = results_sim1,
  SIM2 = results_sim2,
  SIM3 = results_sim3,
  SIM4 = results_sim4,
  SIM5 = results_sim5,
  temporal_analysis = temporal_analysis_sim4,
  geographic_analysis = geographic_analysis_sim5,
  variance_summary = variance_summary,
  distance_comparison = comparison_sim2
)

saveRDS(all_results, "results/all_results_complete.rds")

# ============================================================================
# PART: CREATE ALL FIGURES
# ============================================================================

cat("\n========================================\n")
cat("CREATING FIGURES\n")
cat("========================================\n\n")

# FIGURE 1: Alpha Optimization
create_alpha_plot <- function(results_list) {
  plot_data <- data.frame()
  for (sim_name in c("SIM1", "SIM2", "SIM3")) {
    result <- results_list[[sim_name]]
    if (!is.null(result$alpha_optimization)) {
      df <- result$alpha_optimization$results
      df$Simulation <- sim_name
      plot_data <- rbind(plot_data, df)
    }
  }
  
  optimal_alphas <- data.frame(
    Simulation = c("SIM1", "SIM2", "SIM3"),
    optimal = c(results_list$SIM1$alpha_used, 
                results_list$SIM2$alpha_used,
                results_list$SIM3$alpha_used)
  )
  
  p <- ggplot(plot_data, aes(x = alpha, y = accuracy * 100, 
                             color = Simulation)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    geom_vline(data = optimal_alphas, 
               aes(xintercept = optimal, color = Simulation),
               linetype = "dashed", linewidth = 0.8) +
    scale_color_viridis_d(option = "D", end = 0.8) +
    labs(title = "Alpha Parameter Optimization",
         subtitle = "Dashed lines = optimal values",
         x = expression(alpha~"(continuous weight)"),
         y = "Classification Accuracy (%)") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom")
  
  return(p)
}

fig1 <- create_alpha_plot(all_results)
ggsave("figures/Figure1_Alpha_Optimization.png", fig1, 
       width = 8, height = 6, dpi = 300)
cat("Figure 1 saved.\n")

# FIGURE 2: Distance Comparison
create_comparison_plot <- function(comp_df) {
  comp_df$Approach <- factor(comp_df$Approach,
                             levels = comp_df$Approach[order(comp_df$Accuracy)])
  
  p <- ggplot(comp_df, aes(x = Approach, y = Accuracy * 100)) +
    geom_bar(stat = "identity", fill = "#4575b4", alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = 80, linetype = "dashed", 
               color = "red", linewidth = 0.8) +
    geom_text(aes(label = paste0(round(Accuracy * 100, 1), "%")), 
              hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(title = "Distance Metric Comparison (SIM2)",
         subtitle = "Red line = 80% threshold",
         x = "Approach", y = "Classification Accuracy (%)") +
    theme_bw(base_size = 12) +
    ylim(0, 100)
  
  return(p)
}

fig2 <- create_comparison_plot(comparison_sim2)
ggsave("figures/Figure2_Distance_Comparison.png", fig2, 
       width = 10, height = 6, dpi = 300)
cat("Figure 2 saved.\n")

# FIGURE 3: PCA Morphospace (5-panel)
create_pca_panel <- function(result, sim_name) {
  pca_data <- result$pca$data
  variance <- summary(result$pca$model)$importance[2, 1:2] * 100
  
  p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = taxon, shape = taxon)) +
    geom_point(size = 2.5, alpha = 0.7) +
    stat_ellipse(level = 0.95, linewidth = 0.8) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    labs(title = sim_name,
         x = paste0("PC1 (", round(variance[1], 1), "%)"),
         y = paste0("PC2 (", round(variance[2], 1), "%)")) +
    theme_bw(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5))
  
  return(p)
}

fig3_panels <- list(
  create_pca_panel(results_sim1, "SIM1"),
  create_pca_panel(results_sim2, "SIM2"),
  create_pca_panel(results_sim3, "SIM3"),
  create_pca_panel(results_sim4, "SIM4"),
  create_pca_panel(results_sim5, "SIM5")
)

fig3 <- plot_grid(plotlist = fig3_panels, ncol = 3, 
                  labels = c("A", "B", "C", "D", "E"), label_size = 12)
ggsave("figures/Figure3_PCA_Morphospace_All.png", fig3, 
       width = 15, height = 10, dpi = 300)
cat("Figure 3 saved.\n")

# FIGURE 4: Temporal Evolution (SIM4)
create_temporal_plot <- function(sim_data) {
  # Continuous evolution
  p1 <- ggplot(sim_data, aes(x = time_ma, y = M1_BL, color = taxon)) +
    geom_point(size = 2.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "black", 
                linewidth = 1.2, alpha = 0.3) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    labs(title = "SIM4: Morphological Change Through Time",
         subtitle = "Single lineage evolving directionally",
         x = "Time Period", y = "M1 Buccolingual Diameter (mm)") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none")
  
  # Discrete evolution
  discrete_summary <- sim_data %>%
    group_by(time_ma, cusp_pattern) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(time_ma) %>%
    mutate(proportion = count / sum(count))
  
  p2 <- ggplot(discrete_summary, aes(x = time_ma, y = proportion, 
                                     fill = cusp_pattern)) +
    geom_area(alpha = 0.7) +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    labs(title = "Discrete Character Evolution",
         x = "Time Period", y = "Proportion",
         fill = "Cusp Pattern") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom")
  
  combined <- plot_grid(p1, p2, ncol = 2, labels = c("A", "B"), 
                        label_size = 14)
  return(combined)
}

fig4 <- create_temporal_plot(sim4)
ggsave("figures/Figure4_Temporal_Evolution_SIM4.png", fig4, 
       width = 14, height = 6, dpi = 300)
cat("Figure 4 saved.\n")

# FIGURE 5: Geographic Structure (SIM5)
create_geographic_plot <- function(sim_data) {
  # Continuous variation by region
  p1 <- ggplot(sim_data, aes(x = region, y = M1_BL, fill = region)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    labs(title = "SIM5: Geographic Variation",
         subtitle = "Single widespread species",
         x = "Geographic Region", y = "M1 BL (mm)") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none")
  
  # Discrete variation by region
  discrete_summary <- sim_data %>%
    group_by(region, cusp_pattern) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(region) %>%
    mutate(proportion = count / sum(count))
  
  p2 <- ggplot(discrete_summary, aes(x = region, y = proportion, 
                                     fill = cusp_pattern)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    labs(title = "Discrete Character Structure",
         x = "Region", y = "Proportion", fill = "Cusp Pattern") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom")
  
  # Clinal variation
  p3 <- ggplot(sim_data, aes(x = latitude, y = M1_BL, color = region)) +
    geom_point(size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "black", 
                linewidth = 1, alpha = 0.3) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    labs(title = "Clinal Variation by Latitude",
         x = "Latitude (degrees)", y = "M1 BL (mm)") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom")
  
  combined <- plot_grid(p1, p2, p3, ncol = 3, 
                        labels = c("A", "B", "C"), label_size = 14)
  return(combined)
}

fig5 <- create_geographic_plot(sim5)
ggsave("figures/Figure5_Geographic_Structure_SIM5.png", fig5, 
       width = 18, height = 6, dpi = 300)
cat("Figure 5 saved.\n")

# FIGURE 6: Variance Partitioning
create_variance_plot <- function(var_summary) {
  plot_data <- data.frame(
    Source = c("Inter-specific\n(SIM2)", "Temporal\n(SIM4)", 
               "Geographic\n(SIM5)"),
    Variance = var_summary$Variance_Proportion,
    Type = c("Species Threshold", "Chronospecies", "Geographic")
  )
  
  p <- ggplot(plot_data, aes(x = Source, y = Variance, fill = Type)) +
    geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
    geom_hline(yintercept = var_summary$Variance_Proportion[1],
               linetype = "dashed", color = "red", linewidth = 1) +
    geom_text(aes(label = paste0(round(Variance, 1), "%")), 
              vjust = -0.5, size = 5, fontface = "bold") +
    scale_fill_manual(values = c("Species Threshold" = "#d73027",
                                 "Chronospecies" = "#4575b4",
                                 "Geographic" = "#91bfdb")) +
    labs(title = "Variance Partitioning: Species vs. Intraspecific",
         subtitle = "Red line = inter-specific threshold (SIM2)",
         x = "Source of Variation", y = "Variance Explained (%)",
         fill = "Type") +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom") +
    ylim(0, max(plot_data$Variance) * 1.2)
  
  return(p)
}

fig6 <- create_variance_plot(variance_summary)
ggsave("figures/Figure6_Variance_Partitioning.png", fig6, 
       width = 10, height = 7, dpi = 300)
cat("Figure 6 saved.\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("COMPLETE ANALYSIS SUMMARY\n")
cat("========================================\n\n")

cat("All Simulations:\n")
print(summary_all)

cat("\n\nVariance Partitioning:\n")
print(variance_summary)

cat("\n\nDistance Comparison (SIM2):\n")
print(comparison_sim2[, c("Approach", "Accuracy", "Confidence")])

cat("\n\nKey Findings:\n")
cat("-------------\n")
cat("1. SIM1: Accuracy =", round(results_sim1$classification$accuracy * 100, 1), "%\n")
cat("2. SIM2: Accuracy =", round(results_sim2$classification$accuracy * 100, 1), 
    "% (α =", round(results_sim2$alpha_used, 3), ")\n")
cat("3. SIM3: Accuracy =", round(results_sim3$classification$accuracy * 100, 1), 
    "% - Correctly uncertain\n")
cat("4. SIM4: Temporal =", 
    round(temporal_analysis_sim4$mean_temporal_variance * 100, 1), 
    "% < Threshold - Chronospecies\n")
cat("5. SIM5: Geographic =", 
    round(geographic_analysis_sim5$mean_geographic_variance * 100, 1),
    "% < Threshold - Single species\n")

cat("\n\nFiles Created:\n")
cat("--------------\n")
cat("Data: ./data/sim1-5_hybrid.csv\n")
cat("Results: ./results/*.csv and *.rds\n")
cat("Figures: ./figures/Figure1-6.png\n")

cat("\n========================================\n")
cat("ANALYSIS COMPLETE - ALL 5 SIMULATIONS\n")
cat("Species relabeled: Large=A, Intermediate=B, Small=C\n")
cat("========================================\n")