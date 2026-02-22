# ============================================================================
# CREATE TAXONOMY FROM COMBINED DISTANCE ANALYSIS RESULTS
# This script takes results from the master analysis and creates formal taxonomy
# ============================================================================

# This script assumes you've already run the master script and have:
# - all_results (the RDS file with all simulation results)
# - Real data analyzed with the combined method

# ============================================================================
# SECTION 1: SETUP AND LOAD RESULTS
# ============================================================================

# Load the results from your master analysis
all_results <- readRDS("results/all_results_complete.rds")

# If you have real Australopithecus data, load it here
# For now, we'll demonstrate using SIM2 as if it were real data
demonstration_data <- read.csv("data/sim2_combined.csv", stringsAsFactors = TRUE)

cat("\n========================================\n")
cat("TAXONOMY GENERATION FROM RESULTS\n")
cat("========================================\n\n")

# ============================================================================
# SECTION 2: DEFINE DECISION THRESHOLDS
# ============================================================================
define_taxonomic_thresholds <- function(sim2_results, sim3_results) {
  #' Extract decision criteria from simulation benchmarks
  
  cat("Defining taxonomic decision thresholds...\n")
  
  # From SIM2 (REALISTIC SPECIES - these ARE distinct)
  sim2_dist <- sim2_results$combined$distance_matrix
  sim2_between <- mean(sim2_dist[upper.tri(sim2_dist)])
  
  # From SIM3 (OVERSPLIT - these are NOT distinct)
  sim3_dist <- sim3_results$combined$distance_matrix
  sim3_between <- mean(sim3_dist[upper.tri(sim3_dist)])
  
  thresholds <- list(
    # STRONG evidence for species distinction
    strong = list(
      mean_distance = sim2_between * 0.8,     # 80% of SIM2 mean
      accuracy = 0.80,
      silhouette = 0.60,
      confidence = 0.85,
      description = "Recognize as distinct species"
    ),
    
    # MODERATE evidence
    moderate = list(
      mean_distance = (sim2_between + sim3_between) / 2,  # Midpoint
      accuracy = 0.70,
      silhouette = 0.45,
      confidence = 0.75,
      description = "Tentatively recognize"
    ),
    
    # WEAK evidence (should lump)
    weak = list(
      mean_distance = sim3_between * 1.2,     # 120% of SIM3 mean
      accuracy = 0.65,
      silhouette = 0.40,
      confidence = 0.70,
      description = "Synonymize"
    ),
    
    # Reference values
    reference = list(
      sim2_between = sim2_between,
      sim3_between = sim3_between,
      sim2_accuracy = sim2_results$classification$accuracy,
      sim3_accuracy = sim3_results$classification$accuracy,
      sim2_silhouette = sim2_results$clustering$best_silhouette,
      sim3_silhouette = sim3_results$clustering$best_silhouette
    )
  )
  
  cat("  SIM2 (distinct species) mean distance:", round(sim2_between, 3), "\n")
  cat("  SIM3 (oversplit) mean distance:", round(sim3_between, 3), "\n")
  cat("  Species threshold set at:", round(thresholds$strong$mean_distance, 3), "\n\n")
  
  return(thresholds)
}

# Define thresholds
thresholds <- define_taxonomic_thresholds(
  all_results$SIM2,
  all_results$SIM3
)

# ============================================================================
# SECTION 3: ANALYZE YOUR REAL DATA (OR DEMONSTRATION DATA)
# ============================================================================

analyze_real_data_for_taxonomy <- function(data, continuous_vars, discrete_vars,
                                           alpha = 0.65) {
  #' Analyze real fossil data using combined distance method
  #' 
  #' @param data Data frame with specimens (rows) and measurements + taxon labels
  #' @param continuous_vars Vector of continuous variable names
  #' @param discrete_vars Vector of discrete variable names
  #' @param alpha Weight for continuous data (from optimization)
  #' @return Complete analysis results for taxonomy
  
  cat("Analyzing data for taxonomic revision...\n")
  cat("  Specimens:", nrow(data), "\n")
  cat("  Taxa:", length(unique(data$taxon)), "\n\n")
  
  # Get taxon labels
  true_taxa <- data$taxon
  taxa_list <- unique(true_taxa)
  
  # Calculate combined distance
  combined <- calc_combined_distance(
    data, continuous_vars, discrete_vars,
    alpha = alpha, robust = TRUE
  )
  
  # Classification
  classification <- classify_with_distance(
    combined$distance, true_taxa, k_folds = 5
  )
  
  # Clustering
  clustering <- cluster_with_distance(
    combined$distance, true_k = length(taxa_list)
  )
  
  # PCA
  pca_result <- prcomp(data[, continuous_vars], scale. = TRUE)
  
  results <- list(
    data = data,
    taxa_list = taxa_list,
    combined = combined,
    classification = classification,
    clustering = clustering,
    pca = pca_result
  )
  
  cat("Analysis complete.\n")
  cat("  Classification accuracy:", round(classification$accuracy * 100, 1), "%\n")
  cat("  Optimal clusters found:", clustering$optimal_k, "\n")
  cat("  Silhouette score:", round(clustering$best_silhouette, 3), "\n\n")
  
  return(results)
}

# Analyze the data
continuous_vars <- c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")
discrete_vars <- c("cusp_pattern", "hypocone_size", "cingulum")

real_data_results <- analyze_real_data_for_taxonomy(
  demonstration_data, 
  continuous_vars, 
  discrete_vars,
  alpha = 0.65
)

# ============================================================================
# SECTION 4: MAKE PAIRWISE TAXONOMIC DECISIONS
# ============================================================================

make_pairwise_decision <- function(taxon_A, taxon_B, 
                                   results, thresholds) {
  #' Make taxonomic decision for one pair of taxa
  #' 
  #' @param taxon_A First taxon name
  #' @param taxon_B Second taxon name
  #' @param results Output from analyze_real_data_for_taxonomy()
  #' @param thresholds Decision thresholds
  #' @return Decision with supporting evidence
  
  # Get specimen indices
  idx_A <- which(results$data$taxon == taxon_A)
  idx_B <- which(results$data$taxon == taxon_B)
  
  dist_mat <- results$combined$distance_matrix
  
  # Calculate between-taxon distance
  between_distances <- dist_mat[idx_A, idx_B]
  mean_between <- mean(between_distances)
  
  # Calculate within-taxon distances
  if (length(idx_A) > 1) {
    within_A_dist <- dist_mat[idx_A, idx_A][upper.tri(dist_mat[idx_A, idx_A])]
    mean_within_A <- mean(within_A_dist)
  } else {
    mean_within_A <- 0
  }
  
  if (length(idx_B) > 1) {
    within_B_dist <- dist_mat[idx_B, idx_B][upper.tri(dist_mat[idx_B, idx_B])]
    mean_within_B <- mean(within_B_dist)
  } else {
    mean_within_B <- 0
  }
  
  mean_within <- mean(c(mean_within_A, mean_within_B))
  
  # Separation ratio
  if (mean_within > 0) {
    separation_ratio <- mean_between / mean_within
  } else {
    separation_ratio <- NA
  }
  
  # Count evidence
  evidence_strong <- 0
  evidence_moderate <- 0
  
  # Check against strong thresholds
  if (mean_between > thresholds$strong$mean_distance) evidence_strong <- evidence_strong + 1
  if (results$classification$accuracy > thresholds$strong$accuracy) evidence_strong <- evidence_strong + 1
  if (results$clustering$best_silhouette > thresholds$strong$silhouette) evidence_strong <- evidence_strong + 1
  if (!is.na(separation_ratio) && separation_ratio > 2.0) evidence_strong <- evidence_strong + 1
  
  # Check against moderate thresholds
  if (mean_between > thresholds$moderate$mean_distance) evidence_moderate <- evidence_moderate + 1
  if (results$classification$accuracy > thresholds$moderate$accuracy) evidence_moderate <- evidence_moderate + 1
  
  # Make decision
  if (evidence_strong >= 3) {
    action <- "RECOGNIZE_AS_DISTINCT"
    confidence_level <- "HIGH"
    recommendation <- paste0(
      "Recognize ", taxon_A, " and ", taxon_B, " as distinct species. ",
      "Strong statistical evidence (", evidence_strong, "/4 criteria)."
    )
  } else if (evidence_moderate >= 2) {
    action <- "TENTATIVELY_DISTINCT"
    confidence_level <- "MODERATE"
    recommendation <- paste0(
      "Tentatively recognize ", taxon_A, " and ", taxon_B, " as distinct. ",
      "Moderate evidence. Additional data recommended."
    )
  } else {
    action <- "SYNONYMIZE"
    confidence_level <- "HIGH"
    recommendation <- paste0(
      "Synonymize ", taxon_A, " and ", taxon_B, ". ",
      "Insufficient evidence for separation."
    )
  }
  
  decision <- list(
    taxon_A = taxon_A,
    taxon_B = taxon_B,
    action = action,
    confidence = confidence_level,
    recommendation = recommendation,
    metrics = list(
      mean_between = mean_between,
      mean_within = mean_within,
      separation_ratio = separation_ratio,
      evidence_strong = evidence_strong,
      evidence_moderate = evidence_moderate
    )
  )
  
  return(decision)
}

# ============================================================================
# SECTION 5: CREATE COMPLETE TAXONOMIC MATRIX
# ============================================================================

create_taxonomic_comparison_matrix <- function(results, thresholds) {
  #' Create pairwise comparison matrix for all taxa
  
  taxa_list <- results$taxa_list
  n_taxa <- length(taxa_list)
  
  cat("Creating pairwise taxonomic comparison matrix...\n")
  cat("  Comparing", n_taxa, "taxa (", n_taxa*(n_taxa-1)/2, "pairs)\n\n")
  
  # Initialize matrices
  distance_matrix <- matrix(NA, n_taxa, n_taxa)
  decision_matrix <- matrix("", n_taxa, n_taxa)
  confidence_matrix <- matrix("", n_taxa, n_taxa)
  
  rownames(distance_matrix) <- colnames(distance_matrix) <- taxa_list
  rownames(decision_matrix) <- colnames(decision_matrix) <- taxa_list
  rownames(confidence_matrix) <- colnames(confidence_matrix) <- taxa_list
  
  # All pairwise comparisons
  all_decisions <- list()
  
  for (i in 1:(n_taxa-1)) {
    for (j in (i+1):n_taxa) {
      
      decision <- make_pairwise_decision(
        taxa_list[i],
        taxa_list[j],
        results,
        thresholds
      )
      
      # Store in matrices
      distance_matrix[i, j] <- decision$metrics$mean_between
      distance_matrix[j, i] <- decision$metrics$mean_between
      
      decision_matrix[i, j] <- decision$action
      decision_matrix[j, i] <- decision$action
      
      confidence_matrix[i, j] <- decision$confidence
      confidence_matrix[j, i] <- decision$confidence
      
      # Store full decision
      pair_name <- paste0(taxa_list[i], "_vs_", taxa_list[j])
      all_decisions[[pair_name]] <- decision
      
      cat("  ", taxa_list[i], "vs", taxa_list[j], ":", decision$action, "\n")
    }
  }
  
  # Diagonal
  diag(distance_matrix) <- 0
  diag(decision_matrix) <- "SAME"
  diag(confidence_matrix) <- "N/A"
  
  cat("\n")
  
  return(list(
    distances = distance_matrix,
    decisions = decision_matrix,
    confidence = confidence_matrix,
    all_decisions = all_decisions
  ))
}

# Create the matrix
taxonomic_matrix <- create_taxonomic_comparison_matrix(
  real_data_results,
  thresholds
)

# ============================================================================
# SECTION 6: GENERATE FORMAL TAXONOMY
# ============================================================================

generate_formal_taxonomy <- function(taxonomic_matrix, taxa_list) {
  #' Generate formal taxonomic revision text
  
  cat("Generating formal taxonomic revision...\n\n")
  
  decisions <- taxonomic_matrix$decisions
  distances <- taxonomic_matrix$distances
  
  # Identify valid species and synonyms
  valid_species <- c()
  synonyms <- list()
  
  for (taxon in taxa_list) {
    # Get decisions for this taxon vs all others
    taxon_decisions <- decisions[taxon, ]
    taxon_decisions <- taxon_decisions[taxon_decisions != "SAME"]
    
    # Count decisions
    n_distinct <- sum(taxon_decisions %in% c("RECOGNIZE_AS_DISTINCT", "TENTATIVELY_DISTINCT"))
    n_synonymize <- sum(taxon_decisions == "SYNONYMIZE")
    
    # If mostly distinct from others, it's valid
    if (n_distinct >= n_synonymize) {
      valid_species <- c(valid_species, taxon)
    } else {
      # Find what to synonymize with
      synonymize_with <- names(taxon_decisions)[taxon_decisions == "SYNONYMIZE"]
      if (length(synonymize_with) > 0) {
        # Use first valid species it should be synonymized with
        for (senior in synonymize_with) {
          if (senior %in% valid_species || 
              !(senior %in% names(synonyms))) {
            if (is.null(synonyms[[senior]])) {
              synonyms[[senior]] <- c()
            }
            synonyms[[senior]] <- c(synonyms[[senior]], taxon)
            break
          }
        }
      }
    }
  }
  
  # Build revision text
  revision <- list()
  
  # SYNOPSIS
  synopsis <- "TAXONOMIC SYNOPSIS\n"
  synopsis <- paste0(synopsis, "==================\n\n")
  synopsis <- paste0(synopsis, "RECOGNIZED SPECIES: ", length(valid_species), "\n\n")
  
  for (sp in valid_species) {
    synopsis <- paste0(synopsis, "**", sp, "** - VALID SPECIES\n")
    if (!is.null(synonyms[[sp]])) {
      synopsis <- paste0(synopsis, "  Junior synonyms:\n")
      for (syn in synonyms[[sp]]) {
        synopsis <- paste0(synopsis, "    - ", syn, "\n")
      }
    }
    synopsis <- paste0(synopsis, "\n")
  }
  
  revision$synopsis <- synopsis
  
  # JUSTIFICATIONS
  justifications <- "TAXONOMIC JUSTIFICATIONS\n"
  justifications <- paste0(justifications, "========================\n\n")
  
  for (sp in valid_species) {
    justifications <- paste0(justifications, "**", sp, "**\n")
    justifications <- paste0(justifications, "Status: Valid species\n\n")
    
    # Get distances to other valid species
    other_valid <- setdiff(valid_species, sp)
    if (length(other_valid) > 0) {
      avg_dist <- mean(distances[sp, other_valid])
      min_dist <- min(distances[sp, other_valid])
      max_dist <- max(distances[sp, other_valid])
      
      justifications <- paste0(justifications, "Statistical support:\n")
      justifications <- paste0(justifications, 
                               "  - Mean distance to congeners: ", round(avg_dist, 3), "\n")
      justifications <- paste0(justifications,
                               "  - Distance range: ", round(min_dist, 3), " - ", round(max_dist, 3), "\n")
    }
    
    # List closest relative
    if (length(other_valid) > 0) {
      closest <- other_valid[which.min(distances[sp, other_valid])]
      justifications <- paste0(justifications,
                               "  - Closest relative: ", closest, 
                               " (D = ", round(distances[sp, closest], 3), ")\n")
    }
    
    justifications <- paste0(justifications, "\n")
  }
  
  # Add synonymies
  if (length(synonyms) > 0) {
    justifications <- paste0(justifications, "\nSYNONYMIES\n")
    justifications <- paste0(justifications, "==========\n\n")
    
    for (senior in names(synonyms)) {
      for (junior in synonyms[[senior]]) {
        justifications <- paste0(justifications,
                                 "**", junior, "** = **", senior, "**\n\n")
        justifications <- paste0(justifications,
                                 "Justification: Insufficient morphological separation. ",
                                 "Distance = ", round(distances[junior, senior], 3), " ",
                                 "(below species threshold).\n\n")
      }
    }
  }
  
  revision$justifications <- justifications
  
  # DECISION MATRIX TABLE
  matrix_table <- "\nPAIRWISE DECISION MATRIX\n"
  matrix_table <- paste0(matrix_table, "========================\n\n")
  
  # Print header
  matrix_table <- paste0(matrix_table, sprintf("%-20s", ""))
  for (taxon in taxa_list) {
    matrix_table <- paste0(matrix_table, sprintf("%-12s", substr(taxon, 1, 11)))
  }
  matrix_table <- paste0(matrix_table, "\n")
  matrix_table <- paste0(matrix_table, paste(rep("-", 20 + 12*length(taxa_list)), collapse=""), "\n")
  
  # Print rows
  for (i in 1:length(taxa_list)) {
    matrix_table <- paste0(matrix_table, sprintf("%-20s", taxa_list[i]))
    for (j in 1:length(taxa_list)) {
      if (i == j) {
        matrix_table <- paste0(matrix_table, sprintf("%-12s", "-"))
      } else {
        decision <- substr(decisions[i, j], 1, 11)
        matrix_table <- paste0(matrix_table, sprintf("%-12s", decision))
      }
    }
    matrix_table <- paste0(matrix_table, "\n")
  }
  
  revision$matrix_table <- matrix_table
  
  return(revision)
}

# Generate the revision
formal_taxonomy <- generate_formal_taxonomy(
  taxonomic_matrix,
  real_data_results$taxa_list
)

# ============================================================================
# SECTION 7: SAVE OUTPUTS
# ============================================================================

cat("Saving taxonomy outputs...\n")

# Save text outputs
dir.create("taxonomy_outputs", showWarnings = FALSE)

writeLines(formal_taxonomy$synopsis, "taxonomy_outputs/taxonomic_synopsis.txt")
writeLines(formal_taxonomy$justifications, "taxonomy_outputs/taxonomic_justifications.txt")
writeLines(formal_taxonomy$matrix_table, "taxonomy_outputs/decision_matrix.txt")

# Save matrices as CSV
write.csv(taxonomic_matrix$distances, "taxonomy_outputs/distance_matrix.csv")
write.csv(taxonomic_matrix$decisions, "taxonomy_outputs/decision_matrix.csv")
write.csv(taxonomic_matrix$confidence, "taxonomy_outputs/confidence_matrix.csv")

# Save complete taxonomy object
saveRDS(list(
  taxonomy = formal_taxonomy,
  matrices = taxonomic_matrix,
  thresholds = thresholds,
  results = real_data_results
), "taxonomy_outputs/complete_taxonomy.rds")

cat("\nOutputs saved to ./taxonomy_outputs/\n")

# ============================================================================
# SECTION 8: PRINT SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("TAXONOMY GENERATION COMPLETE\n")
cat("========================================\n\n")

cat(formal_taxonomy$synopsis)
cat("\n")
cat(formal_taxonomy$matrix_table)
cat("\n")

cat("Files created:\n")
cat("  - taxonomy_outputs/taxonomic_synopsis.txt\n")
cat("  - taxonomy_outputs/taxonomic_justifications.txt\n")
cat("  - taxonomy_outputs/decision_matrix.txt\n")
cat("  - taxonomy_outputs/distance_matrix.csv\n")
cat("  - taxonomy_outputs/complete_taxonomy.rds\n")

cat("\n========================================\n")

source("Tax_Visualization.R")

# Create all figures
taxonomy_figures <- create_all_taxonomy_figures(
  results = real_data_results,
  taxonomic_matrix = taxonomic_matrix,
  output_dir = "taxonomy_figures"
)

# Or create quick combined figure
quick_fig <- quick_taxonomy_viz(real_data_results, taxonomic_matrix)
ggsave("taxonomy_figures/Quick_Summary.png", quick_fig, 
       width = 16, height = 14, dpi = 300)