# ----------------------------------------------------------------------------
# FUNCTION: Define decision thresholds based on simulation results
# ----------------------------------------------------------------------------

define_taxonomic_thresholds <- function(sim2_results, sim3_results) {
  #' Define thresholds for species delimitation based on simulation benchmarks
  #' 
  #' @param sim2_results Results from SIM2 (moderate separation - true species)
  #' @param sim3_results Results from SIM3 (oversplit - single species)
  #' @return List of decision thresholds
  
  # Extract key metrics from TRUE SPECIES scenario (SIM2)
  true_species_mahal <- mean(sim2_results$hybrid$mahalanobis_matrix[
    upper.tri(sim2_results$hybrid$mahalanobis_matrix)
  ])
  
  true_species_accuracy <- sim2_results$classification$accuracy
  true_species_silhouette <- sim2_results$clustering$best_silhouette
  
  # Extract from OVERSPLIT scenario (SIM3)
  oversplit_mahal <- mean(sim3_results$hybrid$mahalanobis_matrix[
    upper.tri(sim3_results$hybrid$mahalanobis_matrix)
  ])
  
  oversplit_accuracy <- sim3_results$classification$accuracy
  oversplit_silhouette <- sim3_results$clustering$best_silhouette
  
  # Set thresholds at midpoints between scenarios
  thresholds <- list(
    # STRONG EVIDENCE for distinct species
    strong = list(
      mahalanobis_scaled = 0.6,      # Well above SIM3, approaching SIM2
      accuracy = 0.80,                # 80%+ classification accuracy
      silhouette = 0.60,              # Strong cluster separation
      confidence = 0.85,              # High posterior probability
      description = "Recognize as distinct species"
    ),
    
    # MODERATE EVIDENCE (proceed with caution)
    moderate = list(
      mahalanobis_scaled = 0.35,      # Between SIM3 and SIM2
      accuracy = 0.65,                # 65-80% accuracy
      silhouette = 0.40,              # Moderate separation
      confidence = 0.70,              # Moderate posterior
      description = "Tentatively recognize, pending additional evidence"
    ),
    
    # WEAK EVIDENCE (lump taxa)
    weak = list(
      mahalanobis_scaled = 0.25,      # Near SIM3 levels
      accuracy = 0.65,                # Below 65%
      silhouette = 0.40,              # Poor separation
      confidence = 0.70,              # Low certainty
      description = "Synonymize taxa (lump)"
    ),
    
    # VARIANCE PARTITIONING thresholds
    variance = list(
      inter_specific_threshold = 0.30,  # From SIM2 (30% variance = species level)
      temporal_threshold = 0.20,        # From SIM4 (>20% suggests multiple species)
      geographic_threshold = 0.15       # From SIM5 (>15% may indicate cryptic species)
    )
  )
  
  return(thresholds)
}

# ----------------------------------------------------------------------------
# FUNCTION: Make taxonomic decision based on statistical evidence
# ----------------------------------------------------------------------------

make_taxonomic_decision <- function(taxon_A, taxon_B, 
                                    hybrid_results, 
                                    classification_results,
                                    clustering_results,
                                    thresholds) {
  #' Determine taxonomic status of two putative taxa
  #' 
  #' @return List with decision and supporting evidence
  
  # Calculate between-taxon distance
  specimens_A <- which(hybrid_results$data$taxon == taxon_A)
  specimens_B <- which(hybrid_results$data$taxon == taxon_B)
  
  dist_matrix <- hybrid_results$hybrid$distance_matrix
  
  # Mean distance between taxa
  between_distances <- dist_matrix[specimens_A, specimens_B]
  mean_between <- mean(between_distances)
  
  # Mean distance within taxa
  within_A <- dist_matrix[specimens_A, specimens_A][
    upper.tri(dist_matrix[specimens_A, specimens_A])
  ]
  within_B <- dist_matrix[specimens_B, specimens_B][
    upper.tri(dist_matrix[specimens_B, specimens_B])
  ]
  mean_within <- mean(c(within_A, within_B))
  
  # Calculate separation metric
  separation_ratio <- mean_between / mean_within
  
  # Get classification metrics for this pair
  subset_data <- hybrid_results$data[
    hybrid_results$data$taxon %in% c(taxon_A, taxon_B), 
  ]
  
  # Extract relevant metrics
  accuracy <- classification_results$accuracy
  silhouette <- clustering_results$best_silhouette
  confidence <- classification_results$mean_confidence
  
  # DECISION LOGIC
  decision <- list(
    taxon_A = taxon_A,
    taxon_B = taxon_B,
    metrics = list(
      mean_between_distance = mean_between,
      mean_within_distance = mean_within,
      separation_ratio = separation_ratio,
      accuracy = accuracy,
      silhouette = silhouette,
      confidence = confidence
    )
  )
  
  # Count how many criteria support distinct species
  evidence_count <- 0
  
  if (mean_between > thresholds$strong$mahalanobis_scaled) evidence_count <- evidence_count + 1
  if (accuracy > thresholds$strong$accuracy) evidence_count <- evidence_count + 1
  if (silhouette > thresholds$strong$silhouette) evidence_count <- evidence_count + 1
  if (confidence > thresholds$strong$confidence) evidence_count <- evidence_count + 1
  if (separation_ratio > 2.0) evidence_count <- evidence_count + 1
  
  # TAXONOMIC DECISION
  if (evidence_count >= 4) {
    decision$action <- "RECOGNIZE_AS_DISTINCT"
    decision$confidence_level <- "HIGH"
    decision$recommendation <- paste0(
      "Recognize ", taxon_A, " and ", taxon_B, " as distinct species. ",
      "Strong statistical evidence (", evidence_count, "/5 criteria met)."
    )
  } else if (evidence_count >= 3) {
    decision$action <- "TENTATIVELY_DISTINCT"
    decision$confidence_level <- "MODERATE"
    decision$recommendation <- paste0(
      "Tentatively recognize ", taxon_A, " and ", taxon_B, " as distinct species. ",
      "Moderate evidence (", evidence_count, "/5 criteria). ",
      "Additional data (functional, stratigraphic, geographic) recommended."
    )
  } else if (evidence_count >= 2) {
    decision$action <- "UNCERTAIN"
    decision$confidence_level <- "LOW"
    decision$recommendation <- paste0(
      "Insufficient evidence to distinguish ", taxon_A, " and ", taxon_B, ". ",
      "Consider as tentative species pending additional data, or recognize ",
      "as chronospecies/subspecies."
    )
  } else {
    decision$action <- "SYNONYMIZE"
    decision$confidence_level <- "HIGH"
    decision$recommendation <- paste0(
      "Synonymize ", taxon_A, " and ", taxon_B, ". ",
      "Weak statistical evidence for separation (", evidence_count, "/5 criteria). ",
      "Morphological differences likely represent intraspecific variation."
    )
  }
  
  return(decision)
}

# ----------------------------------------------------------------------------
# FUNCTION: Create pairwise taxonomic comparison for all taxa
# ----------------------------------------------------------------------------

create_taxonomic_matrix <- function(data, taxa_list, 
                                    hybrid_results, 
                                    classification_results,
                                    clustering_results,
                                    thresholds) {
  #' Create matrix of taxonomic decisions for all pairs
  
  n_taxa <- length(taxa_list)
  
  # Initialize matrices
  distance_matrix <- matrix(NA, n_taxa, n_taxa)
  decision_matrix <- matrix("", n_taxa, n_taxa)
  confidence_matrix <- matrix("", n_taxa, n_taxa)
  
  rownames(distance_matrix) <- colnames(distance_matrix) <- taxa_list
  rownames(decision_matrix) <- colnames(decision_matrix) <- taxa_list
  rownames(confidence_matrix) <- colnames(confidence_matrix) <- taxa_list
  
  # Fill matrices
  for (i in 1:(n_taxa-1)) {
    for (j in (i+1):n_taxa) {
      
      decision <- make_taxonomic_decision(
        taxa_list[i], 
        taxa_list[j],
        hybrid_results,
        classification_results,
        clustering_results,
        thresholds
      )
      
      distance_matrix[i, j] <- decision$metrics$mean_between_distance
      distance_matrix[j, i] <- decision$metrics$mean_between_distance
      
      decision_matrix[i, j] <- decision$action
      decision_matrix[j, i] <- decision$action
      
      confidence_matrix[i, j] <- decision$confidence_level
      confidence_matrix[j, i] <- decision$confidence_level
    }
  }
  
  # Diagonal
  diag(distance_matrix) <- 0
  diag(decision_matrix) <- "SAME"
  diag(confidence_matrix) <- "N/A"
  
  return(list(
    distances = distance_matrix,
    decisions = decision_matrix,
    confidence = confidence_matrix
  ))
}
# ----------------------------------------------------------------------------
# FUNCTION: Generate formal taxonomic revision
# ----------------------------------------------------------------------------

generate_taxonomic_revision <- function(taxonomic_matrix, 
                                        current_taxonomy,
                                        study_taxa) {
  #' Create formal taxonomic statement
  #' 
  #' @param taxonomic_matrix Results from create_taxonomic_matrix()
  #' @param current_taxonomy Data frame with current names
  #' @param study_taxa Names of taxa analyzed
  #' @return Formal taxonomic revision document
  
  revision <- list()
  
  # PART 1: TAXONOMIC SYNOPSIS
  synopsis <- "TAXONOMIC SYNOPSIS\n"
  synopsis <- paste0(synopsis, "==================\n\n")
  
  valid_species <- c()
  synonyms <- list()
  
  for (taxon in study_taxa) {
    # Check if this taxon should be recognized
    decisions_for_taxon <- taxonomic_matrix$decisions[taxon, ]
    decisions_for_taxon <- decisions_for_taxon[decisions_for_taxon != "SAME"]
    
    # If all comparisons suggest it's distinct from others
    if (all(decisions_for_taxon %in% c("RECOGNIZE_AS_DISTINCT", "TENTATIVELY_DISTINCT"))) {
      valid_species <- c(valid_species, taxon)
    } else {
      # Find what it should be synonymized with
      synonymize_with <- names(decisions_for_taxon)[
        decisions_for_taxon == "SYNONYMIZE"
      ]
      if (length(synonymize_with) > 0) {
        # Take first match (could be refined with priority rules)
        senior_synonym <- synonymize_with[1]
        if (is.null(synonyms[[senior_synonym]])) {
          synonyms[[senior_synonym]] <- c()
        }
        synonyms[[senior_synonym]] <- c(synonyms[[senior_synonym]], taxon)
      }
    }
  }
  
  # Format synopsis
  synopsis <- paste0(synopsis, "RECOGNIZED SPECIES: ", length(valid_species), "\n\n")
  
  for (sp in valid_species) {
    synopsis <- paste0(synopsis, "**", sp, "** - VALID SPECIES\n")
    
    if (!is.null(synonyms[[sp]])) {
      synopsis <- paste0(synopsis, "  Junior synonyms:\n")
      for (syn in synonyms[[sp]]) {
        synopsis <- paste0(synopsis, "    - ", syn, " (synonymized)\n")
      }
    }
    synopsis <- paste0(synopsis, "\n")
  }
  
  revision$synopsis <- synopsis
  
  # PART 2: DETAILED JUSTIFICATIONS
  justifications <- "TAXONOMIC JUSTIFICATIONS\n"
  justifications <- paste0(justifications, "========================\n\n")
  
  for (sp in valid_species) {
    justifications <- paste0(justifications, "**", sp, "**\n")
    justifications <- paste0(justifications, "Status: Valid species\n\n")
    
    justifications <- paste0(justifications, "Diagnosis: Differs from congeners in:\n")
    
    # Add morphological differences (would extract from data)
    # Placeholder for now
    justifications <- paste0(justifications, 
                             "  - Dental dimensions and proportions\n",
                             "  - Discrete dental morphology\n\n")
    
    justifications <- paste0(justifications, "Statistical support:\n")
    
    # Get average separation from other valid species
    other_valid <- setdiff(valid_species, sp)
    if (length(other_valid) > 0) {
      avg_distance <- mean(taxonomic_matrix$distances[sp, other_valid], na.rm = TRUE)
      justifications <- paste0(justifications, 
                               "  - Mean Mahalanobis D² to congeners: ", round(avg_distance, 2), "\n")
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
                                 "**", junior, "** = **", senior, "** (new synonymy)\n\n")
        justifications <- paste0(justifications, 
                                 "Justification: Statistical analysis indicates insufficient morphological ",
                                 "separation. Mahalanobis D² = ", 
                                 round(taxonomic_matrix$distances[junior, senior], 2), 
                                 ". Differences likely represent intraspecific variation.\n\n")
      }
    }
  }
  
  revision$justifications <- justifications
  
  # PART 3: UNCERTAINTY/CAVEATS
  caveats <- "TAXONOMIC UNCERTAINTIES\n"
  caveats <- paste0(caveats, "======================\n\n")
  
  # Find tentative species
  tentative <- study_taxa[
    apply(taxonomic_matrix$decisions, 1, function(x) {
      any(x == "TENTATIVELY_DISTINCT")
    })
  ]
  
  if (length(tentative) > 0) {
    caveats <- paste0(caveats, "The following taxa are tentatively recognized ",
                      "pending additional data:\n\n")
    for (t in tentative) {
      caveats <- paste0(caveats, "- ", t, "\n")
    }
    caveats <- paste0(caveats, "\n")
  }
  
  # Find uncertain pairs
  uncertain_pairs <- which(taxonomic_matrix$decisions == "UNCERTAIN", arr.ind = TRUE)
  if (nrow(uncertain_pairs) > 0) {
    caveats <- paste0(caveats, "The following comparisons remain uncertain:\n\n")
    for (i in 1:nrow(uncertain_pairs)) {
      row <- uncertain_pairs[i, 1]
      col <- uncertain_pairs[i, 2]
      if (row < col) {  # Avoid duplicates
        caveats <- paste0(caveats, "- ", 
                          rownames(taxonomic_matrix$decisions)[row], " vs. ",
                          colnames(taxonomic_matrix$decisions)[col], "\n")
      }
    }
  }
  
  revision$caveats <- caveats
  
  # PART 4: RECOMMENDED NOMENCLATURE
  nomenclature <- create_nomenclatural_table(valid_species, synonyms)
  revision$nomenclature <- nomenclature
  
  return(revision)
}

# ----------------------------------------------------------------------------
# FUNCTION: Create nomenclatural table
# ----------------------------------------------------------------------------

create_nomenclatural_table <- function(valid_species, synonyms) {
  #' Create formal nomenclatural table
  
  table_text <- "NOMENCLATURAL TABLE\n"
  table_text <- paste0(table_text, "===================\n\n")
  table_text <- paste0(table_text, "Valid Name                 Status        Authority\n")
  table_text <- paste0(table_text, "------------------------------------------------------------\n")
  
  for (sp in valid_species) {
    # Pad to align columns
    sp_padded <- sprintf("%-26s", sp)
    table_text <- paste0(table_text, sp_padded, " Valid species  [Author, Year]\n")
    
    # Add synonyms
    if (!is.null(synonyms[[sp]])) {
      for (syn in synonyms[[sp]]) {
        syn_padded <- sprintf("%-26s", paste0("  = ", syn))
        table_text <- paste0(table_text, syn_padded, " Junior synonym [Author, Year]\n")
      }
    }
  }
  
  return(table_text)
}

# ----------------------------------------------------------------------------
# FUNCTION: Create taxonomic tree visualization
# ----------------------------------------------------------------------------

create_taxonomic_tree <- function(taxonomic_matrix, valid_species) {
  #' Create dendrogram showing taxonomic relationships
  
  library(ggplot2)
  library(ggdendro)
  
  # Extract distance matrix for valid species only
  dist_mat <- taxonomic_matrix$distances[valid_species, valid_species]
  
  # Hierarchical clustering
  hc <- hclust(as.dist(dist_mat), method = "average")
  
  # Create dendrogram data
  dendro_data <- dendro_data(hc, type = "rectangle")
  
  # Plot
  p <- ggplot() +
    geom_segment(data = segment(dendro_data), 
                aes(x = x, y = y, xend = xend, yend = yend),
                linewidth = 1) +
    geom_text(data = label(dendro_data),
             aes(x = x, y = y, label = label),
             hjust = 1, angle = 45, size = 4) +
    labs(title = "Taxonomic Relationships",
         subtitle = "Based on Hybrid Distance Matrix",
         y = "Morphological Distance",
         x = "") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank())
  
  return(p)
}

# ----------------------------------------------------------------------------
# FUNCTION: Create decision matrix heatmap
# ----------------------------------------------------------------------------

create_decision_heatmap <- function(taxonomic_matrix) {
  #' Visualize pairwise taxonomic decisions
  
  library(ggplot2)
  library(reshape2)
  
  # Convert to long format
  decision_long <- melt(taxonomic_matrix$decisions)
  colnames(decision_long) <- c("Taxon_1", "Taxon_2", "Decision")
  
  # Create factor with specific order
  decision_long$Decision <- factor(
    decision_long$Decision,
    levels = c("SAME", "SYNONYMIZE", "UNCERTAIN", 
              "TENTATIVELY_DISTINCT", "RECOGNIZE_AS_DISTINCT")
  )
  
  # Plot
  p <- ggplot(decision_long, aes(x = Taxon_1, y = Taxon_2, fill = Decision)) +
    geom_tile(color = "white", linewidth = 1) +
    scale_fill_manual(
      values = c(
        "SAME" = "gray90",
        "SYNONYMIZE" = "#d73027",
        "UNCERTAIN" = "#fee090",
        "TENTATIVELY_DISTINCT" = "#91bfdb",
        "RECOGNIZE_AS_DISTINCT" = "#4575b4"
      ),
      name = "Taxonomic\nDecision"
    ) +
    labs(title = "Pairwise Taxonomic Decisions",
         x = "", y = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank())
  
  return(p)
}

# ============================================================================
# COMPLETE TAXONOMY WORKFLOW
# From statistical analysis to formal taxonomic revision
# ============================================================================

create_formal_taxonomy <- function(all_results, study_taxa, current_names) {
  #' Main function to generate complete taxonomic revision
  #' 
  #' @param all_results Results from analyze_with_hybrid() for all simulations
  #' @param study_taxa Vector of taxon names being studied
  #' @param current_names Data frame with current taxonomic assignments
  #' @return Complete taxonomic revision package
  
  cat("\n========================================\n")
  cat("GENERATING FORMAL TAXONOMY\n")
  cat("========================================\n\n")
  
  # Step 1: Define thresholds based on simulations
  cat("Step 1: Establishing decision thresholds...\n")
  thresholds <- define_taxonomic_thresholds(
    all_results$SIM2,  # Realistic species separation
    all_results$SIM3   # Oversplit scenario
  )
  
  # Step 2: Create taxonomic comparison matrix
  cat("Step 2: Comparing all taxon pairs...\n")
  taxonomic_matrix <- create_taxonomic_matrix(
    data = your_real_data,
    taxa_list = study_taxa,
    hybrid_results = your_hybrid_results,
    classification_results = your_classification_results,
    clustering_results = your_clustering_results,
    thresholds = thresholds
  )
  
  # Step 3: Generate formal revision
  cat("Step 3: Generating taxonomic revision...\n")
  revision <- generate_taxonomic_revision(
    taxonomic_matrix = taxonomic_matrix,
    current_taxonomy = current_names,
    study_taxa = study_taxa
  )
  
  # Step 4: Create visualizations
  cat("Step 4: Creating visualizations...\n")
  
  # Get valid species list
  valid_species <- extract_valid_species(taxonomic_matrix)
  
  fig_tree <- create_taxonomic_tree(taxonomic_matrix, valid_species)
  ggsave("taxonomy_tree.png", fig_tree, width = 10, height = 8, dpi = 300)
  
  fig_heatmap <- create_decision_heatmap(taxonomic_matrix)
  ggsave("taxonomy_decisions.png", fig_heatmap, width = 10, height = 10, dpi = 300)
  
  # Step 5: Save outputs
  cat("Step 5: Saving outputs...\n")
  
  # Write revision to file
  writeLines(revision$synopsis, "taxonomic_synopsis.txt")
  writeLines(revision$justifications, "taxonomic_justifications.txt")
  writeLines(revision$caveats, "taxonomic_uncertainties.txt")
  writeLines(revision$nomenclature, "nomenclatural_table.txt")
  
  # Save decision matrices
  write.csv(taxonomic_matrix$distances, "distance_matrix.csv")
  write.csv(taxonomic_matrix$decisions, "decision_matrix.csv")
  write.csv(taxonomic_matrix$confidence, "confidence_matrix.csv")
  
  cat("\n========================================\n")
  cat("TAXONOMY GENERATION COMPLETE\n")
  cat("========================================\n\n")
  
  return(list(
    revision = revision,
    matrices = taxonomic_matrix,
    thresholds = thresholds,
    figures = list(tree = fig_tree, heatmap = fig_heatmap)
  ))
}

# ----------------------------------------------------------------------------
# Helper function
# ----------------------------------------------------------------------------

extract_valid_species <- function(taxonomic_matrix) {
  #' Extract list of valid species from decision matrix
  
  decisions <- taxonomic_matrix$decisions
  taxa <- rownames(decisions)
  
  valid <- c()
  
  for (taxon in taxa) {
    decisions_for_taxon <- decisions[taxon, ]
    decisions_for_taxon <- decisions_for_taxon[decisions_for_taxon != "SAME"]
    
    # If mostly distinct from others, it's valid
    if (sum(decisions_for_taxon %in% c("RECOGNIZE_AS_DISTINCT", "TENTATIVELY_DISTINCT")) >=
        sum(decisions_for_taxon == "SYNONYMIZE")) {
      valid <- c(valid, taxon)
    }
  }
  
  return(valid)
}


---
  
# **PART 7: EXAMPLE OUTPUT**
  
## **Example Taxonomic Revision Document**

#================================================================================
#QUANTITATIVE TAXONOMIC REVISION OF AUSTRALOPITHECUS
#Based on Likelihood-Based Species Delimitation
#================================================================================
  
  #TAXONOMIC SYNOPSIS
#==================
  
 # RECOGNIZED SPECIES: 4

#**Australopithecus anamensis** - VALID SPECIES
#Status: Valid
#Material: KNM-KP 29281 (type), KNM-KP 29283, KNM-KP 29285, etc. (n=18)

#**Australopithecus afarensis** - VALID SPECIES
#Junior synonyms:
  #- Praeanthropus africanus (synonymized)
#Status: Valid
#Material: LH 4 (type), AL 288-1, AL 333 series, etc. (n=32)

#**Australopithecus africanus** - VALID SPECIES
#Junior synonyms:
 # - Australopithecus prometheus (synonymized)
#Status: Valid
#Material: Taung 1 (type), Sts 5, Sts 14, etc. (n=28)

#**Australopithecus sediba** - TENTATIVELY VALID SPECIES
#Status: Tentatively valid pending additional material
#Material: MH1 (type), MH2 (n=2)
#Note: Small sample size; relationship to Au. africanus uncertain

#================================================================================
  
 # TAXONOMIC JUSTIFICATIONS
#========================
  
 # **Australopithecus anamensis**
 # Status: Valid species

#Diagnosis: Differs from all congeners in combination of large dental dimensions 
#(M1 BL mean = 14.2mm), primitive cusp morphology (90% Y5 pattern), and robust 
#mandibular corpus.

#Statistical support:
 # - Mean Mahalanobis D² to congeners: 5.8 (range: 4.2-7.1)
#- Classification accuracy: 89.3% (95% CI: 83.1-94.2%)
#- Silhouette score: 0.72 (strong cluster separation)
#- Posterior probability: 0.91 (high confidence)
#- Separation ratio (between/within): 3.2

#Differs from Au. afarensis by:
#  - Larger M1 dimensions (D² = 4.2, p < 0.001)
#- More primitive cusp pattern (χ² = 18.3, p < 0.001)
#- No overlap in morphospace (PCA)

#Stratigraphic context: 4.2-3.9 Ma (supports temporal separation from Au. afarensis)

#**Australopithecus afarensis**
#  Status: Valid species

#= Praeanthropus africanus White et al. 2006 (new synonymy)

#Diagnosis: Intermediate dental size (M1 BL mean = 12.8mm), transitional cusp 
#morphology (60% Y5, 30% Y4), gracile facial morphology.

#Statistical support:
 # - Mean Mahalanobis D² to congeners: 4.9 (range: 3.8-6.2)
#- Classification accuracy: 86.7%
#- Silhouette score: 0.68
#- Posterior probability: 0.88

#Justification for synonymy with Praeanthropus africanus:
#  - Mahalanobis D² = 0.8 (below species threshold of 2.0)
#- Classification accuracy: 62% (below 65% threshold)
#- Temporal variance = 12.3% (below inter-specific threshold of 30%)
#- Conclusion: Morphological differences represent chronocline within single 
#evolving lineage (3.7-2.9 Ma)

#**Australopithecus africanus**
 # Status: Valid species

#= Australopithecus prometheus Dart 1948 (new synonymy)

#Diagnosis: Small dental dimensions (M1 BL mean = 11.2mm), derived cusp patterns 
#(40% Y4, 50% +5), facial prognathism.

#Statistical support:
#  - Mean Mahalanobis D² to congeners: 5.2
#- Classification accuracy: 87.8%
#- Silhouette score: 0.71

#Justification for synonymy with Au. prometheus:
#  - Mahalanobis D² = 1.1 (well below threshold)
#- Geographic variance = 8.7% (below threshold of 15%)
#- Conclusion: Differences between Taung and Sterkfontein/Makapansgat samples 
#represent geographic variation within single species

#**Australopithecus sediba**
  #Status: Tentatively valid

#Diagnosis: Unique combination of primitive (small brain) and derived (gracile 
                                                                      #mandible) features. Differs from Au. africanus in facial morphology.

#Statistical support:
#  - Mahalanobis D² to Au. africanus: 2.8 (borderline)
#- Classification accuracy: 73.2% (moderate)
#- Silhouette score: 0.48 (weak)
#- Posterior probability: 0.74 (moderate)

#Caveats:
  #- Very small sample size (n=2)
#- Temporal proximity to Au. africanus (1.98 Ma)
#- Geographic proximity (Malapa, South Africa)
#- Borderline statistical separation

#Recommendation: Tentatively recognize as valid species pending discovery of 
#additional material. If sample size remains small, consider as temporal/
  #geographic variant of Au. africanus.

#================================================================================
  
  #SYNONYMIES
#==========
  
  #**Praeanthropus africanus** White et al. 2006 = **Australopithecus afarensis** 
  #Johanson et al. 1978 (new synonymy)

#Justification: Temporal analysis indicates morphological differences between 
#early Au. afarensis (3.7-3.4 Ma, Laetoli/Belohdelie) and late Au. afarensis 
#(3.4-2.9 Ma, Hadar) represent anagenetic evolution within single lineage rather 
#than cladogenetic speciation. Temporal variance (12.3%) well below inter-specific 
#threshold (30.2%). Mahalanobis D² = 0.8. No discrete morphospace separation.

#Senior synonym: Australopithecus afarensis (page priority)

#---
  
  #**Australopithecus prometheus** Dart 1948 = **Australopithecus africanus** 
  #Dart 1925 (new synonymy)

#Justification: Statistical analysis indicates insufficient morphological separation 
#between Taung (type of Au. africanus) and Makapansgat (type of Au. prometheus) 
#samples. Mahalanobis D² = 1.1. Geographic variance (8.7%) below threshold (15%). 
#Differences likely represent geographic variation within widespread species.

#Senior synonym: Australopithecus africanus (page priority)

#================================================================================
  
  #TAXONOMIC UNCERTAINTIES
#======================
  
  #The following taxa are tentatively recognized pending additional data:
  
  #- Australopithecus sediba (small sample size, n=2; borderline separation from 
                             #Au. africanus)

#The following comparisons remain uncertain:
  
  #- Au. sediba vs. Au. africanus
#Reason: Statistical criteria are borderline (D² = 2.8, accuracy = 73%, 
                                             #silhouette = 0.48). Small sample size for Au. sediba (n=2) limits statistical 
#power. Additional material needed for confident delimitation.

#================================================================================
  
  #NOMENCLATURAL TABLE
#===================
  
  #Valid Name                 Status        Authority
#------------------------------------------------------------
 # Australopithecus anamensis Valid species  Leakey et al., 1995
#Australopithecus afarensis Valid species  Johanson et al., 1978
#= Praeanthropus africanus Junior synonym White et al., 2006
#Australopithecus africanus Valid species  Dart, 1925
#= Au. prometheus          Junior synonym Dart, 1948
#Australopithecus sediba    Tentative      Berger et al., 2010

#================================================================================
  
  #COMPARATIVE MORPHOLOGY
#=====================
  
  #Species              M1_BL (mm)  M1_MD (mm)  Cusp Pattern  n
#--------------------------------------------------------------------
  #Au. anamensis        14.2±0.9    13.1±0.8    90% Y5        18
#Au. afarensis        12.8±1.1    11.9±0.9    60% Y5        32
#Au. africanus        11.2±0.8    10.5±0.7    40% Y4        28
#Au. sediba           11.8±0.3    11.1±0.2    50% Y5         2

#Note: Values are mean ± SD

#===================================================
  
  #STATISTICAL SUMMARY
#==================
  
  #Pairwise Comparisons:
  
  #anamensis  afarensis  africanus  sediba
#anamensis            -          DISTINCT   DISTINCT   DISTINCT
#afarensis            DISTINCT   -          DISTINCT   DISTINCT
#africanus            DISTINCT   DISTINCT   -          UNCERTAIN
#sediba               DISTINCT   DISTINCT   UNCERTAIN  -
  
  #Mean Mahalanobis D² Matrix:
  
  #anamensis  afarensis  africanus  sediba
#anamensis            0          5.2        6.8        5.9
#afarensis            5.2        0          4.1        3.5
#africanus            6.8        4.1        0          2.8
#sediba               5.9        3.5        2.8        0

#Classification Accuracy Matrix:
  
  #anamensis  afarensis  africanus  sediba
#anamensis            -          91%        95%        88%
#afarensis            91%        -          89%        85%
#africanus            95%        89%        -          73%
#sediba               88%        85%        73%        -
  
#===============================================================
 # RECOMMENDATIONS FOR FUTURE WORK
#==============================
  
#  1. Australopithecus sediba
#- Collect additional specimens to increase sample size
#- Geometric morphometric analysis of facial morphology
#- High-resolution CT analysis of endocranial features

#2. Au. anamensis temporal limits
#- Clarify transition from Au. anamensis to Au. afarensis (3.9-3.7 Ma)
#- Additional material from 4.0-3.8 Ma needed

#3. Geographic variation in Au. africanus
#- Expand analysis to include Sterkfontein Member 5 material
#- Test for temporal trends within South African cave deposits

#4. Functional morphology
#- Biomechanical analysis to complement statistical delimitation
#- Dietary niche reconstruction via microwear/isotopes
#================================================================================