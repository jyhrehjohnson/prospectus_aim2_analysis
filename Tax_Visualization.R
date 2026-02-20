# ============================================================================
# TAXONOMY VISUALIZATION FUNCTIONS
# Creates publication-quality figures for taxonomic decisions
# ============================================================================

library(ggplot2)
library(ggdendro)
library(cowplot)
library(viridis)
library(reshape2)
library(ggraph)
library(igraph)

# ============================================================================
# FIGURE 1: TAXONOMIC DENDROGRAM
# ============================================================================

create_taxonomic_dendrogram <- function(taxonomic_matrix, 
                                        title = "Taxonomic Relationships") {
  #' Create hierarchical clustering dendrogram based on hybrid distances
  #' 
  #' @param taxonomic_matrix Output from create_taxonomic_comparison_matrix()
  #' @param title Plot title
  #' @return ggplot object
  
  # Extract distance matrix
  dist_mat <- as.dist(taxonomic_matrix$distances)
  
  # Hierarchical clustering
  hc <- hclust(dist_mat, method = "average")
  
  # Convert to dendrogram data for ggplot
  dendro_data <- dendro_data(hc, type = "rectangle")
  
  # Get decision matrix to color by validity
  decisions <- taxonomic_matrix$decisions
  taxa_names <- rownames(decisions)
  
  # Create plot
  p <- ggplot() +
    # Draw dendrogram segments
    geom_segment(data = segment(dendro_data), 
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 1, color = "gray30") +
    # Add taxon labels
    geom_text(data = label(dendro_data),
              aes(x = x, y = y, label = label),
              hjust = 1.1, angle = 0, size = 5, fontface = "bold") +
    # Add horizontal lines at decision thresholds
    geom_hline(yintercept = 0.4, linetype = "dashed", 
               color = "#d73027", linewidth = 0.8, alpha = 0.7) +
    geom_hline(yintercept = 0.25, linetype = "dashed", 
               color = "#fee090", linewidth = 0.8, alpha = 0.7) +
    # Labels
    labs(
      title = title,
      subtitle = "Based on Hybrid Distance Matrix (Mahalanobis + Gower)",
      y = "Morphological Distance (Hybrid D)",
      x = ""
    ) +
    # Annotations for thresholds
    annotate("text", x = 0.5, y = 0.42, 
             label = "Species threshold", 
             color = "#d73027", size = 3.5, hjust = 0) +
    annotate("text", x = 0.5, y = 0.27, 
             label = "Subspecies threshold", 
             color = "#fee090", size = 3.5, hjust = 0) +
    # Theme
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, color = "gray40")
    ) +
    coord_flip()
  
  return(p)
}

# ============================================================================
# FIGURE 2: DECISION MATRIX HEATMAP
# ============================================================================

create_decision_heatmap <- function(taxonomic_matrix, 
                                    show_distances = TRUE) {
  #' Create heatmap showing all pairwise taxonomic decisions
  #' 
  #' @param taxonomic_matrix Output from create_taxonomic_comparison_matrix()
  #' @param show_distances If TRUE, displays distance values in cells
  #' @return ggplot object
  
  # Prepare data
  decisions <- taxonomic_matrix$decisions
  distances <- taxonomic_matrix$distances
  
  # Convert to long format
  decision_long <- melt(decisions)
  colnames(decision_long) <- c("Taxon_1", "Taxon_2", "Decision")
  
  distance_long <- melt(distances)
  colnames(distance_long) <- c("Taxon_1", "Taxon_2", "Distance")
  
  # Combine
  plot_data <- merge(decision_long, distance_long)
  
  # Create factor with specific order
  plot_data$Decision <- factor(
    plot_data$Decision,
    levels = c("SAME", "SYNONYMIZE", "UNCERTAIN", 
               "TENTATIVELY_DISTINCT", "RECOGNIZE_AS_DISTINCT")
  )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Taxon_1, y = Taxon_2, fill = Decision)) +
    geom_tile(color = "white", linewidth = 1.5) +
    scale_fill_manual(
      values = c(
        "SAME" = "gray95",
        "SYNONYMIZE" = "#d73027",
        "UNCERTAIN" = "#fee090",
        "TENTATIVELY_DISTINCT" = "#91bfdb",
        "RECOGNIZE_AS_DISTINCT" = "#4575b4"
      ),
      name = "Taxonomic\nDecision",
      labels = c(
        "SAME" = "Same taxon",
        "SYNONYMIZE" = "Synonymize (lump)",
        "UNCERTAIN" = "Uncertain",
        "TENTATIVELY_DISTINCT" = "Tentatively distinct",
        "RECOGNIZE_AS_DISTINCT" = "Distinct species"
      )
    )
  
  # Add distance values if requested
  if (show_distances) {
    p <- p + geom_text(
      aes(label = ifelse(Decision != "SAME", 
                         sprintf("%.2f", Distance), 
                         "")),
      size = 4, fontface = "bold", color = "white"
    )
  }
  
  p <- p +
    labs(
      title = "Pairwise Taxonomic Decisions",
      subtitle = "Based on multiple statistical criteria",
      x = "", y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "right"
    ) +
    coord_equal()
  
  return(p)
}

# ============================================================================
# FIGURE 3: MORPHOSPACE WITH TAXONOMIC GROUPS
# ============================================================================

create_taxonomic_morphospace <- function(results, taxonomic_matrix,
                                         pc_x = 1, pc_y = 2) {
  #' PCA morphospace colored by taxonomic decision
  #' 
  #' @param results Output from analyze_real_data_for_taxonomy()
  #' @param taxonomic_matrix Taxonomic decision matrix
  #' @param pc_x Which PC for x-axis (default 1)
  #' @param pc_y Which PC for y-axis (default 2)
  #' @return ggplot object
  
  # Get PCA data
  pca <- results$pca
  pca_scores <- as.data.frame(pca$x)
  pca_scores$taxon <- results$data$taxon
  pca_scores$specimen_id <- results$data$specimen_id
  
  # Variance explained
  var_explained <- summary(pca)$importance[2, ] * 100
  
  # Determine valid species from decision matrix
  decisions <- taxonomic_matrix$decisions
  taxa_list <- rownames(decisions)
  
  # Simple rule: taxon is valid if most comparisons say "distinct"
  valid_status <- sapply(taxa_list, function(taxon) {
    taxon_decisions <- decisions[taxon, ]
    taxon_decisions <- taxon_decisions[taxon_decisions != "SAME"]
    n_distinct <- sum(taxon_decisions %in% 
                        c("RECOGNIZE_AS_DISTINCT", "TENTATIVELY_DISTINCT"))
    n_synonymize <- sum(taxon_decisions == "SYNONYMIZE")
    
    if (n_distinct > n_synonymize) {
      return("Valid species")
    } else {
      return("Junior synonym")
    }
  })
  
  # Add validity status to data
  pca_scores$validity <- valid_status[as.character(pca_scores$taxon)]
  
  # Create plot
  pc_x_name <- paste0("PC", pc_x)
  pc_y_name <- paste0("PC", pc_y)
  
  p <- ggplot(pca_scores, 
              aes(x = .data[[pc_x_name]], 
                  y = .data[[pc_y_name]], 
                  color = taxon,
                  shape = validity)) +
    geom_point(size = 4, alpha = 0.7) +
    stat_ellipse(aes(group = taxon), level = 0.95, linewidth = 1) +
    scale_color_viridis_d(option = "D", end = 0.9, name = "Taxon") +
    scale_shape_manual(
      values = c("Valid species" = 16, "Junior synonym" = 1),
      name = "Taxonomic\nStatus"
    ) +
    labs(
      title = "Morphospace Analysis with Taxonomic Decisions",
      subtitle = "95% confidence ellipses shown for each taxon",
      x = paste0("PC", pc_x, " (", round(var_explained[pc_x], 1), "%)"),
      y = paste0("PC", pc_y, " (", round(var_explained[pc_y], 1), "%)")
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11)
    )
  
  return(p)
}

# ============================================================================
# FIGURE 4: DISTANCE DISTRIBUTIONS BY COMPARISON TYPE
# ============================================================================

create_distance_distribution_plot <- function(results, taxonomic_matrix) {
  #' Violin plot showing within-taxon vs. between-taxon distances
  #' 
  #' @param results Analysis results
  #' @param taxonomic_matrix Taxonomic decisions
  #' @return ggplot object
  
  dist_mat <- results$hybrid$distance_matrix
  taxa <- results$data$taxon
  
  # Collect distances
  distance_data <- data.frame()
  
  for (i in 1:(nrow(dist_mat)-1)) {
    for (j in (i+1):nrow(dist_mat)) {
      
      distance <- dist_mat[i, j]
      taxon_i <- taxa[i]
      taxon_j <- taxa[j]
      
      if (taxon_i == taxon_j) {
        comparison_type <- "Within taxon"
        taxon_pair <- as.character(taxon_i)
      } else {
        comparison_type <- "Between taxa"
        taxon_pair <- paste0(taxon_i, " - ", taxon_j)
      }
      
      distance_data <- rbind(distance_data, data.frame(
        distance = distance,
        comparison_type = comparison_type,
        taxon_pair = taxon_pair
      ))
    }
  }
  
  # Create plot
  p <- ggplot(distance_data, 
              aes(x = comparison_type, y = distance, fill = comparison_type)) +
    geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
    geom_hline(yintercept = 0.4, linetype = "dashed", 
               color = "#d73027", linewidth = 1) +
    scale_fill_manual(
      values = c("Within taxon" = "#4575b4", "Between taxa" = "#d73027")
    ) +
    annotate("text", x = 1.5, y = 0.42, 
             label = "Species threshold", 
             color = "#d73027", size = 4) +
    labs(
      title = "Distribution of Morphological Distances",
      subtitle = "Comparing within-taxon vs. between-taxa variation",
      x = "Comparison Type",
      y = "Hybrid Distance (Mahalanobis + Gower)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}

# ============================================================================
# FIGURE 5: EVIDENCE SCORECARD
# ============================================================================

create_evidence_scorecard <- function(taxonomic_matrix) {
  #' Heatmap showing how many criteria each comparison meets
  #' 
  #' @param taxonomic_matrix Taxonomic decisions with evidence counts
  #' @return ggplot object
  
  # Extract evidence counts from all_decisions
  all_decisions <- taxonomic_matrix$all_decisions
  
  # Get taxa list
  taxa <- unique(c(
    sapply(all_decisions, function(x) x$taxon_A),
    sapply(all_decisions, function(x) x$taxon_B)
  ))
  n_taxa <- length(taxa)
  
  # Create evidence matrix
  evidence_matrix <- matrix(NA, n_taxa, n_taxa)
  rownames(evidence_matrix) <- colnames(evidence_matrix) <- taxa
  
  for (decision_name in names(all_decisions)) {
    decision <- all_decisions[[decision_name]]
    taxon_A <- decision$taxon_A
    taxon_B <- decision$taxon_B
    evidence <- decision$metrics$evidence_strong
    
    idx_A <- which(taxa == taxon_A)
    idx_B <- which(taxa == taxon_B)
    
    evidence_matrix[idx_A, idx_B] <- evidence
    evidence_matrix[idx_B, idx_A] <- evidence
  }
  
  diag(evidence_matrix) <- NA
  
  # Convert to long format
  evidence_long <- melt(evidence_matrix)
  colnames(evidence_long) <- c("Taxon_1", "Taxon_2", "Evidence_Count")
  
  # Remove NA
  evidence_long <- evidence_long[!is.na(evidence_long$Evidence_Count), ]
  
  # Create plot
  p <- ggplot(evidence_long, 
              aes(x = Taxon_1, y = Taxon_2, fill = Evidence_Count)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = Evidence_Count), 
              size = 6, fontface = "bold", color = "white") +
    scale_fill_gradient2(
      low = "#d73027", mid = "#fee090", high = "#4575b4",
      midpoint = 2,
      limits = c(0, 4),
      name = "Evidence\nScore\n(out of 4)",
      breaks = 0:4
    ) +
    labs(
      title = "Evidence Scorecard for Species Distinction",
      subtitle = "Number of statistical criteria supporting separate species status",
      x = "", y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "right"
    ) +
    coord_equal()
  
  return(p)
}

# ============================================================================
# FIGURE 6: NETWORK DIAGRAM OF RELATIONSHIPS
# ============================================================================

create_taxonomic_network <- function(taxonomic_matrix) {
  #' Network diagram showing taxonomic relationships
  #' Edge thickness = similarity, edge color = decision
  #' 
  #' @param taxonomic_matrix Taxonomic decisions
  #' @return ggplot object
  
  library(igraph)
  library(ggraph)
  
  # Get data
  distances <- taxonomic_matrix$distances
  decisions <- taxonomic_matrix$decisions
  taxa <- rownames(distances)
  
  # Create edge list
  edges <- data.frame()
  
  for (i in 1:(length(taxa)-1)) {
    for (j in (i+1):length(taxa)) {
      edges <- rbind(edges, data.frame(
        from = taxa[i],
        to = taxa[j],
        distance = distances[i, j],
        similarity = 1 - distances[i, j],  # Convert distance to similarity
        decision = decisions[i, j]
      ))
    }
  }
  
  # Create igraph object
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = taxa)
  
  # Set edge attributes
  E(g)$weight <- edges$similarity
  E(g)$decision <- edges$decision
  
  # Color mapping for decisions
  decision_colors <- c(
    "SYNONYMIZE" = "#d73027",
    "UNCERTAIN" = "#fee090",
    "TENTATIVELY_DISTINCT" = "#91bfdb",
    "RECOGNIZE_AS_DISTINCT" = "#4575b4"
  )
  
  E(g)$color <- decision_colors[E(g)$decision]
  
  # Create layout
  set.seed(42)
  layout <- create_layout(g, layout = "fr")
  
  # Plot
  p <- ggraph(layout) +
    geom_edge_link(aes(width = similarity, color = decision), 
                   alpha = 0.7) +
    scale_edge_width(range = c(0.5, 3), name = "Similarity") +
    scale_edge_color_manual(
      values = decision_colors,
      name = "Decision"
    ) +
    geom_node_point(size = 15, color = "#2c3e50", alpha = 0.8) +
    geom_node_text(aes(label = name), 
                   color = "white", size = 5, fontface = "bold") +
    labs(
      title = "Taxonomic Relationship Network",
      subtitle = "Edge width = similarity | Edge color = taxonomic decision"
    ) +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right"
    )
  
  return(p)
}

# ============================================================================
# MASTER FUNCTION: CREATE ALL TAXONOMY FIGURES
# ============================================================================

create_all_taxonomy_figures <- function(results, taxonomic_matrix, 
                                        output_dir = "taxonomy_figures") {
  #' Generate all taxonomy visualization figures
  #' 
  #' @param results Analysis results from analyze_real_data_for_taxonomy()
  #' @param taxonomic_matrix Output from create_taxonomic_comparison_matrix()
  #' @param output_dir Directory to save figures
  #' @return List of all plot objects
  
  cat("\n========================================\n")
  cat("CREATING TAXONOMY VISUALIZATIONS\n")
  cat("========================================\n\n")
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)
  
  # Figure 1: Dendrogram
  cat("Creating Figure 1: Taxonomic Dendrogram...\n")
  fig1 <- create_taxonomic_dendrogram(taxonomic_matrix)
  ggsave(
    file.path(output_dir, "Fig1_Taxonomic_Dendrogram.png"),
    fig1, width = 10, height = 8, dpi = 300
  )
  ggsave(
    file.path(output_dir, "Fig1_Taxonomic_Dendrogram.pdf"),
    fig1, width = 10, height = 8
  )
  
  # Figure 2: Decision Heatmap
  cat("Creating Figure 2: Decision Heatmap...\n")
  fig2 <- create_decision_heatmap(taxonomic_matrix, show_distances = TRUE)
  ggsave(
    file.path(output_dir, "Fig2_Decision_Heatmap.png"),
    fig2, width = 10, height = 9, dpi = 300
  )
  ggsave(
    file.path(output_dir, "Fig2_Decision_Heatmap.pdf"),
    fig2, width = 10, height = 9
  )
  
  # Figure 3: Morphospace
  cat("Creating Figure 3: Taxonomic Morphospace...\n")
  fig3 <- create_taxonomic_morphospace(results, taxonomic_matrix)
  ggsave(
    file.path(output_dir, "Fig3_Taxonomic_Morphospace.png"),
    fig3, width = 10, height = 8, dpi = 300
  )
  ggsave(
    file.path(output_dir, "Fig3_Taxonomic_Morphospace.pdf"),
    fig3, width = 10, height = 8
  )
  
  # Figure 4: Distance Distributions
  cat("Creating Figure 4: Distance Distributions...\n")
  fig4 <- create_distance_distribution_plot(results, taxonomic_matrix)
  ggsave(
    file.path(output_dir, "Fig4_Distance_Distributions.png"),
    fig4, width = 10, height = 7, dpi = 300
  )
  ggsave(
    file.path(output_dir, "Fig4_Distance_Distributions.pdf"),
    fig4, width = 10, height = 7
  )
  
  # Figure 5: Evidence Scorecard
  cat("Creating Figure 5: Evidence Scorecard...\n")
  fig5 <- create_evidence_scorecard(taxonomic_matrix)
  ggsave(
    file.path(output_dir, "Fig5_Evidence_Scorecard.png"),
    fig5, width = 10, height = 9, dpi = 300
  )
  ggsave(
    file.path(output_dir, "Fig5_Evidence_Scorecard.pdf"),
    fig5, width = 10, height = 9
  )
  
  # Figure 6: Network
  cat("Creating Figure 6: Taxonomic Network...\n")
  fig6 <- create_taxonomic_network(taxonomic_matrix)
  ggsave(
    file.path(output_dir, "Fig6_Taxonomic_Network.png"),
    fig6, width = 12, height = 10, dpi = 300
  )
  ggsave(
    file.path(output_dir, "Fig6_Taxonomic_Network.pdf"),
    fig6, width = 12, height = 10
  )
  
  cat("\n========================================\n")
  cat("ALL FIGURES CREATED\n")
  cat("========================================\n\n")
  
  cat("Saved to:", output_dir, "\n")
  cat("  - Fig1_Taxonomic_Dendrogram.png/.pdf\n")
  cat("  - Fig2_Decision_Heatmap.png/.pdf\n")
  cat("  - Fig3_Taxonomic_Morphospace.png/.pdf\n")
  cat("  - Fig4_Distance_Distributions.png/.pdf\n")
  cat("  - Fig5_Evidence_Scorecard.png/.pdf\n")
  cat("  - Fig6_Taxonomic_Network.png/.pdf\n\n")
  
  return(list(
    dendrogram = fig1,
    heatmap = fig2,
    morphospace = fig3,
    distributions = fig4,
    scorecard = fig5,
    network = fig6
  ))
}

# ============================================================================
# CONVENIENCE FUNCTION: QUICK VISUALIZATION
# ============================================================================

quick_taxonomy_viz <- function(results, taxonomic_matrix) {
  #' Quick 2x3 panel figure showing key visualizations
  #' 
  #' @return Combined plot
  
  fig1 <- create_decision_heatmap(taxonomic_matrix, show_distances = FALSE)
  fig2 <- create_taxonomic_morphospace(results, taxonomic_matrix)
  fig3 <- create_distance_distribution_plot(results, taxonomic_matrix)
  fig4 <- create_evidence_scorecard(taxonomic_matrix)
  
  combined <- plot_grid(
    fig1, fig2, fig3, fig4,
    ncol = 2, nrow = 2,
    labels = c("A", "B", "C", "D"),
    label_size = 16
  )
  
  return(combined)
}