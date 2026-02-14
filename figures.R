# ============================================================================
# FIGURE GENERATION FOR PROPOSAL
# Creates publication-quality figures for all simulations
# ============================================================================

library(ggplot2)
library(cowplot)
library(viridis)
library(gridExtra)
library(ggpubr)
library(reshape2)

# Load results
all_results <- readRDS("all_simulation_results.rds")
sensitivity_results <- readRDS("sensitivity_results.rds")
summary_table <- read.csv("results_summary_table.csv")

# Set theme
theme_set(theme_bw(base_size = 12))

# Create figures directory
dir.create("figures", showWarnings = FALSE)

# ----------------------------------------------------------------------------
# FIGURE 1: PCA Morphospace for All Simulations (5-panel)
# ----------------------------------------------------------------------------

create_figure_1 <- function() {
  
  cat("Creating Figure 1: PCA Morphospace...\n")
  
  # Extract PCA data for each simulation
  plot_list <- list()
  
  for (sim_name in names(all_results)) {
    
    pca_data <- all_results[[sim_name]]$pca$data
    pca_model <- all_results[[sim_name]]$pca$model
    
    # Variance explained
    var_pc1 <- round(summary(pca_model)$importance[2, 1] * 100, 1)
    var_pc2 <- round(summary(pca_model)$importance[2, 2] * 100, 1)
    
    # Create plot
    p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = taxon, shape = taxon)) +
      geom_point(size = 3, alpha = 0.7) +
      stat_ellipse(level = 0.95, linewidth = 0.8) +
      scale_color_viridis_d(option = "D", end = 0.9) +
      labs(
        title = sim_name,
        x = paste0("PC1 (", var_pc1, "%)"),
        y = paste0("PC2 (", var_pc2, "%)")
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    plot_list[[sim_name]] <- p
  }
  
  # Combine into 5-panel figure
  fig1 <- plot_grid(
    plotlist = plot_list,
    ncol = 3,
    labels = c("A", "B", "C", "D", "E"),
    label_size = 14
  )
  
  # Save
  ggsave(
    "figures/Figure1_PCA_Morphospace.pdf",
    fig1,
    width = 12,
    height = 8,
    units = "in"
  )
  
  ggsave(
    "figures/Figure1_PCA_Morphospace.png",
    fig1,
    width = 12,
    height = 8,
    units = "in",
    dpi = 300
  )
  
  cat("Figure 1 saved.\n\n")
  
  return(fig1)
}

# ----------------------------------------------------------------------------
# FIGURE 2: Classification Accuracy Comparison
# ----------------------------------------------------------------------------

create_figure_2 <- function() {
  
  cat("Creating Figure 2: Classification Accuracy...\n")
  
  # Prepare data
  accuracy_data <- data.frame(
    Simulation = summary_table$Simulation,
    Euclidean = summary_table$Euclidean_Accuracy * 100,
    Mahalanobis = summary_table$Mahalanobis_Accuracy * 100,
    True_K = summary_table$True_K,
    Estimated_K = summary_table$Estimated_K
  )
  
  # Reshape for plotting
  accuracy_long <- melt(
    accuracy_data[, c("Simulation", "Euclidean", "Mahalanobis")],
    id.vars = "Simulation",
    variable.name = "Metric",
    value.name = "Accuracy"
  )
  
  # Panel A: Accuracy by metric
  panel_a <- ggplot(accuracy_long, aes(x = Simulation, y = Accuracy, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "red", linewidth = 0.8) +
    scale_fill_manual(values = c("Euclidean" = "#E69F00", "Mahalanobis" = "#56B4E9")) +
    labs(
      title = "Classification Accuracy by Distance Metric",
      x = "Simulation",
      y = "Cross-Validated Accuracy (%)"
    ) +
    annotate("text", x = 5, y = 82, label = "80% threshold", 
             color = "red", size = 3, hjust = 1) +
    theme(legend.position = "bottom")
  
  # Panel B: True vs. Estimated K
  panel_b <- ggplot(accuracy_data, aes(x = True_K, y = Estimated_K)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = Simulation), size = 4) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    labs(
      title = "True vs. Estimated Number of Species",
      x = "True Number of Species",
      y = "Estimated Number of Species (Mclust)"
    ) +
    coord_fixed(ratio = 1, xlim = c(0.5, 3.5), ylim = c(0.5, 3.5)) +
    theme(legend.position = "bottom")
  
  # Combine
  fig2 <- plot_grid(
    panel_a, panel_b,
    ncol = 2,
    labels = c("A", "B"),
    label_size = 14
  )
  
  # Save
  ggsave(
    "figures/Figure2_Classification_Performance.pdf",
    fig2,
    width = 12,
    height = 5,
    units = "in"
  )
  
  ggsave(
    "figures/Figure2_Classification_Performance.png",
    fig2,
    width = 12,
    height = 5,
    units = "in",
    dpi = 300
  )
  
  cat("Figure 2 saved.\n\n")
  
  return(fig2)
}

# ----------------------------------------------------------------------------
# FIGURE 3: Temporal and Geographic Variance Partitioning
# ----------------------------------------------------------------------------

create_figure_3 <- function() {
  
  cat("Creating Figure 3: Variance Partitioning...\n")
  
  # Extract temporal results (SIM4)
  temporal_results <- all_results$SIM4$temporal
  
  # Extract variance components for M1_BL as example
  if (!is.null(temporal_results) && "M1_BL" %in% names(temporal_results)) {
    
    temp_var <- temporal_results$M1_BL$variance_components
    
    # Panel A: Temporal variance partitioning
    temp_data <- data.frame(
      Component = c("Temporal\n(among time periods)", "Residual\n(within time periods)"),
      Variance = c(
        temp_var$vcov[temp_var$grp == "taxon"],
        temp_var$vcov[temp_var$grp == "Residual"]
      )
    )
    temp_data$Percentage <- temp_data$Variance / sum(temp_data$Variance) * 100
    
    panel_a <- ggplot(temp_data, aes(x = "", y = Percentage, fill = Component)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("#E69F00", "#999999")) +
      labs(
        title = "Temporal Variance (SIM4: Chronospecies)",
        fill = "Variance Component"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      ) +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5)
  } else {
    panel_a <- ggplot() + labs(title = "Temporal data not available")
  }
  
  # Extract geographic results (SIM5)
  geographic_results <- all_results$SIM5$geographic
  
  if (!is.null(geographic_results) && "M1_BL" %in% names(geographic_results)) {
    
    geo_var <- geographic_results$M1_BL$variance_components
    
    # Panel B: Geographic variance partitioning
    geo_data <- data.frame(
      Component = c("Geographic\n(among regions)", "Residual\n(within regions)"),
      Variance = c(
        geo_var$vcov[geo_var$grp == "region"],
        geo_var$vcov[geo_var$grp == "Residual"]
      )
    )
    geo_data$Percentage <- geo_data$Variance / sum(geo_data$Variance) * 100
    
    panel_b <- ggplot(geo_data, aes(x = "", y = Percentage, fill = Component)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("#56B4E9", "#999999")) +
      labs(
        title = "Geographic Variance (SIM5: Spatial Structure)",
        fill = "Variance Component"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      ) +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5)
  } else {
    panel_b <- ggplot() + labs(title = "Geographic data not available")
  }
  
  # Combine
  fig3 <- plot_grid(
    panel_a, panel_b,
    ncol = 2,
    labels = c("A", "B"),
    label_size = 14
  )
  
  # Save
  ggsave(
    "figures/Figure3_Variance_Partitioning.pdf",
    fig3,
    width = 10,
    height = 5,
    units = "in"
  )
  
  ggsave(
    "figures/Figure3_Variance_Partitioning.png",
    fig3,
    width = 10,
    height = 5,
    units = "in",
    dpi = 300
  )
  
  cat("Figure 3 saved.\n\n")
  
  return(fig3)
}

# ----------------------------------------------------------------------------
# FIGURE 4: Sensitivity Analyses (3-panel)
# ----------------------------------------------------------------------------

create_figure_4 <- function() {
  
  cat("Creating Figure 4: Sensitivity Analyses...\n")
  
  # Panel A: Sample size
  sample_size_summary <- sensitivity_results$sample_size %>%
    group_by(sample_size) %>%
    summarise(
      mean_accuracy = mean(accuracy) * 100,
      sd_accuracy = sd(accuracy) * 100,
      se_accuracy = sd_accuracy / sqrt(n())
    )
  
  panel_a <- ggplot(sample_size_summary, 
                    aes(x = sample_size, y = mean_accuracy)) +
    geom_line(linewidth = 1.2, color = "#E69F00") +
    geom_point(size = 3, color = "#E69F00") +
    geom_ribbon(aes(ymin = mean_accuracy - se_accuracy,
                    ymax = mean_accuracy + se_accuracy),
                alpha = 0.2, fill = "#E69F00") +
    geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
    labs(
      title = "Sample Size Effect",
      x = "Sample Size (per taxon)",
      y = "Classification Accuracy (%)"
    ) +
    annotate("text", x = max(sample_size_summary$sample_size), y = 82,
             label = "80% threshold", color = "red", hjust = 1, size = 3)
  
  # Panel B: Missing data
  missing_data_summary <- sensitivity_results$missing_data %>%
    group_by(pct_missing) %>%
    summarise(
      mean_accuracy = mean(accuracy) * 100,
      sd_accuracy = sd(accuracy) * 100,
      se_accuracy = sd_accuracy / sqrt(n())
    )
  
  panel_b <- ggplot(missing_data_summary,
                    aes(x = pct_missing, y = mean_accuracy)) +
    geom_line(linewidth = 1.2, color = "#56B4E9") +
    geom_point(size = 3, color = "#56B4E9") +
    geom_ribbon(aes(ymin = mean_accuracy - se_accuracy,
                    ymax = mean_accuracy + se_accuracy),
                alpha = 0.2, fill = "#56B4E9") +
    geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
    labs(
      title = "Missing Data Effect",
      x = "Percentage Missing Data",
      y = "Classification Accuracy (%)"
    )
  
  # Panel C: Measurement error
  error_summary <- sensitivity_results$measurement_error %>%
    group_by(error_sd) %>%
    summarise(
      mean_accuracy = mean(accuracy) * 100,
      sd_accuracy = sd(accuracy) * 100,
      se_accuracy = sd_accuracy / sqrt(n())
    )
  
  panel_c <- ggplot(error_summary,
                    aes(x = error_sd, y = mean_accuracy)) +
    geom_line(linewidth = 1.2, color = "#009E73") +
    geom_point(size = 3, color = "#009E73") +
    geom_ribbon(aes(ymin = mean_accuracy - se_accuracy,
                    ymax = mean_accuracy + se_accuracy),
                alpha = 0.2, fill = "#009E73") +
    geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
    labs(
      title = "Measurement Error Effect",
      x = "Measurement Error SD (mm)",
      y = "Classification Accuracy (%)"
    ) +
    annotate("text", x = 0.5, y = 50,
             label = "Typical inter-observer\nerror ≈ 0.5mm",
             size = 3, hjust = 0)
  
  # Combine
  fig4 <- plot_grid(
    panel_a, panel_b, panel_c,
    ncol = 3,
    labels = c("A", "B", "C"),
    label_size = 14
  )
  
  # Save
  ggsave(
    "figures/Figure4_Sensitivity_Analyses.pdf",
    fig4,
    width = 15,
    height = 5,
    units = "in"
  )
  
  ggsave(
    "figures/Figure4_Sensitivity_Analyses.png",
    fig4,
    width = 15,
    height = 5,
    units = "in",
    dpi = 300
  )
  
  cat("Figure 4 saved.\n\n")
  
  return(fig4)
}

# ----------------------------------------------------------------------------
# SUPPLEMENTARY FIGURE: Silhouette Plots
# ----------------------------------------------------------------------------

create_supplementary_figure_S1 <- function() {
  
  cat("Creating Supplementary Figure S1: Silhouette Plots...\n")
  
  library(cluster)
  
  plot_list <- list()
  
  for (sim_name in c("SIM1", "SIM2", "SIM3")) {
    
    # Get clustering results
    clustering <- all_results[[sim_name]]$clustering
    
    if (clustering$optimal_k > 1) {
      
      # Calculate distance matrix
      sim_data <- get(tolower(sim_name))
      measurements <- sim_data[, c("M1_BL", "M1_MD", "M2_BL", "M2_MD", "P4_BL")]
      dist_mat <- dist(measurements)
      
      # Silhouette
      sil <- silhouette(clustering$classification, dist_mat)
      
      # Convert to data frame for ggplot
      sil_df <- data.frame(
        cluster = sil[, 1],
        neighbor = sil[, 2],
        sil_width = sil[, 3]
      )
      sil_df$cluster <- factor(sil_df$cluster)
      
      # Plot
      p <- ggplot(sil_df, aes(x = reorder(1:nrow(sil_df), sil_width),
                              y = sil_width, fill = cluster)) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_d(option = "D", end = 0.9) +
        labs(
          title = paste0(sim_name, " (Avg Silhouette = ", 
                         round(clustering$silhouette, 2), ")"),
          x = "Specimen",
          y = "Silhouette Width"
        ) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom"
        )
      
      plot_list[[sim_name]] <- p
    }
  }
  
  # Combine
  fig_s1 <- plot_grid(
    plotlist = plot_list,
    ncol = 3,
    labels = c("A", "B", "C"),
    label_size = 14
  )
  
  # Save
  ggsave(
    "figures/FigureS1_Silhouette_Plots.pdf",
    fig_s1,
    width = 15,
    height = 5,
    units = "in"
  )
  
  cat("Supplementary Figure S1 saved.\n\n")
  
  return(fig_s1)
}

# ----------------------------------------------------------------------------
# Generate all figures
# ----------------------------------------------------------------------------

cat("\n==========================================\n")
cat("GENERATING ALL FIGURES\n")
cat("==========================================\n\n")

figure1 <- create_figure_1()
figure2 <- create_figure_2()
figure3 <- create_figure_3()
figure4 <- create_figure_4()
figureS1 <- create_supplementary_figure_S1()

cat("\n=== ALL FIGURES GENERATED ===\n")
cat("Figures saved in ./figures/ directory:\n")
cat("  - Figure 1: PCA Morphospace (5-panel)\n")
cat("  - Figure 2: Classification Performance\n")
cat("  - Figure 3: Variance Partitioning\n")
cat("  - Figure 4: Sensitivity Analyses\n")
cat("  - Figure S1: Silhouette Plots\n")