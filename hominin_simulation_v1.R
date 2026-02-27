# ============================================================
# Hominin Alpha Taxonomy Simulation
# Sequential vs. Simultaneous Species Delimitation
# Based on de Queiroz's General Lineage Concept
# ============================================================
# Required packages: phytools, geomorph, mclust, MASS, mvtnorm
# Install if needed:
# install.packages(c("phytools", "geomorph", "mclust", "MASS", "mvtnorm", "ape"))
# ============================================================

library(phytools)
library(ape)
library(MASS)
library(mclust)
library(mvtnorm)

set.seed(42)  # For reproducibility

# ============================================================
# SECTION 1: SIMULATE A HOMININ PHYLOGENY (3 taxa)
# ============================================================
# We manually specify a plausible Australopithecus topology:
#   ((Au. afarensis, Au. africanus), P. robustus)
# Branch lengths are in millions of years (approximate).

hominin_tree <- read.tree(text = "(Au_robustus:1.5,(Au_afarensis:1.0,Au_africanus:0.8):0.5);")
hominin_tree$tip.label <- c("Au. robustus", "Au. afarensis", "Au. africanus")

cat("=== Hominin Phylogeny ===\n")
print(hominin_tree)
plot(hominin_tree, main = "Simulated Hominin Phylogeny", edge.width = 2, cex = 1.2)
axisPhylo()

# ============================================================
# SECTION 2: SIMULATE MORPHOLOGICAL TRAIT EVOLUTION
# ============================================================
# Simulate 5 continuous traits (e.g., dental/cranial measurements)
# under Brownian motion along the phylogeny using phytools.
# Higher sigma2 = faster morphological divergence.

n_traits <- 5
trait_names <- c("M1_length", "M2_width", "cranial_capacity",
                 "facial_prognathism", "palate_breadth")

# Simulate species-level means via Brownian motion
# Returns a matrix: rows = taxa, cols = traits
species_means <- matrix(NA, nrow = Ntip(hominin_tree), ncol = n_traits,
                        dimnames = list(hominin_tree$tip.label, trait_names))

for (i in seq_len(n_traits)) {
  sim_result <- fastBM(hominin_tree, sig2 = 0.5, nsim = 1)
  species_means[, i] <- sim_result[hominin_tree$tip.label]
}

cat("\n=== Simulated Species Trait Means ===\n")
print(round(species_means, 3))

# ============================================================
# SECTION 3: SAMPLE FOSSIL "SPECIMENS" FROM SPECIES
# ============================================================
# Layer population-level variation on top of species means.
# Includes:
#   - Within-species variance (Sigma_w)
#   - Sexual dimorphism (bimodal component, ~30% of variance)
#   - Realistic, unequal sample sizes per taxon

# Within-species covariance: modest correlation among traits
Sigma_w <- matrix(0.15, n_traits, n_traits)
diag(Sigma_w) <- 0.4   # Higher variance on diagonal

# Sexual dimorphism offset (males slightly larger)
dimorphism_offset <- c(0.3, 0.2, 0.4, 0.1, 0.2)

# Fossil sample sizes per taxon (unequal, as in real record)
sample_sizes <- c("Au. robustus" = 8, "Au. afarensis" = 15, "Au. africanus" = 10)

specimen_list <- list()

for (taxon in hominin_tree$tip.label) {
  n <- sample_sizes[taxon]
  mu <- species_means[taxon, ]

  # Assign sex randomly (~50/50)
  sex <- sample(c("M", "F"), n, replace = TRUE)

  # Generate trait values with dimorphism
  specimens <- t(sapply(sex, function(s) {
    offset <- if (s == "M") dimorphism_offset else rep(0, n_traits)
    rmvnorm(1, mean = mu + offset, sigma = Sigma_w)
  }))

  colnames(specimens) <- trait_names
  specimen_list[[taxon]] <- data.frame(
    taxon    = taxon,
    sex      = sex,
    specimens,
    check.names = FALSE
  )
}

all_specimens <- do.call(rbind, specimen_list)
rownames(all_specimens) <- NULL

cat("\n=== Specimen Data (first 6 rows) ===\n")
print(head(all_specimens))
cat(sprintf("\nTotal specimens simulated: %d\n", nrow(all_specimens)))

# ============================================================
# SECTION 4: RANDOMIZE DISCOVERY ORDER
# ============================================================
# Simulate contingent fossil discovery by randomly shuffling
# the specimen order (mimicking field discovery history).

n_orderings <- 5  # Number of random discovery orders to test

cat("\n=== Discovery Orders Generated:", n_orderings, "===\n")

discovery_orders <- lapply(seq_len(n_orderings), function(i) {
  sample(nrow(all_specimens))
})

# ============================================================
# SECTION 5: SEQUENTIAL CLASSIFICATION PROTOCOL
# ============================================================
# Mimics real paleoanthropological practice:
#   - Start with first k specimens → form initial hypodigms via LDA
#   - Add one specimen at a time
#   - Assign to nearest existing group centroid or erect new group
#     if Mahalanobis distance exceeds threshold (new_group_threshold)

sequential_classify <- function(specimens, discovery_order,
                                 seed_size = 6,
                                 new_group_threshold = 3.5) {

  trait_cols <- trait_names
  n <- nrow(specimens)
  assignments <- rep(NA, n)
  ordered <- specimens[discovery_order, ]

  # --- Seed phase: cluster initial specimens unsupervised ---
  seed_data <- as.matrix(ordered[seq_len(seed_size), trait_cols])
  seed_clust <- kmeans(seed_data, centers = min(3, seed_size), nstart = 10)
  assignments[seq_len(seed_size)] <- seed_clust$cluster

  # Compute group centroids from seed
  n_groups <- max(seed_clust$cluster)
  group_centroids <- lapply(seq_len(n_groups), function(g) {
    colMeans(seed_data[seed_clust$cluster == g, , drop = FALSE])
  })

  # --- Sequential assignment phase ---
  for (i in (seed_size + 1):n) {
    specimen <- as.numeric(ordered[i, trait_cols])

    # Mahalanobis distance to each group centroid
    dists <- sapply(group_centroids, function(centroid) {
      sqrt(sum((specimen - centroid)^2))  # Euclidean as proxy
    })

    if (min(dists) > new_group_threshold) {
      # Erect new group
      n_groups <- n_groups + 1
      assignments[i] <- n_groups
      group_centroids[[n_groups]] <- specimen
    } else {
      # Assign to nearest group and update centroid
      best <- which.min(dists)
      assignments[i] <- best
      members <- which(assignments[seq_len(i)] == best)
      group_centroids[[best]] <- colMeans(
        as.matrix(ordered[members, trait_cols, drop = FALSE])
      )
    }
  }

  # Map back to original row indices
  result <- rep(NA, n)
  result[discovery_order] <- assignments
  result
}

# ============================================================
# SECTION 6: SIMULTANEOUS CLASSIFICATION PROTOCOL
# ============================================================
# De novo approach: treat all specimens at once using
# Gaussian Mixture Model clustering (mclust).
# Let the data determine the number of species.

simultaneous_classify <- function(specimens, max_groups = 6) {
  trait_data <- as.matrix(specimens[, trait_names])
  fit <- Mclust(trait_data, G = 2:max_groups, verbose = FALSE)
  fit$classification
}

# ============================================================
# RUN SIMULATION & EVALUATE PERFORMANCE
# ============================================================

# True labels (numeric)
true_labels <- as.integer(factor(all_specimens$taxon,
                                  levels = hominin_tree$tip.label))

evaluate <- function(predicted, true) {
  # Adjusted Rand Index via mclust
  adjustedRandIndex(predicted, true)
}

cat("\n=== Running Simulation ===\n\n")

# Simultaneous result (one run, not order-dependent)
simult_assignments <- simultaneous_classify(all_specimens)
simult_ari         <- evaluate(simult_assignments, true_labels)
simult_n_groups    <- length(unique(simult_assignments))

cat(sprintf("SIMULTANEOUS | Groups detected: %d | ARI: %.3f\n",
            simult_n_groups, simult_ari))

# Sequential results across discovery orders
seq_results <- lapply(seq_along(discovery_orders), function(i) {
  ord   <- discovery_orders[[i]]
  assn  <- sequential_classify(all_specimens, ord)
  ari   <- evaluate(assn, true_labels)
  n_grp <- length(unique(assn[!is.na(assn)]))
  list(ordering = i, n_groups = n_grp, ari = ari, assignments = assn)
})

cat("\nSEQUENTIAL results across discovery orderings:\n")
for (res in seq_results) {
  cat(sprintf("  Order %d | Groups detected: %d | ARI: %.3f\n",
              res$ordering, res$n_groups, res$ari))
}

# Summary comparison
seq_aris <- sapply(seq_results, `[[`, "ari")
cat(sprintf("\nSEQUENTIAL mean ARI: %.3f (SD: %.3f)\n",
            mean(seq_aris), sd(seq_aris)))
cat(sprintf("SIMULTANEOUS ARI:    %.3f\n", simult_ari))

# ============================================================
# VISUALIZATION
# ============================================================

# PCA of morphospace for visualization
pca <- prcomp(all_specimens[, trait_names], scale. = TRUE)
pc_scores <- as.data.frame(pca$x[, 1:2])
pc_scores$true_taxon   <- all_specimens$taxon
pc_scores$simult_group <- factor(simult_assignments)
pc_scores$seq_group    <- factor(seq_results[[1]]$assignments)

# Color palette
taxon_cols <- c("Au. robustus"  = "#E74C3C",
                "Au. afarensis" = "#3498DB",
                "Au. africanus" = "#2ECC71")

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# Panel 1: True species
plot(pc_scores$PC1, pc_scores$PC2,
     col  = taxon_cols[pc_scores$true_taxon],
     pch  = 19, cex = 1.4,
     xlab = "PC1", ylab = "PC2",
     main = "TRUE SPECIES")
legend("topright", legend = names(taxon_cols),
       col = taxon_cols, pch = 19, cex = 0.75)

# Panel 2: Simultaneous (mclust)
plot(pc_scores$PC1, pc_scores$PC2,
     col  = as.integer(pc_scores$simult_group) + 1,
     pch  = 19, cex = 1.4,
     xlab = "PC1", ylab = "PC2",
     main = sprintf("SIMULTANEOUS\n(n groups = %d, ARI = %.2f)",
                    simult_n_groups, simult_ari))

# Panel 3: Sequential (first discovery order)
plot(pc_scores$PC1, pc_scores$PC2,
     col  = as.integer(pc_scores$seq_group) + 1,
     pch  = 19, cex = 1.4,
     xlab = "PC1", ylab = "PC2",
     main = sprintf("SEQUENTIAL (Order 1)\n(n groups = %d, ARI = %.2f)",
                    seq_results[[1]]$n_groups, seq_results[[1]]$ari))

cat("\n=== Simulation complete ===\n")
cat("Plots rendered. Adjust seed, sigma2, sample_sizes, or\n")
cat("new_group_threshold to explore parameter sensitivity.\n")
