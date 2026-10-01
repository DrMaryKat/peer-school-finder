# =============================================================================
# Peer School Finder: reproducible R template
# Dr. Mary K Boudreaux, EdD | SCSU Educational Leadership & Policy Studies (EDL)
#
# Method adapted from: Butler, A. (2020). Identifying similar schools: Guide to
# creating comparison school groups. Strategic Data Project.
# https://github.com/drbtlr/similar-schools
#
# HOW TO USE WITH A NEW DISTRICT OR STATE
#   1. Put your file in data/ (one row per school; see data/peer_finder_template.csv)
#   2. Edit the CONFIG block below to match your column names
#   3. Run the whole script from the repository root; results go to output/
# Packages: dplyr, readr, tidyr, ggplot2, cluster
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(cluster)

# ----------------------------- CONFIG ----------------------------------------
data_path    <- "data/ky_sample.csv"        # your CSV
id_col       <- "state_sch_id"              # unique school ID
name_col     <- "sch_name"                  # school name
group_col    <- "level"                     # school level/type, or NA to skip
outcome_cols <- c("prof_rd", "prof_ma")     # outcomes: never used for matching
exclude_cols <- c()                         # context columns to leave out on purpose
target_id    <- "034165052"                 # school of interest
n_peers      <- 15                          # peer set size
var_goal     <- 0.80                        # variance the kept dimensions must explain
max_missing  <- 0.20                        # drop context columns missing more than this
impute       <- "median"                    # "median" or "regression"
# -----------------------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
src <- read_csv(data_path, col_types = cols(.default = col_character()))
stopifnot(id_col %in% names(src), name_col %in% names(src), !anyDuplicated(src[[id_col]]))

to_num <- function(x) suppressWarnings(as.numeric(gsub("[,%$]", "", x)))
num_ok <- sapply(src, function(x) {
  v <- x[!is.na(x) & x != ""]
  length(v) > 0 && mean(!is.na(to_num(v))) >= 0.8
})
context_cols <- setdiff(names(src)[num_ok], c(id_col, name_col, group_col, outcome_cols, exclude_cols))
src <- src %>% mutate(across(all_of(c(context_cols, intersect(outcome_cols, names(src)))), to_num))

# Stratify: compare only schools in the target's group
if (!is.na(group_col)) {
  tgt_group <- src[[group_col]][src[[id_col]] == target_id]
  src <- src %>% filter(.data[[group_col]] == tgt_group)
}

# Missing-data report (keep for the methods section)
miss <- src %>%
  summarise(across(all_of(context_cols), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))
write_csv(miss, "output/missing_data_report.csv")

keep <- miss %>% filter(pct_missing <= max_missing) %>% pull(variable)
keep <- keep[sapply(keep, function(v) isTRUE(sd(src[[v]], na.rm = TRUE) > 0))]
cat("Context variables used:", length(keep), "\nDropped:", setdiff(context_cols, keep), "\n")

# Imputation
fill_median <- function(x) { x[is.na(x)] <- median(x, na.rm = TRUE); x }
X <- src %>% select(all_of(keep)) %>% mutate(across(everything(), fill_median))
if (impute == "regression") {
  for (v in keep[colSums(is.na(src[keep])) > 0]) {
    preds <- setdiff(keep, v)
    dat   <- cbind(X[preds], setNames(src[v], v))
    fit   <- lm(reformulate(preds, response = v), data = dat)
    i     <- is.na(src[[v]])
    X[[v]][i] <- predict(fit, newdata = X[i, preds])
  }
}

# PCA
pca     <- prcomp(X, center = TRUE, scale. = TRUE)
var_exp <- pca$sdev^2 / sum(pca$sdev^2)
n_pc    <- max(2, which(cumsum(var_exp) >= var_goal)[1])
scores  <- pca$x[, 1:n_pc, drop = FALSE]
cat("Schools:", nrow(X), "| Dimensions kept:", n_pc,
    "| Variance kept:", round(sum(var_exp[1:n_pc]), 3), "\n")
if (nrow(X) < 2 * length(keep)) warning("Fewer than 2 schools per variable; trim the variable list.")

write_csv(tibble(component = seq_along(var_exp), var_explained = var_exp,
                 cumulative = cumsum(var_exp)), "output/variance_explained.csv")
write_csv(as_tibble(pca$rotation[, 1:n_pc], rownames = "variable"), "output/loadings.csv")

# Cluster-structure check: low silhouette means a continuum, so prefer peer sets
ks  <- 2:min(15, nrow(X) - 1)
sil <- sapply(ks, function(k) {
  set.seed(2468)
  km <- kmeans(scores, k, nstart = 25, iter.max = 100)
  mean(silhouette(km$cluster, dist(scores))[, 3])
})
write_csv(tibble(k = ks, avg_silhouette = sil), "output/silhouette_by_k.csv")

# Nearest-neighbor peers for every school
D <- as.matrix(dist(scores))
all_peers <- bind_rows(lapply(seq_len(nrow(src)), function(i) {
  idx <- order(D[i, ])[2:(n_peers + 1)]
  tibble(school_id = src[[id_col]][i], school_name = src[[name_col]][i],
         peer_rank = seq_along(idx), peer_id = src[[id_col]][idx],
         peer_name = src[[name_col]][idx], distance = D[i, idx])
}))
write_csv(all_peers, "output/peer_lists_all_schools.csv")

# Target benchmark
t   <- which(src[[id_col]] == target_id)
idx <- order(D[t, ])[2:(n_peers + 1)]
bench <- src[c(t, idx), c(id_col, name_col, intersect(outcome_cols, names(src)))] %>%
  mutate(role = c("Target", rep("Peer", length(idx))), distance = D[t, c(t, idx)])
write_csv(bench, "output/target_peers.csv")
print(bench, n = Inf)

if (length(outcome_cols)) {
  o  <- outcome_cols[1]
  pb <- bench %>% filter(!is.na(.data[[o]]))
  ggplot(pb, aes(reorder(.data[[name_col]], .data[[o]]), .data[[o]], fill = role)) +
    geom_col() +
    geom_hline(yintercept = median(pb[[o]][pb$role == "Peer"]),
               linetype = "dashed", color = "#B0832E") +
    scale_fill_manual(values = c(Target = "#14746F", Peer = "#D9D4C7")) +
    coord_flip() +
    labs(x = NULL, y = o,
         title = paste(src[[name_col]][t], "and its", n_peers, "nearest peers"),
         subtitle = "Dashed line: peer median. Peers matched on context variables only.") +
    theme_minimal() +
    theme(legend.position = "none", plot.title.position = "plot",
          panel.grid.major.y = element_blank())
  ggsave("output/benchmark_plot.png", width = 8, height = 5.5, dpi = 200)
}
