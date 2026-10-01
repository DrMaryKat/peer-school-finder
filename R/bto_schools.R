# =============================================================================
# Beating the Odds (BTO) with PUBLIC school-level data
# Dr. Mary K Boudreaux, EdD | SCSU Educational Leadership & Policy Studies (EDL)
#
# Adapted from: Butler, A., & Poquette, H. (2020). Beating the odds: Implementing
# a BTO analysis. Strategic Data Project. https://github.com/drbtlr/beating-the-odds
# The original guide needs student-level records. This version uses the
# school-level files states publish (one row per school, or per school-year).
#
# HOW TO USE
#   1. One row per school (single year) or per school per year (multi-year).
#      Multi-year data is strongly preferred: it separates a school's lasting
#      effect from year-to-year noise.
#   2. Edit CONFIG. Predictors should describe WHO the school serves (things the
#      school does not control), not WHAT it does (attendance, staffing, climate).
#   3. Run from the repository root. Results go to output/.
# Packages: dplyr, readr, ggplot2, lme4
# =============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(lme4)

# ----------------------------- CONFIG ----------------------------------------
data_path    <- "data/ky_sample.csv"
id_col       <- "state_sch_id"
name_col     <- "sch_name"
year_col     <- NA                     # e.g. "school_year"; NA for one year
outcome_col  <- "prof_rd"              # school outcome (e.g. percent proficient)
predictors   <- c("stn_frpl_pct", "stn_ell_pct", "stn_iep_pct",
                  "stn_homeless_pct", "stn_migrant_pct", "stn_membership")
race_col     <- "stn_white_pct"        # set NA to leave race out of expectations
weight_col   <- "stn_membership"       # larger schools give more precise rates; NA for none
run_both_race_specs <- TRUE            # report how flags change with and without race
# -----------------------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
to_num <- function(x) suppressWarnings(as.numeric(gsub("[,%$]", "", x)))
src <- read_csv(data_path, col_types = cols(.default = col_character()))
num_cols <- unique(na.omit(c(outcome_col, predictors, race_col, weight_col)))
src <- src %>% mutate(across(all_of(num_cols), to_num))

multi_year <- !is.na(year_col)
if (multi_year) src[[year_col]] <- factor(src[[year_col]])

# Listwise deletion is reported, not hidden: suppressed outcomes are not random
needed <- c(outcome_col, predictors, if (!is.na(race_col)) race_col)
dat <- src %>% filter(if_all(all_of(needed), ~ !is.na(.)))
cat("Rows:", nrow(src), "| used:", nrow(dat), "| dropped (missing/suppressed):", nrow(src) - nrow(dat), "\n")
write_csv(src %>% filter(!if_all(all_of(needed), ~ !is.na(.))) %>% select(all_of(c(id_col, name_col))),
          "output/bto_dropped_schools.csv")

# Standardize predictors so coefficients are comparable (1 SD change)
z <- function(x) (x - mean(x)) / sd(x)
dat <- dat %>% mutate(across(all_of(c(predictors, if (!is.na(race_col)) race_col)), z, .names = "z_{.col}"))

fit_bto <- function(dat, use_race) {
  rhs <- paste0("z_", c(predictors, if (use_race && !is.na(race_col)) race_col))
  w   <- if (!is.na(weight_col)) dat[[weight_col]] / mean(dat[[weight_col]]) else NULL
  if (multi_year) {
    # School-year rows nested in schools: the random intercept is the school's
    # lasting effect, shrunk toward zero when evidence is thin
    f <- reformulate(c(rhs, year_col, paste0("(1|", id_col, ")")), response = outcome_col)
    m <- lmer(f, data = dat, weights = w)
    re <- ranef(m, condVar = TRUE)[[id_col]]
    eff <- tibble(!!id_col := rownames(re), effect = re[, 1],
                  se = sqrt(as.numeric(attr(re, "postVar"))))
  } else {
    # One year: the residual mixes real school effects with noise
    f <- reformulate(rhs, response = outcome_col)
    m <- lm(f, data = dat, weights = w)
    eff <- tibble(!!id_col := dat[[id_col]], effect = residuals(m),
                  se = sigma(m) * sqrt(1 - hatvalues(m)) / if (is.null(w)) 1 else sqrt(w))
  }
  # Expected rank (accounts for both size and uncertainty of each effect)
  n <- nrow(eff)
  eff$pct_er <- sapply(seq_len(n), function(i)
    100 * (sum(pnorm((eff$effect[i] - eff$effect[-i]) / sqrt(eff$se[i]^2 + eff$se[-i]^2))) + 0.5) / n)
  eff %>% mutate(
    lower = effect - 1.96 * se, upper = effect + 1.96 * se,
    status = case_when(lower > 0 ~ "Above expected", upper < 0 ~ "Below expected", TRUE ~ "As expected")
  ) %>% list(model = m, eff = .)
}

main <- fit_bto(dat, use_race = !is.na(race_col))
print(summary(main$model)$coefficients)

school_info <- dat %>% group_by(.data[[id_col]]) %>%
  summarise(school_name = first(.data[[name_col]]),
            observed = mean(.data[[outcome_col]]), .groups = "drop")
results <- main$eff %>% left_join(school_info, by = id_col) %>%
  relocate(all_of(id_col), school_name, observed) %>% arrange(desc(effect))
write_csv(results, "output/bto_results.csv")
print(count(results, status))

# Sensitivity: do flags depend on whether race sets expectations?
if (run_both_race_specs && !is.na(race_col)) {
  alt <- fit_bto(dat, use_race = FALSE)$eff %>% select(all_of(id_col), status_no_race = status)
  comp <- results %>% select(all_of(id_col), school_name, status_with_race = status) %>% left_join(alt, by = id_col)
  write_csv(comp, "output/bto_race_sensitivity.csv")
  cat("\nSchools whose status changes when race is removed:",
      sum(comp$status_with_race != comp$status_no_race), "\n")
  print(table(with_race = comp$status_with_race, without_race = comp$status_no_race))
}

# Plot: each school's effect with its 95% interval
results %>% mutate(rank = rank(effect)) %>%
  ggplot(aes(rank, effect, color = status)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(ymin = lower, ymax = upper), alpha = 0.35) +
  geom_point(size = 1.2) +
  scale_color_manual(values = c("Above expected" = "#14746F", "Below expected" = "#B0832E",
                                "As expected" = "grey70")) +
  labs(x = "Schools, ordered by effect", y = paste("Observed minus expected", outcome_col),
       color = NULL, title = "Schools performing above or below expectations",
       subtitle = "Flagged only when the 95% interval excludes zero") +
  theme_minimal() + theme(legend.position = "top", panel.grid.minor = element_blank(),
                          plot.title.position = "plot")
ggsave("output/bto_plot.png", width = 8, height = 5, dpi = 200)
