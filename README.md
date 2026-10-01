# Peer School Finder

Find the schools most similar to any school using public data, then benchmark outcomes against that peer set. Peers are matched on context only (enrollment, student needs, staffing, resources); outcomes are never used for matching.

Two ways to use it:

- **In the browser:** open `docs/index.html` (or the GitHub Pages site for this repository), upload a CSV, match your columns, and pick a school. No installation needed. Good for exploration and teaching.
- **In R:** run `R/peer_schools.R` for a documented, reproducible analysis with saved outputs for a methods section. Use this for anything you publish.

Both use the same method and produce the same peers on the same data (median imputation).

## Preparing data from a new district or state

1. One row per school. Start from `data/peer_finder_template.csv`.
2. Required: a unique school ID and a school name.
3. Context variables must be numeric. Recode categories as 0/1 indicators (for example, `title1_schoolwide`). Proportions (0 to 1) and percents (0 to 100) both work, since every variable is standardized.
4. Outcome columns (proficiency, graduation) can be included; mark them as outcomes so they are excluded from matching.
5. Leave suppressed values blank. Columns missing more than 20 percent are dropped by default.
6. Include a level or type column if the file mixes elementary, middle, and high schools; comparisons are then made within level.

Useful public sources: CT EdSight, state report cards, NCES Common Core of Data, the Civil Rights Data Collection, and the Urban Institute Education Data Portal (`educationdata` R package).

## Running the R template

1. Put your CSV in `data/`.
2. Edit the CONFIG block at the top of `R/peer_schools.R`.
3. Run the script from the repository root.

Outputs in `output/`:

| File | Use |
|---|---|
| `missing_data_report.csv` | Missingness by variable (report in methods) |
| `variance_explained.csv` | Scree data and variance kept |
| `loadings.csv` | What each dimension represents |
| `silhouette_by_k.csv` | Whether natural clusters exist (low values mean a continuum) |
| `peer_lists_all_schools.csv` | Every school's peer set |
| `target_peers.csv`, `benchmark_plot.png` | The target school's comparison |

## Method

Variables are standardized, reduced with principal component analysis (keeping enough dimensions to explain 80 percent of the variance by default), and each school's peers are its nearest neighbors in that space. This replaces fixed k-means groups, which perform poorly when schools form a continuum rather than distinct clusters. Record every variable inclusion and exclusion in `docs/codebook.md`.

## Beating the odds (R/bto_schools.R)

Identifies schools performing above or below what their student population predicts, using public school-level data. Adapted from Butler and Poquette (2020), which requires student-level records.

- Use multi-year data (one row per school per year) whenever possible; set `year_col` in CONFIG. Single-year results mix real school effects with noise.
- Predictors should describe who a school serves (poverty, English learners, disabilities, mobility), not what it does (attendance, staffing, climate).
- A school is flagged only when its 95 percent interval excludes zero. Expected-rank percentiles are reported for ranking, not for flagging.
- The script reruns the model with and without race and reports which schools change status (`output/bto_race_sensitivity.csv`).
- Pair results with the peer finder: for each school flagged above expected, its peer list identifies comparison schools for case study selection.

Butler, A., & Poquette, H. (2020, December 8). *Beating the odds: Implementing a BTO analysis*. Strategic Data Project, Center for Education Policy Research at Harvard University. https://github.com/drbtlr/beating-the-odds

## Responsible use

Peer sets reflect only the variables chosen. Including demographic variables (such as race) changes who counts as a peer and can normalize lower expectations; run the analysis with and without them and report the difference. Check every peer list against local knowledge: magnet and selective-admission schools can look similar on paper.

## Citation

Method adapted from:

Butler, A. (2020, April 7). *Identifying similar schools: Guide to creating comparison school groups*. Strategic Data Project, Center for Education Policy Research at Harvard University. https://drbtlr.github.io/similar-schools/similar_schools_code.html

Sample data: Kentucky School Report Card, 2018/19 (elementary schools), as distributed with Butler (2020); `title1_status` recoded to `title1_schoolwide` and `student_teacher_ratio` inverted to students per teacher.

To cite this repository, see `CITATION.cff`.
