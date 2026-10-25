#!/usr/bin/env Rscript
#
# Distribution of Self-Reported Racial Identity by Generation and Ancestry
# Date: October 25th, 2026
#
# Rebuilds the two self-identification tables (children: Table 4,
# tab:asianbygen; adults: Table 5, tab:asianbygen-adults) in the form
# Reviewer 2 asked for at JPAM-2025-13688.R2 (comment 2): one observation
# count per row and the distribution across the identity categories used by
# the multinomial logit (Asian only, White only, Asian and White), plus a
# residual "Other" column, instead of a binary Asian / not-only-Asian split.
# Rows follow the subsamples the paper estimates on: parental configuration
# for the second generation and the number of Asian-born grandparents for
# the third.
#
# Category definitions match script 44: CPS race code 651 is Asian only, 100
# is White only, 803/804/809/813 are Asian-White combinations, everything
# else (including 652, Asian or Pacific Islander) is Other.
#
# Supersedes the table output of scripts 05 and 36, which are left in place.
# Must not: change samples or weights (counts are unweighted, as before).
# Runner: programs/95-make-all.R (listed, commented like its neighbours).
#
# Results -> output/tables/tab02-observations-by-gen.tex
#         -> output/tables/tab02-observations-by-gen.csv
#         -> output/tables/tab54-observations-by-gen-adults.tex
#         -> output/tables/tab54-observations-by-gen-adults.csv

suppressPackageStartupMessages({
  library(data.table)
})

# ---- Paths ----------------------------------------------------------------
resolve_root <- function() {
  env_root <- Sys.getenv("ASIAN_IDENTITY_ROOT")
  if (nzchar(env_root)) return(normalizePath(env_root, mustWork = TRUE))
  if (exists("git_mdir", inherits = TRUE)) {
    return(normalizePath(get("git_mdir", inherits = TRUE), mustWork = TRUE))
  }
  args <- commandArgs(trailingOnly = TRUE)
  normalizePath(if (length(args) >= 1) args[[1]] else getwd(),
                mustWork = TRUE)
}
root <- resolve_root()
datasets_dir <- file.path(root, "data", "datasets")
tables_dir <- file.path(root, "output", "tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
script_name <- "53-make-identity-distribution-tables.R"
assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

categories <- c("Asian_only", "White_only", "Asian_and_White", "Other")
category_labels <- c(Asian_only = "Asian only", White_only = "White only",
                     Asian_and_White = "Asian and White", Other = "Other")

read_panel <- function(file) {
  path <- file.path(datasets_dir, file)
  assert_true(file.exists(path), sprintf("Missing input: %s", path))
  dat <- fread(path, showProgress = FALSE)
  dat[, category := fifelse(race == 651L, "Asian_only",
                      fifelse(race == 100L, "White_only",
                        fifelse(race %in% c(803L, 804L, 809L, 813L),
                                "Asian_and_White", "Other")))]
  dat
}

# ---- Row definitions ------------------------------------------------------
# Each row: label, indent level, and a logical mask over the panel.
kids <- read_panel("CPS_IAT_asian.csv")
kids[, n_asian_gp := nchar(gsub("[^A]", "", Grandparent_Type))]
kid_rows <- list(
  list(label = "First generation", indent = 0L,
       mask = kids$FirstGen_Asian == 1),
  list(label = "Second generation", indent = 0L,
       mask = kids$SecondGen_Asian == 1),
  list(label = "Both parents Asian-born", indent = 1L,
       mask = kids$SecondGen_Asian == 1 & kids$AA_0bj == 1),
  list(label = "One Asian-born parent", indent = 1L,
       mask = kids$SecondGen_Asian == 1 & (kids$AW_0bj == 1 | kids$WA_0bj == 1)),
  list(label = "Asian father--White mother", indent = 2L,
       mask = kids$SecondGen_Asian == 1 & kids$AW_0bj == 1),
  list(label = "White father--Asian mother", indent = 2L,
       mask = kids$SecondGen_Asian == 1 & kids$WA_0bj == 1),
  list(label = "Third generation", indent = 0L,
       mask = kids$ThirdGen_Asian == 1),
  list(label = "Four Asian grandparents", indent = 1L,
       mask = kids$ThirdGen_Asian == 1 & kids$n_asian_gp == 4L),
  list(label = "Three Asian grandparents", indent = 1L,
       mask = kids$ThirdGen_Asian == 1 & kids$n_asian_gp == 3L),
  list(label = "Two Asian grandparents", indent = 1L,
       mask = kids$ThirdGen_Asian == 1 & kids$n_asian_gp == 2L),
  list(label = "One Asian grandparent", indent = 1L,
       mask = kids$ThirdGen_Asian == 1 & kids$n_asian_gp == 1L)
)

adults <- read_panel("CPS_IAT_asian_adults.csv")
adult_rows <- list(
  list(label = "First generation", indent = 0L,
       mask = adults$FirstGen_Asian == 1),
  list(label = "Second generation", indent = 0L,
       mask = adults$SecondGen_Asian == 1),
  list(label = "Both parents Asian-born", indent = 1L,
       mask = adults$SecondGen_Asian == 1 & adults$AA_0bj == 1),
  list(label = "One Asian-born parent", indent = 1L,
       mask = adults$SecondGen_Asian == 1 &
         (adults$AW_0bj == 1 | adults$WA_0bj == 1)),
  list(label = "Asian father--White mother", indent = 2L,
       mask = adults$SecondGen_Asian == 1 & adults$AW_0bj == 1),
  list(label = "White father--Asian mother", indent = 2L,
       mask = adults$SecondGen_Asian == 1 & adults$WA_0bj == 1)
)

tabulate_rows <- function(dat, rows, sample_label) {
  out <- rbindlist(lapply(rows, function(r) {
    sub <- dat[r$mask]
    n <- nrow(sub)
    assert_true(n > 0, sprintf("Empty row: %s (%s)", r$label, sample_label))
    shares <- vapply(categories, function(ct) mean(sub$category == ct),
                     numeric(1))
    data.table(run_id = run_id, script = script_name, sample = sample_label,
               row_label = r$label, indent = r$indent, n_obs = n,
               share_asian_only = shares[["Asian_only"]],
               share_white_only = shares[["White_only"]],
               share_asian_and_white = shares[["Asian_and_White"]],
               share_other = shares[["Other"]])
  }))
  out
}
kid_tab <- tabulate_rows(kids, kid_rows, "children ages 17 and younger")
adult_tab <- tabulate_rows(adults, adult_rows, "adults ages 18 and older")

# Subgroup rows must partition their generation.
check_partition <- function(tab, parent, children) {
  total <- tab[row_label == parent, n_obs]
  parts <- sum(tab[row_label %in% children, n_obs])
  assert_true(total == parts,
              sprintf("%s rows (%s) do not sum to the generation total (%s)",
                      parent, format(parts, big.mark = ","),
                      format(total, big.mark = ",")))
}
check_partition(kid_tab, "Second generation",
                c("Both parents Asian-born", "One Asian-born parent"))
check_partition(kid_tab, "One Asian-born parent",
                c("Asian father--White mother", "White father--Asian mother"))
check_partition(kid_tab, "Third generation",
                c("Four Asian grandparents", "Three Asian grandparents",
                  "Two Asian grandparents", "One Asian grandparent"))
check_partition(adult_tab, "Second generation",
                c("Both parents Asian-born", "One Asian-born parent"))
check_partition(adult_tab, "One Asian-born parent",
                c("Asian father--White mother", "White father--Asian mother"))

# The text describes the non-Asian-born parent in mixed families as mostly
# US-born; record the share so the number has provenance (CPS BPL 9900 is
# "United States").
mixed_kids <- kids[SecondGen_Asian == 1 & (AW_0bj == 1 | WA_0bj == 1)]
us_born_parent <- fifelse(mixed_kids$AW_0bj == 1, mixed_kids$mbpl == 9900L,
                          mixed_kids$fbpl == 9900L)
us_born_share <- data.table(
  run_id = run_id, script = script_name,
  group = c("AW mothers", "WA fathers", "pooled"),
  share_us_born = c(mean(us_born_parent[mixed_kids$AW_0bj == 1]),
                    mean(us_born_parent[mixed_kids$WA_0bj == 1]),
                    mean(us_born_parent)),
  n_obs = c(sum(mixed_kids$AW_0bj == 1), sum(mixed_kids$WA_0bj == 1),
            nrow(mixed_kids))
)
write.csv(us_born_share,
          file.path(tables_dir, "tab02-mixed-parent-us-born-share.csv"),
          row.names = FALSE)
cat(sprintf("Non-Asian-born parent US-born: AW %.3f, WA %.3f, pooled %.3f\n",
            us_born_share$share_us_born[1], us_born_share$share_us_born[2],
            us_born_share$share_us_born[3]))

write.csv(kid_tab, file.path(tables_dir, "tab02-observations-by-gen.csv"),
          row.names = FALSE)
write.csv(adult_tab,
          file.path(tables_dir, "tab54-observations-by-gen-adults.csv"),
          row.names = FALSE)

# ---- LaTeX ----------------------------------------------------------------
fmt_share <- function(x) formatC(x, format = "f", digits = 2)
fmt_n <- function(n) formatC(n, format = "d", big.mark = ",")
row_tex <- function(tab) {
  vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i]
    label <- if (r$indent == 0L) sprintf("\\textbf{%s}", r$row_label) else
      paste0(strrep("\\hspace{1em}", r$indent), r$row_label)
    sprintf("%s & %s & %s & %s & %s & %s\\\\", label, fmt_n(r$n_obs),
            fmt_share(r$share_asian_only), fmt_share(r$share_white_only),
            fmt_share(r$share_asian_and_white), fmt_share(r$share_other))
  }, character(1))
}
header <- c(
  "\\toprule",
  " & & \\multicolumn{4}{c}{Share self-reporting each identity} \\\\",
  "\\cmidrule(l{3pt}r{3pt}){3-6}",
  " & Observations & Asian only & White only & \\specialcell{Asian and \\\\ White} & Other\\\\",
  "\\midrule"
)
category_note <- paste(
  "Columns report the share of each row's observations in the racial",
  "identity categories used in the multinomial logit: ``Asian only,''",
  "``White only,'' and ``Asian and White'' (any combination of Asian and",
  "White, with or without Pacific Islander). ``Other'' collects Asian or",
  "Pacific Islander, other single races, and multiracial combinations",
  "without both an Asian and a White component; the multinomial logit",
  "excludes this category. Shares sum to one across the four columns.")

make_table <- function(tab, caption, label, sample_note, source_note, file) {
  tex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\small\\setstretch{1.0}",
    sprintf("\\caption{%s \\label{%s}}", caption, label),
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccc}",
    header,
    row_tex(tab),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "\\small",
    sprintf("\\item[1] %s", sample_note),
    sprintf("\\item[2] %s", category_note),
    sprintf("\\item[3] %s", source_note),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  writeLines(tex, file.path(tables_dir, file))
}

make_table(
  kid_tab,
  caption = "Self-Reported Racial Identity of Children of Asian Ancestry by Generation",
  label = "tab:asianbygen",
  sample_note = paste(
    "The samples include children ages 17 and below who live in intact",
    "families. First-generation Asian immigrant children were born in an",
    "Asian country. Native-born second-generation children have at least one",
    "parent born in an Asian country; ``One Asian-born parent'' rows require",
    "the other parent to be born outside Asia (the AW and WA labels follow the paper's convention). Native-born third-generation",
    "children have native-born parents and at least one grandparent born in",
    "an Asian country, grouped by the number of Asian-born grandparents."),
  source_note = "Data source is the 2004--2021 Current Population Survey.",
  file = "tab02-observations-by-gen.tex"
)
make_table(
  adult_tab,
  caption = "Self-Reported Racial Identity of Adults of Asian Ancestry by Generation",
  label = "tab:asianbygen-adults",
  sample_note = paste(
    "The samples include people of Asian ancestry ages 18 and above.",
    "First-generation Asian immigrants were born in an Asian country.",
    "Native-born second-generation Asian Americans have at least one parent",
    "born in an Asian country; ``One Asian-born parent'' rows require the",
    "other parent to be born outside Asia (the AW and WA labels follow the paper's convention). Grandparents' birthplaces are not",
    "observed for adults, so the third generation cannot be identified."),
  source_note = "Data source is the 2004--2021 Current Population Survey.",
  file = "tab54-observations-by-gen-adults.tex"
)

cat(sprintf("Wrote %d children rows and %d adult rows (run %s)\n",
            nrow(kid_tab), nrow(adult_tab), run_id))
print(kid_tab[, .(row_label, n_obs, asian_only = round(share_asian_only, 3),
                  white_only = round(share_white_only, 3),
                  asian_white = round(share_asian_and_white, 3),
                  other = round(share_other, 3))])
print(adult_tab[, .(row_label, n_obs, asian_only = round(share_asian_only, 3),
                    white_only = round(share_white_only, 3),
                    asian_white = round(share_asian_and_white, 3),
                    other = round(share_other, 3))])
