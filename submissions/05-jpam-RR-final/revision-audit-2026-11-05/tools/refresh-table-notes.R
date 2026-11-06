#!/usr/bin/env Rscript
# Refresh Table 8 and A.5 notes without estimating models.
# Run before rebuilding the final-review package after a note-only change.
# Literal note wording is read from each analysis script. Existing formatted
# numeric clauses are retained verbatim; no model or numeric-note expression
# is evaluated. Numeric table bodies must not change. This review utility is
# deliberately outside programs/95-make-all.R, not a replacement for estimation.
# Results -> output/tables/tab50-heterogeneity-interactions.tex
#         -> output/tables/tab51-pooled-mnl-bias-x-aw.tex
#         -> matching 05-jpam-RR-final table copies

args <- commandArgs(trailingOnly = TRUE)
env_root <- Sys.getenv("ASIAN_IDENTITY_ROOT")
root <- normalizePath(if (nzchar(env_root)) env_root else
  if (length(args) >= 1L) args[[1L]] else getwd(), mustWork = TRUE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

refresh_notes <- function(script, existing_note) {
  expressions <- parse(file = file.path(root, "programs", script))
  matches <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("notes"))
  }, as.list(expressions))
  assert_true(length(matches) == 1L,
              sprintf("Expected one notes assignment in %s", script))
  rhs <- matches[[1L]][[3L]]
  assert_true(is.call(rhs) && identical(rhs[[1L]], as.name("paste")),
              sprintf("Notes must be assembled by paste() in %s", script))
  escape_regex <- function(text) {
    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    special <- c("\\", ".", "^", "$", "|", "?", "*", "+",
                 "(", ")", "[", "]", "{", "}")
    paste0(ifelse(chars %in% special, paste0("\\", chars), chars),
           collapse = "")
  }
  pieces <- vapply(as.list(rhs)[-1L], function(expr) {
    if (is.character(expr) && length(expr) == 1L) return(expr)
    assert_true(is.call(expr) &&
                  identical(expr[[1L]], as.name("sprintf")) &&
                  is.character(expr[[2L]]) && length(expr[[2L]]) == 1L,
                sprintf("Unsupported note component in %s", script))
    # Preserve the stored numeric clause rather than executing model-dependent
    # arguments. A changed format fails instead of guessing replacement values.
    chunks <- strsplit(expr[[2L]], "%[sd]", perl = TRUE)[[1L]]
    pattern <- paste(vapply(chunks, escape_regex, character(1)),
                     collapse = "[-+0-9.,{}]+")
    hits <- regmatches(existing_note,
                       gregexpr(pattern, existing_note, perl = TRUE))[[1L]]
    assert_true(length(hits) == 1L,
                sprintf("Cannot identify one saved numeric clause in %s",
                        script))
    hits[[1L]]
  }, character(1))
  table_expr <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("tex"))
  }, as.list(expressions))
  assert_true(length(table_expr) == 1L,
              sprintf("Expected one tex assignment in %s", script))
  formats <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("sprintf")) &&
      length(expr) == 3L && identical(expr[[3L]], as.name("notes")) &&
      is.character(expr[[2L]]) && length(expr[[2L]]) == 1L
  }, as.list(table_expr[[1L]][[3L]])[-1L])
  assert_true(length(formats) == 1L,
              sprintf("Expected one note format in %s", script))
  list(text = paste(pieces, collapse = " "), format = formats[[1L]][[2L]])
}

table_body <- function(lines) {
  start <- which(startsWith(lines, "\\begin{tabular}"))
  end <- which(startsWith(lines, "\\end{tabular}"))
  assert_true(length(start) == 1L && length(end) == 1L && start < end,
              "Expected exactly one complete tabular body")
  lines[seq.int(start, end)]
}

sources <- c(
  "tab50-heterogeneity-interactions.tex" =
    "50-make-heterogeneity-interaction-table.R",
  "tab51-pooled-mnl-bias-x-aw.tex" = "51-run-pooled-mnl-bias-x-aw.R"
)
destinations <- c("output/tables", "submissions/05-jpam-RR-final")

# Validate every destination before writing any file.
pending <- list()
for (name in names(sources)) {
  for (directory in destinations) {
    path <- file.path(root, directory, name)
    assert_true(file.exists(path), sprintf("Missing table: %s", path))
    old <- readLines(path, warn = FALSE)
    marker <- which(startsWith(old, "\\item[1] "))
    assert_true(length(marker) == 1L,
                sprintf("Expected one numbered note in %s", path))
    note <- refresh_notes(sources[[name]], old[marker])
    revised <- old
    revised[marker] <- sprintf(note$format, note$text)
    assert_true(identical(table_body(old), table_body(revised)),
                sprintf("Numeric table body changed in %s", path))
    pending[[path]] <- revised
  }
}
for (path in names(pending)) writeLines(pending[[path]], path)
cat(sprintf("PASS: refreshed %d table-note copies; all numeric bodies unchanged; no models run\n",
            length(pending)))
