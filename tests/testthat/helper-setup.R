# Auto-sourced by testthat before tests run. Sources the pipeline code so tests
# can call the helpers and the orchestrator directly. During test_dir() the
# working directory is tests/testthat, so the repo root is two levels up.
.ao_root <- normalizePath(file.path(getwd(), "..", ".."))

source(file.path(.ao_root, "scripts", "config.R"))
source(file.path(.ao_root, "scripts", "helpers.R"))

.ao_update <- file.path(.ao_root, "scripts", "update.R")
if (file.exists(.ao_update)) source(.ao_update)

fixture_path <- function(...) {
  file.path(.ao_root, "tests", "testthat", "fixtures", ...)
}

# One MirrorCache stat_download row as default_io$fetch_stats would emit it.
stats_row <- function(id, cnt_1d, cnt_7d, cnt_30d, cnt_total,
                      first_seen = 1767139200L, cnt_today = 0L) {
  data.frame(id = as.integer(id), cnt_today = as.integer(cnt_today),
             cnt_1d = as.integer(cnt_1d), cnt_7d = as.integer(cnt_7d),
             cnt_30d = as.integer(cnt_30d), cnt_total = as.integer(cnt_total),
             first_seen = as.integer(first_seen), stringsAsFactors = FALSE)
}

# An all-NA stat row, as a transiently failed MirrorCache fetch produces.
na_stats_row <- function(id) {
  data.frame(id = as.integer(id), cnt_today = NA_integer_, cnt_1d = NA_integer_,
             cnt_7d = NA_integer_, cnt_30d = NA_integer_, cnt_total = NA_integer_,
             first_seen = NA_integer_, stringsAsFactors = FALSE)
}
