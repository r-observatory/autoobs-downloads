# scripts/helpers.R: pure functions used by update.R, unit-tested in tests/testthat/.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Extract the primary.xml location href from an rpm-md repomd.xml document.
parse_repomd_primary_href <- function(xml_text) {
  doc <- xml2::read_xml(xml_text)
  xml2::xml_ns_strip(doc)
  node <- xml2::xml_find_first(doc, "//data[@type='primary']/location")
  if (length(node) == 0 || is.na(xml2::xml_attr(node, "href"))) return(NA_character_)
  xml2::xml_attr(node, "href")
}

# Extract the unique package names from an rpm-md primary.xml document. The
# autoCRAN binaries are named "R-<cran-name>" (e.g. R-Rcpp, R-aae.pop). A
# primary.xml is ~150 MB uncompressed with one <package> per arch, so this avoids
# building a DOM: <name> is the only such element per package, and the file is
# line-oriented. `xml` may be a single string or a vector of lines (the producer
# passes readLines() output to skip materializing the whole file as one string).
parse_primary_names <- function(xml) {
  lines <- if (length(xml) == 1L) strsplit(xml, "\n", fixed = TRUE)[[1]] else xml
  hits  <- grep("<name>", lines, fixed = TRUE, value = TRUE)
  # Extract EVERY <name>...</name> on each line (not just the last), so a line with
  # several elements, or a compact single-line primary.xml, is parsed correctly.
  m  <- regmatches(hits, gregexpr("<name>[^<]*</name>", hits, perl = TRUE))
  nm <- unlist(m, use.names = FALSE)
  nm <- sub("^<name>", "", sub("</name>$", "", nm))
  sort(unique(nm[nzchar(nm)]), method = "radix")  # locale-independent, deterministic
}

# Parse a MirrorCache /rest/package/<name> response into a numeric package id, or
# NA when the package is unknown / the response is malformed.
parse_id_response <- function(txt) {
  obj <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$id) || length(obj$id) == 0) return(NA_integer_)
  as.integer(obj$id)
}

# Parse a MirrorCache /rest/package/<id>/stat_download response into the rolling
# download windows. MirrorCache returns the counts as JSON strings; they are
# coerced to integers here. A missing or empty body yields all-NA.
parse_stat_response <- function(txt) {
  empty <- list(cnt_today = NA_integer_, cnt_1d = NA_integer_, cnt_7d = NA_integer_,
                cnt_30d = NA_integer_, cnt_total = NA_integer_, first_seen = NA_integer_)
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$data)) return(empty)
  d <- obj$data
  if (is.data.frame(d)) { if (nrow(d) == 0) return(empty); d <- as.list(d[1, , drop = FALSE]) }
  else if (is.list(d)) { if (length(d) == 0) return(empty); d <- d[[1]] }
  else return(empty)
  gi <- function(k) { v <- d[[k]]; if (is.null(v) || length(v) == 0 || is.na(v[1])) NA_integer_ else as.integer(v[1]) }
  list(cnt_today = gi("cnt_today"), cnt_1d = gi("cnt_1d"), cnt_7d = gi("cnt_7d"),
       cnt_30d = gi("cnt_30d"), cnt_total = gi("cnt_total"), first_seen = gi("first_seen"))
}

# Build the per-day daily rows from a stats frame: the day's count is cnt_1d (the
# trailing-day download figure), attributed to `attribute_date` (the UTC day that
# just completed). Zero/NA days are dropped to keep the long-tail series compact.
build_daily_rows <- function(stats_df, attribute_date) {
  empty <- data.frame(package = character(0), date = character(0),
                      count = integer(0), stringsAsFactors = FALSE)
  if (nrow(stats_df) == 0) return(empty)
  keep <- !is.na(stats_df$cnt_1d) & stats_df$cnt_1d > 0
  if (!any(keep)) return(empty)
  data.frame(package = stats_df$package[keep], date = attribute_date,
             count = as.integer(stats_df$cnt_1d[keep]), stringsAsFactors = FALSE)
}

unix_to_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  format(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
}

# Write the daily-series table for one shard. daily_df has (package, date, count).
export_shard <- function(path, daily_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, "
    CREATE TABLE autoobs_downloads_daily (
      package TEXT    NOT NULL,
      date    TEXT    NOT NULL,
      count   INTEGER NOT NULL,
      PRIMARY KEY (package, date))")
  DBI::dbExecute(con, "CREATE INDEX idx_add_date ON autoobs_downloads_daily(date)")
  if (nrow(daily_df) > 0) {
    DBI::dbWriteTable(con, "autoobs_downloads_daily",
      daily_df[c("package", "date", "count")], append = TRUE)
  }
  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

# Write a minimal SQLite file containing only the summary table (for the merger).
export_summary_shard <- function(path, summary) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, summary_table_ddl("autoobs_downloads_summary"))
  if (nrow(summary) > 0) DBI::dbWriteTable(con, "autoobs_downloads_summary", summary, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

# Shared DDL so the summary schema is identical in the summary-only shard and when
# embedded into the recent shard.
summary_table_ddl <- function(table) {
  sprintf("CREATE TABLE %s (
      package       TEXT,
      package_lower TEXT,
      id            INTEGER,
      total_1d      INTEGER,
      total_7d      INTEGER,
      total_30d     INTEGER,
      cnt_total     INTEGER,
      avg_daily_30d REAL,
      rank_30d      INTEGER,
      rank_total    INTEGER,
      trend         REAL,
      first_seen    TEXT,
      last_snapshot TEXT,
      PRIMARY KEY (package))", table)
}

SUMMARY_COLS <- c("package", "package_lower", "id", "total_1d", "total_7d", "total_30d",
                  "cnt_total", "avg_daily_30d", "rank_30d", "rank_total", "trend",
                  "first_seen", "last_snapshot")

empty_summary <- function() {
  as.data.frame(setNames(lapply(SUMMARY_COLS, function(x) switch(x,
    package = , package_lower = , first_seen = , last_snapshot = character(0),
    avg_daily_30d = , trend = numeric(0), integer(0))), SUMMARY_COLS),
    stringsAsFactors = FALSE)
}

# Build the per-package summary. The rolling windows (total_1d/7d/30d, cnt_total)
# come straight from MirrorCache's own counters in `stats_df` (so the summary is
# meaningful from the first run, before this pipeline has accumulated its own
# series). `trend` compares the last 30 days of the locally-accumulated daily
# series to the prior 30, so it is NULL until ~60 days of history exist. Ranks are
# global (this project ships a single category of packages).
build_summary <- function(daily_con, stats_df, anchor_date, snapshot_date) {
  if (nrow(stats_df) == 0) return(empty_summary())
  base <- data.frame(
    package       = stats_df$package,
    package_lower = tolower(stats_df$package),
    id            = as.integer(stats_df$id),
    total_1d      = as.integer(stats_df$cnt_1d),
    total_7d      = as.integer(stats_df$cnt_7d),
    total_30d     = as.integer(stats_df$cnt_30d),
    cnt_total     = as.integer(stats_df$cnt_total),
    avg_daily_30d = round(as.numeric(stats_df$cnt_30d) / 30, 2),
    first_seen    = vapply(stats_df$first_seen, unix_to_date, character(1)),
    last_snapshot = snapshot_date,
    stringsAsFactors = FALSE)

  a  <- format(as.Date(anchor_date), "%Y-%m-%d")
  tr <- DBI::dbGetQuery(daily_con, sprintf("
    SELECT package,
      SUM(CASE WHEN date > date('%1$s','-30 days') THEN count ELSE 0 END) AS last30,
      SUM(CASE WHEN date > date('%1$s','-60 days')
                AND date <= date('%1$s','-30 days') THEN count ELSE 0 END) AS prev30
    FROM autoobs_downloads_daily GROUP BY package", a))

  m <- merge(base, tr, by = "package", all.x = TRUE)
  m$trend <- ifelse(!is.na(m$prev30) & m$prev30 > 0,
                    round((m$last30 / m$prev30 - 1) * 100, 2), NA_real_)
  m$rank_30d   <- as.integer(rank(-ifelse(is.na(m$total_30d), 0L, m$total_30d), ties.method = "min"))
  m$rank_total <- as.integer(rank(-ifelse(is.na(m$cnt_total), 0L, m$cnt_total), ties.method = "min"))
  m <- m[order(m$rank_30d, m$package), , drop = FALSE]
  rownames(m) <- NULL
  m[SUMMARY_COLS]
}

# Trailing-window rows of the daily series, for autoobs-downloads-recent.db.
extract_recent <- function(con, today, window_days) {
  cutoff <- format(as.Date(today) - as.integer(window_days), "%Y-%m-%d")
  DBI::dbGetQuery(con, sprintf("
    SELECT package, date, count FROM autoobs_downloads_daily
     WHERE date >= '%s' ORDER BY package, date", cutoff))
}

# All daily rows for a calendar year.
extract_year <- function(con, year) {
  yp <- sprintf("%04d", as.integer(year))
  DBI::dbGetQuery(con, sprintf("
    SELECT package, date, count FROM autoobs_downloads_daily
     WHERE substr(date,1,4) = '%s' ORDER BY package, date", yp))
}

coverage <- function(rows) {
  if (nrow(rows) == 0) return(list(rows = 0L, date_min = NA, date_max = NA))
  list(rows = nrow(rows), date_min = min(rows$date), date_max = max(rows$date))
}

merge_shard_coverage <- function(prev, updates) {
  out <- prev %||% list()
  for (k in names(updates)) out[[k]] <- updates[[k]]
  out
}

write_manifest <- function(path, obj) {
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null"), path)
}

# Render the GitHub release body (markdown) from a manifest object.
write_release_notes <- function(path, manifest) {
  ts  <- function(s) if (is.null(s) || is.na(s)) "n/a" else sub("Z$", " UTC", sub("T", " ", s))
  big <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "0" else
    formatC(as.numeric(x), format = "d", big.mark = ",")
  cs <- manifest$changed_shards
  changed <- if (length(cs) == 0) "none (source unreachable this run)" else
    paste(unlist(cs), collapse = ", ")

  lines <- c(
    "Daily per-package download statistics for the [autoCRAN](https://build.opensuse.org/project/show/devel:languages:R:autoCRAN) openSUSE Build Service project (CRAN packages built as RPMs). Counts come from the MirrorCache redirector on download.opensuse.org, which serves the binaries. See the [README](https://github.com/r-observatory/autoobs-downloads#readme) for the important name-aggregation caveat.",
    "",
    "This is a single rolling release. Assets are SQLite shards: per-year archives (`autoobs-downloads-YYYY.db`), a rolling 400-day window (`autoobs-downloads-recent.db`), and a summary-only file (`autoobs-downloads-summary.db`), alongside `manifest.json`. Each run replaces only the shards that changed.",
    "",
    "| | |",
    "|---|---|",
    sprintf("| **Last checked** | %s |", ts(manifest$last_checked)),
    sprintf("| **Source this run** | %s |", manifest$source_kind %||% "n/a"),
    sprintf("| **Latest day** | %s |", manifest$summary$latest_date %||% "n/a"),
    sprintf("| **Packages tracked** | %s |", big(manifest$summary$packages)),
    sprintf("| **Changed this run** | %s |", changed),
    "",
    "## Shard coverage",
    "",
    "| Shard | Rows | From | To |",
    "|---|---:|---|---|")
  shards <- manifest$shards %||% list()
  for (nm in sort(names(shards))) {
    s <- shards[[nm]]
    lines <- c(lines, sprintf("| `%s` | %s | %s | %s |",
      nm, big(s$rows), s$date_min %||% "n/a", s$date_max %||% "n/a"))
  }
  lines <- c(lines, "",
    "_Fetch the rolling window:_",
    "```bash",
    "gh release download current --repo r-observatory/autoobs-downloads --pattern autoobs-downloads-recent.db",
    "```")
  writeLines(lines, path)
  invisible(NULL)
}
