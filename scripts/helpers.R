# scripts/helpers.R: pure functions used by update.R, unit-tested in tests/testthat/.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Classify autoCRAN RPM names against the shared identity maps. Each binary is
# named "R-<cran-name>" (CRAN casing preserved), so strip a leading "R-" and
# resolve the remainder via robservatory::resolve_identity. The R- prefix asserts
# "is an R package," not CRAN-vs-Bioc, so prefix_hint is NULL (a "cran" hint would
# warn spuriously on legitimately-Bioc R-* builds). A name without a leading "R-",
# or one the resolver does not know, is origin='other' (honest unknown), so the
# promote-only filter in build_summary drops it.
resolve_identities <- function(names, maps) {
  n <- length(names)
  origin    <- rep("other", n)
  canonical <- rep(NA_character_, n)
  state     <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    p <- names[i]
    if (!startsWith(p, "R-")) next  # not an R-<name> binary: leave as other
    stripped <- substring(p, nchar("R-") + 1L)
    r <- robservatory::resolve_identity(stripped, maps, prefix_hint = NULL)
    if (isTRUE(r$in_scope)) {
      origin[i]    <- r$origin
      canonical[i] <- r$canonical_name
      state[i]     <- r$identity_state
    }
  }
  data.frame(package = names, origin = origin,
             canonical_name = canonical, identity_state = state,
             stringsAsFactors = FALSE)
}

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

# Extract the repository paths from a MirrorCache /rest/search/package_locations
# response (each entry carries a "path" like
# "/repositories/devel:/languages:/R:/autoCRAN/openSUSE_Tumbleweed/x86_64").
parse_location_paths <- function(txt) {
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$data)) return(character(0))
  d <- obj$data
  if (is.data.frame(d)) return(as.character(d$path %||% character(0)))
  if (is.list(d)) return(unlist(lapply(d, function(x) x$path), use.names = FALSE))
  character(0)
}

# Decide whether a package is served ONLY by autoCRAN: 1 if every location path is
# under an autoCRAN repo, 0 if any path is elsewhere (the distribution or another
# devel:languages:R: project, so its name-aggregated count is a superset), NA when
# no locations are known (cannot classify; left to retry on a later run).
classify_autocran_only <- function(paths) {
  if (length(paths) == 0) return(NA_integer_)
  as.integer(all(grepl("autoCRAN", paths, fixed = TRUE)))
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
      package        TEXT,
      package_lower  TEXT,
      origin         TEXT,
      canonical_name TEXT,
      identity_state TEXT,
      id             INTEGER,
      total_1d       INTEGER,
      total_7d       INTEGER,
      total_30d      INTEGER,
      cnt_total      INTEGER,
      avg_daily_30d  REAL,
      rank_30d       INTEGER,
      rank_total     INTEGER,
      trend          REAL,
      autocran_only  INTEGER,
      first_seen     TEXT,
      last_snapshot  TEXT,
      PRIMARY KEY (package))", table)
}

SUMMARY_COLS <- c("package", "package_lower", "origin", "canonical_name", "identity_state",
                  "id", "total_1d", "total_7d", "total_30d",
                  "cnt_total", "avg_daily_30d", "rank_30d", "rank_total", "trend",
                  "autocran_only", "first_seen", "last_snapshot")

empty_summary <- function() {
  as.data.frame(setNames(lapply(SUMMARY_COLS, function(x) switch(x,
    package = , package_lower = , origin = , canonical_name = , identity_state = ,
    first_seen = , last_snapshot = character(0),
    avg_daily_30d = , trend = numeric(0), integer(0))), SUMMARY_COLS),
    stringsAsFactors = FALSE)
}

# Build the per-package summary. The rolling windows (total_1d/7d/30d, cnt_total)
# come straight from MirrorCache's own counters in `stats_df` (so the summary is
# meaningful from the first run, before this pipeline has accumulated its own
# series). `trend` compares the last 30 days of the locally-accumulated daily
# series to the prior 30, so it is NULL until ~60 days of history exist. The
# summary is promote-only: origin='other' rows (not a known CRAN/Bioc package) are
# dropped before ranking, so rank_30d/rank_total are dense over the in-scope
# survivors only.
build_summary <- function(daily_con, stats_df, anchor_date, snapshot_date,
                          identity_df = NULL, autocran_map = NULL) {
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

  # Attach origin/canonical_name/identity_state from the ledger classification,
  # keyed by the full R-<name>. An absent identity frame or an unmatched row is an
  # honest unknown -> origin='other', which the promote-only filter below drops.
  if (is.null(identity_df) || nrow(identity_df) == 0) {
    base$origin         <- rep("other", nrow(base))
    base$canonical_name <- rep(NA_character_, nrow(base))
    base$identity_state <- rep(NA_character_, nrow(base))
  } else {
    idx <- match(base$package, identity_df$package)
    base$origin         <- identity_df$origin[idx]
    base$canonical_name <- identity_df$canonical_name[idx]
    base$identity_state <- identity_df$identity_state[idx]
    base$origin         <- ifelse(is.na(base$origin), "other", base$origin)
  }

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

  # Promote only in-scope packages: drop origin='other' (the R-base runtime,
  # -devel/-debuginfo subpackages, distro-shared RPMs, and any R-* the ledger does
  # not know) BEFORE ranking, so ranks are dense over the in-scope survivors.
  m <- m[m$origin %in% c("cran", "bioc"), , drop = FALSE]
  if (nrow(m) == 0) return(empty_summary())

  m$rank_30d   <- as.integer(rank(-ifelse(is.na(m$total_30d), 0L, m$total_30d), ties.method = "min"))
  m$rank_total <- as.integer(rank(-ifelse(is.na(m$cnt_total), 0L, m$cnt_total), ties.method = "min"))
  m$autocran_only <- if (is.null(autocran_map) || nrow(autocran_map) == 0) NA_integer_
    else as.integer(autocran_map$autocran_only[match(m$package, autocran_map$package)])
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

# Compute the lowercase hex SHA-256 of a file's exact on-disk bytes.
#
# Uses whatever the runner already provides, in preference order:
#   1. digest  package        (if installed)
#   2. openssl package        (if installed)
#   3. sha256sum (coreutils)  — present on the ubuntu-latest CI runner
#   4. shasum -a 256 (BSD)    — macOS/local fallback
# No heavy dependency is declared: on CI (which installs only DBI, RSQLite,
# jsonlite, curl, xml2, testthat, withr, robservatory) neither digest nor openssl
# is present, so the coreutils `sha256sum` path is used. If a sibling pipeline
# already declares `digest`, that path wins automatically.
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(file = path, algo = "sha256")))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(tolower(as.character(openssl::sha256(con))))
  }
  sha_tool <- Sys.which("sha256sum")
  if (nzchar(sha_tool)) {
    out <- system2(sha_tool, shQuote(path), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  shasum_tool <- Sys.which("shasum")
  if (nzchar(shasum_tool)) {
    out <- system2(shasum_tool, c("-a", "256", shQuote(path)), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  stop("No SHA-256 backend found (need one of: digest, openssl, sha256sum, shasum)")
}

# Build the integrity / completeness core describing a finalized SQLite file.
#
# Returns a named list of TOP-LEVEL manifest fields computed from the exact
# on-disk bytes of `db_path` (call this only after the file is finalized):
#   * db_filename — basename of the file
#   * db_bytes    — integer byte size of the file
#   * db_sha256   — lowercase hex sha256 of the file's exact bytes
#   * tables      — named list mapping each user table to its row count
#   * complete    — passed through by the caller (TRUE for a full rebuild)
# Lets a downstream merge content-verify the asset it pulls and confirm the
# expected tables/rows are present.
summary_integrity_core <- function(db_path, complete = TRUE) {
  stopifnot(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tbl_names <- DBI::dbGetQuery(con, "
    SELECT name FROM sqlite_master
     WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
     ORDER BY name")$name

  tables <- stats::setNames(
    lapply(tbl_names, function(t) {
      DBI::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
    }),
    tbl_names
  )

  list(
    db_filename = basename(db_path),
    db_bytes    = as.integer(file.size(db_path)),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
}

# `core` (optional) is a named list of TOP-LEVEL fields to merge into the manifest
# — used to attach the integrity/completeness core built by
# summary_integrity_core() (db_filename, db_bytes, db_sha256, tables, complete).
write_manifest <- function(path, obj, core = NULL) {
  if (!is.null(core)) obj <- c(obj, core)  # merge as top-level fields, not nested
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
    sprintf("| **autoCRAN-only (exact counts)** | %s of %s |",
            big(manifest$summary$autocran_only), big(manifest$summary$packages)),
    sprintf("| **Changed this run** | %s |", changed),
    "",
    "> The remaining packages share an RPM name with the openSUSE distribution or another devel repo, so MirrorCache's count for them is not exclusive to autoCRAN. Filter `WHERE autocran_only = 1` (or sum only those rows) for autoCRAN-only totals.",
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
