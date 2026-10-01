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

# Write a cran_names_all/bioc_names_all fixture DB at `path` (schema matching the
# real identity assets), populated from `names` (a character vector of canonical
# package names). Always creates the table, even for an empty vector, since
# robservatory::load_identity requires both tables to exist.
.write_names_db <- function(path, table, names) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s (name_lower TEXT PRIMARY KEY, canonical_name TEXT,
       identity_state TEXT, first_seen TEXT, last_seen TEXT)", table))
  if (length(names) > 0L) {
    DBI::dbWriteTable(con, table, data.frame(
      name_lower = tolower(names), canonical_name = names,
      identity_state = "live", first_seen = "x", last_seen = "y",
      stringsAsFactors = FALSE), append = TRUE)
  }
}

# MirrorCache's counters at snapshot S from a truth frame (package, date, count):
# cnt_1d is day S-1 once aggregated, and the windows run from S-7 and S-30 to the
# last day MirrorCache has counted: S-1 (aggregated), S-2, or `last` when
# MirrorCache is further behind. `ids` maps package to id, in output order.
mc_counters <- function(truth, ids, S, aggregated = TRUE, last = NULL) {
  S <- as.Date(S); end <- last %||% format(S - if (aggregated) 1L else 2L)
  counted <- end == format(S - 1L)
  tot <- function(p, from) sum(truth$count[truth$package == p & truth$date >= from & truth$date <= end])
  do.call(rbind, lapply(names(ids), function(p) {
    one <- if (counted) sum(truth$count[truth$package == p & truth$date == format(S - 1L)]) else 0
    cbind(stats_row(ids[[p]], one, tot(p, format(S - 7L)), tot(p, format(S - 30L)), 0),
          package = p, stringsAsFactors = FALSE)
  }))
}

# A truth series for two packages; R-b has no downloads on 06-05.
fill_truth <- function(from = "2026-05-01", to = "2026-06-10") {
  d <- format(seq(as.Date(from), as.Date(to), by = "day"))
  t <- rbind(data.frame(package = "R-a", date = d, count = 10L + seq_along(d) %% 5L),
             data.frame(package = "R-b", date = d, count = 1L + seq_along(d) %% 3L))
  t$count[t$package == "R-b" & t$date == "2026-06-05"] <- 0L
  t
}

# The truth series plus R-x, a package outside the summary's scope.
with_outsider <- function(truth) {
  d <- sort(unique(truth$date))
  rbind(truth, data.frame(package = "R-x", date = d, count = 2L + seq_along(d) %% 4L))
}

# One run and its counters as a refill reads them. `source` is "run" for a
# pipeline run; `last` is the last day MirrorCache had counted when it was late.
snapshot_fixture <- function(truth, ids, S, source = "observatory.db x", last = NULL, hour = 0L) {
  cn <- mc_counters(truth, ids, S, last = last)
  cs <- counter_stats(cn)
  rid <- as.integer(as.numeric(as.POSIXct(S, tz = "UTC"))) + as.integer(hour) * 3600L
  list(run = run_row(c(list(run_id = rid, snapshot_date = S, source = source, outcome = "ok",
                            window_end = window_end_for(S, cs$day_aggregated)), cs)),
       cn = cbind(cn[setdiff(names(cn), c("id", "first_seen"))], run_id = rid))
}

# A history.db as the history build in r-observatory/data writes it: one dated
# release per snapshot date, the series recorded as 'autoobs_summary' with
# outcome 'applied', and its counters folded into autoobs_summary_history
# episodes (first_seen and last_seen are snapshot days). The names are literal
# here so a drift in config.R shows up. `observe` overrides a tag's series,
# outcome, rows_read or source_as_of. `wide` (ids, until) makes the releases up
# to a date hold more packages, as the real ones did before the scope filter.
write_history_fixture <- function(path, truth, ids, dates, unaggregated = character(0),
                                  observe = list(), wide = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE history_snapshots (family TEXT, tag TEXT, snapshot_at TEXT,
    snapshot_on TEXT, asset TEXT, bytes INTEGER, sha256 TEXT, outcome TEXT, note TEXT,
    processed_at TEXT, PRIMARY KEY (family, tag))")
  DBI::dbExecute(con, "CREATE TABLE history_series_observations (family TEXT, tag TEXT, series TEXT,
    rows_read INTEGER, rows_kept INTEGER, outcome TEXT, source_as_of TEXT, columns TEXT)")
  DBI::dbExecute(con, "CREATE TABLE autoobs_summary_history (package TEXT, episode_seq INTEGER,
    origin TEXT, identity_state TEXT, total_1d INTEGER, total_7d INTEGER, total_30d INTEGER,
    cnt_total INTEGER, first_seen TEXT, last_seen TEXT, ended_on TEXT, PRIMARY KEY (package, episode_seq))")
  open <- list(); done <- list(); vals <- c("total_1d", "total_7d", "total_30d", "cnt_total")
  for (D in dates) {
    use <- if (!is.null(wide) && D <= wide$until) wide$ids else ids
    cn <- mc_counters(truth, use, D, aggregated = !(D %in% unaggregated))
    cn <- data.frame(package = cn$package, total_1d = cn$cnt_1d, total_7d = cn$cnt_7d,
                     total_30d = cn$cnt_30d, cnt_total = cn$cnt_total, stringsAsFactors = FALSE)
    for (p in setdiff(names(open), cn$package)) {      # no longer held: the episode ends
      o <- open[[p]]; o$ended_on <- D; done[[length(done) + 1L]] <- o; open[[p]] <- NULL
    }
    for (i in seq_len(nrow(cn))) {
      p <- cn$package[i]; o <- open[[p]]
      if (!is.null(o) && identical(unlist(o[vals]), unlist(cn[i, vals]))) { open[[p]]$last_seen <- D; next }
      if (!is.null(o)) { o$ended_on <- D; done[[length(done) + 1L]] <- o }
      open[[p]] <- c(list(package = p, episode_seq = if (is.null(o)) 1L else o$episode_seq + 1L,
                          origin = "cran", identity_state = "live"),
                     as.list(cn[i, vals]), list(first_seen = D, last_seen = D, ended_on = NA_character_))
    }
    tag <- paste0("v", D); ob <- observe[[tag]] %||% list()
    DBI::dbExecute(con, "INSERT INTO history_snapshots (family, tag, snapshot_at, snapshot_on, outcome)
                         VALUES ('data', ?, ?, ?, 'processed')",
                   params = list(tag, paste0(D, "T21:00:00Z"), D))
    DBI::dbExecute(con, "INSERT INTO history_series_observations
                         VALUES ('data', ?, ?, ?, ?, ?, ?, NULL)",
                   params = list(tag, ob$series %||% "autoobs_summary", ob$rows_read %||% nrow(cn),
                                 nrow(cn), ob$outcome %||% "applied",
                                 ob$source_as_of %||% D))
  }
  eps <- do.call(rbind, lapply(c(done, open), as.data.frame, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "autoobs_summary_history", eps, append = TRUE)
  invisible(path)
}
