#!/usr/bin/env Rscript
# scripts/import_release_snapshots.R: one-off, run locally.
#
# Rebuilds the autoobs counters each dated observatory.db release held, from the
# history asset, keeps the snapshots that agree with the stored series, writes
# autoobs-counters-import.db for a refill run, and prints what a refill would fill.
# Give the counters asset too, so the preview solves over the same runs as the refill.
#   Rscript scripts/import_release_snapshots.R <history.db> <autoobs-downloads-recent.db> <out_dir> \
#     [autoobs-counters-recent.db]

suppressPackageStartupMessages({ library(DBI); library(RSQLite); library(jsonlite) })

if (!exists("snapshot_runs", mode = "function")) {
  .imp_dir <- local({
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(f) == 1L && nzchar(f)) dirname(f) else "scripts"
  })
  source(file.path(.imp_dir, "config.R"))
  source(file.path(.imp_dir, "helpers.R"))
}

write_import <- function(path, runs, counters, log, left_out) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, COUNTERS_DDL)
  if (nrow(counters) > 0) DBI::dbWriteTable(con, "autoobs_counters", counters, append = TRUE)
  DBI::dbExecute(con, runs_table_ddl())
  if (nrow(runs) > 0) DBI::dbWriteTable(con, "autoobs_runs", normalize_runs(runs), append = TRUE)
  DBI::dbWriteTable(con, "autoobs_import_log", log)
  DBI::dbWriteTable(con, "autoobs_import_left_out", left_out)
  DBI::dbExecute(con, "VACUUM")
  invisible(path)
}

import_release_snapshots <- function(history_path, recent_path, out_dir, counters_path = NULL) {
  h <- DBI::dbConnect(RSQLite::SQLite(), history_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(h), add = TRUE)
  check_history_contract(h)
  sr <- snapshot_runs(history_autoobs_snapshots(h))

  r <- DBI::dbConnect(RSQLite::SQLite(), recent_path, flags = RSQLite::SQLITE_RO)
  daily <- DBI::dbGetQuery(r, "SELECT package, date, count FROM autoobs_downloads_daily")
  DBI::dbDisconnect(r)
  days <- read_days(recent_path) %||% bootstrap_days(daily)

  v <- validate_snapshots(sr$runs, sr$counters, daily, days$date)
  bad <- sr$runs[!v$keep, , drop = FALSE]
  if (nrow(bad) > 0) {
    i <- match(bad$snapshot_date, sr$log$source_as_of)
    sr$log$outcome[i] <- "dropped"
    sr$log$reason[i]  <- v$reason[!v$keep]
  }
  runs <- sr$runs[v$keep, , drop = FALSE]
  counters <- sr$counters[sr$counters$run_id %in% runs$run_id, , drop = FALSE]
  # What a refill would fill: the solve a refill run does, over these snapshots
  # and the pipeline runs whose counters are in the counters asset.
  real_runs <- read_runs(recent_path)
  real_cn <- empty_counters()
  if (!is.null(counters_path)) {
    if (!counters_readable(counters_path)) stop("cannot read the counters asset ", counters_path)
    real_cn <- read_counters(counters_path)
  }
  prior <- read_rebuilt(recent_path)
  rf <- refill_solve(real_runs, real_cn, runs, counters, daily, days, run_id = 0L,
                     covered = prior$covered, skip = prior$skip)
  # The file keeps every row the kept releases held; the refill leaves packages out.
  out_dir <- sub("(.)/+$", "\\1", out_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path <- write_import(file.path(out_dir, COUNTERS_IMPORT_ASSET), runs, counters, sr$log, rf$left_out)

  cat(sprintf("snapshots: %d read, %d kept, %d dropped\n",
              nrow(sr$log), sum(sr$log$outcome == "kept"), sum(sr$log$outcome == "dropped")))
  for (i in which(sr$log$outcome == "dropped"))
    cat(sprintf("  dropped %s (%s): %s\n", sr$log$tag[i], sr$log$source_as_of[i], sr$log$reason[i]))
  n_real <- sum(real_runs$run_id %in% real_cn$run_id & !is.na(real_runs$window_end))
  cat(if (is.null(counters_path)) "no counters asset given: the preview uses the snapshots alone\n"
      else sprintf("with %d pipeline run%s from the counters asset\n", n_real, if (n_real == 1L) "" else "s"))
  cat(sprintf("packages the snapshots cover: %d\n", length(rf$covered)))
  n_out <- nrow(rf$left_out)
  if (n_out > 0) {
    shown <- utils::head(sort(rf$left_out$package), 10L)
    cat(sprintf("held by a snapshot and left out: %d (%s%s)\n", n_out, paste(shown, collapse = ", "),
                if (n_out > length(shown)) sprintf(" and %d more", n_out - length(shown)) else ""))
  }
  if (nrow(rf$days) > 0) {
    cat(sprintf("a refill would fill %d days, %s to %s; rejected %d; residual %d\n",
                nrow(rf$days), min(rf$days$date), max(rf$days$date), rf$rejected, rf$residual))
    print(rf$days[order(rf$days$date), c("date", "method", "packages", "downloads")], row.names = FALSE)
  } else {
    cat(sprintf("a refill would fill no days; rejected %d; residual %d\n", rf$rejected, rf$residual))
  }
  cat("wrote ", path, " (sha256 ", file_sha256(path), ")\n", sep = "")
  invisible(list(path = path, log = sr$log, fill = rf))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args) %in% 3:4)
    stop("usage: import_release_snapshots.R <history.db> <autoobs-downloads-recent.db> <out_dir> ",
         "[autoobs-counters-recent.db]")
  import_release_snapshots(args[1], args[2], args[3], if (length(args) == 4L) args[4] else NULL)
}
