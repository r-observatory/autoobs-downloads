#!/usr/bin/env Rscript
# scripts/update.R: autoobs-downloads producer.
#
# Every run enumerates the autoCRAN package names from the openSUSE repodata,
# resolves any new names to MirrorCache ids (cached across runs), fetches the
# current per-package download windows from MirrorCache, records the trailing-day
# count (cnt_1d) as one point in the per-day series, and re-exports the affected
# year shard plus the recent and summary shards. When MirrorCache is unreachable
# the run is a cheap heartbeat that leaves the prior release intact.
# run_update(io, out_dir) takes an injectable io for offline testing.

options(timeout = 600)

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(jsonlite)
})

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f))
  NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("parse_primary_names", mode = "function")) {
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "helpers.R"))
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Embed the summary table and the name->id cache into the recent shard, so a
# single download answers most queries and the next run can skip re-resolving ids.
embed_aux <- function(recent_path, summary_df, cache_df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS autoobs_downloads_summary")
  DBI::dbExecute(con, summary_table_ddl("autoobs_downloads_summary"))
  if (nrow(summary_df) > 0) DBI::dbWriteTable(con, "autoobs_downloads_summary", summary_df, append = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS autoobs_packages")
  DBI::dbExecute(con, "CREATE TABLE autoobs_packages (package TEXT PRIMARY KEY, id INTEGER)")
  if (nrow(cache_df) > 0)
    DBI::dbWriteTable(con, "autoobs_packages", cache_df[c("package", "id")], append = TRUE)
}

run_update <- function(io, out_dir, force_full = FALSE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(out_dir, "manifest.json")
  recent_path   <- file.path(out_dir, "autoobs-downloads-recent.db")

  if (io$release_exists()) {
    mcode <- io$release_download("manifest.json", out_dir)
    rcode <- io$release_download("autoobs-downloads-recent.db", out_dir)
    # A release exists, so the prior history and id cache must be loaded before we
    # rebuild and clobber-upload. If either download fails, abort rather than
    # silently treat this as a cold start and overwrite the accumulated series.
    if (!identical(as.integer(mcode), 0L) || !file.exists(manifest_path) ||
        !identical(as.integer(rcode), 0L) || !file.exists(recent_path)) {
      stop("release 'current' exists but its manifest/recent shard could not be ",
           "downloaded; aborting to protect accumulated history")
    }
  }
  prev <- if (file.exists(manifest_path))
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE) else list()
  prev_shards <- prev$shards %||% list()

  now            <- io$now()
  snapshot_date  <- as.Date(format(now, "%Y-%m-%d", tz = "UTC"))
  snap_str       <- format(snapshot_date, "%Y-%m-%d")
  attribute_date <- format(snapshot_date - 1L, "%Y-%m-%d")  # cnt_1d ~ the day just ended

  cache      <- data.frame(package = character(0), id = integer(0), stringsAsFactors = FALSE)
  daily_hist <- data.frame(package = character(0), date = character(0),
                           count = integer(0), stringsAsFactors = FALSE)
  load_daily <- function(path) {
    if (!file.exists(path)) return(invisible())
    c2 <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(c2), add = TRUE)
    if ("autoobs_downloads_daily" %in% DBI::dbListTables(c2))
      daily_hist <<- rbind(daily_hist,
        DBI::dbGetQuery(c2, "SELECT package, date, count FROM autoobs_downloads_daily"))
  }
  if (file.exists(recent_path)) {
    rc <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
    if ("autoobs_packages" %in% DBI::dbListTables(rc))
      cache <- DBI::dbGetQuery(rc, "SELECT package, id FROM autoobs_packages")
    DBI::dbDisconnect(rc)
    load_daily(recent_path)
  }

  heartbeat <- function(reason) {
    out <- if (length(prev) > 0) prev else list()
    out$last_checked   <- iso(now)
    out$source_kind    <- "frozen"
    out$changed_shards <- list()
    write_manifest(manifest_path, out)
    write_release_notes(file.path(out_dir, "release_notes.md"), out)
    message("heartbeat: ", reason)
    list(changed_shards = character(0), manifest = out)
  }

  # Enumerate package names; fall back to the cached set if enumeration fails.
  names <- tryCatch(io$list_packages(), error = function(e) character(0))
  if (length(names) == 0) names <- cache$package
  new_names <- setdiff(names, cache$package)
  if (length(new_names) > 0) {
    resolved <- tryCatch(io$resolve_ids(new_names), error = function(e) NULL)
    if (!is.null(resolved) && nrow(resolved) > 0)
      cache <- rbind(cache, resolved[c("package", "id")])
  }
  cache <- cache[!duplicated(cache$package) & !is.na(cache$id), , drop = FALSE]
  if (nrow(cache) == 0) {
    if (length(prev) == 0)
      stop("no autoCRAN packages could be enumerated and no prior release exists; cannot bootstrap")
    return(heartbeat("no packages resolved"))
  }

  stats <- tryCatch(io$fetch_stats(cache$id), error = function(e) NULL)
  if (is.null(stats) || nrow(stats) == 0) {
    if (length(prev) == 0)
      stop("MirrorCache returned no stats and no prior release exists; cannot bootstrap")
    return(heartbeat("no stats fetched"))
  }
  stats_df <- merge(stats, cache, by = "id")
  stats_df <- stats_df[!is.na(stats_df$package), , drop = FALSE]
  # Drop rows whose counters are all NA (a transiently failed fetch). Keeping them
  # would publish NULL windows and a bottom rank for an otherwise-tracked package,
  # overwriting its real values; dropping simply leaves that package unchanged this
  # run (mc_multi already retries failures, so this is rare).
  all_na <- is.na(stats_df$cnt_1d) & is.na(stats_df$cnt_7d) &
            is.na(stats_df$cnt_30d) & is.na(stats_df$cnt_total)
  stats_df <- stats_df[!all_na, , drop = FALSE]
  if (nrow(stats_df) == 0) {
    if (length(prev) == 0)
      stop("MirrorCache returned only empty stats and no prior release exists; cannot bootstrap")
    return(heartbeat("all stat fetches returned empty"))
  }

  daily_today <- build_daily_rows(stats_df, attribute_date)

  # A full rebuild pulls every year shard into the history so all shards can be
  # re-exported; load_daily appends each into daily_hist.
  if (isTRUE(force_full)) {
    for (nm in names(prev_shards)) {
      if (grepl("^autoobs-downloads-[0-9]{4}\\.db$", nm)) {
        io$release_download(nm, out_dir)
        load_daily(file.path(out_dir, nm))
      }
    }
  }
  daily_hist <- daily_hist[daily_hist$date != attribute_date, , drop = FALSE]
  daily_all  <- rbind(daily_hist, daily_today)
  daily_all  <- daily_all[!duplicated(daily_all[c("package", "date")]), , drop = FALSE]

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (nrow(daily_all) > 0) {
    DBI::dbWriteTable(con, "autoobs_downloads_daily", daily_all[c("package", "date", "count")])
  } else {
    DBI::dbExecute(con, "CREATE TABLE autoobs_downloads_daily (package TEXT, date TEXT, count INTEGER)")
  }
  summary_df <- build_summary(con, stats_df, attribute_date, snap_str)

  years <- if (isTRUE(force_full) && nrow(daily_all) > 0)
    sort(unique(substr(daily_all$date, 1, 4))) else substr(attribute_date, 1, 4)
  changed_shards <- character(0); shard_updates <- list()
  for (yr in years) {
    shard <- sprintf("autoobs-downloads-%s.db", yr)
    dy <- daily_all[substr(daily_all$date, 1, 4) == yr, , drop = FALSE]
    export_shard(file.path(out_dir, shard), dy)
    changed_shards <- c(changed_shards, shard)
    shard_updates[[shard]] <- coverage(dy)
  }

  win_cut <- format(snapshot_date - RECENT_WINDOW_DAYS, "%Y-%m-%d")
  r_rows  <- daily_all[daily_all$date >= win_cut, , drop = FALSE]
  export_shard(recent_path, r_rows)
  embed_aux(recent_path, summary_df, cache)
  export_summary_shard(file.path(out_dir, "autoobs-downloads-summary.db"), summary_df)
  changed_shards <- c(changed_shards, "autoobs-downloads-recent.db", "autoobs-downloads-summary.db")
  shard_updates[["autoobs-downloads-recent.db"]] <- coverage(r_rows)

  out <- list(
    tag            = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at   = iso(now),
    last_checked   = iso(now),
    last_changed   = iso(now),
    source_kind    = "mirrorcache",
    project        = OBS_PROJECT,
    granularities  = list("daily"),
    changed_shards = as.list(changed_shards),
    shards         = merge_shard_coverage(prev_shards, shard_updates),
    summary        = list(
      packages         = nrow(summary_df),
      latest_date      = attribute_date,
      snapshot_date    = snap_str,
      daily_rows_today = nrow(daily_today)))
  write_manifest(manifest_path, out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out)
  list(changed_shards = changed_shards, manifest = out)
}

with_retry <- function(expr, tries = 3L, wait = 3) {
  for (i in seq_len(tries)) {
    val <- tryCatch(force(expr), error = function(e) e)
    if (!inherits(val, "error")) return(val)
    if (i < tries) Sys.sleep(wait * i)
  }
  stop(val)
}

# Concurrent GETs against MirrorCache, capped at FETCH_POOL connections and run in
# chunks with a short gap between them, so the volunteer-run download host is not
# hammered. Returns a list aligned to `urls`: the response body string on HTTP 200,
# NULL otherwise.
mc_multi <- function(urls, pool_size = FETCH_POOL, chunk = 1000L) {
  out <- vector("list", length(urls))
  n <- length(urls)
  if (n == 0) return(out)
  run_batch <- function(idxs) {
    for (start in seq(1L, length(idxs), by = chunk)) {
      end  <- min(start + chunk - 1L, length(idxs))
      sel  <- idxs[start:end]
      pool <- curl::new_pool(total_con = pool_size, host_con = pool_size)
      for (j in sel) {
        local({
          jj <- j
          h <- curl::new_handle(useragent = USER_AGENT, timeout = 60L, connecttimeout = 20L)
          curl::handle_setopt(h, url = urls[jj])
          curl::multi_add(h,
            done = function(res) if (isTRUE(res$status_code == 200L)) out[[jj]] <<- rawToChar(res$content),
            fail = function(err) invisible(NULL),
            pool = pool)
        })
      }
      curl::multi_run(pool = pool)
      if (end < length(idxs)) Sys.sleep(0.2)
    }
  }
  run_batch(seq_len(n))
  # One retry pass over the transient failures, so a hiccup against the busy
  # redirector does not leave a tracked package with no data for the day.
  failed <- which(vapply(out, is.null, logical(1)))
  if (length(failed) > 0) { Sys.sleep(1); run_batch(failed) }
  out
}

default_io <- function() {
  empty_stats <- function() data.frame(
    id = integer(0), cnt_today = integer(0), cnt_1d = integer(0), cnt_7d = integer(0),
    cnt_30d = integer(0), cnt_total = integer(0), first_seen = integer(0),
    stringsAsFactors = FALSE)

  list(
    release_exists = function() {
      st <- suppressWarnings(system2("gh",
        c("release", "view", "current", "--repo", PUBLISH_REPO),
        stdout = FALSE, stderr = FALSE))
      identical(as.integer(st), 0L)
    },
    release_download = function(pattern, dir) {
      for (i in seq_len(3L)) {
        st <- suppressWarnings(system2("gh",
          c("release", "download", "current", "--repo", PUBLISH_REPO,
            "--pattern", pattern, "--dir", dir, "--clobber"),
          stdout = TRUE, stderr = TRUE))
        code <- as.integer(attr(st, "status") %||% 0L)
        if (identical(code, 0L)) return(0L)
        if (i < 3L) Sys.sleep(3 * i)
      }
      code
    },
    list_packages = function() {
      all_names <- character(0)
      for (repo in AUTOCRAN_REPOS) {
        repo_url <- paste0(AUTOCRAN_REPO_BASE, "/", repo, "/")
        repomd <- tryCatch(rawToChar(with_retry(curl::curl_fetch_memory(
          paste0(repo_url, "repodata/repomd.xml"),
          handle = curl::new_handle(useragent = USER_AGENT, timeout = 60L)))$content),
          error = function(e) NULL)
        if (is.null(repomd)) { message("repomd unreachable: ", repo); next }
        href <- parse_repomd_primary_href(repomd)
        if (is.na(href)) { message("no primary href: ", repo); next }
        tf <- tempfile(fileext = ".xml.gz")
        ok <- tryCatch({ with_retry(curl::curl_download(paste0(repo_url, href), tf)); TRUE },
                       error = function(e) FALSE)
        if (!ok) { message("primary download failed: ", repo); next }
        zc <- gzfile(tf, "rt"); lines <- readLines(zc, warn = FALSE); close(zc); unlink(tf)
        all_names <- union(all_names, parse_primary_names(lines))
        message("enumerated ", repo, ": running union ", length(all_names))
      }
      all_names <- sort(unique(all_names), method = "radix")
      if (PACKAGE_LIMIT > 0L && length(all_names) > PACKAGE_LIMIT)
        all_names <- all_names[seq_len(PACKAGE_LIMIT)]
      all_names
    },
    resolve_ids = function(names) {
      if (length(names) == 0) return(data.frame(package = character(0), id = integer(0)))
      urls <- paste0(MIRRORCACHE_BASE, "/rest/package/",
                     vapply(names, curl::curl_escape, character(1)))
      res <- mc_multi(urls)
      ids <- vapply(res, function(x) if (is.null(x)) NA_integer_ else parse_id_response(x), integer(1))
      df  <- data.frame(package = names, id = ids, stringsAsFactors = FALSE)
      df[!is.na(df$id), , drop = FALSE]
    },
    fetch_stats = function(ids) {
      if (length(ids) == 0) return(empty_stats())
      urls   <- paste0(MIRRORCACHE_BASE, "/rest/package/", ids, "/stat_download")
      res    <- mc_multi(urls)
      parsed <- lapply(res, function(x) if (is.null(x)) parse_stat_response("") else parse_stat_response(x))
      data.frame(
        id         = as.integer(ids),
        cnt_today  = vapply(parsed, function(p) p$cnt_today,  integer(1)),
        cnt_1d     = vapply(parsed, function(p) p$cnt_1d,     integer(1)),
        cnt_7d     = vapply(parsed, function(p) p$cnt_7d,     integer(1)),
        cnt_30d    = vapply(parsed, function(p) p$cnt_30d,    integer(1)),
        cnt_total  = vapply(parsed, function(p) p$cnt_total,  integer(1)),
        first_seen = vapply(parsed, function(p) p$first_seen, integer(1)),
        stringsAsFactors = FALSE)
    },
    now = function() Sys.time())
}

if (sys.nframe() == 0L) {
  args       <- commandArgs(trailingOnly = TRUE)
  out_dir    <- if (length(args) >= 1) args[1] else "out"
  force_full <- tolower(Sys.getenv("AUTOOBS_FORCE_REBUILD", "")) %in% c("true", "1", "yes")
  res <- run_update(default_io(), out_dir, force_full = force_full)
  cat("Changed shards:", if (length(res$changed_shards))
        paste(res$changed_shards, collapse = ", ") else "(none)", "\n")
}
