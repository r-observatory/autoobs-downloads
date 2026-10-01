publish <- function(out, pub) {
  for (f in list.files(out)) {
    if (grepl("\\.(db|json)$", f)) file.copy(file.path(out, f), file.path(pub, f), overwrite = TRUE)
  }
}

# stats_df: a data frame of stat rows (id + counters); fetch_stats returns the rows
# for the requested ids, in order. resolve_ids resolves names via idmap and records
# how many names it was asked to resolve (to prove the id cache is reused). cran/bioc
# populate fixture cran_names_all/bioc_names_all DBs surfaced via identity_dbs(); the
# cran default strips "R-" from every enumerated name so tests are in-scope unless
# overridden. fail_identity simulates the identity assets being unreachable.
#
# The identity fixture DBs live in a tempdir that is NEVER unlinked here (no
# withr::defer): fake_io() is normally called as a bare argument expression at
# the call site (`run_update(fake_io(...), out1, ...)`), and a deferred cleanup
# registered against parent.frame() has, in sibling producers' conversions,
# fired before run_update's later identity_dbs() call ever reads the files --
# silently degrading every resolution in the test. Leftover tempdirs are
# reclaimed by the OS/session temp cleanup.
fake_io <- function(pub, names, idmap, stats_df, now, log = NULL,
                    stats_ok = TRUE, list_ok = TRUE, only_pkgs = NULL, loc_fail = FALSE,
                    cran = sub("^R-", "", names), bioc = character(0),
                    fail_identity = FALSE, assets_ok = TRUE) {
  ident_dir <- tempfile("identity-dbs-"); dir.create(ident_dir)
  cran_db <- file.path(ident_dir, "cran-archive.db")
  bioc_db <- file.path(ident_dir, "bioc-meta.db")
  .write_names_db(cran_db, "cran_names_all", cran)
  .write_names_db(bioc_db, "bioc_names_all", bioc)
  list(
    release_exists   = function() file.exists(file.path(pub, "manifest.json")),
    release_download = function(pattern, dir) {
      src <- file.path(pub, pattern)
      if (file.exists(src)) { file.copy(src, file.path(dir, pattern), overwrite = TRUE); 0L } else 1L
    },
    release_asset_names = function() {
      if (!assets_ok) stop("release listing failed")
      list.files(pub)
    },
    list_packages = function() if (list_ok) names else character(0),
    resolve_ids = function(nm) {
      if (!is.null(log)) log$resolved <- c(log$resolved, length(nm))
      data.frame(package = nm, id = as.integer(idmap[nm]), stringsAsFactors = FALSE)
    },
    fetch_stats = function(ids) {
      if (!stats_ok) return(NULL)
      stats_df[match(ids, stats_df$id), , drop = FALSE]
    },
    fetch_locations = function(nm) {
      if (!is.null(log)) log$located <- c(log$located, length(nm))
      ao <- if (loc_fail) rep(NA_integer_, length(nm))
            else if (is.null(only_pkgs)) rep(1L, length(nm))
            else as.integer(nm %in% only_pkgs)
      data.frame(package = nm, autocran_only = ao, stringsAsFactors = FALSE)
    },
    identity_dbs = function() {
      if (isTRUE(fail_identity)) stop("identity assets unreachable")
      list(cran = cran_db, bioc = bioc_db)
    },
    now = function() now)
}

day_stats <- function(...) do.call(rbind, list(...))

test_that("run_update bootstraps, builds the daily series, reuses the id cache", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-AER")
  idmap <- c("R-Rcpp" = 100L, "R-AER" = 200L)
  log <- new.env()

  s1 <- day_stats(stats_row(100, cnt_1d = 10, 70, 300, 1000),
                  stats_row(200, cnt_1d = 0,   5,  20,   50))
  out1 <- file.path(tmp, "out1")
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC"), log), out1,
             cran_floor = 1L, bioc_floor = 0L)
  expect_true(file.exists(file.path(out1, "autoobs-downloads-2026.db")))
  expect_true(file.exists(file.path(out1, "autoobs-downloads-recent.db")))
  expect_true(file.exists(file.path(out1, "autoobs-downloads-summary.db")))
  expect_equal(log$resolved, 2L)             # both names resolved on the cold run
  publish(out1, pub)

  con1 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-recent.db"))
  d1 <- DBI::dbGetQuery(con1, "SELECT * FROM autoobs_downloads_daily")
  expect_equal(nrow(d1), 1L)                 # only R-Rcpp had a positive cnt_1d
  expect_equal(d1$date, "2026-06-10")        # attributed to the completed day
  expect_equal(d1$count, 10L)
  pk <- DBI::dbGetQuery(con1, "SELECT * FROM autoobs_packages")
  expect_equal(nrow(pk), 2L)
  DBI::dbDisconnect(con1)

  # Day 2: same names -> the cache covers them -> resolve_ids gets an empty set.
  s2 <- day_stats(stats_row(100, cnt_1d = 12, 80, 320, 1012),
                  stats_row(200, cnt_1d = 3,  8,  23,   53))
  out2 <- file.path(tmp, "out2")
  log2 <- new.env()
  run_update(fake_io(pub, names, idmap, s2, as.POSIXct("2026-06-12 04:00:00", tz = "UTC"), log2), out2,
             cran_floor = 1L, bioc_floor = 0L)
  expect_null(log2$resolved)                 # no new names -> resolve_ids never called

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con2))
  d2 <- DBI::dbGetQuery(con2, "SELECT * FROM autoobs_downloads_daily ORDER BY package, date")
  expect_equal(nrow(d2), 3L)                 # R-Rcpp 06-10 & 06-11, R-AER 06-11
  expect_equal(d2$count[d2$package == "R-AER"], 3L)
  s <- DBI::dbGetQuery(con2, "SELECT * FROM autoobs_downloads_summary")
  expect_equal(nrow(s), 2L)
  expect_equal(s$total_1d[s$package == "R-Rcpp"], 12L)
})

test_that("run_update falls back to the cached package set when enumeration fails", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  s1 <- stats_row(100, cnt_1d = 10, 70, 300, 1000)
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC")),
             file.path(tmp, "out1"), cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "out1"), pub)

  out2 <- file.path(tmp, "out2")
  s2 <- stats_row(100, cnt_1d = 12, 80, 320, 1012)
  res <- run_update(fake_io(pub, names, idmap, s2, as.POSIXct("2026-06-12 04:00:00", tz = "UTC"),
                            list_ok = FALSE), out2,
                    cran_floor = 1L, bioc_floor = 0L)   # repodata enumeration "down"
  expect_true("autoobs-downloads-summary.db" %in% res$changed_shards)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_downloads_daily")$n, 2L)
})

test_that("run_update classifies autocran_only and refreshes new names only within the window", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-AER"); idmap <- c("R-Rcpp" = 100L, "R-AER" = 200L)
  s1 <- day_stats(stats_row(100, 10, 70, 300, 1000), stats_row(200, 5, 40, 200, 500))
  log1 <- new.env()
  # R-Rcpp is shared (also elsewhere), R-AER is autoCRAN-only.
  out1 <- file.path(tmp, "out1")
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC"),
                     log = log1, only_pkgs = "R-AER"), out1, cran_floor = 1L, bioc_floor = 0L)
  expect_equal(log1$located, 2L)              # cold run classifies every name

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-recent.db"))
  s <- DBI::dbGetQuery(con, "SELECT package, autocran_only FROM autoobs_downloads_summary ORDER BY package")
  expect_equal(s$autocran_only[s$package == "R-AER"], 1L)
  expect_equal(s$autocran_only[s$package == "R-Rcpp"], 0L)
  pk <- DBI::dbGetQuery(con, "SELECT package, autocran_only FROM autoobs_packages ORDER BY package")
  expect_equal(nrow(pk), 2L)
  DBI::dbDisconnect(con)
  man <- jsonlite::fromJSON(file.path(out1, "manifest.json"), simplifyVector = FALSE)
  expect_false(is.null(man$last_classified))
  expect_equal(man$summary$autocran_only, 1L)
  publish(out1, pub)

  # Day 2 within the refresh window: no new names -> no classification calls.
  log2 <- new.env()
  out2 <- file.path(tmp, "out2")
  run_update(fake_io(pub, names, idmap,
                     day_stats(stats_row(100, 11, 71, 301, 1001), stats_row(200, 6, 41, 201, 501)),
                     as.POSIXct("2026-06-12 04:00:00", tz = "UTC"), log = log2, only_pkgs = "R-AER"), out2,
             cran_floor = 1L, bioc_floor = 0L)
  expect_null(log2$located)                   # cache still fresh -> package_locations not hit
  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con2))
  s2 <- DBI::dbGetQuery(con2, "SELECT package, autocran_only FROM autoobs_downloads_summary")
  expect_equal(s2$autocran_only[s2$package == "R-AER"], 1L)   # carried in the cache
})

test_that("a refresh where every location lookup fails does not stamp the weekly clock", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-AER"; idmap <- c("R-AER" = 200L)
  s <- stats_row(200, 5, 40, 200, 500)
  run_update(fake_io(pub, names, idmap, s, as.POSIXct("2026-06-11 04:00:00", tz = "UTC"),
                     loc_fail = TRUE), file.path(tmp, "out1"), cran_floor = 1L, bioc_floor = 0L)
  man <- jsonlite::fromJSON(file.path(tmp, "out1", "manifest.json"), simplifyVector = FALSE)
  expect_null(man$last_classified)            # nothing classified -> clock not stamped

  # Next run still treats it as due and classifies for real.
  publish(file.path(tmp, "out1"), pub)
  log2 <- new.env()
  run_update(fake_io(pub, names, idmap, s, as.POSIXct("2026-06-12 04:00:00", tz = "UTC"),
                     log = log2), file.path(tmp, "out2"), cran_floor = 1L, bioc_floor = 0L)
  expect_equal(log2$located, 1L)              # retried because the prior refresh did not stamp
})

test_that("run_update re-runs a full classification after the refresh window", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-AER"; idmap <- c("R-AER" = 200L)
  run_update(fake_io(pub, names, idmap, stats_row(200, 5, 40, 200, 500),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "out1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "out1"), pub)

  log2 <- new.env()
  run_update(fake_io(pub, names, idmap, stats_row(200, 6, 41, 201, 501),
                     as.POSIXct("2026-06-20 04:00:00", tz = "UTC"), log = log2), file.path(tmp, "out2"),
             cran_floor = 1L, bioc_floor = 0L)
  expect_equal(log2$located, 1L)              # >7 days later -> full re-classify
})

test_that("run_update drops all-NA stat rows so a failed fetch never publishes NULL windows", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-AER"); idmap <- c("R-Rcpp" = 100L, "R-AER" = 200L)
  s1 <- day_stats(stats_row(100, cnt_1d = 10, 70, 300, 1000), na_stats_row(200))
  out1 <- file.path(tmp, "out1")
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), out1,
             cran_floor = 1L, bioc_floor = 0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT package FROM autoobs_downloads_summary")
  expect_equal(s$package, "R-Rcpp")          # the all-NA R-AER row is excluded
  # R-AER is still resolved and cached for future runs.
  expect_true("R-AER" %in% DBI::dbGetQuery(con, "SELECT package FROM autoobs_packages")$package)
})

test_that("run_update aborts when the release exists but the recent shard cannot be downloaded", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 1000),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "out1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "out1"), pub)

  io <- fake_io(pub, names, idmap, stats_row(100, 12, 80, 320, 1012),
                as.POSIXct("2026-06-12 04:00:00", tz = "UTC"))
  io$release_download <- function(pattern, dir) {           # recent download fails
    if (grepl("recent", pattern)) return(1L)
    src <- file.path(pub, pattern)
    if (file.exists(src)) { file.copy(src, file.path(dir, pattern), overwrite = TRUE); 0L } else 1L
  }
  expect_error(run_update(io, file.path(tmp, "out2"), cran_floor = 1L, bioc_floor = 0L),
               "protect accumulated history")
})

test_that("run_update heartbeats when MirrorCache stats are unavailable", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 1000),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "out1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "out1"), pub)

  out2 <- file.path(tmp, "out2")
  res <- run_update(fake_io(pub, names, idmap, NULL,
                            as.POSIXct("2026-06-12 04:00:00", tz = "UTC"), stats_ok = FALSE), out2,
                    cran_floor = 1L, bioc_floor = 0L)
  expect_equal(res$changed_shards, "autoobs-downloads-recent.db")   # only its run row changed
  man <- jsonlite::fromJSON(file.path(out2, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$source_kind, "frozen")
  expect_false(man$counters$published)
  prior <- jsonlite::fromJSON(file.path(pub, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$db_sha256, prior$db_sha256)       # summary shard untouched, binding intact
  expect_false(file.exists(file.path(out2, "autoobs-downloads-summary.db")))
  expect_false(file.exists(file.path(out2, COUNTERS_ASSET)))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  runs <- DBI::dbGetQuery(con, "SELECT outcome, reason, stats_requested FROM autoobs_runs ORDER BY run_id")
  expect_equal(runs$outcome, c("ok", "heartbeat"))
  expect_equal(runs$reason[2], "no stats fetched")
  expect_equal(runs$stats_requested[2], 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_downloads_daily")$n, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_packages")$n, 1L)
})

test_that("run_update with no prior release and no stats errors", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  expect_error(
    run_update(fake_io(pub, "R-Rcpp", c("R-Rcpp" = 100L), NULL,
                       as.POSIXct("2026-06-11 04:00:00", tz = "UTC"), stats_ok = FALSE),
               file.path(tmp, "out"), cran_floor = 1L, bioc_floor = 0L),
    "cannot bootstrap")
})

test_that("run_update is idempotent on a same-day re-run", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  run_update(fake_io(pub, names, idmap, stats_row(100, cnt_1d = 10, 70, 300, 1000),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o1"), pub)

  out2 <- file.path(tmp, "o2")
  run_update(fake_io(pub, names, idmap, stats_row(100, cnt_1d = 13, 73, 303, 1003),
                     as.POSIXct("2026-06-11 18:00:00", tz = "UTC")), out2,
             cran_floor = 1L, bioc_floor = 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_downloads_daily WHERE package='R-Rcpp'")$n
  expect_equal(n, 1L)               # same attributed day replaced, not duplicated
  v <- DBI::dbGetQuery(con, "SELECT count FROM autoobs_downloads_daily")$count
  expect_equal(v, 13L)
})

test_that("run_update drops an out-of-scope package from the summary but keeps its raw counts", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-notacran"); idmap <- c("R-Rcpp" = 100L, "R-notacran" = 300L)
  s1 <- day_stats(stats_row(100, cnt_1d = 10, 70, 300, 1000),
                  stats_row(300, cnt_1d = 4,   8,  40,  120))
  out1 <- file.path(tmp, "out1")
  # cran fixture knows Rcpp only -> R-notacran resolves to origin='other'.
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC"),
                     cran = "Rcpp"), out1, cran_floor = 1L, bioc_floor = 0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT package, origin FROM autoobs_downloads_summary")
  expect_equal(s$package, "R-Rcpp")            # out-of-scope R-notacran not promoted
  expect_equal(s$origin, "cran")

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-2026.db"))
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  raw <- DBI::dbGetQuery(con2, "SELECT count FROM autoobs_downloads_daily WHERE package='R-notacran'")
  expect_equal(raw$count, 4L)                  # unfiltered raw retention in the year shard
  man <- jsonlite::fromJSON(file.path(out1, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$summary$out_of_scope, 1L)
  expect_equal(man$summary$packages, 1L)
})

test_that("autoobs_packages cache persists origin, canonical_name, identity_state", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-notacran"); idmap <- c("R-Rcpp" = 100L, "R-notacran" = 300L)
  s1 <- day_stats(stats_row(100, 10, 70, 300, 1000), stats_row(300, 4, 8, 40, 120))
  out1 <- file.path(tmp, "out1")
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC"),
                     cran = "Rcpp"), out1, cran_floor = 1L, bioc_floor = 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  pk <- DBI::dbGetQuery(con, "SELECT * FROM autoobs_packages ORDER BY package")
  expect_true(all(c("origin", "canonical_name", "identity_state") %in% names(pk)))
  expect_equal(pk$origin[pk$package == "R-Rcpp"], "cran")
  expect_equal(pk$canonical_name[pk$package == "R-Rcpp"], "Rcpp")
  expect_equal(pk$origin[pk$package == "R-notacran"], "other")   # classified, not promoted
  expect_true(is.na(pk$canonical_name[pk$package == "R-notacran"]))
})

test_that("run_update degrades to cached origins when the identity ledger is unreachable", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-AER"); idmap <- c("R-Rcpp" = 100L, "R-AER" = 200L)
  s1 <- day_stats(stats_row(100, 10, 70, 300, 1000), stats_row(200, 5, 40, 200, 500))
  out1 <- file.path(tmp, "out1")
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC")),
             out1, cran_floor = 1L, bioc_floor = 0L)
  publish(out1, pub)

  out2 <- file.path(tmp, "out2")
  s2 <- day_stats(stats_row(100, 12, 80, 320, 1012), stats_row(200, 6, 41, 201, 501))
  run_update(fake_io(pub, names, idmap, s2, as.POSIXct("2026-06-12 04:00:00", tz = "UTC"),
                     fail_identity = TRUE), out2, cran_floor = 1L, bioc_floor = 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con,
    "SELECT package, origin, canonical_name FROM autoobs_downloads_summary ORDER BY package")
  expect_equal(nrow(s), 2L)                      # both survive via cached origins
  expect_equal(s$origin[s$package == "R-Rcpp"], "cran")
  expect_equal(s$canonical_name[s$package == "R-AER"], "AER")
})

test_that("run_update aborts on a cold run when the identity ledger is unreachable", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  expect_error(
    run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 1000),
                       as.POSIXct("2026-06-11 04:00:00", tz = "UTC"), fail_identity = TRUE),
               file.path(tmp, "out1"), cran_floor = 1L, bioc_floor = 0L),
    "cold run")
  expect_false(file.exists(file.path(tmp, "out1", "manifest.json")))
})

test_that("run_update aborts on a cold run when the identity size gate fails", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  expect_error(
    run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 1000),
                       as.POSIXct("2026-06-11 04:00:00", tz = "UTC")),
               file.path(tmp, "out1"), cran_floor = 999999L, bioc_floor = 0L),
    "cold run")
})

test_that("raw counts survive in the shards while ranks stay dense over in-scope packages", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-AER", "R-base", "R-notacran")
  idmap <- c("R-Rcpp" = 100L, "R-AER" = 200L, "R-base" = 300L, "R-notacran" = 400L)
  s1 <- day_stats(
    stats_row(100, cnt_1d = 30, 200, 900, 9000),   # in-scope, biggest
    stats_row(200, cnt_1d = 10,  70, 300, 3000),   # in-scope
    stats_row(300, cnt_1d = 99, 700, 999, 9999),   # R-base runtime, out of scope
    stats_row(400, cnt_1d =  5,  40, 120, 1200))   # unknown token, out of scope
  out1 <- file.path(tmp, "out1")
  # cran fixture knows only Rcpp and AER -> R-base and R-notacran fall to 'other'.
  run_update(fake_io(pub, names, idmap, s1, as.POSIXct("2026-06-11 04:00:00", tz = "UTC"),
                     cran = c("Rcpp", "AER")), out1, cran_floor = 1L, bioc_floor = 0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con,
    "SELECT package, rank_30d FROM autoobs_downloads_summary ORDER BY rank_30d")
  expect_equal(s$package, c("R-Rcpp", "R-AER"))      # only in-scope, dense ranks
  expect_equal(s$rank_30d, c(1L, 2L))                # no gaps from the dropped rows

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, "autoobs-downloads-2026.db"))
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  raw <- DBI::dbGetQuery(con2,
    "SELECT package, count FROM autoobs_downloads_daily ORDER BY package")
  expect_true(all(c("R-base", "R-notacran") %in% raw$package))   # out-of-scope raw retained
  man <- jsonlite::fromJSON(file.path(out1, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$summary$in_scope + man$summary$out_of_scope, man$summary$raw_tracked)
  expect_equal(man$summary$out_of_scope, 2L)
})

test_that("an ok run records its run row in the recent and summary shards", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-AER", "R-notacran")
  idmap <- c("R-Rcpp" = 100L, "R-AER" = 200L, "R-notacran" = 300L)
  s1 <- day_stats(stats_row(100, cnt_1d = 10, 70, 300, 0, cnt_today = 2),
                  stats_row(200, cnt_1d = 0,   5,  20, 50),
                  na_stats_row(300))
  s1$responded <- c(1L, 1L, 0L)
  out1 <- file.path(tmp, "out1")
  now <- as.POSIXct("2026-06-11 04:10:00", tz = "UTC")
  run_update(fake_io(pub, names, idmap, s1, now), out1, cran_floor = 1L, bioc_floor = 0L)

  for (f in c("autoobs-downloads-recent.db", "autoobs-downloads-summary.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, f))
    r <- DBI::dbGetQuery(con, "SELECT * FROM autoobs_runs")
    DBI::dbDisconnect(con)
    expect_equal(nrow(r), 1L)
    expect_equal(r$run_id, as.integer(as.numeric(now)))
    expect_equal(r$run_at, "2026-06-11T04:10:00Z")
    expect_equal(r$snapshot_date, "2026-06-11")
    expect_equal(r$source, "run")
    expect_equal(r$outcome, "ok")
    expect_equal(r$names_listed, 3L)
    expect_equal(r$ids_cached, 0L)
    expect_equal(r$ids_new, 3L)
    expect_equal(r$stats_requested, 3L)
    expect_equal(r$stats_responded, 2L)
    expect_equal(r$stats_non_na, 2L)             # the all-NA row is dropped
    expect_equal(c(r$pos_today, r$pos_1d, r$pos_7d, r$pos_30d, r$pos_total), c(1L, 1L, 2L, 2L, 1L))
    expect_equal(c(r$sum_1d, r$sum_7d, r$sum_30d), c(10L, 75L, 320L))
    expect_equal(r$day_aggregated, 1L)
    expect_equal(r$window_end, "2026-06-10")
    expect_equal(r$in_scope, 2L)
    expect_equal(r$counters_prior, "none")
    expect_equal(r$counters_published, 1L)
  }
  man <- jsonlite::fromJSON(file.path(out1, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$tables$autoobs_runs, 1L)       # the integrity core sees the new table
})

test_that("run rows accumulate across ok, heartbeat and ok runs", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 1000),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o1"), pub)
  run_update(fake_io(pub, names, idmap, NULL, as.POSIXct("2026-06-12 04:00:00", tz = "UTC"),
                     stats_ok = FALSE), file.path(tmp, "o2"), cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o2"), pub)
  run_update(fake_io(pub, names, idmap, stats_row(100, 0, 60, 290, 1000),
                     as.POSIXct("2026-06-13 04:00:00", tz = "UTC")), file.path(tmp, "o3"),
             cran_floor = 1L, bioc_floor = 0L)
  for (f in c("autoobs-downloads-recent.db", "autoobs-downloads-summary.db")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, "o3", f))
    r <- DBI::dbGetQuery(con, "SELECT snapshot_date, outcome, day_aggregated, window_end
                                 FROM autoobs_runs ORDER BY run_id")
    DBI::dbDisconnect(con)
    expect_equal(r$snapshot_date, c("2026-06-11", "2026-06-12", "2026-06-13"))
    expect_equal(r$outcome, c("ok", "heartbeat", "ok"))
    expect_equal(r$day_aggregated, c(1L, NA, 0L))
    expect_equal(r$window_end, c("2026-06-10", NA, "2026-06-11"))   # S-2 when not aggregated
  }
})

test_that("counters append across runs and keep only the last 40 days", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-Rcpp", "R-notacran"); idmap <- c("R-Rcpp" = 100L, "R-notacran" = 300L)
  day1 <- as.POSIXct("2026-06-01 04:00:00", tz = "UTC")
  for (i in 0:40) {
    out <- file.path(tmp, sprintf("o%02d", i))
    run_update(fake_io(pub, names, idmap,
                       day_stats(stats_row(100, 10 + i, 70, 300, 0), stats_row(300, 1, 2, 3, 0)),
                       day1 + i * 86400, cran = "Rcpp"), out, cran_floor = 1L, bioc_floor = 0L)
    if (i == 0) {
      man <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
      expect_equal(man$counters$prior, "none")
    }
    publish(out, pub)
  }
  man <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  expect_true(man$counters$published)
  expect_equal(man$counters$prior, "loaded")
  expect_equal(man$counters$runs, 40L)
  expect_equal(man$counters$first_run, "2026-06-02T04:00:00Z")   # 06-01 fell out on 07-11
  expect_null(man$shards[[COUNTERS_ASSET]])                        # never under shards
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, COUNTERS_ASSET))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_counters")$n, 80L)
  last <- DBI::dbGetQuery(con, "SELECT package, cnt_1d FROM autoobs_counters
                                 WHERE run_id = (SELECT MAX(run_id) FROM autoobs_counters)
                                 ORDER BY package")
  expect_equal(last$package, c("R-Rcpp", "R-notacran"))           # out-of-scope rows kept
  expect_equal(last$cnt_1d, c(50L, 1L))
})

test_that("a failed or unreadable counters download never stops the run", {
  for (mode in c("fail", "garbage")) {
    tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
    names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
    run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 0),
                       as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
               cran_floor = 1L, bioc_floor = 0L)
    publish(file.path(tmp, "o1"), pub)
    if (mode == "garbage") writeLines("not a database", file.path(pub, COUNTERS_ASSET))
    io <- fake_io(pub, names, idmap, stats_row(100, 12, 80, 320, 0),
                  as.POSIXct("2026-06-12 04:00:00", tz = "UTC"))
    if (mode == "fail") {
      base_dl <- io$release_download
      io$release_download <- function(pattern, dir) if (pattern == COUNTERS_ASSET) 1L else base_dl(pattern, dir)
    }
    out2 <- file.path(tmp, "o2")
    res <- run_update(io, out2, cran_floor = 1L, bioc_floor = 0L)
    expect_true("autoobs-downloads-2026.db" %in% res$changed_shards)   # the day still lands
    expect_false(file.exists(file.path(out2, COUNTERS_ASSET)))         # nothing to upload
    man <- jsonlite::fromJSON(file.path(out2, "manifest.json"), simplifyVector = FALSE)
    expect_false(man$counters$published)
    expect_equal(man$counters$prior, "download_failed")
    expect_equal(man$counters$runs, 1L)                                # still describes the prior window
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "autoobs-downloads-recent.db"))
    r <- DBI::dbGetQuery(con, "SELECT counters_prior, counters_published FROM autoobs_runs ORDER BY run_id")
    d <- DBI::dbGetQuery(con, "SELECT date, count FROM autoobs_downloads_daily ORDER BY date")
    DBI::dbDisconnect(con)
    expect_equal(r$counters_prior, c("none", "download_failed"))
    expect_equal(r$counters_published, c(1L, 0L))
    expect_equal(d$count, c(10L, 12L))
  }
})

test_that("a listed counters asset missing from the release starts a new window", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 0),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o1"), pub)
  unlink(file.path(pub, COUNTERS_ASSET))       # a clobber upload that deleted and then failed

  out2 <- file.path(tmp, "o2")
  run_update(fake_io(pub, names, idmap, stats_row(100, 12, 80, 320, 0),
                     as.POSIXct("2026-06-12 04:00:00", tz = "UTC")), out2,
             cran_floor = 1L, bioc_floor = 0L)
  man <- jsonlite::fromJSON(file.path(out2, "manifest.json"), simplifyVector = FALSE)
  expect_true(man$counters$published)
  expect_equal(man$counters$prior, "none")
  expect_equal(man$counters$runs, 1L)

  out3 <- file.path(tmp, "o3")                 # listing itself fails: hold the upload
  run_update(fake_io(pub, names, idmap, stats_row(100, 12, 80, 320, 0),
                     as.POSIXct("2026-06-12 04:00:00", tz = "UTC"), assets_ok = FALSE), out3,
             cran_floor = 1L, bioc_floor = 0L)
  man3 <- jsonlite::fromJSON(file.path(out3, "manifest.json"), simplifyVector = FALSE)
  expect_false(man3$counters$published)
  expect_equal(man3$counters$prior, "download_failed")
})

test_that("runs on a release made before the run record start the record", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-Rcpp"; idmap <- c("R-Rcpp" = 100L)
  run_update(fake_io(pub, names, idmap, stats_row(100, 10, 70, 300, 0),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o1"), pub)
  # Make the published release look like one from before this change.
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(pub, "autoobs-downloads-recent.db"))
  DBI::dbExecute(con, "DROP TABLE autoobs_runs")
  DBI::dbDisconnect(con)
  man <- jsonlite::fromJSON(file.path(pub, "manifest.json"), simplifyVector = FALSE)
  man$counters <- NULL
  write_manifest(file.path(pub, "manifest.json"), man)
  unlink(file.path(pub, COUNTERS_ASSET))

  res <- run_update(fake_io(pub, names, idmap, NULL, as.POSIXct("2026-06-12 04:00:00", tz = "UTC"),
                            stats_ok = FALSE), file.path(tmp, "o2"), cran_floor = 1L, bioc_floor = 0L)
  expect_equal(res$changed_shards, "autoobs-downloads-recent.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, "o2", "autoobs-downloads-recent.db"))
  r <- DBI::dbGetQuery(con, "SELECT outcome, reason FROM autoobs_runs")
  DBI::dbDisconnect(con)
  expect_equal(r$outcome, "heartbeat")

  run_update(fake_io(pub, names, idmap, stats_row(100, 12, 80, 320, 0),
                     as.POSIXct("2026-06-12 04:00:00", tz = "UTC")), file.path(tmp, "o3"),
             cran_floor = 1L, bioc_floor = 0L)
  man3 <- jsonlite::fromJSON(file.path(tmp, "o3", "manifest.json"), simplifyVector = FALSE)
  expect_equal(man3$counters$prior, "none")
  expect_true(man3$counters$published)
})

# Runs the pipeline once per snapshot date against a truth series and publishes.
# `last` names, per snapshot date, the last day MirrorCache had counted when it is
# further behind than one day; `cran` is the in-scope set; `...` goes to run_update.
run_days <- function(pub, tmp, truth, ids, dates, unaggregated = character(0), hour = "04:00:00",
                     last = list(), cran = sub("^R-", "", names(ids)), ...) {
  res <- NULL
  for (S in dates) {
    out <- file.path(tmp, paste0("o", gsub("-", "", S), gsub(":", "", hour)))
    cn  <- mc_counters(truth, ids, S, aggregated = !(S %in% unaggregated), last = last[[S]])
    res <- run_update(fake_io(pub, names(ids), ids, cn[setdiff(names(cn), "package")],
                              as.POSIXct(paste(S, hour), tz = "UTC"), cran = cran),
                      out, cran_floor = 1L, bioc_floor = 0L, ...)
    publish(out, pub)
  }
  list(out = out, res = res)
}

test_that("run_update fills a missed day at the next aggregated run", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  ids <- c("R-a" = 1L, "R-b" = 2L)
  truth <- fill_truth("2026-06-01")
  dates <- format(seq(as.Date("2026-06-02"), as.Date("2026-06-11"), by = "day"))
  r <- run_days(pub, tmp, truth, ids, dates, unaggregated = "2026-06-10")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT package, count FROM autoobs_downloads_daily
                                WHERE date = '2026-06-09' ORDER BY package")
  want <- truth[truth$date == "2026-06-09", ]
  expect_equal(got$count, want$count[order(want$package)])
  days <- DBI::dbGetQuery(con, "SELECT date, method FROM autoobs_days ORDER BY date")
  expect_equal(days$date, format(seq(as.Date("2026-06-01"), as.Date("2026-06-10"), by = "day")))
  expect_equal(days$method[days$date == "2026-06-09"], "window")
  expect_equal(days$method[days$date == "2026-06-10"], "cnt_1d")
  runs <- DBI::dbGetQuery(con, "SELECT days_filled, fill_rejected, window_residual
                                 FROM autoobs_runs ORDER BY run_id DESC LIMIT 1")
  expect_equal(unlist(runs, use.names = FALSE), c(1L, 0L, 0L))
  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-2026.db"))
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_equal(DBI::dbGetQuery(con2, "SELECT method FROM autoobs_days WHERE date = '2026-06-09'")$method,
               "window")
  expect_equal(DBI::dbListFields(con2, "autoobs_downloads_daily"), c("package", "date", "count"))
})

test_that("a January run that fills a December day re-exports and lists that year's shard", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  ids <- c("R-a" = 1L, "R-b" = 2L)
  truth <- fill_truth("2025-12-20", "2026-01-02")
  dates <- format(seq(as.Date("2025-12-25"), as.Date("2026-01-02"), by = "day"))
  r <- run_days(pub, tmp, truth, ids, dates, unaggregated = "2026-01-01")
  expect_true(all(c("autoobs-downloads-2025.db", "autoobs-downloads-2026.db") %in% r$res$changed_shards))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-2025.db"))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT SUM(count) n FROM autoobs_downloads_daily
                                      WHERE date = '2025-12-31'")$n,
               sum(truth$count[truth$date == "2025-12-31"]))
  man <- jsonlite::fromJSON(file.path(r$out, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$shards[["autoobs-downloads-2025.db"]]$date_max, "2025-12-31")
})

test_that("a same-day rerun with failed fetches keeps the stored rows it missed", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- c("R-a", "R-b"); idmap <- c("R-a" = 1L, "R-b" = 2L)
  run_update(fake_io(pub, names, idmap, day_stats(stats_row(1, 10, 70, 300, 0), stats_row(2, 5, 9, 40, 0)),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o1"), pub)
  run_update(fake_io(pub, names, idmap, day_stats(stats_row(1, 13, 73, 303, 0), na_stats_row(2)),
                     as.POSIXct("2026-06-11 18:00:00", tz = "UTC")), file.path(tmp, "o2"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o2"), pub)
  run_update(fake_io(pub, names, idmap, day_stats(stats_row(1, 0, 60, 290, 0), stats_row(2, 0, 4, 35, 0)),
                     as.POSIXct("2026-06-11 20:00:00", tz = "UTC")), file.path(tmp, "o3"),
             cran_floor = 1L, bioc_floor = 0L)
  for (o in c("o2", "o3")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, o, "autoobs-downloads-recent.db"))
    d <- DBI::dbGetQuery(con, "SELECT package, count FROM autoobs_downloads_daily
                                WHERE date = '2026-06-10' ORDER BY package")
    DBI::dbDisconnect(con)
    expect_equal(d$package, c("R-a", "R-b"))
    expect_equal(d$count, c(13L, 5L))        # R-a replaced, R-b kept; the unaggregated rerun changes nothing
  }
})

test_that("the first run on a shard without autoobs_days bootstraps it from the stored dates", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-a"; idmap <- c("R-a" = 1L)
  run_update(fake_io(pub, names, idmap, stats_row(1, 10, 70, 300, 0),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o1"), pub)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(pub, "autoobs-downloads-recent.db"))
  DBI::dbExecute(con, "DROP TABLE autoobs_days")
  DBI::dbDisconnect(con)
  run_update(fake_io(pub, names, idmap, stats_row(1, 0, 70, 300, 0),
                     as.POSIXct("2026-06-12 04:00:00", tz = "UTC")), file.path(tmp, "o2"),
             cran_floor = 1L, bioc_floor = 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, "o2", "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con, "SELECT date, method, run_id FROM autoobs_days")
  expect_equal(d$date, "2026-06-10")
  expect_equal(d$method, "cnt_1d")
  expect_true(is.na(d$run_id))
})

test_that("run rows written before the fill columns existed are carried with NA", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  names <- "R-a"; idmap <- c("R-a" = 1L)
  run_update(fake_io(pub, names, idmap, stats_row(1, 10, 70, 300, 0),
                     as.POSIXct("2026-06-11 04:00:00", tz = "UTC")), file.path(tmp, "o1"),
             cran_floor = 1L, bioc_floor = 0L)
  publish(file.path(tmp, "o1"), pub)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(pub, "autoobs-downloads-recent.db"))
  DBI::dbExecute(con, "CREATE TABLE old_runs AS SELECT run_id, run_at, snapshot_date, source, outcome,
                         day_aggregated, window_end, counters_prior FROM autoobs_runs")
  DBI::dbExecute(con, "DROP TABLE autoobs_runs")
  DBI::dbExecute(con, "ALTER TABLE old_runs RENAME TO autoobs_runs")
  DBI::dbDisconnect(con)
  run_update(fake_io(pub, names, idmap, stats_row(1, 12, 80, 310, 0),
                     as.POSIXct("2026-06-12 04:00:00", tz = "UTC")), file.path(tmp, "o2"),
             cran_floor = 1L, bioc_floor = 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, "o2", "autoobs-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  r <- DBI::dbGetQuery(con, "SELECT snapshot_date, days_filled, stats_requested FROM autoobs_runs ORDER BY run_id")
  expect_equal(r$snapshot_date, c("2026-06-11", "2026-06-12"))
  expect_equal(r$days_filled, c(NA, 0L))
  expect_equal(r$stats_requested, c(NA, 1L))
})

test_that("a day MirrorCache later revises is not rewritten and shows in the residual", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  ids <- c("R-a" = 1L, "R-b" = 2L)
  truth <- fill_truth("2026-06-01")
  run_days(pub, tmp, truth, ids, format(seq(as.Date("2026-06-02"), as.Date("2026-06-11"), by = "day")))
  revised <- truth
  revised$count[revised$package == "R-a" & revised$date == "2026-06-08"] <-
    revised$count[revised$package == "R-a" & revised$date == "2026-06-08"] + 5L
  r <- run_days(pub, tmp, revised, ids, "2026-06-12")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  kept <- DBI::dbGetQuery(con, "SELECT count FROM autoobs_downloads_daily
                                 WHERE package = 'R-a' AND date = '2026-06-08'")$count
  expect_equal(kept, truth$count[truth$package == "R-a" & truth$date == "2026-06-08"])
  run <- DBI::dbGetQuery(con, "SELECT days_filled, window_residual FROM autoobs_runs
                                ORDER BY run_id DESC LIMIT 1")
  expect_equal(run$days_filled, 0L)
  expect_equal(run$window_residual, 1L)
})

test_that("the second of two unaggregated runs fills the first one's day", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  ids <- c("R-a" = 1L, "R-b" = 2L)
  truth <- fill_truth("2026-06-01", "2026-06-11")
  dates <- format(seq(as.Date("2026-06-02"), as.Date("2026-06-11"), by = "day"))
  r <- run_days(pub, tmp, truth, ids, dates, unaggregated = c("2026-06-10", "2026-06-11"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  days <- DBI::dbGetQuery(con, "SELECT date, method FROM autoobs_days WHERE date >= '2026-06-09'")
  expect_equal(days$date, "2026-06-09")              # 06-10 is still a hole
  expect_equal(days$method, "window")
  got <- DBI::dbGetQuery(con, "SELECT package, count FROM autoobs_downloads_daily
                                WHERE date = '2026-06-09' ORDER BY package")
  want <- truth[truth$date == "2026-06-09", ]
  expect_equal(got$count, want$count[order(want$package)])
})

test_that("a day MirrorCache counts two days late is never recorded as uncounted", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  ids <- c("R-a" = 1L, "R-b" = 2L)
  truth <- fill_truth("2026-05-01", "2026-06-20")
  dates <- format(seq(as.Date("2026-05-19"), as.Date("2026-06-17"), by = "day"))
  # 06-09 is uncounted at the 06-10 run and still uncounted at the 06-11 run.
  late <- list("2026-06-10" = "2026-06-08", "2026-06-11" = "2026-06-08")
  r <- run_days(pub, tmp, truth, ids, dates[dates <= "2026-06-12"], last = late)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-recent.db"))
  days <- DBI::dbGetQuery(con, "SELECT date, method FROM autoobs_days")
  runs <- DBI::dbGetQuery(con, "SELECT snapshot_date, days_filled, fill_rejected FROM autoobs_runs
                                 WHERE snapshot_date >= '2026-06-11' ORDER BY run_id")
  DBI::dbDisconnect(con)
  expect_false(any(c("2026-06-09", "2026-06-10") %in% days$date))   # both stay holes
  expect_false("upstream_missing" %in% days$method)
  expect_equal(runs$days_filled, c(0L, 0L))
  expect_equal(runs$fill_rejected, c(0L, 0L))

  # Later windows hold each day alone and fill it with the real counts.
  r <- run_days(pub, tmp, truth, ids, dates[dates > "2026-06-12"])
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  days <- DBI::dbGetQuery(con, "SELECT date, method FROM autoobs_days ORDER BY date")
  expect_equal(days$method[days$date %in% c("2026-06-09", "2026-06-10")], c("window", "window"))
  d <- DBI::dbGetQuery(con, "SELECT package, date, count FROM autoobs_downloads_daily")
  m <- merge(d, truth[truth$count > 0, ], by = c("package", "date"), all = TRUE)
  m <- m[m$date >= "2026-05-18" & m$date <= "2026-06-16", ]
  expect_false(anyNA(m$count.x))
  expect_equal(m$count.x, m$count.y)                 # every stored value is the truth
})

test_that("a MirrorCache more than a week behind never has a day recorded as uncounted", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  ids <- c("R-a" = 1L, "R-b" = 2L)
  truth <- fill_truth("2026-05-01", "2026-06-20")
  dates <- format(seq(as.Date("2026-05-19"), as.Date("2026-06-19"), by = "day"))
  # Nothing after 06-08 is counted until the 06-18 run: eight runs read the same windows.
  stale <- format(seq(as.Date("2026-06-10"), as.Date("2026-06-17"), by = "day"))
  late <- stats::setNames(rep(list("2026-06-08"), length(stale)), stale)
  r <- run_days(pub, tmp, truth, ids, dates[dates <= "2026-06-17"], last = late)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-recent.db"))
  days <- DBI::dbGetQuery(con, "SELECT date, method FROM autoobs_days")
  DBI::dbDisconnect(con)
  expect_equal(max(days$date), "2026-06-08")         # 06-09, eight days back, stays a hole
  expect_false("upstream_missing" %in% days$method)

  # MirrorCache catches up: the next windows must not read 06-09 as a zero day.
  r <- run_days(pub, tmp, truth, ids, dates[dates > "2026-06-17"])
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-recent.db"))
  on.exit(DBI::dbDisconnect(con))
  days <- DBI::dbGetQuery(con, "SELECT date, method FROM autoobs_days ORDER BY date")
  expect_equal(days$date[days$date > "2026-06-08"], c("2026-06-17", "2026-06-18"))
  expect_false("upstream_missing" %in% days$method)
  runs <- DBI::dbGetQuery(con, "SELECT SUM(fill_rejected) r, SUM(window_residual) w FROM autoobs_runs")
  expect_equal(c(runs$r, runs$w), c(0L, 0L))
  d <- DBI::dbGetQuery(con, "SELECT package, date, count FROM autoobs_downloads_daily")
  m <- merge(d, truth[truth$count > 0, ], by = c("package", "date"), all = TRUE)
  m <- m[m$date %in% days$date, ]
  expect_false(anyNA(m$count.x))
  expect_equal(m$count.x, m$count.y)                 # every stored value is the truth
})

test_that("an unaggregated run leaves total_1d NULL and the summary has no rank_total", {
  tmp <- withr::local_tempdir(); pub <- file.path(tmp, "pub"); dir.create(pub)
  ids <- c("R-a" = 1L, "R-b" = 2L)
  truth <- fill_truth("2026-06-01")
  dates <- format(seq(as.Date("2026-06-02"), as.Date("2026-06-10"), by = "day"))
  r <- run_days(pub, tmp, truth, ids, dates, unaggregated = "2026-06-10")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(r$out, "autoobs-downloads-summary.db"))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT total_1d, total_7d FROM autoobs_downloads_summary")
  expect_true(all(is.na(s$total_1d)))
  expect_true(all(s$total_7d > 0))
  expect_false("rank_total" %in% DBI::dbListFields(con, "autoobs_downloads_summary"))
})
