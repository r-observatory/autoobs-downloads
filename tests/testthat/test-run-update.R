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
                    fail_identity = FALSE) {
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
  expect_length(res$changed_shards, 0L)
  man <- jsonlite::fromJSON(file.path(out2, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$source_kind, "frozen")
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
