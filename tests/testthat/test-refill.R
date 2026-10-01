# Rebuilding past days from the dated observatory.db snapshots.
ids2 <- c("R-a" = 1L, "R-b" = 2L)

test_that("an aggregated snapshot must match the stored day before it", {
  truth <- fill_truth("2026-05-01", "2026-06-20")
  daily <- truth[truth$count > 0 & truth$date <= "2026-06-10", ]
  known <- unique(daily$date)
  mk <- function(S, agg = TRUE) {
    cn <- mc_counters(truth, ids2, S, aggregated = agg)
    rid <- as.integer(as.numeric(as.POSIXct(S, tz = "UTC")))
    list(run = run_row(c(list(run_id = rid, snapshot_date = S, source = "observatory.db x", outcome = "ok"),
                         counter_stats(cn))),
         cn = cbind(cn[setdiff(names(cn), c("id", "first_seen"))], run_id = rid))
  }
  good <- mk("2026-06-08"); bad <- mk("2026-06-09"); unk <- mk("2026-06-12"); flat <- mk("2026-06-13", FALSE)
  bad$cn$cnt_1d[1] <- bad$cn$cnt_1d[1] + 1L
  runs <- rbind(good$run, bad$run, unk$run, flat$run)
  v <- validate_snapshots(runs, rbind(good$cn, bad$cn, unk$cn, flat$cn), daily, known)
  expect_equal(v$keep, c(TRUE, FALSE, FALSE, TRUE))
  expect_match(v$reason[2], "differs")
  expect_match(v$reason[3], "not stored")
})

test_that("a difference of two runs uses only packages both runs counted", {
  runs <- normalize_runs(data.frame(run_id = c(1L, 2L), snapshot_date = c("2026-06-10", "2026-06-11"),
                                    source = c("observatory.db a", "run"), outcome = "ok",
                                    day_aggregated = 1L, window_end = c("2026-06-09", "2026-06-10")))
  cn <- data.frame(run_id = c(1L, 2L, 2L), package = c("R-a", "R-a", "R-new"),
                   cnt_today = NA, cnt_1d = 1L, cnt_7d = c(70L, 72L, 9L), cnt_30d = c(300L, 305L, 9L))
  eqs <- diff_equations(runs, cn)
  expect_length(eqs, 2L)
  expect_equal(eqs[[1]]$value$package, "R-a")          # R-new is not differenced against 0
  expect_equal(eqs[[1]]$value$value, 2)
  expect_equal(eqs[[1]]$coef[["2026-06-10"]], 1)
  expect_equal(eqs[[1]]$coef[["2026-06-03"]], -1)
  expect_equal(length(eqs[[1]]$coef), 2L)
  expect_equal(eqs[[1]]$method, "release")
  expect_null(eqs[[1]]$soft)                           # both runs saw the day before counted
  runs$day_aggregated[2] <- 0L; runs$window_end[2] <- "2026-06-09"
  soft <- diff_equations(runs, cn)[[1]]$soft
  expect_equal(soft, c("2026-06-09" = 2L))             # the later run's last day is unsure
})

test_that("strict mode refuses a day another equation contradicts", {
  truth <- fill_truth("2026-06-01", "2026-06-11")
  cn <- mc_counters(truth, ids2, "2026-06-11")
  cn2 <- mc_counters(truth, ids2, "2026-06-12")
  cn2$cnt_7d[cn2$package == "R-a"] <- cn2$cnt_7d[cn2$package == "R-a"] + 3L
  eqs <- c(run_equations(cn, "2026-06-11", "2026-06-10"), run_equations(cn2, "2026-06-12", "2026-06-11"))
  daily <- truth[truth$date != "2026-06-09" & truth$count > 0, ]
  known <- setdiff(unique(truth$date), "2026-06-09")
  lax <- solve_days(eqs, daily, known, floor = "2026-06-01", run_id = 1L)
  expect_equal(lax$days$date, "2026-06-09")
  expect_equal(lax$residual, 1L)
  strict <- solve_days(eqs, daily, known, floor = "2026-06-01", run_id = 1L, strict = TRUE)
  expect_equal(nrow(strict$days), 0L)
  expect_equal(strict$rejected, 1L)
})

test_that("refill_solve drops a snapshot that disagrees and fills from the kept ones", {
  truth <- fill_truth("2026-05-01", "2026-06-20")
  daily <- truth[truth$count > 0 & truth$date <= "2026-06-10" & truth$date != "2026-06-09", ]
  days <- bootstrap_days(daily)
  mk <- function(S) {
    cn <- mc_counters(truth, ids2, S)
    rid <- as.integer(as.numeric(as.POSIXct(S, tz = "UTC")))
    list(run = run_row(c(list(run_id = rid, snapshot_date = S, source = "observatory.db x",
                              outcome = "ok", window_end = window_end_for(S, 1L)), counter_stats(cn))),
         cn = cbind(cn[setdiff(names(cn), c("id", "first_seen"))], run_id = rid))
  }
  good <- mk("2026-06-11"); bad <- mk("2026-06-08")
  bad$cn$cnt_1d[1] <- bad$cn$cnt_1d[1] + 1L
  rf <- refill_solve(normalize_runs(data.frame()), empty_counters(), rbind(good$run, bad$run),
                     rbind(good$cn, bad$cn), daily, days, run_id = 3L)
  expect_equal(rf$kept$run_id, good$run$run_id)
  expect_equal(rf$dropped$run_id, bad$run$run_id)
  expect_match(rf$dropped$reason, "differs")
  expect_equal(rf$days$date, "2026-06-09")
  want <- truth[truth$date == "2026-06-09" & truth$count > 0, ]
  expect_equal(rf$rows[order(rf$rows$package), "count"], want$count[order(want$package)])
})

test_that("a run whose last window day is unsure is not trusted until that day is confirmed", {
  truth <- fill_truth("2026-05-01", "2026-06-20")
  holes <- c("2026-06-09", "2026-06-10")
  stored_to <- function(to) truth[truth$count > 0 & truth$date <= to & !(truth$date %in% holes), ]
  solve <- function(snaps, daily) {
    refill_solve(normalize_runs(data.frame()), empty_counters(),
                 do.call(rbind, lapply(snaps, `[[`, "run")), do.call(rbind, lapply(snaps, `[[`, "cn")),
                 daily, bootstrap_days(daily), run_id = 9L)
  }
  # MirrorCache two days late on 06-11: its windows still end on 06-08, like the 06-10 run's.
  s <- list(snapshot_fixture(truth, ids2, "2026-06-10", last = "2026-06-08"),
            snapshot_fixture(truth, ids2, "2026-06-11", last = "2026-06-08"),
            snapshot_fixture(truth, ids2, "2026-06-12"), snapshot_fixture(truth, ids2, "2026-06-13"),
            snapshot_fixture(truth, ids2, "2026-06-14"))
  rf <- solve(s, stored_to("2026-06-13"))
  expect_equal(nrow(rf$kept), 5L)
  expect_equal(nrow(rf$days), 0L)          # 06-09 is not a zero day and 06-10 is not the sum of both
  expect_equal(nrow(rf$rows), 0L)
  expect_equal(rf$rejected, 0L)

  # Later snapshots hold each day alone; the 06-11 run's windows stay out of the check.
  s2 <- c(s, lapply(c("2026-06-15", "2026-06-16", "2026-06-17", "2026-06-18"),
                    function(S) snapshot_fixture(truth, ids2, S)))
  rf2 <- solve(s2, stored_to("2026-06-17"))
  expect_setequal(rf2$days$date, holes)
  expect_equal(unique(rf2$days$method), "release")
  expect_equal(rf2$rejected, 0L)
  m <- merge(rf2$rows, truth, by = c("package", "date"))
  expect_equal(nrow(m), 4L)
  expect_equal(m$count.x, m$count.y)
})

test_that("a day filled later does not confirm an earlier run's unsure last day", {
  days <- data.frame(date = c("2026-06-08", "2026-06-09"), method = c("cnt_1d", "window"),
                     run_id = c(NA, 900L), packages = 2L, downloads = 9L)
  held <- days_since(days)
  expect_equal(held[["2026-06-08"]], as.integer(as.numeric(as.POSIXct("2026-06-09", tz = "UTC"))))
  expect_equal(held[["2026-06-09"]], 900L)

  truth <- fill_truth("2026-06-01", "2026-06-12")
  daily <- truth[truth$count > 0 & truth$date != "2026-06-05", ]
  known <- setdiff(unique(truth$date), "2026-06-05")
  # Run 500 read its windows two days late (they end on 06-08) and took 06-09 as their end.
  unsure <- run_equations(mc_counters(truth, ids2, "2026-06-11", last = "2026-06-08"), "2026-06-11",
                          "2026-06-09", sure_end = FALSE, run_id = 500L)
  sure <- run_equations(mc_counters(truth, ids2, "2026-06-12"), "2026-06-12", "2026-06-11")
  since <- stats::setNames(rep(0L, length(known)), known)
  since[["2026-06-09"]] <- 900L                        # 06-09 was filled by a later run
  alone <- solve_days(unsure, daily, known, floor = "2026-06-01", run_id = 1L, strict = TRUE, since = since)
  expect_equal(nrow(alone$days), 0L)                   # run 500's windows solve nothing
  both <- solve_days(c(unsure, sure), daily, known, floor = "2026-06-01", run_id = 1L,
                     strict = TRUE, since = since)
  expect_equal(both$days$date, "2026-06-05")           # and they do not veto the sure run
  expect_equal(both$rejected, 0L)
  expect_equal(both$residual, 0L)
  expect_equal(sum(both$rows$count), sum(truth$count[truth$date == "2026-06-05"]))
  since[["2026-06-09"]] <- 400L                        # held before run 500: its windows count
  trusted <- solve_days(c(unsure, sure), daily, known, floor = "2026-06-01", run_id = 1L,
                        strict = TRUE, since = since)
  expect_equal(nrow(trusted$days), 0L)
  expect_equal(trusted$rejected, 1L)
})

test_that("a refill solves only over the packages the rebuilt runs cover", {
  ids3 <- c(ids2, "R-x" = 3L)
  truth <- with_outsider(fill_truth("2026-05-01", "2026-06-20"))
  holes <- c("2026-06-09", "2026-06-12")
  daily <- truth[truth$count > 0 & truth$date <= "2026-06-19" & !(truth$date %in% holes), ]
  rebuilt <- snapshot_fixture(truth[truth$package != "R-x", ], ids2, "2026-06-11")
  real <- snapshot_fixture(truth, ids3, "2026-06-20", source = "run", hour = 4L)
  rf <- refill_solve(real$run, real$cn, rebuilt$run, rebuilt$cn, daily, bootstrap_days(daily), run_id = 9L)
  expect_setequal(rf$covered, c("R-a", "R-b"))
  expect_setequal(rf$days$date, holes)     # 06-09 from the rebuilt run, 06-12 from the real run's 30 days
  expect_equal(unique(rf$days$method), "release")
  expect_equal(rf$rejected, 0L)
  expect_equal(rf$residual, 0L)
  expect_false("R-x" %in% rf$rows$package) # not read as 0 on 06-09, so not overstated on 06-12
  expect_equal(nrow(rf$left_out), 0L)      # no snapshot holds R-x, so there is nothing to leave out
  m <- merge(rf$rows, truth, by = c("package", "date"))
  expect_equal(nrow(m), nrow(rf$rows))
  expect_equal(m$count.x, m$count.y)
})

test_that("a package is covered only when every later snapshot holds it", {
  runs <- normalize_runs(data.frame(run_id = 1:4, snapshot_date = sprintf("2026-06-%02d", 10:13),
                                    source = "observatory.db x", outcome = "ok",
                                    day_aggregated = c(1L, 1L, 0L, 1L)))
  # R-a is in all four, R-x in the first two, R-g misses the third, R-n and R-s arrive late.
  cn <- data.frame(run_id  = c(1:4, 1:2, c(1L, 2L, 4L), 3:4, 4L),
                   package = c(rep("R-a", 4), rep("R-x", 2), rep("R-g", 3), rep("R-n", 2), "R-s"),
                   stringsAsFactors = FALSE)
  # R-s has a stored row on 06-10, the day before the second snapshot, which does not hold it.
  daily <- data.frame(package = c("R-a", "R-x", "R-s"), date = c("2026-06-09", "2026-06-12", "2026-06-10"),
                      count = 1L, stringsAsFactors = FALSE)
  cov <- snapshot_cover(runs, cn, daily)
  expect_setequal(cov$covered, c("R-a", "R-n"))        # a package first held later is covered
  lo <- cov$left_out[order(cov$left_out$package), ]
  expect_equal(lo$package, c("R-g", "R-s", "R-x"))
  expect_equal(lo$first_snapshot, c("2026-06-10", "2026-06-13", "2026-06-10"))
  expect_equal(lo$missing_from, c("2026-06-12", "2026-06-11", "2026-06-12"))
  expect_equal(lo$reason, c("held by an earlier snapshot", "stored on the day before",
                            "held by an earlier snapshot"))
  none <- snapshot_cover(runs[0, ], cn[0, ], daily)
  expect_equal(none$covered, character(0))
  expect_equal(nrow(none$left_out), 0L)
})

test_that("a refill leaves out a package only the early snapshots hold", {
  ids3 <- c(ids2, "R-x" = 3L)
  truth <- with_outsider(fill_truth("2026-05-01", "2026-06-20"))
  holes <- c("2026-06-07", "2026-06-12")
  daily <- truth[truth$count > 0 & truth$date <= "2026-06-19" & !(truth$date %in% holes), ]
  # The snapshots up to 06-09 hold every tracked package, the later ones only R-a and R-b.
  snaps <- list(snapshot_fixture(truth, ids3, "2026-06-08", last = "2026-06-06"),
                snapshot_fixture(truth, ids3, "2026-06-09"),
                snapshot_fixture(truth, ids2, "2026-06-10"), snapshot_fixture(truth, ids2, "2026-06-11"),
                snapshot_fixture(truth, ids2, "2026-06-13", last = "2026-06-11"),
                snapshot_fixture(truth, ids2, "2026-06-14"))
  imp_runs <- do.call(rbind, lapply(snaps, `[[`, "run"))
  imp_cn <- do.call(rbind, lapply(snaps, `[[`, "cn"))
  real <- snapshot_fixture(truth, ids3, "2026-06-20", source = "run", hour = 4L)
  rf <- refill_solve(real$run, real$cn, imp_runs, imp_cn, daily, bootstrap_days(daily), run_id = 9L)
  expect_equal(nrow(rf$kept), 6L)
  expect_setequal(rf$covered, c("R-a", "R-b"))
  expect_equal(rf$left_out$package, "R-x")
  expect_equal(rf$left_out$missing_from, "2026-06-10")
  expect_setequal(rf$days$date, holes)     # 06-07 from an early snapshot, 06-12 from a later one
  expect_equal(unique(rf$days$method), "release")
  expect_equal(rf$rejected, 0L)            # the pipeline run's R-x counters are not set against 06-12
  expect_equal(rf$residual, 0L)
  expect_false("R-x" %in% rf$rows$package) # nor rebuilt on 06-07 alone
  m <- merge(rf$rows, truth, by = c("package", "date"))
  expect_equal(nrow(m), nrow(rf$rows))
  expect_equal(m$count.x, m$count.y)

  # Once a release day is stored, an earlier refill's choices hold: what it left out stays out.
  days <- rbind(bootstrap_days(daily), data.frame(date = "2026-04-30", method = "release", run_id = 1L,
                                                  packages = 0L, downloads = 0L))
  again <- refill_solve(real$run, real$cn, imp_runs, imp_cn, daily, days, run_id = 9L,
                        covered = c("R-a", "R-b", "R-x"), skip = "R-b")
  expect_equal(again$covered, "R-a")       # R-x has a gap, R-b was left out before
  expect_setequal(unique(again$rows$package), "R-a")
  # With no release day stored, nothing binds the refill.
  fresh <- refill_solve(real$run, real$cn, imp_runs, imp_cn, daily, bootstrap_days(daily), run_id = 9L,
                        skip = "R-b")
  expect_setequal(fresh$covered, c("R-a", "R-b"))
})

test_that("a refill leaves out a package the stored series held before the snapshots did", {
  ids3 <- c(ids2, "R-x" = 3L)
  truth <- with_outsider(fill_truth("2026-05-01", "2026-06-20"))
  daily <- truth[truth$count > 0 & truth$date <= "2026-06-19" & truth$date != "2026-06-07", ]
  # R-x was tracked all along and entered the summary on 06-10.
  snaps <- list(snapshot_fixture(truth, ids2, "2026-06-08", last = "2026-06-06"),
                snapshot_fixture(truth, ids2, "2026-06-09"),
                snapshot_fixture(truth, ids3, "2026-06-10"), snapshot_fixture(truth, ids3, "2026-06-11"))
  real <- snapshot_fixture(truth, ids3, "2026-06-20", source = "run", hour = 4L)
  rf <- refill_solve(real$run, real$cn, do.call(rbind, lapply(snaps, `[[`, "run")),
                     do.call(rbind, lapply(snaps, `[[`, "cn")), daily, bootstrap_days(daily), run_id = 9L)
  expect_setequal(rf$covered, c("R-a", "R-b"))
  expect_equal(rf$left_out$package, "R-x")
  expect_equal(rf$left_out$missing_from, "2026-06-09")   # stored on 06-08, not in that snapshot
  expect_equal(rf$left_out$reason, "stored on the day before")
  expect_equal(rf$days$date, "2026-06-07")               # rebuilt from a snapshot without R-x
  expect_equal(rf$rejected, 0L)                          # and not refused over the pipeline run's R-x
  expect_false("R-x" %in% rf$rows$package)
  m <- merge(rf$rows, truth, by = c("package", "date"))
  expect_equal(m$count.x, m$count.y)
})

test_that("a window that holds a release day leaves uncovered packages out", {
  ids3 <- c(ids2, "R-x" = 3L)
  truth <- with_outsider(fill_truth("2026-06-01", "2026-06-11"))
  # 06-05 was rebuilt for R-a and R-b only, so R-x has no row there.
  daily <- truth[truth$count > 0 & truth$date != "2026-06-09" &
                 !(truth$package == "R-x" & truth$date == "2026-06-05"), ]
  known <- setdiff(unique(truth$date), "2026-06-09")
  eqs <- run_equations(mc_counters(truth, ids3, "2026-06-11"), "2026-06-11", "2026-06-10")
  f <- solve_days(eqs, daily, known, floor = "2026-06-01", run_id = 1L,
                  partial = "2026-06-05", uncovered = "R-x")
  expect_equal(f$days$date, "2026-06-09")
  expect_equal(f$days$method, "release")   # the filled day holds covered packages only
  expect_setequal(f$rows$package, c("R-a", "R-b"))
  expect_equal(f$residual, 0L)             # R-x is not checked against a day it has no data for
  m <- merge(f$rows, truth, by = c("package", "date"))
  expect_equal(m$count.x, m$count.y)

  # A window without a release day keeps every package.
  full <- truth[truth$count > 0 & truth$date != "2026-06-09", ]
  g <- solve_days(eqs, full, known, floor = "2026-06-01", run_id = 1L,
                  partial = "2026-06-02", uncovered = "R-x")
  expect_equal(g$days$method, "window")
  expect_equal(g$rows$count[g$rows$package == "R-x"],
               truth$count[truth$package == "R-x" & truth$date == "2026-06-09"])
})

test_that("the history contract check names what is missing", {
  p <- tempfile(fileext = ".db")
  write_history_fixture(p, fill_truth(), ids2, "2026-06-05")
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  expect_true(check_history_contract(con))     # the series is 'autoobs_summary', as the history build names it
  # The table name is not the series name: a build that recorded it that way is refused.
  DBI::dbExecute(con, "UPDATE history_series_observations SET series = 'autoobs_summary_history'")
  expect_error(check_history_contract(con),
               "no autoobs_summary/applied observations; found: autoobs_summary_history/applied")
  DBI::dbExecute(con, "ALTER TABLE autoobs_summary_history DROP COLUMN cnt_total")
  expect_error(check_history_contract(con), "lacks cnt_total")
})

test_that("snapshots are rebuilt from episodes, one per autoobs run", {
  truth <- fill_truth()
  p <- tempfile(fileext = ".db")
  write_history_fixture(p, truth, ids2, c("2026-06-05", "2026-06-06", "2026-06-07", "2026-06-08"),
                        observe = list("v2026-06-06" = list(outcome = "unhealthy"),
                                       "v2026-06-07" = list(source_as_of = "2026-06-05"),
                                       "v2026-06-08" = list(rows_read = 5L)))
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  snaps <- history_autoobs_snapshots(con)
  expect_equal(vapply(snaps, `[[`, "", "tag"), c("v2026-06-05", "v2026-06-07", "v2026-06-08"))
  sr <- snapshot_runs(snaps)
  expect_equal(sr$log$outcome, c("kept", "dropped", "dropped"))
  expect_match(sr$log$reason[2], "same autoobs run")
  expect_match(sr$log$reason[3], "rows_read")
  expect_equal(sr$runs$run_id, as.integer(as.numeric(as.POSIXct("2026-06-05", tz = "UTC"))))
  expect_equal(sr$runs$source, "observatory.db v2026-06-05")
  expect_true(is.na(sr$runs$run_at))
  expect_equal(sr$runs$window_end, "2026-06-04")
  want <- mc_counters(truth, ids2, "2026-06-05")
  got <- sr$counters[order(sr$counters$package), ]
  expect_equal(got$cnt_7d, want$cnt_7d)
  expect_equal(got$cnt_30d, want$cnt_30d)
  expect_true(all(is.na(got$cnt_today)))
})

test_that("a snapshot the extraction skipped is not read", {
  p <- tempfile(fileext = ".db")
  write_history_fixture(p, fill_truth(), ids2, c("2026-06-05", "2026-06-06"))
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "UPDATE history_snapshots SET outcome = 'skipped' WHERE tag = 'v2026-06-05'")
  expect_equal(vapply(history_autoobs_snapshots(con), `[[`, "", "tag"), "v2026-06-06")
})

test_that("the import script writes the import asset and reports the refill", {
  truth <- fill_truth("2026-04-01", "2026-06-25")
  tmp <- withr::local_tempdir()
  hist <- file.path(tmp, "history.db")
  write_history_fixture(hist, truth, ids2, format(seq(as.Date("2026-06-12"), as.Date("2026-06-20"), by = "day")),
                        unaggregated = c("2026-06-15", "2026-06-18"))
  recent <- file.path(tmp, "recent.db")
  daily <- truth[truth$count > 0 & truth$date >= "2026-06-11" & truth$date <= "2026-06-19" &
                 !(truth$date %in% c("2026-06-14", "2026-06-17")), ]
  export_shard(recent, daily, days_df = bootstrap_days(daily))
  source(file.path(.ao_root, "scripts", "import_release_snapshots.R"))
  out <- capture.output(res <- import_release_snapshots(hist, recent, file.path(tmp, "out")))
  expect_match(out[1], "snapshots: 9 read, 9 kept, 0 dropped")
  expect_true(any(grepl("a refill would fill", out)))
  expect_true(any(grepl("no counters asset given", out)))
  con <- DBI::dbConnect(RSQLite::SQLite(), res$path); on.exit(DBI::dbDisconnect(con))
  expect_setequal(DBI::dbListTables(con), c("autoobs_counters", "autoobs_runs", "autoobs_import_log",
                                            "autoobs_import_left_out"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_import_left_out")$n, 0L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_runs")$n, 9L)
  f <- res$fill
  expect_true(all(c("2026-06-14", "2026-06-17") %in% f$days$date))
  expect_true(any(f$days$date < "2026-06-11"))
  expect_equal(f$rejected, 0L)
  m <- merge(f$rows, truth, by = c("package", "date"))
  expect_equal(m$count.x, m$count.y)
  expect_equal(nrow(m), nrow(f$rows))
})

test_that("the import leaves out a package the later releases stopped holding", {
  ids3 <- c(ids2, "R-x" = 3L)
  truth <- with_outsider(fill_truth("2026-04-01", "2026-06-25"))
  holes <- c("2026-06-14", "2026-06-17")
  tmp <- withr::local_tempdir()
  hist <- file.path(tmp, "history.db")
  # The releases up to 06-13 held every tracked package, the later ones only R-a and R-b.
  write_history_fixture(hist, truth, ids2, format(seq(as.Date("2026-06-12"), as.Date("2026-06-20"), by = "day")),
                        unaggregated = c("2026-06-15", "2026-06-18"),
                        wide = list(ids = ids3, until = "2026-06-13"))
  daily <- truth[truth$count > 0 & truth$date >= "2026-06-11" & truth$date <= "2026-06-19" &
                 !(truth$date %in% holes), ]
  recent <- file.path(tmp, "recent.db")
  export_shard(recent, daily, days_df = bootstrap_days(daily))
  real <- snapshot_fixture(truth, ids3, "2026-06-20", source = "run", hour = 4L)
  write_runs(recent, real$run)
  cpath <- file.path(tmp, COUNTERS_ASSET)
  update_counters(cpath, real$cn[names(empty_counters())], keep_from = 0L, fresh = TRUE)
  source(file.path(.ao_root, "scripts", "import_release_snapshots.R"))
  out <- capture.output(res <- import_release_snapshots(hist, recent, file.path(tmp, "out"), cpath))
  expect_match(out[1], "snapshots: 9 read, 9 kept, 0 dropped")
  expect_true(any(grepl("packages the snapshots cover: 2", out)))
  expect_true(any(grepl("held by a snapshot and left out: 1 (R-x)", out, fixed = TRUE)))
  con <- DBI::dbConnect(RSQLite::SQLite(), res$path); on.exit(DBI::dbDisconnect(con))
  lo <- DBI::dbReadTable(con, "autoobs_import_left_out")
  expect_equal(lo$package, "R-x")
  expect_equal(lo$first_snapshot, "2026-06-12")
  expect_equal(lo$missing_from, "2026-06-14")
  expect_equal(lo$reason, "held by an earlier snapshot")
  # The file keeps what each release held; the refill is what leaves R-x out.
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_counters WHERE package = 'R-x'")$n, 2L)
  f <- res$fill
  expect_setequal(f$covered, c("R-a", "R-b"))
  expect_true(all(holes %in% f$days$date))                  # both rebuilt from the later releases
  expect_true(any(f$days$date < "2026-06-11"))              # and days from the early ones
  expect_equal(f$rejected, 0L)
  expect_equal(f$residual, 0L)
  expect_false("R-x" %in% f$rows$package)
  m <- merge(f$rows, truth, by = c("package", "date"))
  expect_equal(nrow(m), nrow(f$rows))
  expect_equal(m$count.x, m$count.y)
})

test_that("the preview keeps out a package an earlier refill left out", {
  truth <- fill_truth("2026-04-01", "2026-06-25")
  tmp <- withr::local_tempdir()
  hist <- file.path(tmp, "history.db")
  write_history_fixture(hist, truth, ids2, format(seq(as.Date("2026-06-12"), as.Date("2026-06-20"), by = "day")),
                        unaggregated = "2026-06-15")
  daily <- truth[truth$count > 0 & truth$date >= "2026-06-11" & truth$date <= "2026-06-19" &
                 truth$date != "2026-06-14", ]
  days <- rbind(bootstrap_days(daily), data.frame(date = "2026-06-10", method = "release", run_id = 5L,
                                                  packages = 1L, downloads = 12L))
  recent <- file.path(tmp, "recent.db")
  export_shard(recent, rbind(daily, truth[truth$package == "R-a" & truth$date == "2026-06-10", ]),
               days_df = days)
  expect_equal(read_rebuilt(recent), list(covered = character(0), skip = character(0)))
  con <- DBI::dbConnect(RSQLite::SQLite(), recent)
  DBI::dbWriteTable(con, "autoobs_packages", data.frame(package = c("R-a", "R-b"), rebuilt = c(1L, 0L)))
  DBI::dbDisconnect(con)
  expect_equal(read_rebuilt(recent), list(covered = "R-a", skip = "R-b"))
  source(file.path(.ao_root, "scripts", "import_release_snapshots.R"))
  capture.output(res <- import_release_snapshots(hist, recent, file.path(tmp, "out")))
  expect_equal(res$fill$covered, "R-a")
  expect_true("2026-06-14" %in% res$fill$days$date)
  expect_setequal(unique(res$fill$rows$package), "R-a")
})

test_that("the import preview solves with the pipeline runs and counters a refill run will read", {
  ids3 <- c(ids2, "R-x" = 3L)
  truth <- with_outsider(fill_truth("2026-05-01", "2026-06-20"))
  holes <- c("2026-06-09", "2026-06-12")
  tmp <- withr::local_tempdir()
  hist <- file.path(tmp, "history.db")
  write_history_fixture(hist, truth[truth$package != "R-x", ], ids2, "2026-06-11")
  daily <- truth[truth$count > 0 & truth$date <= "2026-06-19" & !(truth$date %in% holes), ]
  recent <- file.path(tmp, "recent.db")
  export_shard(recent, daily, days_df = bootstrap_days(daily))
  real <- snapshot_fixture(truth, ids3, "2026-06-20", source = "run", hour = 4L)
  write_runs(recent, real$run)
  cpath <- file.path(tmp, COUNTERS_ASSET)
  update_counters(cpath, real$cn[names(empty_counters())], keep_from = 0L, fresh = TRUE)
  source(file.path(.ao_root, "scripts", "import_release_snapshots.R"))

  capture.output(alone <- import_release_snapshots(hist, recent, file.path(tmp, "o1")))
  expect_equal(alone$fill$days$date, "2026-06-09")          # the snapshot's 7 days hold only this hole
  out <- capture.output(both <- import_release_snapshots(hist, recent, file.path(tmp, "o2"), cpath))
  expect_true(any(grepl("with 1 pipeline run from the counters asset", out)))
  expect_setequal(both$fill$days$date, holes)               # 06-12 needs the pipeline run's 30 days
  expect_equal(both$fill$rejected, 0L)
  expect_false("R-x" %in% both$fill$rows$package)
  m <- merge(both$fill$rows, truth, by = c("package", "date"))
  expect_equal(m$count.x, m$count.y)
})
