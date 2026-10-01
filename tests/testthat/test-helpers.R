test_that("build_daily_rows keeps positive cnt_1d, attributes to the completed day", {
  stats <- rbind(
    cbind(stats_row(1, cnt_1d = 10, 70, 300, 1000), package = "R-a", stringsAsFactors = FALSE),
    cbind(stats_row(2, cnt_1d = 0,  5,  20,   50), package = "R-b", stringsAsFactors = FALSE),
    cbind(stats_row(3, cnt_1d = NA, 1,  1,    1), package = "R-c", stringsAsFactors = FALSE))
  d <- build_daily_rows(stats, "2026-06-10")
  expect_equal(nrow(d), 1L)            # only R-a has a positive trailing-day count
  expect_equal(d$package, "R-a")
  expect_equal(d$date, "2026-06-10")
  expect_equal(d$count, 10L)
})

test_that("build_daily_rows returns an empty frame when nothing is positive", {
  stats <- cbind(stats_row(2, cnt_1d = 0, 5, 20, 50), package = "R-b", stringsAsFactors = FALSE)
  expect_equal(nrow(build_daily_rows(stats, "2026-06-10")), 0L)
})

test_that("unix_to_date converts seconds to a UTC date, NA passthrough", {
  expect_equal(unix_to_date(1767139200L), "2025-12-31")
  expect_true(is.na(unix_to_date(NA)))
})

test_that("export_shard round-trips the daily table", {
  d <- data.frame(package = c("R-a", "R-a"), date = c("2026-06-09", "2026-06-10"),
                  count = c(5L, 7L), stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".db")
  export_shard(p, d)
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM autoobs_downloads_daily")$n, 2L)
})

test_that("build_summary draws windows from the API and trend from the daily series", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  # 60+ days of local series for R-a so trend is computable.
  dates <- format(seq(as.Date("2026-04-01"), as.Date("2026-06-10"), by = 1))
  daily <- data.frame(
    package = "R-a", date = dates,
    count = c(rep(1L, 40), rep(3L, length(dates) - 40)), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "autoobs_downloads_daily", daily)

  stats_df <- rbind(
    cbind(stats_row(1, cnt_1d = 3, 21, 90, 5000), package = "R-a", stringsAsFactors = FALSE),
    cbind(stats_row(2, cnt_1d = 0, 1,  4,   10), package = "R-b", stringsAsFactors = FALSE))
  ident <- data.frame(package = c("R-a", "R-b"), origin = c("cran", "cran"),
                      canonical_name = c("a", "b"), identity_state = c("live", "live"),
                      stringsAsFactors = FALSE)
  s <- build_summary(con, stats_df, anchor_date = "2026-06-10", snapshot_date = "2026-06-11",
                     identity_df = ident)

  expect_setequal(names(s), SUMMARY_COLS)
  ra <- s[s$package == "R-a", ]
  expect_equal(ra$total_1d, 3L)
  expect_equal(ra$total_30d, 90L)
  expect_equal(ra$cnt_total, 5000L)
  expect_equal(ra$rank_30d, 1L)             # 90 vs 4
  expect_equal(ra$first_seen, "2025-12-31")
  expect_equal(ra$last_snapshot, "2026-06-11")
  expect_true(is.finite(ra$trend))          # enough history -> a number
  rb <- s[s$package == "R-b", ]
  expect_true(is.na(rb$trend))              # no local series -> NULL trend
})

test_that("SUMMARY_COLS carries the identity columns right after package_lower", {
  expect_equal(SUMMARY_COLS[1:5],
    c("package", "package_lower", "origin", "canonical_name", "identity_state"))
  # the DDL and the empty frame agree with SUMMARY_COLS
  expect_setequal(names(empty_summary()), SUMMARY_COLS)
  p <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, summary_table_ddl("t"))
  expect_true(all(c("origin", "canonical_name", "identity_state") %in%
                  DBI::dbListFields(con, "t")))
})

test_that("build_summary attaches identity and promotes only in-scope rows, ranked densely", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  # R-a has 60+ days of local series so its trend is computable.
  dates <- format(seq(as.Date("2026-04-01"), as.Date("2026-06-10"), by = 1))
  daily <- data.frame(package = "R-a", date = dates,
                      count = c(rep(1L, 40), rep(3L, length(dates) - 40)),
                      stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "autoobs_downloads_daily", daily)

  stats_df <- rbind(
    cbind(stats_row(1, cnt_1d = 3, 21, 90, 5000), package = "R-a", stringsAsFactors = FALSE),
    cbind(stats_row(2, cnt_1d = 9, 40, 300, 6000), package = "R-base", stringsAsFactors = FALSE),
    cbind(stats_row(3, cnt_1d = 0, 1,   4,   10), package = "R-b", stringsAsFactors = FALSE))
  ident <- data.frame(
    package        = c("R-a", "R-base", "R-b"),
    origin         = c("cran", "other", "cran"),
    canonical_name = c("a", NA, "b"),
    identity_state = c("live", NA, "archived"), stringsAsFactors = FALSE)
  s <- build_summary(con, stats_df, anchor_date = "2026-06-10",
                     snapshot_date = "2026-06-11", identity_df = ident)

  expect_setequal(names(s), SUMMARY_COLS)
  expect_setequal(s$package, c("R-a", "R-b"))            # R-base (other) dropped
  expect_equal(s$rank_30d[s$package == "R-a"], 1L)       # dense over in-scope only (90 vs 4)
  expect_equal(s$rank_30d[s$package == "R-b"], 2L)
  expect_equal(s$origin[s$package == "R-a"], "cran")
  expect_equal(s$canonical_name[s$package == "R-a"], "a")
  expect_equal(s$identity_state[s$package == "R-b"], "archived")
})

test_that("build_summary returns an empty frame when nothing is in scope", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE autoobs_downloads_daily (package TEXT, date TEXT, count INTEGER)")
  stats_df <- cbind(stats_row(1, cnt_1d = 3, 21, 90, 5000), package = "R-base",
                    stringsAsFactors = FALSE)
  ident <- data.frame(package = "R-base", origin = "other",
                      canonical_name = NA_character_, identity_state = NA_character_,
                      stringsAsFactors = FALSE)
  s <- build_summary(con, stats_df, anchor_date = "2026-06-10",
                     snapshot_date = "2026-06-11", identity_df = ident)
  expect_equal(nrow(s), 0L)
  expect_setequal(names(s), SUMMARY_COLS)
})

test_that("autoobs_runs DDL, normalisation and merge agree with RUNS_SCHEMA", {
  p <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, runs_table_ddl())
  expect_equal(DBI::dbListFields(con, "autoobs_runs"), names(RUNS_SCHEMA))
  n <- normalize_runs(data.frame(run_id = 5, outcome = "ok", stringsAsFactors = FALSE))
  expect_equal(names(n), names(RUNS_SCHEMA))
  expect_type(n$run_id, "integer")
  expect_type(n$window_end, "character")
  expect_true(is.na(n$stats_requested))
  m <- merge_runs(data.frame(run_id = c(9L, 3L), outcome = c("ok", "ok")),
                  data.frame(run_id = 9L, outcome = "heartbeat"))
  expect_equal(m$run_id, c(3L, 9L))
  expect_equal(m$outcome, c("ok", "heartbeat"))   # a repeated run_id keeps the newer row
})

test_that("counter_stats and window_end_for read aggregation from cnt_1d", {
  s <- rbind(stats_row(1, 0, 7, 30, 0, cnt_today = 1), stats_row(2, NA, 3, 5, 9))
  cs <- counter_stats(s)
  expect_equal(cs$day_aggregated, 0L)
  expect_equal(c(cs$pos_today, cs$pos_7d, cs$pos_total), c(1L, 2L, 1L))
  expect_equal(c(cs$sum_7d, cs$sum_30d), c(10L, 35L))
  expect_equal(window_end_for("2026-09-30", 0L), "2026-09-28")
  expect_equal(window_end_for("2026-09-30", 1L), "2026-09-29")
})

test_that("update_counters appends, replaces a repeated run and trims old runs", {
  p <- tempfile(fileext = ".db")
  st <- cbind(stats_row(1, 1, 2, 3, 4), package = "R-a", stringsAsFactors = FALSE)
  update_counters(p, counters_rows(st, 100L), keep_from = 0L, fresh = TRUE)
  update_counters(p, counters_rows(st, 200L), keep_from = 0L)
  r <- update_counters(p, counters_rows(rbind(st, st), 200L), keep_from = 150L)
  expect_equal(r$rows, 1L)                     # run 100 trimmed, duplicate package dropped
  expect_equal(r$runs, 1L)
  expect_equal(r$first_run, 200L)
  expect_true(counters_readable(p))
  g <- tempfile(fileext = ".db"); writeLines("not a database", g)
  expect_false(counters_readable(g))
  expect_false(counters_readable(tempfile()))
})

test_that("counters_note says whether the window moved", {
  expect_equal(counters_note(NULL), "not yet published")
  expect_match(counters_note(list(asset = "a.db", published = TRUE, runs = 3, window_days = 40)),
               "3 runs over the last 40 days")
  expect_match(counters_note(list(published = FALSE, prior = "download_failed")), "could not be downloaded")
  expect_equal(counters_note(list(published = FALSE, prior = "loaded")), "not updated this run")
})

test_that("a database without the counters table is not a readable counters asset", {
  p <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, "CREATE TABLE something_else (x INTEGER)")
  DBI::dbDisconnect(con)
  expect_false(counters_readable(p))
})

test_that("bootstrap_days writes one cnt_1d row per stored date", {
  d <- data.frame(package = c("R-a", "R-b", "R-a"), date = c("2026-06-01", "2026-06-01", "2026-06-03"),
                  count = c(2L, 3L, 4L), stringsAsFactors = FALSE)
  b <- bootstrap_days(d)
  expect_equal(b$date, c("2026-06-01", "2026-06-03"))
  expect_equal(b$method, c("cnt_1d", "cnt_1d"))
  expect_true(all(is.na(b$run_id)))
  expect_equal(b$packages, c(2L, 1L))
  expect_equal(b$downloads, c(5L, 4L))
})

test_that("autoobs_days is written beside the daily rows and read back", {
  p <- tempfile(fileext = ".db")
  d <- data.frame(package = "R-a", date = "2026-06-01", count = 2L, stringsAsFactors = FALSE)
  export_shard(p, d, days_df = rbind(bootstrap_days(d), data.frame(
    date = "2026-06-02", method = "upstream_missing", run_id = 9L, packages = 0L,
    downloads = 0L, stringsAsFactors = FALSE)))
  back <- read_days(p)
  expect_equal(back$date, c("2026-06-01", "2026-06-02"))
  expect_equal(back$method, c("cnt_1d", "upstream_missing"))
  expect_null(read_days(tempfile()))
  q <- tempfile(fileext = ".db")
  export_shard(q, d)                       # a shard without days has no autoobs_days
  expect_null(read_days(q))
  m <- upsert_days(back, data.frame(date = "2026-06-02", method = "window", run_id = 10L,
                                    packages = 1L, downloads = 4L, stringsAsFactors = FALSE))
  expect_equal(m$method, c("cnt_1d", "window"))     # a newer row for a date wins
})

test_that("the run record carries the fill columns", {
  expect_true(all(c("days_filled", "fill_rejected", "window_residual") %in% names(RUNS_SCHEMA)))
  n <- normalize_runs(data.frame(run_id = 5, days_filled = 2))
  expect_equal(n$days_filled, 2L)
  expect_true(is.na(n$fill_rejected))
})

test_that("the summary has no rank_total and nulls total_1d before aggregation", {
  expect_false("rank_total" %in% SUMMARY_COLS)
  p <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), p); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, summary_table_ddl("t"))
  expect_false("rank_total" %in% DBI::dbListFields(con, "t"))
  DBI::dbExecute(con, "CREATE TABLE autoobs_downloads_daily (package TEXT, date TEXT, count INTEGER)")
  st <- cbind(stats_row(1, cnt_1d = 0, 21, 90, 0), package = "R-a", stringsAsFactors = FALSE)
  ident <- data.frame(package = "R-a", origin = "cran", canonical_name = "a",
                      identity_state = "live", stringsAsFactors = FALSE)
  s0 <- build_summary(con, st, "2026-06-10", "2026-06-11", identity_df = ident, day_aggregated = FALSE)
  expect_true(is.na(s0$total_1d))
  s1 <- build_summary(con, st, "2026-06-10", "2026-06-11", identity_df = ident, day_aggregated = TRUE)
  expect_equal(s1$total_1d, 0L)
})
