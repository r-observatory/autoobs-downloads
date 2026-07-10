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
