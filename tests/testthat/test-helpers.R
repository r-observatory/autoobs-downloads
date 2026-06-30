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
  s <- build_summary(con, stats_df, anchor_date = "2026-06-10", snapshot_date = "2026-06-11")

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
