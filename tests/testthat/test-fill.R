# The window solver on its own: exact fills, refusals, zero days and residuals.
ids2 <- c("R-a" = 1L, "R-b" = 2L)
stored <- function(truth, holes) truth[!(truth$date %in% holes) & truth$count > 0, ]
known_of <- function(truth, holes) setdiff(unique(truth$date), holes)

test_that("a day missed at S+1 is filled at S+2 from W7, exactly", {
  truth <- fill_truth("2026-06-01")
  cn <- mc_counters(truth, ids2, "2026-06-11")
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"),
                  stored(truth, "2026-06-09"), known_of(truth, "2026-06-09"),
                  floor = "2026-06-01", run_id = 7L)
  expect_equal(f$days$date, "2026-06-09")
  expect_equal(f$days$method, "window")
  expect_equal(f$days$run_id, 7L)
  want <- truth[truth$date == "2026-06-09", ]
  got  <- f$rows[order(f$rows$package), ]
  expect_equal(got$count, want$count[order(want$package)])
  expect_equal(f$days$downloads, sum(want$count))
  expect_equal(f$rejected, 0L)
  expect_equal(f$residual, 0L)             # R-b's missing 06-05 row counted as 0
})

test_that("W30 minus W7 fills when W7 holds two unknowns, and two everywhere fills nothing", {
  truth <- fill_truth()
  cn <- mc_counters(truth, ids2, "2026-06-11")
  holes <- c("2026-05-20", "2026-06-08", "2026-06-09")
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"), stored(truth, holes),
                  known_of(truth, holes), floor = "2026-05-01", run_id = 1L)
  expect_equal(f$days$date, "2026-05-20")
  expect_equal(sum(f$rows$count), sum(truth$count[truth$date == "2026-05-20"]))
  holes2 <- c(holes, "2026-05-21")
  f2 <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"), stored(truth, holes2),
                   known_of(truth, holes2), floor = "2026-05-01", run_id = 1L)
  expect_equal(nrow(f2$days), 0L)
  expect_equal(nrow(f2$rows), 0L)
})

test_that("a negative package refuses the day and counts it", {
  truth <- fill_truth("2026-06-01")
  cn <- mc_counters(truth, ids2, "2026-06-11")
  cn$cnt_7d[cn$package == "R-b"] <- 1L
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"),
                  stored(truth, "2026-06-09"), known_of(truth, "2026-06-09"),
                  floor = "2026-06-01", run_id = 1L)
  expect_equal(nrow(f$days), 0L)
  expect_equal(nrow(f$rows), 0L)
  expect_equal(f$rejected, 1L)
})

test_that("a day that sums to zero is upstream_missing with no rows", {
  truth <- fill_truth("2026-06-01")
  truth$count[truth$date == "2026-06-09"] <- 0L
  cn <- mc_counters(truth, ids2, "2026-06-11")
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"),
                  stored(truth, "2026-06-09"), known_of(truth, "2026-06-09"),
                  floor = "2026-06-01", run_id = 1L)
  expect_equal(f$days$method, "upstream_missing")
  expect_equal(c(f$days$packages, f$days$downloads), c(0L, 0L))
  expect_equal(nrow(f$rows), 0L)
})

test_that("a package with an NA counter sits out that window without refusing the day", {
  truth <- fill_truth("2026-06-01")
  cn <- mc_counters(truth, ids2, "2026-06-11")
  cn$cnt_7d[cn$package == "R-b"] <- NA_integer_
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"),
                  stored(truth, "2026-06-09"), known_of(truth, "2026-06-09"),
                  floor = "2026-06-01", run_id = 1L)
  expect_equal(f$days$date, "2026-06-09")
  expect_equal(f$rows$package, "R-a")
  expect_equal(f$rejected, 0L)
})

test_that("windows reaching before the floor are skipped", {
  truth <- fill_truth("2026-06-01")
  cn <- mc_counters(truth, ids2, "2026-06-11")
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"),
                  stored(truth, "2026-06-09"), known_of(truth, "2026-06-09"),
                  floor = "2026-06-05", run_id = 1L)
  expect_equal(nrow(f$days), 0L)
})

test_that("a fully known window that does not add up is counted in residual", {
  truth <- fill_truth("2026-06-01")
  cn <- mc_counters(truth, ids2, "2026-06-11")
  cn$cnt_7d[cn$package == "R-a"] <- cn$cnt_7d[cn$package == "R-a"] + 1L
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"), stored(truth, character(0)),
                  known_of(truth, character(0)), floor = "2026-06-01", run_id = 1L)
  expect_equal(nrow(f$days), 0L)
  expect_equal(f$residual, 1L)
})

test_that("a package missing from the run's counters gets no row on the filled day", {
  truth <- fill_truth("2026-06-01")
  cn <- mc_counters(truth, ids2, "2026-06-11")
  cn <- cn[cn$package == "R-a", ]
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-10"),
                  stored(truth, "2026-06-09"), known_of(truth, "2026-06-09"),
                  floor = "2026-06-01", run_id = 1L)
  expect_equal(f$days$date, "2026-06-09")
  expect_equal(f$rows$package, "R-a")
  expect_equal(f$rejected, 0L)
})

test_that("an unaggregated run's zero for its own last window day leaves the day unknown", {
  truth <- fill_truth("2026-06-01")
  holes <- c("2026-06-09", "2026-06-10")
  # MirrorCache two days late: at the 06-11 run its windows still end on 06-08.
  late <- mc_counters(truth, ids2, "2026-06-11", last = "2026-06-08")
  f <- solve_days(run_equations(late, "2026-06-11", "2026-06-09", sure_end = FALSE, run_id = 5L),
                  stored(truth, holes), known_of(truth, holes), floor = "2026-06-01", run_id = 5L)
  expect_equal(nrow(f$days), 0L)             # a hole, not upstream_missing
  expect_equal(nrow(f$rows), 0L)
  expect_equal(f$rejected, 0L)
  # One day late: the same run has 06-09 inside its windows and fills it.
  ontime <- mc_counters(truth, ids2, "2026-06-11", aggregated = FALSE)
  g <- solve_days(run_equations(ontime, "2026-06-11", "2026-06-09", sure_end = FALSE, run_id = 5L),
                  stored(truth, holes), known_of(truth, holes), floor = "2026-06-01", run_id = 5L)
  expect_equal(g$days$date, "2026-06-09")
  expect_equal(g$days$method, "window")
  expect_equal(sum(g$rows$count), sum(truth$count[truth$date == "2026-06-09"]))
})

test_that("an unaggregated run still records a zero day its older windows prove", {
  truth <- fill_truth()
  truth$count[truth$date == "2026-05-20"] <- 0L
  holes <- c("2026-05-20", "2026-06-10")
  cn <- mc_counters(truth, ids2, "2026-06-11", aggregated = FALSE)
  f <- solve_days(run_equations(cn, "2026-06-11", "2026-06-09", sure_end = FALSE, run_id = 5L),
                  stored(truth, holes), known_of(truth, holes), floor = "2026-05-01", run_id = 5L)
  expect_equal(f$days$date, "2026-05-20")
  expect_equal(f$days$method, "upstream_missing")
})

test_that("an unaggregated run with no 7-day counts leaves a zero eight days back unknown", {
  truth <- fill_truth()
  holes <- format(seq(as.Date("2026-06-03"), as.Date("2026-06-10"), by = "day"))
  # MirrorCache nine days behind: at the 06-11 run nothing after 06-02 is counted.
  stalled <- mc_counters(truth, ids2, "2026-06-11", last = "2026-06-02")
  eqs <- run_equations(stalled, "2026-06-11", "2026-06-09", sure_end = FALSE, run_id = 5L)
  expect_equal(eqs[[3]]$soft, c("2026-06-03" = 5L))
  f <- solve_days(eqs, stored(truth, holes), known_of(truth, holes), floor = "2026-05-01", run_id = 5L)
  expect_equal(nrow(f$days), 0L)             # 06-03 stays a hole, not upstream_missing
  expect_equal(f$rejected, 0L)
  # Eight days behind: 06-03 is counted, and the same equation fills it.
  counted <- mc_counters(truth, ids2, "2026-06-11", last = "2026-06-03")
  g <- solve_days(run_equations(counted, "2026-06-11", "2026-06-09", sure_end = FALSE, run_id = 5L),
                  stored(truth, holes), known_of(truth, holes), floor = "2026-05-01", run_id = 5L)
  expect_equal(g$days$date, "2026-06-03")
  expect_equal(g$days$method, "window")
  expect_equal(sum(g$rows$count), sum(truth$count[truth$date == "2026-06-03"]))
})

test_that("an unaggregated run with 7-day counts still records a zero day eight days back", {
  truth <- fill_truth()
  truth$count[truth$date == "2026-06-03"] <- 0L
  holes <- c("2026-06-03", "2026-06-07", "2026-06-08", "2026-06-10")
  cn <- mc_counters(truth, ids2, "2026-06-11", aggregated = FALSE)
  eqs <- run_equations(cn, "2026-06-11", "2026-06-09", sure_end = FALSE, run_id = 5L)
  expect_null(eqs[[3]]$soft)                 # the 7-day window holds counted days
  f <- solve_days(eqs, stored(truth, holes), known_of(truth, holes), floor = "2026-05-01", run_id = 5L)
  expect_equal(f$days$date, "2026-06-03")
  expect_equal(f$days$method, "upstream_missing")
})
