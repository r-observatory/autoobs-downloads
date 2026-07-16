# Integrity / completeness core attached to manifest.json.

# Build a tiny, real summary DB on disk (canonical schema via export_summary_shard,
# which creates the autoobs_downloads_summary table). *.db is gitignored, so the
# fixture is built in-test rather than committed.
build_summary_db <- function(n = 3L) {
  tmp <- tempfile(fileext = ".db")
  export_summary_shard(path = tmp, summary = data.frame(
    package        = paste0("R-pkg", seq_len(n)),
    package_lower  = paste0("r-pkg", seq_len(n)),
    origin         = rep("cran", n),
    canonical_name = paste0("pkg", seq_len(n)),
    identity_state = rep("live", n),
    id             = seq_len(n),
    total_1d       = seq_len(n) * 1L,
    total_7d       = seq_len(n) * 7L,
    total_30d      = seq_len(n) * 10L,
    cnt_total      = seq_len(n) * 100L,
    avg_daily_30d  = seq_len(n) * 1.5,
    rank_30d       = seq_len(n),
    rank_total     = seq_len(n),
    trend          = rep(NA_real_, n),
    autocran_only  = rep(1L, n),
    first_seen     = rep("2025-12-31", n),
    last_snapshot  = rep("2026-06-11", n),
    stringsAsFactors = FALSE
  ))
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_summary_db(3L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes is a double (not cast to integer) so files >= ~2 GiB do not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count
  expect_equal(core$tables, list(autoobs_downloads_summary = 3L))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  # Compute the expected hash via an external CLI tool, independent of
  # file_sha256()'s own preferred backend (digest/openssl), so this test
  # genuinely cross-checks the code path instead of re-running the same
  # library. Skip only if neither tool is on PATH (both are expected on CI).
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_summary_db(2L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest merges the integrity core as top-level fields", {
  db <- build_summary_db(4L)
  on.exit(unlink(db), add = TRUE)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_manifest(
    path = tmp,
    obj  = list(
      tag            = "v20260714-000000",
      changed_shards = list("autoobs-downloads-summary.db"),
      summary        = list(packages = 1L)
    ),
    core = core
  )

  parsed <- jsonlite::fromJSON(tmp)
  # existing fields preserved
  expect_equal(parsed$tag, "v20260714-000000")
  expect_equal(parsed$summary$packages, 1L)
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$autoobs_downloads_summary, 4L)
  expect_true(parsed$complete)
})
