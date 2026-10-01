# The publish step is shell, so these read update.yml as text.
.wf <- function() readLines(file.path(.ao_root, ".github", "workflows", "update.yml"))

test_that("the counters asset uploads only when published, before the shards and manifest", {
  wf <- .wf()
  guard    <- grep(".counters.published // false", wf, fixed = TRUE)
  counters <- grep('gh release upload current "out/$COUNTERS_ASSET" --clobber', wf, fixed = TRUE)
  shards   <- grep('gh release upload current "out/$asset" --clobber', wf, fixed = TRUE)
  manifest <- grep("gh release upload current out/manifest.json --clobber", wf, fixed = TRUE)
  expect_length(guard, 1L); expect_length(counters, 1L)
  expect_true(guard < counters && counters < shards && shards < manifest)
})

test_that("the refill input reaches the update script", {
  wf <- .wf()
  expect_true(any(grepl("^      refill:$", wf)))
  expect_true(any(grepl("AUTOOBS_REFILL: ${{ inputs.refill }}", wf, fixed = TRUE)))
})
