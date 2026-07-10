test_that("config exposes the identity-asset settings and floors", {
  expect_true(exists("CRAN_ARCHIVE_REPO") && CRAN_ARCHIVE_REPO == "r-observatory/cran-archive")
  expect_true(exists("CRAN_ARCHIVE_DB")   && CRAN_ARCHIVE_DB   == "cran-archive.db")
  expect_true(exists("BIOC_META_REPO")    && BIOC_META_REPO    == "r-observatory/bioconductor-metadata")
  expect_true(exists("BIOC_META_DB")      && BIOC_META_DB      == "bioconductor-metadata.db")
  expect_true(exists("CRAN_NAMES_FLOOR")  && CRAN_NAMES_FLOOR  == 15000L)
  expect_true(exists("BIOC_NAMES_FLOOR")  && BIOC_NAMES_FLOOR  == 1500L)
})
