# Build a real maps object from robservatory using small fixture name sets.
.mk_maps <- function() {
  cran <- data.frame(
    name_lower     = c("rcpp", "aer", "aae.pop", "maptools", "mass"),
    canonical_name = c("Rcpp", "AER", "aae.pop", "maptools", "MASS"),
    identity_state = c("live", "live", "live", "archived", "live"),
    stringsAsFactors = FALSE)
  bioc <- data.frame(
    name_lower = "complexheatmap", canonical_name = "ComplexHeatmap",
    identity_state = "live", stringsAsFactors = FALSE)
  robservatory::resolve_identity_set(cran, bioc)
}

test_that("resolve_identities strips R- and classifies against the ledger", {
  maps <- .mk_maps()
  out <- resolve_identities(
    c("R-Rcpp", "R-AER", "R-aae.pop", "R-ComplexHeatmap",
      "R-notacran", "libcurl-devel", "R-base"), maps)
  row <- function(p) out[out$package == p, ]
  expect_equal(row("R-Rcpp")$origin, "cran")             # live CRAN
  expect_equal(row("R-Rcpp")$canonical_name, "Rcpp")     # display casing restored
  expect_equal(row("R-AER")$origin, "cran")              # casing passes through
  expect_equal(row("R-aae.pop")$origin, "cran")          # dots pass through
  expect_equal(row("R-ComplexHeatmap")$origin, "bioc")   # Bioc R-* build via ledger
  expect_equal(row("R-notacran")$origin, "other")        # unknown token
  expect_true(is.na(row("R-notacran")$canonical_name))
  expect_equal(row("libcurl-devel")$origin, "other")     # no R- prefix -> other
  expect_equal(row("R-base")$origin, "other")            # interpreter runtime, not on CRAN
})

test_that("resolve_identities keeps a recommended CRAN package but drops R-base", {
  maps <- .mk_maps()
  out <- resolve_identities(c("R-MASS", "R-base"), maps)
  expect_equal(out$origin[out$package == "R-MASS"], "cran")   # recommended pkg, on CRAN
  expect_equal(out$origin[out$package == "R-base"], "other")  # base-R runtime, dropped
})

test_that("resolve_identities passes prefix_hint = NULL so Bioc R-* builds do not warn", {
  maps <- .mk_maps()
  expect_no_warning(resolve_identities("R-ComplexHeatmap", maps))
})

.fixture_io <- function(cran = character(0), bioc = character(0), fail_identity = FALSE) {
  d <- tempfile("identity-dbs-"); dir.create(d)
  withr::defer(unlink(d, recursive = TRUE), envir = parent.frame())
  cran_db <- file.path(d, "cran-archive.db"); bioc_db <- file.path(d, "bioc-meta.db")
  .write_names_db(cran_db, "cran_names_all", cran)
  .write_names_db(bioc_db, "bioc_names_all", bioc)
  list(identity_dbs = function() {
    if (isTRUE(fail_identity)) stop("identity assets unreachable")
    list(cran = cran_db, bioc = bioc_db)
  })
}

test_that("resolve_gated_identity classifies through the size gate", {
  io  <- .fixture_io(cran = c("Rcpp", "AER"))
  out <- resolve_gated_identity(io, c("R-Rcpp", "R-notacran"),
                                 cran_floor = 1L, bioc_floor = 0L)
  expect_equal(out$origin[out$package == "R-Rcpp"], "cran")
  expect_equal(out$origin[out$package == "R-notacran"], "other")
})

test_that("resolve_gated_identity errors when the size gate fails", {
  io <- .fixture_io(cran = c("Rcpp", "AER"))
  expect_error(
    resolve_gated_identity(io, "R-Rcpp", cran_floor = 999999L, bioc_floor = 0L),
    "size gate failed")
})

test_that("resolve_gated_identity propagates an unreachable-asset error", {
  io <- .fixture_io(cran = c("Rcpp"), fail_identity = TRUE)
  expect_error(resolve_gated_identity(io, "R-Rcpp", cran_floor = 1L, bioc_floor = 0L),
               "unreachable")
})
