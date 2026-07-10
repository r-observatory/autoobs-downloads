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
