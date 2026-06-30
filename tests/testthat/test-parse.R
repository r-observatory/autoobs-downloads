test_that("parse_repomd_primary_href finds the primary location", {
  xml <- '<?xml version="1.0"?>
<repomd xmlns="http://linux.duke.edu/metadata/repo">
  <data type="primary"><location href="repodata/abc-primary.xml.gz"/></data>
  <data type="filelists"><location href="repodata/def-filelists.xml.gz"/></data>
</repomd>'
  expect_equal(parse_repomd_primary_href(xml), "repodata/abc-primary.xml.gz")
})

test_that("parse_repomd_primary_href returns NA when there is no primary entry", {
  expect_true(is.na(parse_repomd_primary_href("<repomd></repomd>")))
})

test_that("parse_primary_names extracts unique sorted package names (string or lines)", {
  xml <- paste(
    '<metadata xmlns="http://linux.duke.edu/metadata/common">',
    '<package type="rpm"><name>R-Rcpp</name><arch>x86_64</arch></package>',
    '<package type="rpm"><name>R-Rcpp</name><arch>i586</arch></package>',
    '<package type="rpm"><name>R-aae.pop</name><arch>x86_64</arch></package>',
    '</metadata>', sep = "\n")
  expect_equal(parse_primary_names(xml), c("R-Rcpp", "R-aae.pop"))
  # Accepts a vector of lines too (what the producer passes for the big file).
  expect_equal(parse_primary_names(strsplit(xml, "\n", fixed = TRUE)[[1]]),
               c("R-Rcpp", "R-aae.pop"))
})

test_that("parse_primary_names handles compact (single-line, multi-name) XML", {
  compact <- '<metadata><package><name>R-a</name></package><package><name>R-b</name></package><package><name>R-c</name></package></metadata>'
  expect_equal(parse_primary_names(compact), c("R-a", "R-b", "R-c"))
})

test_that("parse_id_response reads the numeric id, NA on a bad body", {
  expect_equal(parse_id_response('{"id":60612,"name":"R-base"}'), 60612L)
  expect_true(is.na(parse_id_response("not json")))
  expect_true(is.na(parse_id_response('{"error":"not found"}')))
})

test_that("parse_stat_response coerces the string counters to integers", {
  body <- '{"data":[{"cnt_1d":"196","cnt_30d":"6612","cnt_7d":"1559","cnt_today":"12","cnt_total":"9424","first_seen":1767139200}]}'
  s <- parse_stat_response(body)
  expect_equal(s$cnt_1d, 196L)
  expect_equal(s$cnt_30d, 6612L)
  expect_equal(s$cnt_total, 9424L)
  expect_equal(s$first_seen, 1767139200L)
})

test_that("parse_stat_response returns all-NA for empty data or junk", {
  s <- parse_stat_response('{"data":[]}')
  expect_true(all(is.na(unlist(s))))
  expect_true(all(is.na(unlist(parse_stat_response("")))))
})

test_that("parse_location_paths extracts the repo paths", {
  body <- '{"data":[
    {"file":"R-x-1.rpm","name":"R-x","path":"/repositories/devel:/languages:/R:/autoCRAN/openSUSE_Tumbleweed/x86_64"},
    {"file":"R-x-1.src.rpm","name":"R-x","path":"/repositories/devel:/languages:/R:/autoCRAN/openSUSE_Tumbleweed/src"}]}'
  p <- parse_location_paths(body)
  expect_length(p, 2L)
  expect_true(all(grepl("autoCRAN", p)))
  expect_length(parse_location_paths('{"data":[]}'), 0L)
})

test_that("classify_autocran_only flags only-autoCRAN names", {
  expect_equal(classify_autocran_only(c(
    "/repositories/devel:/languages:/R:/autoCRAN/openSUSE_Tumbleweed/x86_64",
    "/repositories/devel:/languages:/R:/autoCRAN/openSUSE_Leap_16.0/x86_64")), 1L)
  expect_equal(classify_autocran_only(c(
    "/repositories/devel:/languages:/R:/autoCRAN/openSUSE_Tumbleweed/x86_64",
    "/factory/repo/oss/x86_64")), 0L)            # also in the distribution
  expect_true(is.na(classify_autocran_only(character(0))))
})
