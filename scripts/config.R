# scripts/config.R: pipeline constants (sourced by helpers.R consumers and update.R).

# autoCRAN is the openSUSE Build Service project devel:languages:R:autoCRAN, which
# builds most of CRAN as RPMs. Binaries (and the download counters) are served by
# MirrorCache on download.opensuse.org, NOT by OBS itself.
OBS_PROJECT <- "devel:languages:R:autoCRAN"

# MirrorCache REST host. Per-package download counts come from:
#   GET /rest/package/<name>            -> {"id": ...}
#   GET /rest/package/<id>/stat_download -> rolling cnt_1d/cnt_7d/cnt_30d/cnt_total
MIRRORCACHE_BASE <- "https://download.opensuse.org"

# Repository roots that carry autoCRAN binaries. Package names are enumerated from
# each repo's rpm-md primary.xml.gz and unioned (a name shipped by any repo counts).
AUTOCRAN_REPO_BASE <- "https://download.opensuse.org/repositories/devel:/languages:/R:/autoCRAN"
AUTOCRAN_REPOS     <- c("openSUSE_Tumbleweed", "openSUSE_Leap_16.0", "15.6")

PUBLISH_REPO <- "r-observatory/autoobs-downloads"

USER_AGENT <- "r-observatory-autoobs-downloads/1.0 (+https://github.com/r-observatory/autoobs-downloads)"

# Rolling window carried in autoobs-downloads-recent.db (days of the daily series).
RECENT_WINDOW_DAYS <- 400L

# Concurrent MirrorCache connections. Kept small to be a polite guest on the
# volunteer-run openSUSE download infrastructure (~23k packages per daily run).
FETCH_POOL <- 6L

# How often to re-classify every package as autoCRAN-only vs also-shipped-elsewhere
# (via package_locations). New names are classified every run; the full set is
# refreshed at most this often, since repository membership changes slowly.
CLASSIFY_REFRESH_DAYS <- 7L

# Optional cap on the number of packages processed (0 = all). Used only for quick
# local smoke tests; production leaves it unset.
PACKAGE_LIMIT <- as.integer(Sys.getenv("AUTOOBS_LIMIT", "0"))
