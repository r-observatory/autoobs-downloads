# autoCRAN (OBS) Downloads

Daily per-package download statistics for [autoCRAN](https://build.opensuse.org/project/show/devel:languages:R:autoCRAN), the openSUSE Build Service project that builds most of CRAN as RPMs (`R-<package>`). The counts come from the [MirrorCache](https://github.com/openSUSE/MirrorCache) redirector on `download.opensuse.org`, which serves the binaries and exposes per-package download counters over a small REST API. This pipeline enumerates the autoCRAN package set from the openSUSE repository metadata, records each package's trailing-day download count once per day, and publishes the result as SQLite shard files attached to a single rolling GitHub release tag (`current`).

> [!IMPORTANT]
> **What these numbers mean, and what they do not.**
>
> - **Counts are keyed by package name across all of openSUSE, not scoped to autoCRAN.** MirrorCache aggregates downloads by RPM package name over every repository that serves that name. For the long tail of CRAN packages that exist only in autoCRAN (the large majority), the figure is effectively autoCRAN-only. For a package also shipped in the openSUSE distribution or another devel repo (for example a base or recommended R package), the figure is a superset that includes those other repositories. The `autocran_only` column marks which is which: roughly 98% of packages are autoCRAN-only and their counts are exact, while the roughly 2% that are shared are the popular base and recommended packages whose counts are supersets. Use `WHERE autocran_only = 1` when you need exact autoCRAN figures.
> - **The daily series is built by this pipeline.** MirrorCache does not expose a historical time series, only rolling counters. Each run reads `cnt_1d` (the trailing-day count) and stores it as the count for the UTC day that just ended. The per-day series in `autoobs_downloads_daily` therefore begins on this pipeline's first run.
> - **The summary windows come from MirrorCache directly.** `total_7d`, `total_30d`, and `cnt_total` in `autoobs_downloads_summary` are MirrorCache's own rolling counters as of the latest snapshot, so the summary is meaningful from the first run (MirrorCache began per-package tracking around 2025-12-31). `trend` is computed from this pipeline's accumulated daily series and is therefore `NULL` until roughly 60 days of history exist.
> - **`cnt_total` is unreliable for recently added packages.** For packages MirrorCache started tracking recently, the all-time `cnt_total` is often smaller than `total_30d`. Prefer the rolling windows.
> - **Counts include mirror and bot traffic.** Downloads are redirect events at the openSUSE download host and include mirrors, CI systems, containers, and crawlers. Treat the figures as relative popularity and trend signals, not as distinct human installs.
> - **Not comparable to `cran-downloads`, `r2u-downloads`, or `bioconductor-downloads`.** Different platforms, populations, and counting methods. Do not compare magnitudes across datasets.

## Data Access

All shards live as assets on the [`current` release](https://github.com/r-observatory/autoobs-downloads/releases/tag/current). Each daily run uploads only the shards that changed; the rest remain unchanged.

### Recent data (last 400 days)

For most use cases this is the only file you need. It holds the rolling 400-day window of `autoobs_downloads_daily` plus the full `autoobs_downloads_summary` table.

```bash
gh release download current \
  --repo r-observatory/autoobs-downloads \
  --pattern "autoobs-downloads-recent.db"
```

```r
url <- "https://github.com/r-observatory/autoobs-downloads/releases/download/current/autoobs-downloads-recent.db"
download.file(url, "autoobs-downloads-recent.db", mode = "wb")

library(RSQLite)
con <- dbConnect(SQLite(), "autoobs-downloads-recent.db")

# Daily downloads for R-Rcpp over the last 30 days
dbGetQuery(con, "
  SELECT date, count
  FROM autoobs_downloads_daily
  WHERE package = 'R-Rcpp'
  ORDER BY date DESC LIMIT 30
")

# Top 20 packages by 30-day downloads
dbGetQuery(con, "
  SELECT package, total_30d, total_7d, cnt_total, rank_30d
  FROM autoobs_downloads_summary
  ORDER BY rank_30d LIMIT 20
")

dbDisconnect(con)
```

```python
import urllib.request, sqlite3
url = "https://github.com/r-observatory/autoobs-downloads/releases/download/current/autoobs-downloads-recent.db"
urllib.request.urlretrieve(url, "autoobs-downloads-recent.db")

con = sqlite3.connect("autoobs-downloads-recent.db")
for row in con.execute("""
    SELECT package, total_30d, rank_30d
    FROM autoobs_downloads_summary
    ORDER BY rank_30d LIMIT 10"""):
    print(row)
con.close()
```

### Per-year archives

Each calendar year of the daily series has its own shard (history begins the year this pipeline launched):

```bash
gh release download current \
  --repo r-observatory/autoobs-downloads \
  --pattern "autoobs-downloads-2026.db"
```

### Full history (all years)

```bash
gh release download current \
  --repo r-observatory/autoobs-downloads \
  --pattern "autoobs-downloads-*.db"
```

### Summary only

For top-package lists, ranks, and the current windows with the smallest download:

```bash
gh release download current \
  --repo r-observatory/autoobs-downloads \
  --pattern "autoobs-downloads-summary.db"
```

### Manifest

`manifest.json` lists which shards changed in the most recent run, the source kind (`mirrorcache` for a live read, `frozen` for a heartbeat when the source was unreachable), per-shard coverage, and freshness timestamps.

```bash
gh release download current \
  --pattern manifest.json \
  --repo r-observatory/autoobs-downloads
cat manifest.json
```

## Example Queries

### Daily downloads for a package

```sql
SELECT date, count
  FROM autoobs_downloads_daily
 WHERE package = 'R-data.table'
 ORDER BY date DESC
 LIMIT 30;
```

### Top packages by 30-day downloads

```sql
SELECT package, total_30d, total_7d, avg_daily_30d, rank_30d
  FROM autoobs_downloads_summary
 ORDER BY rank_30d
 LIMIT 50;
```

### Fastest-growing packages (once trend is populated)

```sql
SELECT package, total_30d, trend
  FROM autoobs_downloads_summary
 WHERE trend IS NOT NULL
 ORDER BY trend DESC
 LIMIT 20;
```

### Top packages with exact (autoCRAN-only) counts

```sql
SELECT package, total_30d, rank_30d
  FROM autoobs_downloads_summary
 WHERE autocran_only = 1
 ORDER BY total_30d DESC
 LIMIT 50;
```

## Schema

### `autoobs_downloads_daily`

One row per package per day. The count is MirrorCache's trailing-day `cnt_1d` attributed to the UTC day that just ended; zero-count days are omitted to keep the long-tail series compact. Present in `autoobs-downloads-recent.db` (last 400 days) and each `autoobs-downloads-YYYY.db` archive.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | RPM package name, e.g. `R-Rcpp` (PK part 1) |
| `date` | TEXT | The UTC day the downloads occurred, `YYYY-MM-DD` (PK part 2) |
| `count` | INTEGER | Downloads on that day (MirrorCache `cnt_1d`) |

### `autoobs_downloads_summary`

Per-package standing, rebuilt each run. The window counts are MirrorCache's own rolling counters as of the latest snapshot; `trend` is derived from this pipeline's daily series. Present in `autoobs-downloads-recent.db` and `autoobs-downloads-summary.db`.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | RPM package name (PK) |
| `package_lower` | TEXT | Lowercased helper column for case-insensitive joins |
| `id` | INTEGER | MirrorCache numeric package id |
| `total_1d` | INTEGER | MirrorCache `cnt_1d`, the latest trailing-day count |
| `total_7d` | INTEGER | MirrorCache `cnt_7d`, rolling 7-day downloads |
| `total_30d` | INTEGER | MirrorCache `cnt_30d`, rolling 30-day downloads |
| `cnt_total` | INTEGER | MirrorCache `cnt_total`, all-time (unreliable for recently added packages) |
| `avg_daily_30d` | REAL | `total_30d` divided by 30 |
| `rank_30d` | INTEGER | Rank by `total_30d` |
| `rank_total` | INTEGER | Rank by `cnt_total` |
| `trend` | REAL | Percent change: last 30 days vs prior 30 of the local daily series; `NULL` until enough history |
| `autocran_only` | INTEGER | `1` if every openSUSE location of this name is under autoCRAN (count is exact); `0` if also served elsewhere (count is a superset); `NULL` if not yet classified |
| `first_seen` | TEXT | Date MirrorCache began tracking the package (`YYYY-MM-DD`) |
| `last_snapshot` | TEXT | UTC date of the run that produced this row |

### `autoobs_packages`

The package-name to MirrorCache-id cache, carried inside `autoobs-downloads-recent.db` so each run only resolves newly added packages and reuses the `autocran_only` classification between runs.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | RPM package name (PK) |
| `id` | INTEGER | MirrorCache numeric package id |
| `autocran_only` | INTEGER | `1` if served only by autoCRAN, `0` if also served elsewhere, `NULL` if not yet classified |

## How it works

A daily GitHub Actions job (04:00 UTC) enumerates the autoCRAN package names from each openSUSE repository's rpm-md `primary.xml.gz`, resolves any newly seen names to MirrorCache ids (reusing the cached map for the rest), and fetches each package's `stat_download` counters concurrently with a small connection pool to stay polite on the volunteer-run download host. It also classifies each package as autoCRAN-only or shared by reading its `package_locations` (every newly seen name each run, and the full set at most once a week, since repository membership changes slowly). The trailing-day count for every package is appended to the history pulled from the `current` release, the affected year shard plus the rolling `autoobs-downloads-recent.db` and `autoobs-downloads-summary.db` are rebuilt, and only the changed shards are uploaded (with `manifest.json` last, so a crash leaves the prior state authoritative). When MirrorCache is unreachable the run is a cheap heartbeat that refreshes `last_checked` and leaves the prior release intact.

## Attribution

Download counts are read from the public MirrorCache REST API on `download.opensuse.org`; the RPMs are built by the openSUSE Build Service [devel:languages:R:autoCRAN](https://build.opensuse.org/project/show/devel:languages:R:autoCRAN) project. This repository provides only the daily snapshotting and packaging into SQLite. Please respect the openSUSE download infrastructure and terms.

## License

The pipeline code in this repository is proprietary. Copyright (c) 2026 HJJB, LLC. All rights reserved; see [LICENSE](LICENSE). The underlying download counts originate from the openSUSE project.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
