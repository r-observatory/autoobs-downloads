# autoCRAN (OBS) Downloads

Daily per-package download statistics for [autoCRAN](https://build.opensuse.org/project/show/devel:languages:R:autoCRAN), the openSUSE Build Service project that builds most of CRAN as RPMs (`R-<package>`). The counts come from the [MirrorCache](https://github.com/openSUSE/MirrorCache) redirector on `download.opensuse.org`, which serves the binaries and exposes per-package download counters over a small REST API. This pipeline enumerates the autoCRAN package set from the openSUSE repository metadata, records each package's trailing-day download count once per day, and publishes the result as SQLite shard files attached to a single rolling GitHub release tag (`current`).

> [!IMPORTANT]
> **What these numbers mean, and what they do not.**
>
> - **Counts are keyed by package name across all of openSUSE, not scoped to autoCRAN.** MirrorCache aggregates downloads by RPM package name over every repository that serves that name. For the long tail of CRAN packages that exist only in autoCRAN (the large majority), the figure is effectively autoCRAN-only. For a package also shipped in the openSUSE distribution or another devel repo (for example a base or recommended R package), the figure is a superset that includes those other repositories. The `autocran_only` column marks which is which: roughly 98% of packages are autoCRAN-only and their counts are exact, while the roughly 2% that are shared are the popular base and recommended packages whose counts are supersets. Use `WHERE autocran_only = 1` when you need exact autoCRAN figures.
> - **The daily series is built by this pipeline.** MirrorCache does not expose a historical time series, only rolling counters. Each run reads `cnt_1d` (the trailing-day count) and stores it as the count for the UTC day that just ended. The per-day series in `autoobs_downloads_daily` therefore begins on this pipeline's first run. MirrorCache counts a day late and at no fixed hour, so some runs find the day not yet counted; those days are filled at a later run from the 7- and 30-day windows (see [How a missed day is filled](#how-a-missed-day-is-filled)), and `autoobs_days` says which days hold data.
> - **The summary windows come from MirrorCache directly.** `total_7d`, `total_30d`, and `cnt_total` in `autoobs_downloads_summary` are MirrorCache's own rolling counters as of the latest snapshot, so the summary is meaningful from the first run. `trend` is computed from this pipeline's accumulated daily series and is therefore `NULL` until roughly 60 days of history exist.
> - **`cnt_total` is not a lifetime count.** It is MirrorCache's retained total: the sum of the rows MirrorCache keeps at the package's newest "total" timestamp. On 2026-09-30 it was 0 for 24,918 of 28,832 packages and often smaller than `total_30d`. Prefer the rolling windows.
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

### Raw counters (last 40 days)

`autoobs-counters-recent.db` holds the five MirrorCache counters that ok runs read, for every package that returned numbers, in scope or not, over the last 40 days. A heartbeat has no counters to add, and a run that records `download_failed` adds none. Join it to `autoobs_runs` (in the recent and summary shards) on `run_id` for each run's date and state.

```bash
gh release download current \
  --repo r-observatory/autoobs-downloads \
  --pattern "autoobs-counters-recent.db"
```

### Manifest

`manifest.json` lists which shards changed in the most recent run, the newest day the series holds (`summary.latest_date`), whether MirrorCache had counted the day before the run (`summary.day_aggregated`), the last day inside the run's windows (`summary.window_end`), the days the run filled (`summary.days_filled`), the source kind (`mirrorcache` for a live read, `frozen` for a heartbeat when the source was unreachable), per-shard coverage, and freshness timestamps. The `counters` key describes the raw counters asset: its name, whether this run uploaded it (`published`), what the run found before it (`prior`: `loaded`, `none` or `download_failed`), and the rows, runs and first and last run it holds. The counters asset is never listed under `shards`.

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

One row per package per day. The count is MirrorCache's trailing-day `cnt_1d` attributed to the UTC day that just ended, or a value solved from the windows for a day filled later; zero-count days are omitted to keep the long-tail series compact, so read `autoobs_days` to tell a zero from a day with no data. Present in `autoobs-downloads-recent.db` (last 400 days) and each `autoobs-downloads-YYYY.db` archive.

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
| `total_1d` | INTEGER | MirrorCache `cnt_1d`, the count for the day before the snapshot; `NULL` when MirrorCache had not counted that day yet |
| `total_7d` | INTEGER | MirrorCache `cnt_7d`, rolling 7-day downloads |
| `total_30d` | INTEGER | MirrorCache `cnt_30d`, rolling 30-day downloads |
| `cnt_total` | INTEGER | MirrorCache `cnt_total`, its retained total. Not a lifetime count: 0 for most packages |
| `avg_daily_30d` | REAL | `total_30d` divided by 30 |
| `rank_30d` | INTEGER | Rank by `total_30d` |
| `trend` | REAL | Percent change: last 30 days vs prior 30 of the local daily series; `NULL` until enough history |
| `autocran_only` | INTEGER | `1` if every openSUSE location of this name is under autoCRAN (count is exact); `0` if also served elsewhere (count is a superset); `NULL` if not yet classified |
| `first_seen` | TEXT | Oldest day MirrorCache still keeps for the package (`YYYY-MM-DD`). This is MirrorCache's retention edge, not the day it began counting the package |
| `last_snapshot` | TEXT | UTC date of the run that produced this row |

### `autoobs_packages`

The package-name to MirrorCache-id cache, carried inside `autoobs-downloads-recent.db` so each run only resolves newly added packages and reuses the `autocran_only` classification between runs.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | RPM package name (PK) |
| `id` | INTEGER | MirrorCache numeric package id |
| `autocran_only` | INTEGER | `1` if served only by autoCRAN, `0` if also served elsewhere, `NULL` if not yet classified |

### `autoobs_runs`

One row per run that uploaded the recent shard, heartbeats included. A run that stops before that upload, or never starts, leaves no row. An ok run writes the full table to `autoobs-downloads-recent.db` and `autoobs-downloads-summary.db`. A heartbeat adds its row to the recent shard only, so that row reaches the summary shard at the next ok run.

| Column | Type | Description |
|---|---|---|
| `run_id` | INTEGER | Run start in epoch seconds (PK) |
| `run_at` | TEXT | Run start, ISO 8601 UTC |
| `snapshot_date` | TEXT | UTC date of the run (S) |
| `source` | TEXT | `run` for a pipeline run |
| `outcome` | TEXT | `ok`, or `heartbeat` when nothing was collected |
| `reason` | TEXT | Why a heartbeat happened |
| `repos_listed` | INTEGER | autoCRAN repositories whose `primary.xml` was read |
| `names_listed` | INTEGER | Package names enumerated (0 when enumeration failed and the cached set was used) |
| `ids_cached` | INTEGER | Names already mapped to MirrorCache ids |
| `ids_new` | INTEGER | Names newly resolved this run |
| `stats_requested` | INTEGER | Packages whose counters were requested |
| `stats_responded` | INTEGER | Requests that returned a body |
| `stats_non_na` | INTEGER | Packages kept after dropping all-NA answers |
| `pos_today`, `pos_1d`, `pos_7d`, `pos_30d`, `pos_total` | INTEGER | Packages with a positive value of each counter |
| `sum_1d`, `sum_7d`, `sum_30d` | INTEGER | Sums of those counters |
| `day_aggregated` | INTEGER | `1` when any `cnt_1d` was positive, meaning MirrorCache had counted day S-1 |
| `window_end` | TEXT | Last day the run's windows can reach: S-1 when aggregated, else S-2 at best |
| `days_filled` | INTEGER | Days this run solved from its windows, including days recorded as `upstream_missing` |
| `fill_rejected` | INTEGER | Days a window would have filled but refused because a package came out negative |
| `window_residual` | INTEGER | Packages whose fully known window does not equal the stored days, a sign MirrorCache revised a day after it was stored |
| `in_scope` | INTEGER | Rows in the summary |
| `counters_prior` | TEXT | `loaded`, `none` or `download_failed` |
| `counters_published` | INTEGER | `1` when this run's counters went into the counters asset |

### `autoobs_days`

One row per day that holds data. A day missing here is a hole in the series. Present in `autoobs-downloads-recent.db` (every day) and each `autoobs-downloads-YYYY.db` (that year's days).

| Column | Type | Description |
|---|---|---|
| `date` | TEXT | The UTC day (PK) |
| `method` | TEXT | `cnt_1d` (read directly), `window` (solved from the windows) or `upstream_missing` (the windows show MirrorCache never counted the day, so every package is 0) |
| `run_id` | INTEGER | Run that wrote the day; `NULL` for days stored before this table existed |
| `packages` | INTEGER | Packages with a positive count that day |
| `downloads` | INTEGER | Sum of the day's counts |

### `autoobs_counters`

In `autoobs-counters-recent.db`. One row per run and package, for runs in the last 40 days.

| Column | Type | Description |
|---|---|---|
| `run_id` | INTEGER | `autoobs_runs.run_id` (PK part 1) |
| `package` | TEXT | RPM package name, as in `autoobs_downloads_daily` (PK part 2) |
| `cnt_today`, `cnt_1d`, `cnt_7d`, `cnt_30d`, `cnt_total` | INTEGER | MirrorCache's counters as that run read them |

## How it works

A daily GitHub Actions job (04:00 UTC) enumerates the autoCRAN package names from each openSUSE repository's rpm-md `primary.xml.gz`, resolves any newly seen names to MirrorCache ids (reusing the cached map for the rest), and fetches each package's `stat_download` counters concurrently with a small connection pool to stay polite on the volunteer-run download host. It also classifies each package as autoCRAN-only or shared by reading its `package_locations` (every newly seen name each run, and the full set at most once a week, since repository membership changes slowly). The trailing-day count for every package is appended to the history pulled from the `current` release, the affected year shard plus the rolling `autoobs-downloads-recent.db` and `autoobs-downloads-summary.db` are rebuilt, and only the changed shards are uploaded (with `manifest.json` last, so a crash leaves the prior state authoritative). The run also adds its row to `autoobs_runs` and its raw counters to `autoobs-counters-recent.db`, dropping runs older than 40 days. That asset is not the durable record, so when it cannot be downloaded the run carries on, records `download_failed`, and leaves the asset on the release as it was. When MirrorCache is unreachable the run is a heartbeat: it adds its row to `autoobs_runs` in the recent shard, uploads that shard, and leaves the daily series and summary as they were.

## How a missed day is filled

A run on UTC day S reads `cnt_7d`, covering S-7 through the last day MirrorCache has counted, and `cnt_30d`, covering S-30 through the same day. That last day is S-1 when any package has a positive `cnt_1d`, and otherwise S-2 at the latest (`window_end` in `autoobs_runs`). Each window, and the 30-day window minus the 7-day one, is an equation: the counter equals the sum of the daily counts over its days. When exactly one day in a window is missing from `autoobs_days`, and no day falls before the first stored day, each package's count for that day is the counter minus the stored counts on the window's other days, with 0 for a known day where the package has no row. The run repeats this until no window has a single missing day.

- If any package comes out negative, the day is refused and counted in `fill_rejected`.
- If the day sums to 0 across all packages, it is recorded as `upstream_missing` and gets no rows.
- A run that found the day before it not yet counted (`day_aggregated` 0) cannot tell whether MirrorCache had counted S-2 either. When S-2 is the missing day and comes out as 0, the run leaves it missing instead of recording `upstream_missing`, because MirrorCache may only be more than a day late. A later run fills the day once one of its windows holds it as the only missing day.
- When such a run has no positive `cnt_7d` for any package, MirrorCache may be more than a week behind, and the 30-day window minus the 7-day one then stops at the last day it counted and does not reach S-8. When S-8 is the missing day and comes out as 0, the run leaves it missing in the same way.
- A package with no counters in the run gets no row on a filled day.
- A filled day is written into its year shard, and that shard is listed in `changed_shards`, even when it belongs to the previous year.

A rerun on the same day replaces the attributed day only for packages that returned a count, and only once MirrorCache has counted that day, so a rerun with failed fetches never shrinks a stored day.

## Attribution

Download counts are read from the public MirrorCache REST API on `download.opensuse.org`; the RPMs are built by the openSUSE Build Service [devel:languages:R:autoCRAN](https://build.opensuse.org/project/show/devel:languages:R:autoCRAN) project. This repository provides only the daily snapshotting and packaging into SQLite. Please respect the openSUSE download infrastructure and terms.

## License

The pipeline code in this repository is proprietary. Copyright (c) 2026 HJJB, LLC. All rights reserved; see [LICENSE](LICENSE). The underlying download counts originate from the openSUSE project.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
