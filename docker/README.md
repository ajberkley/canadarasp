# Local docker stack

Runs the full canadarasp stack (web-server + compute pipeline) locally in
Docker. The production system is two AWS EC2 hosts that talk over `scp` and
AWS API calls; this collapses that into compose services sharing a named
volume that stands in for the production `/mnt` partition.

## Quickstart

```sh
cd docker
make build                      # ~5 min (cached layers; first build longer)
make web                        # apache + lisp services at http://localhost:8080
make verify MODEL=gdps          # download + capped compute, ~10 min
```

After `make verify` you'll have real windgrams and map PNGs visible at
`http://localhost:8080/RASPtable.html`.

## Layout

```
docker/
├── Dockerfile              # one image, three roles (web + compute + download)
├── docker-compose.yml      # services: web, compute, download
├── Makefile                # convenience wrappers
├── web-vhost.conf          # Apache vhost
├── entrypoints/
│   ├── web.sh              # apache + dynamic-windgram + timezone services
│   ├── download.sh MODEL   # pulls one ECCC cycle into /mnt/download-box
│   └── compute.sh MODEL    # stages + runs do-hrdps-plots-continental.sh
└── overrides/              # per-file bind mounts that shadow the production
    └── continental-test/   #   scripts containing ssh / AWS / mkfs.xfs
```

## Make targets

| Target                       | What it does                                                    |
|------------------------------|-----------------------------------------------------------------|
| `make build`                 | Build the canadarasp:local image                                |
| `make web` (or `make up`)    | Start web service in the background                             |
| `make down`                  | Stop and remove the web container (volume persists)             |
| `make logs`                  | Tail web service logs                                           |
| `make download MODEL=gdps`   | Download one ECCC cycle                                         |
| `make compute MODEL=gdps`    | Run the full pipeline (use `LIMIT=N` to cap windgrams)          |
| `make smoke MODEL=gdps`      | Skip-everything mode — only generate-new-variables runs         |
| `make verify MODEL=gdps`     | download + compute with `WINDGRAM_LIMIT=3` for a fast E2E test  |
| `make shell`                 | Open a bash shell in a one-shot compute container               |
| `make clean`                 | Drop the shared `mnt` volume (reset all generated data)         |
| `make nuke`                  | clean + remove the image                                        |

## Run the full pipeline (without Make)

```sh
# 1. Download one cycle (~3.5 GB on disk for GDPS, ~30 GB for HRDPS)
docker compose run --rm download gdps

# 2. Process it
docker compose run --rm compute gdps

# Or capped for fast iteration:
docker compose run --rm -e WINDGRAM_LIMIT=3 compute gdps
```

GDPS is the recommended dev loop: ~10× smaller and runs every-3h × 99h vs
HRDPS's hourly × 48h. **HRDPS-only features:**
- The dynamic `/windgram?lon=...&lat=...` endpoint (Lisp service hard-codes
  `/mnt/windgram-tiles/hrdps/`)
- `clip-wind-to-terrain.sh` (cosmetic)
- `archive-old-map-pngs.sh` (the BC archive feature)

### Note on HRDPS routing

ECCC retired the legacy polar-stereographic HRDPS feed at
`model_hrdps/continental/grib2/`; the `hrdps` branch of
`continental-test/model-parameters.sh` still points at it (and has a
buggy `downloadfilename` that evaluates its output as a shell command,
producing empty URLs). Current HRDPS data lives at
`<date>/WXO-DD/model_hrdps/continental/2.5km/` under rotated-latlon
naming — the `hrdps_rot` branch.

`docker/entrypoints/download.sh` transparently routes
`make download MODEL=hrdps` through `hrdps_rot`; the trailing
`rename-hrdps-rot.sh` in `download-data.sh` rewrites the rotated names
back to the legacy `CMC_hrdps_continental_*` pattern that the rest of
the pipeline expects, so `make compute MODEL=hrdps` keeps working.

## Sizing & timing (measured on Apple Silicon under Rosetta)

GDPS cycle (24 forecast hours every 3h = 99h horizon, ~3300 GRIB2 files):

| Stage                  | Wall (M-series, emulated) | Disk delta            |
|------------------------|---------------------------|-----------------------|
| download from ECCC     | 5–10 min                  | +3.6 GB `/mnt/download-box` |
| do-generate-new-vars   | 3–5 min                   | small                 |
| do-tile-generation     | 6–10 min                  | +240 MB `/mnt/windgram-tiles` |
| do-windgrams (capped 3)| 2–4 min                   | +160 MB `/mnt/windgrams-data` |
| generate map PNGs      | 8–12 min                  | +2.6 GB `/mnt/map-pngs` |
| **end-to-end (capped)**| **~20–30 min**            | **~6.7 GB total**     |

HRDPS is ~10× larger and slower; expect a multi-hour first run.

## What the overrides change

The production scripts under `continental-test/` are bind-mounted unchanged
except for these five which would either ssh out of the container, call AWS,
or wipe `/mnt`:

| Script                          | Local change                                                              |
|---------------------------------|---------------------------------------------------------------------------|
| `setup-drives.sh`               | Skips `sudo rm -rf /mnt` + `mkfs.xfs` + `swapon`; just `mkdir` subdirs.   |
| `webserver-ip.sh`               | Echoes `web` (the compose service name) instead of an EC2 lookup.         |
| `upload-map-pngs.sh`            | `/mnt` is shared with web — runs cleanup + refreshes `latest`.            |
| `upload-windgram-tiles.sh`      | Same — shared volume; only cleanup runs.                                  |
| `do-windgrams-continental.sh`   | Drops the three ssh/scp blocks; `cp`s `locations.js` into `web-server/`; respects `WINDGRAM_LIMIT`. |

`aws-utils/download-data-and-start-server.sh` and
`continental-test/do-rasp-run-continental.sh` are bypassed entirely — the
docker entrypoints take their place (no `git pull`, no AWS-tag reads, no
EBS-volume create).

## Tuning parallelism

Production assumes 16 cores on a `c5d.4xlarge`; this scaffold defaults to 4.
Override per-run:

```sh
PARALLELSUB=8 PARALLELTILE=8 PARALLELNCL=8 docker compose run --rm compute gdps
```

## Cross-platform

| Host                       | State            | Notes                                                                 |
|----------------------------|------------------|-----------------------------------------------------------------------|
| **Linux amd64**            | Works, **fast**  | No emulation — HRDPS pipeline runs in ~45 min like prod.              |
| **macOS Apple Silicon**    | Works, slow      | All amd64 runs under Rosetta. HRDPS is multi-hour.                    |
| **Linux arm64**            | Works, very slow | `platform: linux/amd64` forces qemu-user. Functional, painful.        |
| **Windows + WSL2**         | Works, with care | Run `make` and `docker compose` from inside WSL2, see gotchas below.  |

### Linux

If your host user's UID isn't 1000, the compute container will write files
(`locations.js` etc.) through the bind mount with UID 1000 ownership and
they'll be read-only to you. Either:

```sh
cp docker/.env.example docker/.env
# edit docker/.env and set:
HOST_UID=$(id -u)
HOST_GID=$(id -g)
docker compose build              # rebuild bakes the new UID in
```

…or chown after each compute run. macOS/OrbStack remaps automatically — no
action needed there.

### Windows + WSL2

1. Clone the repo *inside* WSL2 (`/home/<you>/...`), not on the Windows
   filesystem — Windows-side bind mounts are slow over the WSL2 bridge.
2. `.gitattributes` keeps `*.sh` at LF endings even if Git is configured
   with `autocrlf=true`. Don't disable it — bash chokes on CRLF shebangs.
3. Run `make` / `docker compose` from a WSL2 shell. PowerShell works for
   raw `docker compose` but won't honour the Makefile.
4. Same UID story as Linux above.

## Apple Silicon caveats

The compose file pins `platform: linux/amd64`. NCL is x86_64-only — there is
no native arm64 build. On M-series Macs everything runs under Rosetta:

- A real HRDPS pipeline run that takes 45 min on `c5d.4xlarge` native takes
  many hours under emulation. Use GDPS or `WINDGRAM_LIMIT=N` for dev.
- The trimmed image is ~880 MB but the NCL base image (`qcmiao1998/ncl`)
  pulled during build is 4.18 GB — only needed for the build, gets pruned
  with `docker system prune`.

## How NCL is sourced

Conda's solver (mamba, libmamba, micromamba) deadlocks on NCL's dep graph
under amd64 emulation: verified across multiple attempts, 30+ min wall on
"Resolving Environment" with no CPU progress. Rather than fight the solver,
the Dockerfile multi-stage-copies `/opt/conda` from `qcmiao1998/ncl:latest`
(a public image where NCL 6.6.2 is already resolved), which is a fast
`docker pull` regardless of host architecture. NCL's runtime libs
(libnetcdf, libhdf5, libgfortran4, libgdal.so.26, etc.) are self-contained
in `/opt/conda/lib`, so they coexist with the system GDAL 3.8 without
conflict.

If `qcmiao1998/ncl` ever disappears, alternative paths in order of effort:
1. Pull `dtcenter/ncl:latest` instead (NCL 6.3.0 — older, may not be
   bytewise-compatible with the windgram NCL scripts; test before
   committing).
2. Rebuild on a native amd64 host with a `micromamba install ncl=6.6.2`
   step added back to the Dockerfile; the solver completes in ~3 min on
   real hardware.
3. Generate a `conda-lock.yml` for NCL on a native host, commit it, and
   `micromamba create --file conda-lock.yml` to skip solving on builds.

## Inspecting & cleaning state

```sh
# Where's all the data?
docker compose run --rm --no-deps --entrypoint bash compute \
    -c 'du -sh /mnt/*'

# Free everything generated locally (keeps the image)
make clean

# Nuke the image too
make nuke
```

## Front-end caveats (apply to any non-canadarasp.com hostname)

These are not docker-specific — they affect any local copy of the front-end:

- **Google Maps base layer fails on localhost.** `RASPtable.html` hard-codes
  a Google Maps JS API key that's restricted to `canadarasp.com`. On
  `localhost:8080` Maps refuses to render the base map (you'll see a tiny
  `icon_error.png` placeholder). The overlay PNGs the pipeline generates
  still load fine via the JS — they just have no base map to sit on top of.
  Workarounds: replace the key in the HTML with your own unrestricted key,
  or use an `Apache mod_substitute` to swap it at serve time.
- **`windgrams/index.htm` hard-codes `https://canadarasp.com/...` URLs.**
  So the windgrams index page always shows production windgrams, not your
  local ones. Direct PNG access at e.g.
  `http://localhost:8080/windgrams-data/twoDay/gdpswindgram259.png` works
  fine and that's the simplest way to visually verify a local run.

## What this scaffold does *not* do

- No certbot / SSL — the prod vhost uses Let's Encrypt.
- No `airspace.canadarasp.com` virtual host.
- No surface-winds `/data` proxy (lives in the sibling `hrdps-coastal-info`
  repo).
- No CloudWatch metric push from `check-service.sh`.
- No automatic cron — trigger downloads / compute runs by hand or wire
  up a cron in the web container if you want production-like behavior.

## Troubleshooting

- **Build hangs on `Resolving Environment`**: the conda solver deadlocked.
  Restore the multi-stage NCL copy (see "How NCL is sourced" above).
- **`ncl: command not found` at compute time**: the COPY from the NCL stage
  didn't land or PATH is wrong. Check `docker run --rm canadarasp:local
  bash -lc 'which ncl ; echo $PATH'`.
- **HTTP 403 on `/`**: `/home/ubuntu` lost `o+x`. The Dockerfile sets this
  but a custom-mounted home dir could lose it.
- **No data on `http://localhost:8080/map-pngs/...`**: the compute run
  hasn't completed, or it ran for a different model than the front-end is
  currently asking for (use the Model selector in the UI).
- **`/windgram?lon=...&lat=...&interactive=t` returns "Invalid input or
  internal error"**: dynamic windgrams are HRDPS-only (Lisp service hard-
  codes `/mnt/windgram-tiles/hrdps/`). Run the HRDPS pipeline, or expect
  the graceful failure with GDPS-only data.
- **GDAL version errors**: ECCC GRIB2 files require GDAL ≥ 3.6.1. The
  image has 3.8.4 on `/usr/bin/gdal_translate`; if something resolves
  `/opt/conda/bin/gdal_translate` (which is 3.0.4), check `$PATH`
  ordering.
