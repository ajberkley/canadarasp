#!/bin/bash
# Run the data-processing pipeline for one model cycle.
#
# Usage: compute.sh MODEL [smoke]
#   MODEL  - hrdps | gdps | hrdps_rot
#   smoke  - if present, sets NOTILES=NOWINDGRAMS=NOMAP=NOUPLOAD=NOCLIP=NOFIX=1
#            so the scaffold can be exercised end-to-end without a full run.
#
# Assumes download.sh has already populated /mnt/download-box/$MODEL/latest.
# Skips the production wrapper (do-rasp-run-continental.sh) entirely — that
# script does `git pull`, AWS-tag reads, and an XFS reformat which we don't
# want locally. We jump straight to do-hrdps-plots-continental.sh after a
# `cp` from the download dir into /mnt/input/$MODEL.

set -e
MODEL=${1:?model required: hrdps|gdps|hrdps_rot}
MODE=${2:-full}

cd /home/ubuntu/canadarasp/continental-test
source ./model-parameters.sh "$MODEL"
source ./guess-time.sh "$MODEL"

DOWNLOAD_DIR=/mnt/download-box/${MODEL}/latest
if [ ! -d "$DOWNLOAD_DIR" ]; then
    echo "[compute] no download at $DOWNLOAD_DIR — run docker compose run --rm download $MODEL first"
    exit 1
fi

# guess-time.sh picks the cycle that *should* exist based on the current
# clock. If the most recently downloaded cycle differs (e.g. you downloaded
# overnight and ECCC has since published a new run), use the downloaded one
# rather than fail looking for files that don't exist locally.
DOWNLOADED_CYCLE=$(basename "$(readlink "$DOWNLOAD_DIR")")
EXPECTED_CYCLE="${YEAR}${MONTH}${DAY}${HOUR}"
if [ "$DOWNLOADED_CYCLE" != "$EXPECTED_CYCLE" ]; then
    echo "[compute] clock expects cycle $EXPECTED_CYCLE but you have $DOWNLOADED_CYCLE; using $DOWNLOADED_CYCLE"
    export YEAR=${DOWNLOADED_CYCLE:0:4}
    export MONTH=${DOWNLOADED_CYCLE:4:2}
    export DAY=${DOWNLOADED_CYCLE:6:2}
    export HOUR=${DOWNLOADED_CYCLE:8:2}
fi

echo "[compute] $MODEL cycle ${YEAR}-${MONTH}-${DAY} ${HOUR}Z (mode=$MODE)"

# The override of setup-drives.sh just ensures the subdirs exist.
./setup-drives.sh

# Stage data: hard-link from the download dir into /mnt/input/$MODEL so the
# pipeline's hard-coded $OUTPUTDIR finds it. Hard links so a re-run doesn't
# blow away the downloaded copy; falls back to cp on cross-device.
mkdir -p "$OUTPUTDIR"
cp -alf "$DOWNLOAD_DIR"/* "$OUTPUTDIR"/ 2>/dev/null \
    || cp -af "$DOWNLOAD_DIR"/* "$OUTPUTDIR"/

# Tune parallelism for a laptop — production assumes 16 cores. Honour
# anything the caller already set.
export PARALLELSUB=${PARALLELSUB:-4}
export PARALLELTILE=${PARALLELTILE:-4}
export PARALLELNCL=${PARALLELNCL:-4}

if [ "$MODE" = "smoke" ]; then
    export NOTILES=1 NOWINDGRAMS=1 NOMAP=1 NOUPLOAD=1 NOCLIP=1 NOFIX=1
    echo "[compute] smoke mode — only running do-generate-new-variables.sh"
fi

# NCL is the heaviest dep and refuses to install under amd64 emulation; the
# image ships without it. If it's not on PATH, skip the windgram step rather
# than die. Map PNGs and GRIB2 tile generation still run.
if ! command -v ncl >/dev/null 2>&1 && [ -z "$NOWINDGRAMS" ]; then
    echo "[compute] WARNING: ncl not found — skipping windgrams (NOWINDGRAMS=1)"
    echo "[compute] map PNGs and windgram-tile GRIBs will still be generated"
    export NOWINDGRAMS=1
fi

# Hand off to the production orchestrator. Overrides (mounted via compose)
# replace the ssh-bearing scripts it calls.
./do-hrdps-plots-continental.sh "$YEAR" "$MONTH" "$DAY" "$HOUR"

echo "[compute] done"
