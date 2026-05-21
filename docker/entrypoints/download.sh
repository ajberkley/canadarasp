#!/bin/bash
# Download a single model cycle from ECCC into a per-cycle subdir of the
# shared volume. Mirrors what download-data-and-start-server.sh does on the
# web-server in production, minus the EBS-volume-create dance.
#
# Usage: download.sh hrdps | gdps | hrdps_rot
#
# Sets NODEL=1 so download-data.sh doesn't wipe an unrelated download dir;
# we manage cleanup ourselves with per-cycle target directories.
#
# HRDPS routing: ECCC removed the old polar-stereographic HRDPS feed
# (`model_hrdps/continental/grib2/`) at some point; the URL pattern in the
# `hrdps` branch of model-parameters.sh now 404s. The current data lives at
# `<date>/WXO-DD/model_hrdps/continental/2.5km/` under rotated-latlon names
# — the `hrdps_rot` branch. We transparently route `hrdps` → `hrdps_rot` so
# downstream `make compute MODEL=hrdps` keeps working; download-data.sh's
# trailing `rename-hrdps-rot.sh` rewrites the rotated names back to the
# legacy `CMC_hrdps_continental_*` pattern that the rest of the pipeline
# expects.

set -e
USER_MODEL=${1:?model required: hrdps|gdps|hrdps_rot}
FETCH_MODEL=$USER_MODEL
if [ "$USER_MODEL" = "hrdps" ]; then
    echo "[download] HRDPS: routing through hrdps_rot (ECCC removed the old feed)"
    FETCH_MODEL=hrdps_rot
fi

cd /home/ubuntu/canadarasp/continental-test
source ./guess-time.sh "$FETCH_MODEL"
DEST=/mnt/download-box/${USER_MODEL}/${YEAR}${MONTH}${DAY}${HOUR}
mkdir -p "$DEST"
echo "[download] target: $DEST  (fetch_model=$FETCH_MODEL)"

NODEL=1 ./download-data.sh "$FETCH_MODEL" "$DEST"

# Stamp the cycle that's freshest so compute.sh can pick it up.
ln -sfn "$DEST" "/mnt/download-box/${USER_MODEL}/latest"
echo "[download] latest -> $DEST"
