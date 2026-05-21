#!/bin/bash
# Local-docker override of do-windgrams-continental.sh.
#
# Same compute steps as production, but the three ssh/scp blocks (windgram
# tar upload, locations.js scp, tile upload) are replaced with local cp into
# the shared /mnt volume / bind-mounted source tree. Honours the same env
# switches the production script honours.
set -e
UTCYEAR=$1
UTCMONTH=$2
UTCDAY=$3
HOUR=$4
OUT_DIR=/mnt/windgrams-data
mkdir -p "$OUT_DIR" "$OUT_DIR/twoDay" "$OUT_DIR/oneDay"
for i in $(seq 0 1 5); do
    DAYA=$(date -d "$UTCYEAR-$UTCMONTH-$UTCDAY $HOUR UTC +$i day" +%Y-%m-%d)
    mkdir -p "$OUT_DIR/oneDay/$DAYA"
done
echo "[local-override] UTC starting time $UTCYEAR-$UTCMONTH-$UTCDAY $HOUR"

cd plot-generation

if [ -z "$NOCALCULATE" ]; then
    echo "[local-override] computing windgrams"
    ./locations-group-by-id-and-run-windgrams.lisp
    # WINDGRAM_LIMIT caps the number of tile invocations — useful for fast
    # local verification (each tile renders ~6 sites and takes ~30s under
    # emulation, so the full 305-site set is many minutes).
    if [ -n "$WINDGRAM_LIMIT" ]; then
        head -n "$WINDGRAM_LIMIT" run-my-windgrams.sh > run-my-windgrams.sh.tmp
        mv run-my-windgrams.sh.tmp run-my-windgrams.sh
        echo "[local-override] WINDGRAM_LIMIT=$WINDGRAM_LIMIT  (capping NCL invocations)"
    fi
    export NCARG_ROOT=${NCARG_ROOT:-/home/ubuntu/NCARG}
    PARALLEL_JOBS=${PARALLELNCL:-4}
    parallel --gnu -j "$PARALLEL_JOBS" < run-my-windgrams.sh
    echo "[local-override] windgrams done"
fi

# The compute output already lives in /mnt/windgrams-data which the web
# container serves via apache (shared volume). No upload step needed.

# locations.js / new-locations.js are referenced by the front-end relative
# paths in web-server/ and web-server/windgrams/. Refresh both copies.
if [ -z "$NOUPLOAD" ]; then
    ./make-new-locations.sh
    cp locations.js new-locations.js /home/ubuntu/canadarasp/web-server/
    cp locations.js new-locations.js /home/ubuntu/canadarasp/web-server/windgrams/
    echo "[local-override] locations.js / new-locations.js copied into web-server/"
fi

echo "[local-override] running tile cleanup"
cd ..
./upload-windgram-tiles.sh

date > /mnt/windgrams-done
