#!/bin/bash
# Local-docker override of upload-map-pngs.sh.
#
# Production ssh's into the web-server and tars new PNGs into ~/html/map-pngs.
# In compose, the compute and web services share /mnt as a volume — so the
# PNGs are *already* visible to apache the moment they're written. All we do
# locally is run the cleanup scripts (so disk doesn't grow unbounded) and
# bump the `latest` symlink.
set -e
source ./model-parameters.sh "$MODEL"
echo "[local-override] upload-map-pngs: $PNGDIR is shared with web; only refreshing 'latest' + cleanup"

# Mirror the production cleanup. delete-old-map-pngs is generic; archive is
# HRDPS-only (the BC archive feature) and only runs if a target dir exists.
"$(dirname "$0")/../delete-old-map-pngs.sh" || true
if [ "$MODEL" = "hrdps" ] && [ -d /mnt/hrdps-map-archive ]; then
    "$(dirname "$0")/../archive-old-map-pngs.sh" || true
fi

# Recreate the `latest` symlink pointing at the newest dated subdir.
if [ -d "$PNGDIR" ]; then
    cd "$PNGDIR"
    newest=$(ls -1rt | grep -v '^latest$' | tail -1 || true)
    if [ -n "$newest" ]; then
        rm -f latest
        ln -s "$newest" latest
        echo "[local-override] upload-map-pngs: latest -> $newest"
    fi
fi
