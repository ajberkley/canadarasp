#!/bin/bash
# Local-docker override of upload-windgram-tiles.sh.
#
# Production ssh-tars /mnt/windgram-tiles to the web-server. In compose the
# directory is the shared volume — already visible to web. We keep the
# "remove stale tiles" cleanup that the original script does.
set -e
echo "[local-override] upload-windgram-tiles: shared volume — only running cleanup"
( cd /mnt/windgram-tiles && find . -name "*CMC*" -exec rm {} \; ) || true
