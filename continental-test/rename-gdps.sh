#!/bin/bash
# Rewrite freshly downloaded GDPS files from the new MSC datamart naming
# (issue #7) back to the legacy `CMC_glb_*` names the rest of the pipeline
# reads. Same role `rename-hrdps-rot.sh` plays for the HRDPS rotated-latlon feed.
#
#   source: 20260606T00Z_MSC_GDPS_DewPointDepression_IsbL-0850_LatLon0.15_PT003H.grib2
#   target: CMC_glb_DEPR_ISBL_850_latlon.15x.15_2026060600_P003.grib2
#
# The new-to-legacy variable map is just the line-by-line correspondence
# between `GDPS-NEW-files.txt` (download names) and `GDPS-files.txt` (legacy
# names), so keep the two files the same length and order.
MODEL=${1:-$MODEL}
DOWNLOADDIRECTORY=${2:-/tmp}
if [ "$MODEL" != "gdps" ]; then
    exit 0
fi

RASPBASEDIR=$(cd "$(dirname "$0")" && pwd)
NEWFILE="$RASPBASEDIR/GDPS-NEW-files.txt"
LEGACYFILE="$RASPBASEDIR/GDPS-files.txt"

# The two parallel files must stay the same length or the map silently corrupts.
n_new=$(wc -l < "$NEWFILE")
n_leg=$(wc -l < "$LEGACYFILE")
if [ "$n_new" != "$n_leg" ]; then
    echo "ERROR: GDPS-NEW-files.txt ($n_new lines) and GDPS-files.txt ($n_leg lines) differ -- aborting rename" >&2
    exit 1
fi

echo Starting renaming GDPS files at `date`
declare -A rewrite
while read -r NEW LEGACY; do
    rewrite[$NEW]=$LEGACY
done < <(paste "$NEWFILE" "$LEGACYFILE")

shopt -s nullglob
for FILE in "$DOWNLOADDIRECTORY"/*_MSC_GDPS_*_LatLon0.15_PT*H.grib2; do
    BASENAME=$(basename "$FILE")
    if [[ $BASENAME =~ ^([0-9]{8})T([0-9]{2})Z_MSC_GDPS_(.+)_LatLon0\.15_PT([0-9]+)H\.grib2$ ]]; then
        DATE=${BASH_REMATCH[1]}
        CYCLE=${BASH_REMATCH[2]}
        TOKEN=${BASH_REMATCH[3]}
        FFF=${BASH_REMATCH[4]}
        LEGACY=${rewrite[$TOKEN]}
        if [ -z "$LEGACY" ]; then
            # a variable we do not care about; leave it alone
            continue
        fi
        TARGET="$DOWNLOADDIRECTORY/CMC_glb_${LEGACY}_latlon.15x.15_${DATE}${CYCLE}_P${FFF}.grib2"
        mv "$FILE" "$TARGET"
    fi
done
echo Done renaming GDPS files at `date`
