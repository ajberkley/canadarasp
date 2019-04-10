#!/bin/bash
# Usage ./download-data.sh model directory
MODEL=${1:-$MODEL}
DOWNLOADDIRECTORY=$2
echo Downloading $MODEL data to $DOWNLOADDIRECTORY
export RASPBASEDIR=`pwd`
source ./guess-time.sh $MODEL
source ./model-parameters.sh $MODEL
sr_subscribe foreground hrdps_$HOUR.conf
# -n causes it not to download... will watch it overnight
cd $RASPBASEDIR
