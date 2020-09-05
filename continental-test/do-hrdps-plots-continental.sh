#!/bin/bash

# Usage:
# ./do-hrdps-plots.sh $YEAR $MONTH $DAY $HOUR
# where $HOUR is 00 or 06 or 12 or 18
# ./generate-hrdps-plots.sh 2013 04 18 06 /mnt/tiles
#  output directory is tiles/-122:-120:49:51/$YEAR-$MONTH-$DAY
#  for each tile longitude / latitude and each year / month / day (local time) in the forecast

# Environment variables
#  NOTILES - skip tile generation
#  NOWINDGRAMS - skip windgrams
#  NOMAP - skip map png generation  
# When this script completes it creates a file /mnt/hrdps-plots-done

echo "$0 $@"
echo "Starting HRDPS plots at `date`"
export GRIB2TABLE=/home/ubuntu/continental-test/grib2tables
WGRIB2=wgrib2
PARALLELSUB=15 # for fixing of file names which doesn't use any internal parallelization of wgrib2
PARALLELTILE=15 # WGRIB2 goes nuts if you set this to more than 1 and don't set OMP_NUM_THREADS=1
PARALLELNCL=14
START_PATH=`pwd`
YEAR=${1:-$YEAR}
MONTH=${2:-$MONTH}
DAY=${3:-$DAY}
HOUR=${4:-$HOUR}  # 06 or 18
Z=`date +%-:::z` # This is UTC offset
source ./model-parameters.sh $MODEL
echo "do-hrdps-plots.sh $YEAR-$MONTH-$DAY $HOUR for ${#TIMES[@]} hours, local UTC offset is $Z"

echo "Generating new variables like HCRIT"
./do-generate-new-variables.sh # takes 3 minute
echo "Done generating new variables"

if [ $MODEL == "hrdps" ]; then
  if [ -z $NOCLIP ]; then
    echo "Starting clipping wind to terrain at `date`"
    ./clip-wind-to-terrain.sh
    echo "Done clipping wind to terrain at `date`"
  fi
fi

if [ -z $NOTILES ]; then
    echo "Generating tiles at `date`"
    ./do-tile-generation.sh $YEAR $MONTH $DAY $HOUR 
    echo "Done generating tiles at `date`"
fi
if [ -z $NOWINDGRAMS ]; then
    echo "Generating windgrams at `date`"
    ./do-windgrams-continental.sh $YEAR $MONTH $DAY $HOUR
    echo "Done generating windgrams `date`"
fi
if [ -z $NOMAP ]; then     # if string is NULL
    echo "Starting tile graphic generation at `date`"
    FORECASTHOURS=($(seq -w $TIMESTART $TIMESTEP $TIMESTOP))
    echo "Generating headers and footers for all hours starting at `date`"
    ./generate-single-hrdps-plot-continental.lisp --only-generate-header-footer
    echo "Done generating headers and footers at `date`"
    rm -f /mnt/forecast-hours
    for I in ${FORECASTHOURS[*]}
    do
	echo $I >> /mnt/forecast-hours
    done
    echo "Starting to generate all map pngs at `date`"
    export OMP_NUM_THREADS=1
    parallel --gnu -j 16 ./generate-single-hrdps-plot-continental.lisp {} < /mnt/forecast-hours
    export -n OMP_NUM_THREADS
    #rm /mnt/forecast-hours
    echo "Done tile graphic generation at `date`"
fi

if [ -z $NOUPLOAD ]; then     # if string is NULL
    # copy png images to rasp server. The rasp server connection has been unreliable so each call has been put in a retry loop
    ./upload-map-pngs.sh
fi
echo "Finished with HRDPS plots at `date`"
echo `date` > /mnt/hrdps-plots-done
