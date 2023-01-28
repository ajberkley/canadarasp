#!/bin/bash
# usage:
#  concatenate-windgram-tiles.sh YEAR MONTH DAY HOUR FORECASTHOUR DIRECTORY
# where YEAR MONTH DAY HOUR are the model initialize date and FORECASTHOUR is the forecast hour
# and DIRECTORY has all the source files.
# you must have $MODEL defined (either hrdps or gdps)

YEAR=${1:-$YEAR}
MONTH=${2:-$MONTH}
DAY=${3:-$DAY}
HOUR=${4:-$HOUR}
FORECASTHOUR=$5
H=$FORECASTHOUR
DIRECTORYNAME=$6
source ./model-parameters.sh $MODEL
echo Combining files into windgram tiles from $YEAR-$MONTH-$DAY-$HOUR at forecast hour $FORECASTHOUR in directory $DIRECTORYNAME

P=$DIRECTORYNAME
OUTPUT=$P/$MODEL"_"$YEAR-$MONTH-$DAY-run$HOUR"_P0"$H".grib2"
CATLIST="$P/*VGRD_ISBL_*_P0$H$TAIL $P/*UGRD_ISBL_*_P0$H$TAIL $P/*DEPR_ISBL_*_P0$H$TAIL $P/*TMP_ISBL_*_P0$H$TAIL $P/*HGT_SFC*_P0$H$TAIL $P/*HGT_ISBL*_P0$H$TAIL $P/*TGL*_P0$H$TAIL $P/*PRATE*_P0$H$TAIL $P/*TCDC_SFC*_P0$H$TAIL $P/*PRMSL*_P0$H$TAIL  $P/*HTFL*_P0$H$TAIL"
cat $CATLIST > $OUTPUT
if [ -z $NODEL ]; then
  rm $CATLIST
fi
