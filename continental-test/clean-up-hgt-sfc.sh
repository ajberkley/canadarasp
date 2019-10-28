#!/bin/bash
MODEL=${1:-$MODEL}
DOWNLOADDIRECTORY=$2
source ./model-parameters.sh $MODEL
echo Cleaning up HGT_SFC mess
HGT_SFC_END=$( filename HGT_SFC_0 $TIMESTOP )
HGT_SFC_ZERO=$( filename HGT_SFC_0 0$TIMESTART )
# HRDPS has HGT_SFC for > 0 only, GDPS has HGT_SFC for 0 only
if [ -f $HGT_SFC_ZERO ]; then # GDPS
 echo Fixing GDPS HGT_SFC to exist always
 TIMEONE=$(( $TIMESTART + $TIMESTEP ))
 GOODTIMES=($(seq -w $TIMEONE $TIMESTEP $TIMESTOP))
 for H in ${GOODTIMES[*]}
 do
  ln $DOWNLOADDIRECTORY/$HGT_SFC_ZERO $DOWNLOADDIRECTORY/$( filename HGT_SFC_0 $H )
 done
else
 echo Fixing HRDPS HGT_SFC to exist always
 ln $DOWNLOADDIRECTORY/$HGT_SFC_END $DOWNLOADDIRECTORY/$HGT_SFC_ZERO
fi
