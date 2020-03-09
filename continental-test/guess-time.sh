#!/bin/bash
MODEL=${1:-$MODEL}
MODEL=${MODEL:-"hrdps"}
export MODEL=$MODEL
HR=`date -u +%H`  # takes about 7-9 hours for a run to be complete
# HOUR 00 takes 7 hours to run, so it is ready at 0700 and good until 1300 ... the windgrams don't upload the files in the right place for hour 00
# HOUR 06 takes 7 hours to run, so it is ready at 1300 and good until 1900
# HOUR 12 takes 7 hours to run, so it is ready at 1900 and good until 0000
# HOUR 18 takes 7 hours to run, so it is ready at 0000 and good until 0700 ;; ok, i changed this to be 0000 so we don't have to round back hour=12 too...
ROUNDBACK=0
if [ $MODEL == "gdps" ]; then
  export HOUR=00
else
  if [ $HR -ge 0 -a $HR -le 7 ] ; then export HOUR=18 ; elif [ $HR -gt 7 -a $HR -lt 12 ] ; then export HOUR=00 ; elif [ $HR -ge 12 -a $HR -le 19 ]; then export HOUR=06; else export HOUR=12; fi;
  if [ $HOUR == 18 ]; then ROUNDBACK=1; fi;
fi

# Guess the day
if [ $ROUNDBACK == 1 ]
   then
   export DAY=`date -u --date="-9 hours" +%d` # at 18 we need to round backwards (run started yesterday)
   export MONTH=`date -u --date="-9 hours" +%m` # at 18 we need to round backwards (run started yesterday)
   export YEAR=`date -u --date="-9 hours" +%Y` # at 18 we need to round backwards (run started yesterday)
   else
   export DAY=`date -u +%d` # Otherwise it's today
   export YEAR=`date -u +%Y`
   export MONTH=`date -u +%m`
fi

echo "It is now `date -u`, using data initialized at HOUR $HOUR DAY $DAY MONTH $MONTH YEAR $YEAR from model $MODEL"
