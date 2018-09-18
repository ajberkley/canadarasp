#!/bin/bash
# Usage ./download-data.sh [hrdps | gdps | rdps]

export RASPBASEDIR=`pwd`

source ./guess-time.sh $MODEL
source ./model-parameters.sh $MODEL
mkdir -p $OUTPUTDIR

rm -f /tmp/wget.jobs

echo "Generating $DIRECTORY file names" # takes a few seconds
# This generates a few errors because HGT_SFC (for example) is not available at 000, but at all other times... even though it should be the opposite... whatever.
for H in ${TIMES[*]}
 do
  xargs -I {} echo http://$WEBSERVER/$DIRECTORY/$HOUR/0$H/$FILEHEADER"_"{}$RESOLUTION$YEAR$MONTH$DAY$HOUR"_P0"$H$TAIL < $FILE >> /tmp/wget.jobs
done

echo "Done generating $DIRECTORY file names"

if [ -z $NODEL ]; then     # if string is NULL
  rm -f $OUTPUTDIR/*
fi
cd $OUTPUTDIR

# check that the data is on the server. Looking for hour $TIMESTOP
echo "Checking data is on the server"

for i in {1 .. 180}
do
  echo wget http://$WEBSERVER/$DIRECTORY/$HOUR/0$TIMESTOP/$FILEHEADER"_HGT_ISBL_1015"$RESOLUTION$YEAR$MONTH$DAY$HOUR"_P0"${TIMES[-1]}$TAIL
       wget http://$WEBSERVER/$DIRECTORY/$HOUR/0$TIMESTOP/$FILEHEADER"_HGT_ISBL_1015"$RESOLUTION$YEAR$MONTH$DAY$HOUR"_P0"${TIMES[-1]}$TAIL
  ret=$?
  echo $ret

  if [ ${ret} -eq 0 ]
  then
    break
  fi
  echo "`date +%T` and still no data on server. sleeping for 1 minute"
  sleep 60 # sleep for a minute
done

echo "downloading $DIRECTORY data from time 0 to $TIMESTOP by $TIMESTEP hours input $OUTPUTDIR"
parallel --gnu -n 8 -j 8 wget --timeout=120 -c -nc -nv {} < /tmp/wget.jobs
echo "Second time"
parallel --gnu -n 8 -j 4 wget -c -nc -nv {} < /tmp/wget.jobs
cd $RASPBASEDIR
