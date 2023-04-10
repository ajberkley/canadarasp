#!/bin/bash
# Usage:
#  ./do-windgrams-continental.sh $YEAR $MONTH $DAY $HOUR
# writes  file /mnt/windgrams-done when finished

UTCYEAR=$1 #`echo $DATE_STR | cut -c1-4`
UTCMONTH=$2 #=`echo $DATE_STR | cut -c5-6`
UTCDAY=$3 #=`echo $DATE_STR | cut -c7-8`
HOUR=$4 #=`echo $DATE_STR | cut -c9-10`
OUT_DIR=/mnt/windgrams-data
mkdir -p $OUT_DIR
mkdir -p $OUT_DIR/twoDay
mkdir -p $OUT_DIR/oneDay
for i in `seq 0 1 5`;
 do
 YEARA=`date -d "$UTCYEAR-$UTCMONTH-$UTCDAY $HOUR UTC +$i day" +%Y`
 MONTHA=`date -d "$UTCYEAR-$UTCMONTH-$UTCDAY $HOUR UTC +$i day" +%m`
 DAYA=`date -d "$UTCYEAR-$UTCMONTH-$UTCDAY $HOUR UTC +$i day" +%d`
 mkdir -p $OUT_DIR/oneDay/$YEARA-$MONTHA-$DAYA
done
echo UTC starting time $UTCYEAR-$UTCMONTH-$UTCDAY $HOUR

WEBSERVERIP=`./webserver-ip.sh "$WEBSERVERNAME"`

cd plot-generation

if [ -z $NOCALCULATE ]; then
  echo Calling windgram-continental
  ./locations-group-by-id-and-run-windgrams.lisp
  export NCARG_ROOT=/home/ubuntu/NCARG/
  parallel --gnu -j 15 < run-my-windgrams.sh # -j 15
  echo Done with wingram-continental
fi
# Now should just use tar to help?
if [ -z $NOUPLOAD ]; then 
  echo uploading windgrams
  (cd /mnt/windgrams-data ; tar cf - -- * | ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd html/windgrams-data; tar xf -)")
fi
##############################################################################################
# create a javascript version of the location.txt file
##############################################################################################
if [ -z $NOUPLOAD ]; then
    ./make-new-locations.sh
    echo "scp -i ~/.ssh/montreal.pem locations.js new-locations.js ubuntu@$WEBSERVERIP:html/windgrams-continental-new"
    scp -i ~/.ssh/montreal.pem locations.js new-locations.js ubuntu@$WEBSERVERIP:html/windgrams
    scp -i ~/.ssh/montreal.pem locations.js new-locations.js ubuntu@$WEBSERVERIP:html/
fi
echo "Uploading windgram tiles"
if [ -z $NOUPLOAD ]; then
  cd ..
  ./upload-windgram-tiles.sh
fi
echo `date` > /mnt/windgrams-done
