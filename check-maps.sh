#!/bin/sh
cd /tmp
rm -f blarg.png
HOUR=`date +%H`
if [ "$HOUR" -le 14 ] ; then
	INITHOUR=1800
	YYYYMMDD=`date --date="now - 1 day" +%Y-%m-%d`
else
	INITHOUR=0600
	YYYYMMDD=`date +%Y-%m-%d`
fi
echo Run was $YYYYMMDD $INITHOUR
YYYYMMDDHH=`date --date="$YYYYMMDD $INITHOUR + 47 hours" +%Y-%m-%d_%H00`
YYYYMMDD=`date --date="$YYYYMMDD $INITHOUR + 47 hours" +%Y-%m-%d`
echo Checking run results at $YYYYMMDDHH
wget --quiet --output-document=blarg.png http://canadarasp.com/map-pngs/hrdps/latest/$YYYYMMDD/-122:-120:48:50/sfcwind3_$YYYYMMDDHH.vector.png
if [ -f blarg.png ] && [ "`stat -c %s blarg.png`" -gt 0 ]; then
	echo "Map available, good"
	aws cloudwatch put-metric-data --namespace canadarasp --metric-name hrdps_maps --value 1
else
	echo "Map not available, bad"
	aws cloudwatch put-metric-data --namespace canadarasp --metric-name hrdps_maps --value 0
fi
rm -f blarg.png
