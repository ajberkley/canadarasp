#!/bin/bash
# Run at 8PM each evening PT 
rm -f hrdpswindgram0.png
wget --quiet http://www.canadarasp.com/windgrams-data/oneDay/tomorrow/hrdpswindgram0.png
if [ -f hrdpswindgram0.png ]; then
   aws cloudwatch put-metric-data --namespace canadarasp --metric-name hrdps_monitor_tomorrow --value 1
fi
rm hrdpswindgram0.png
wget --quiet http://www.canadarasp.com/windgrams-data/oneDay/dayaftertomorrow/hrdpswindgram0.png
if [ -f hrdpswindgram0.png ]; then
   aws cloudwatch put-metric-data --namespace canadarasp --metric-name hrdps_monitor_dayaftertomorrow --value 1
fi
rm hrdpswindgram0.png
rm -f gdpswindgram0.png
wget --quiet http://www.canadarasp.com/windgrams-data/oneDay/dayaftertomorrow/gdpswindgram0.png
if [ -f gdpswindgram0.png ]; then
   aws cloudwatch put-metric-data --namespace canadarasp --metric-name gdps_monitor_dayaftertomorrow --value 1
fi
rm gdpswindgram0.png
DATE=`date +%Y-%m-%d`
rm -f windgram-dynamic-test.html
wget --quiet "http://canadarasp.com/windgram?lat=49.07&lon=-122.40&date=$DATE\&interactive=t" -O windgram-dynamic-test.html
if [ -f windgram-dynamic-test.html ]; then
   aws cloudwatch put-metric-data --namespace canadarasp --metric-name dynamic_windgram --value 1
fi
rm windgram-dynamic-test.html
