#!/bin/bash
# Run at 8PM each evening PT 
rm hrdpswindgram0.png
wget http://www.canadarasp.com/windgrams-data/oneDay/tomorrow/hrdpswindgram0.png
if [ -f hrdpswindgram0.png ]; then
   aws cloudwatch put-metric-data --namespace canadarasp --metric-name hrdps_monitor_tomorrow --value 1
fi
rm hrdpswindgram0.png
wget http://www.canadarasp.com/windgrams-data/oneDay/dayaftertomorrow/hrdpswindgram0.png
if [ -f hrdpswindgram0.png ]; then
   aws cloudwatch put-metric-data --namespace canadarasp --metric-name hrdps_monitor_dayaftertomorrow --value 1
fi
rm hrdpswindgram0.png
rm gdpswindgram0.png
wget http://www.canadarasp.com/windgrams-data/oneDay/dayaftertomorrow/gdpswindgram0.png
if [ -f gdpswindgram0.png ]; then
   aws cloudwatch put-metric-data --namespace canadarasp --metric-name gdps_monitor_dayaftertomorrow --value 1
fi
rm gdpswindgram0.png
