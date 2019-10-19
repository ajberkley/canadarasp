#!/bin/bash
TZ='America/Vancouver'
TODAY=`date +%Y-%m-%d`
TOMORROW=`date --date=tomorrow +%Y-%m-%d`
DAYAFTERTOMORROW=`date --date="+2 days" +%Y-%m-%d`
rm -f /mnt/windgrams-data/oneDay/today
rm -f /mnt/windgrams-data/oneDay/tomorrow
rm -f /mnt/windgrams-data/oneDay/dayaftertomorrow
ln -s /mnt/windgrams-data/oneDay/$TODAY /mnt/windgrams-data/oneDay/today
ln -s /mnt/windgrams-data/oneDay/$TOMORROW /mnt/windgrams-data/oneDay/tomorrow
ln -s /mnt/windgrams-data/oneDay/$DAYAFTERTOMORROW /mnt/windgrams-data/oneDay/dayaftertomorrow
