#!/bin/bash
echo "Removing unused files"
(cd /mnt/windgram-tiles ; find . -name "*CMC*" -exec rm {} \;)
WEBSERVERIP=`./webserver-ip.sh "$WEBSERVERNAME"`
DIRSTOCREATE="mkdir -p html/windgram-tiles "
DIRSTOCREATE+=`find /mnt/windgram-tiles -type d | sed s/[/]mnt/'html'/g`

YYYYMMDD=`date -d"$YEAR-$MONTH-$DAY $HOUR:00 -0000 -1 days" +%Y-%m-%d` # local date
echo "Deleting old files from $YYYYMMDD"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "find html/windgram-tiles/$MODEL/ -name \"*$YYYYMMDD*\" -exec rm -f {} \;"

echo "Making directories on webserver"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP $DIRSTOCREATE
echo "Uploading files"
cd /mnt/windgram-tiles
tar cf - -- * | ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd html/windgram-tiles; tar xf -)"
