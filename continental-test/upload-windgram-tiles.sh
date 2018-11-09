#!/bin/bash
echo "Removing unused files"
(cd /mnt/tiles ; find . -name "*CMC*" -exec rm {} \;)
WEBSERVERIP=`./webserver-ip.sh`
DIRSTOCREATE="mkdir -p html/windgram-tiles "
DIRSTOCREATE+=`find /mnt/windgram-tiles -type d | sed s/[/]mnt/'html\\/windgram-tiles'/g`

YYYYMMDD=`date -d"$YEAR-$MONTH-$DAY $HOUR:00 -0000 - 1 days" +%Y-%m-%d` # local date
echo "Deleting old files from $YYYYMMDD"
COMMAND="(cd html/windgram-tiles; find . -name \"*$YYYYMMDD*\" -exec rm {} \;"
echo $COMMAND
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd html/windgram-tiles; find . -name \"*$YYYYMMDD*\" -exec rm {} \;)"

echo "Making directories on webserver"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP $DIRSTOCREATE
echo "Uploading files"
cd /mnt/tiles
tar cf - -- * | ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd html/windgram-tiles; tar xf -)"
