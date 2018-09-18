#!/bin/bash
source ./model-parameters.sh $MODEL
WEBSERVERIP=`./webserver-ip.sh`
BASEDIR=`echo $TILEDIR | sed s/[/]mnt/html/g`

echo "Uploading files to $WEBSERVERIP to directory $BASEDIR"

DIRSTOCREATE="$BASEDIR "
DIRSTOCREATE+=`find $TILEDIR -type d | sed s/[/]mnt/html/g`
echo "Making directories on webserver"
echo $DIRSTOCREATE | xargs -n 200 ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP mkdir -p
echo "Done making directories"
cd /mnt/html
echo "Uploading files"
tar cf - * | ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd html; tar xf -)"
echo "Done uploading files"

echo "Deleting old files"
YYYYMMDD=`date -d"$YEAR-$MONTH-$DAY $HOUR:00 -0000 - 2 days" +%Y-%m-%d` # local date
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd $BASEDIR; rm *$YYYYMMDD*.png)"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd $BASEDIR; find . -type d -name $YYYYMMDD | xargs rm -r)"
