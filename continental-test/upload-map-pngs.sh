#!/bin/bash
source ./model-parameters.sh $MODEL
WEBSERVERIP=`./webserver-ip.sh`
BASEDIR=`echo $PNGDIR | sed s/[/]mnt/html/g`
echo "Deleting old files"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd canadarasp ; ./delete-old-map-pngs.sh"
echo "Uploading files to $WEBSERVERIP to directory $BASEDIR"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP mkdir -p $BASEDIR
DIRSTOCREATE+=`find $PNGDIR -type d | sed s/[/]mnt/html/g`
echo "Making directories on webserver"
echo $DIRSTOCREATE | xargs -n 200 ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP mkdir -p
echo "Done making directories"
cd $PNGDIR
echo "Uploading files"
tar cf - * | ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd $BASEDIR; tar xf -)"
echo "Done uploading files, updating latest link"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd $BASEDIR; rm -f latest ; ln -s `ls -1rt | tail -1` latest )"
echo "Deleting old map-pngs again"
ssh -i ~/.ssh/montreal.pem ubuntu@$WEBSERVERIP "(cd canadarasp ; ./delete-old-map-pngs.sh"
