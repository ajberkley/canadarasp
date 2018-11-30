#!/bin/bash
MODEL=${1:-"gdps"}
TRY=${2:-0}
echo Downloading data for $MODEL
DBOXNAME=download-box-$MODEL
cd /home/ubuntu/canadarasp/aws-utils
source ./create-download-box.sh /$DBOXNAME
source ./aws-write-tag.sh `./get-compute-server-id.sh` model $MODEL
cd /home/ubuntu/canadarasp/continental-test
source ./guess-time.sh $MODEL
/home/ubuntu/canadarasp/aws-utils/check-download-box-mounted.sh /$DBOXNAME
if [ $? -eq 0 ]; then
 echo "Download box successfully mounted, downloading data"
 df
 ./download-data.sh $MODEL /$DBOXNAME
 cd /home/ubuntu/canadarasp/aws-utils
 source ./unmount-download-box.sh
 source ./start-compute-server.sh
 ( sleep 7200 ; source ./stop-compute-server.sh )
else
 echo "Something failed while mounting download box..."
 df
 if [ $TRY -lt 3 ] ; then
  echo Trying again for try $TRY $(( TRY + 1 ))
  /home/ubuntu/canadarasp/aws-utils/download-data-and-start-server.sh $MODEL $(( TRY + 1 ))
 fi
fi
