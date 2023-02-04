#!/bin/bash
MODEL=${1:-"gdps"}
SERVER=${2:-"HRDPS PROD V9"}
TRY=${3:-0}
echo Downloading data for $MODEL
MODEL_ORIG=$MODEL
if [ $MODEL == hrdps_rot ]; then
    DBOXNAME=download-box-hrdps
else
    DBOXNAME=download-box-$MODEL
fi
cd /home/ubuntu/canadarasp/aws-utils
source ./create-download-box.sh $DBOXNAME
COMPUTEID=`./get-compute-server-id.sh "$SERVER"`
echo Server id is $COMPUTEID
source ./aws-write-tag.sh $COMPUTEID model $MODEL
cd /home/ubuntu/canadarasp/continental-test
source ./guess-time.sh $MODEL
/home/ubuntu/canadarasp/aws-utils/check-download-box-mounted.sh /$DBOXNAME
if [ $? -eq 0 ]; then
 echo "Download box successfully mounted, downloading data"
 df
 ./download-data.sh $MODEL /$DBOXNAME
 cd /home/ubuntu/canadarasp/aws-utils
 source ./unmount-download-box.sh
 source ./start-compute-server.sh "$SERVER"
 ( sleep 7200 ; source ./stop-compute-server.sh "$SERVER")
else
 echo "Something failed while mounting download box..."
 df
 if [ $TRY -lt 3 ] ; then
  echo Trying again for try $TRY $(( TRY + 1 ))
  /home/ubuntu/canadarasp/aws-utils/download-data-and-start-server.sh $MODEL_ORIG "$SERVER" $(( TRY + 1 ))
 fi
fi
