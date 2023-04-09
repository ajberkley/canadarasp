#!/bin/bash
# download-data-and-start-server.sh MODEL COMPUTESERVER TRY
MYINSTANCEID=`get-my-instance-id.sh`
MODEL=${1:-"gdps"}
SERVER=${2:-"`aws-read-tag.sh $MYINSTANCEID computename`"}
TRY=${3:-0}
echo I am $MYINSTANCEID downloading data for $MODEL and triggering $SERVER try $TRY
MODEL_ORIG=$MODEL
if [ $MODEL == hrdps_rot ]; then
    DBOXNAME=download-box-hrdps
else
    DBOXNAME=download-box-$MODEL
fi
cd /home/ubuntu/canadarasp/aws-utils
source ./create-download-box.sh $DBOXNAME
COMPUTEID=`./get-compute-server-id.sh "$SERVER"`
echo Server id is $COMPUTEID writing model tag $MODEL
source ./aws-write-tag.sh $COMPUTEID model $MODEL
source ./aws-write-tag.sh $COMPUTEID webserver "`./aws-read-tag.sh $MYINSTANCEID Name`"
echo model tag is `./aws-read-tag.sh $COMPUTEID model`
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
