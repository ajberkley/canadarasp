#!/bin/bash
MODEL=${1:-"gdps"}
cd /home/ubuntu/canadarasp/aws-utils
source create-download-box.sh /mnt
cd ../continental-test
source guess-time.sh $MODEL
source download-data.sh $MODEL
cd ../aws-utils
source unmount-download-box.sh
source start-compute-server.sh
( sleep 7200 ; source stop-compute-server.sh )
