#!/bin/bash
MODEL=gdps
cd /home/ubuntu/canadarasp/aws-utils
source create-download-box.sh /mnt
source guess-time.sh $MODEL
source download-data.sh $MODEL
source unmount-download-box.sh
source start-compute-server.sh
sleep 7200
source stop-compute-server.sh
