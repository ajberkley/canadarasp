#!/bin/bash
# ajb June 2018 main driver script for running a continental or west HRDPS visualization run
# Call this as do-rasp-run-continental.sh [ false | true ]
# If you have already downloaded some data this will not re-download it.
# You can export NODOWNLOAD=1 to not re-download data, make sure to mount /mnt first
git pull  # this works because I did git config credential.helper store
PATH=$PATH:/home/ubuntu/canadarasp/aws-utils:/home/ubuntu/.local/bin
MY_INSTANCE_ID=`get-my-instance-id.sh`
SHUTDOWN=`aws-read-tag.sh $MY_INSTANCE_ID shutdown`
SHUTDOWN=${SHUTDOWN:-true}
MODEL=`aws-read-tag.sh $MY_INSTANCE_ID model`

/home/ubuntu/canadarasp/setup-lisp.sh

if [ $SHUTDOWN == "true" ]; then
 echo Going to shutdown after run
else
 echo Not going to shutdown after run
fi
if [ $MODEL == "none" ]; then 
 echo No model specified, debugging mode enabled
 exit 1;
fi
export MODEL=${MODEL:-"gdps"}
echo Running RASP for model $MODEL, SHUTDOWN=$SHUTDOWN
source ./model-parameters.sh $MODEL
source ./guess-time.sh $MODEL
./setup-drives.sh
echo Moving downloaded data to local disk starting at `date`
source /home/ubuntu/canadarasp/aws-utils/attach-download-box.sh
source /home/ubuntu/canadarasp/aws-utils/mount-download-box.sh /download-box
df
cd /download-box
cp -R * /mnt/input/$MODEL
sync
cd /home/ubuntu/canadarasp/continental-test
df
source /home/ubuntu/canadarasp/aws-utils/unmount-download-box.sh
source /home/ubuntu/canadarasp/aws-utils/delete-download-box.sh
#echo NOT DELETING THE DOWNLOAD BOX
echo Done moving downloaded data to local disk at `date`

# generate HRDPS plots and windgrams
if [ -z $NOPLOT ]; then     # if string is NULL
 echo "generate HRDPS and windgram plots"
 echo "./do-hrdps-plots-continental.sh $YEAR $MONTH $DAY $HOUR > hrdps-plots.log 2>&1"
 mv hrdps-plots.log hrdps-plots.log.old
 ./do-hrdps-plots-continental.sh $YEAR $MONTH $DAY $HOUR > hrdps-plots.log 2>&1
fi

echo "Finished at `date`"
if [ $SHUTDOWN == "true" ] ; then
  echo "Shutting down 30 seconds"
  sleep 30
  sudo shutdown -h now
fi
