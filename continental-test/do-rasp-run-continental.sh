#!/bin/bash
# ajb June 2018 main driver script for running a continental or west HRDPS visualization run
# Call this as do-rasp-run-continental.sh
# If you have already downloaded some data this will not re-download it.
# You can export NODOWNLOAD=1 to not re-download data, make sure to mount /mnt first

./setup-drives.sh
MODEL=${MODEL-:$1}
export MODEL=${MODEL-:"gdps"}
source ./model-parameters.sh $MODEL
source ./guess-time.sh $MODEL
if [ -z $NODOWNLOAD ]; then
    source ./download-data.sh $MODEL
fi
echo "Generating new variables like HCRIT"
./do-generate-new-variables.sh # takes 3 minute
echo "Done generating new variables"

# generate HRDPS plots and windgrams
if [ -z $NOPLOT ]; then     # if string is NULL
 echo "generate HRDPS and windgram plots"
 echo "./do-hrdps-plots-continental.sh $YEAR $MONTH $DAY $HOUR > hrdps-plots.log 2>&1"
 mv hrdps-plots.log hrdps-plots.log.old
 ./do-hrdps-plots-continental.sh $YEAR $MONTH $DAY $HOUR > hrdps-plots.log 2>&1
fi

echo "Finished at `date`, shutting down"
sleep 30
#sudo shutdown -h now
