#!/bin/bash
# Attach our download box
export -n VOL_ID=
export DBOXNAME=${1:-"download-box-$MODEL"}
echo Going to get a $DBOXNAME
export VOL_ID=`aws ec2 describe-volumes --filters Name=tag:Name,Values=$DBOXNAME Name=status,Values=available | head -1 | awk '{ print $8 }'`
export MY_INSTANCE_ID=`curl -s http://169.254.169.254/latest/meta-data/instance-id`
export MY_ZONE=`curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone`
aws ec2 wait volume-available --volume-ids $VOL_ID
echo $VOL_ID available
aws ec2 attach-volume --device /dev/sdh --instance $MY_INSTANCE_ID --volume-id $VOL_ID
echo Attaching $VOL_ID to $MY_INSTANCE_ID
aws ec2 wait volume-in-use --volume-ids $VOL_ID
echo Attached $VOL_ID to $MY_INSTANCE_ID
export BLK=""
while [ "$BLK" == "" ]; do
  sleep 10
  export BLK=`lsblk | grep 30G  | awk '{print $1}'`
  echo BLK is $BLK
done
export BLK_DEV="/dev/$BLK"
echo Going to mount $BLK_DEV
