#!/bin/bash
# Create and mount a standard volume on EC2 on $1 or download-box-$MODEL
# First check to make sure a download-box does not exist already
export -n VOL_ID=
export DBOXNAME=${1:-"download-box-$MODEL"}
export MNT="/$DBOXNAME"
echo Going to get a $DBOXNAME and mount it at $MNT
export VOL_ID=`aws --output json ec2 describe-volumes --filters Name=tag:Name,Values=$DBOXNAME Name=status,Values=available | jq .Volumes[0]."VolumeId" | tr -d '"'`
if [ $VOL_ID == null ]; then
	VOL_ID=""
fi
export MY_INSTANCE_ID=`./get-my-instance-id.sh`
export MY_ZONE=`curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone`
echo MY_INSTANCE_ID is $MY_INSTANCE_ID
echo MY_ZONE is $MY_ZONE
if [ -z $VOL_ID ]; then
  echo No $DBOXNAME available, creating one
  export VOL_ID=`aws --output json ec2 create-volume --no-encrypted --availability-zone $MY_ZONE --size 30 --volume-type gp3 --tag-specifications 'ResourceType=volume,Tags=[{Key=Name,Value='$DBOXNAME'}]' | jq .VolumeId | tr -d '"'`
  if [ -z $VOL_ID ]; then
    echo Failed creating one.  Aborting
    exit 1
  fi
  echo Created $VOL_ID in $MY_ZONE
else
  echo Volume $VOL_ID already available, using it
fi
aws ec2 wait volume-available --volume-ids $VOL_ID
while [ $? == 255 ]; do
  echo $VOL_ID not created yet...
  sleep 5
  aws ec2 wait volume-available --volume-ids $VOL_ID
done
echo $VOL_ID available
sleep 15
source ./attach-download-box.sh $DBOXNAME
sleep 5
sudo mkfs.ext4 $BLK_DEV
sudo mkdir -p $MNT
sudo mount $BLK_DEV $MNT
sudo chown ubuntu.ubuntu $MNT
