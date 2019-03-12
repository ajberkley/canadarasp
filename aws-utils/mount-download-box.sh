#!/bin/bash
# Create and mount a standard volume on EC2 on $1
# First check to make sure a download-box does not exist already
MNT=${1:-/mnt}
export -n VOL_ID=
export DBOXNAME="download-box-$MODEL"
echo Going to get a $DBOXNAME and mount it at $MNT
export VOL_ID=`aws ec2 describe-volumes --filters Name=tag:Name,Values=$DBOXNAME Name=status,Values=available | head -1 | awk '{ print $7 }'`
export MY_INSTANCE_ID=`curl -s http://169.254.169.254/latest/meta-data/instance-id`
export MY_ZONE=`curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone`
aws ec2 wait volume-available --volume-ids $VOL_ID
echo $VOL_ID available
aws ec2 attach-volume --device /dev/sdf --instance $MY_INSTANCE_ID --volume-id $VOL_ID
echo Attaching $VOL_ID to $MY_INSTANCE_ID
aws ec2 wait volume-in-use --volume-ids $VOL_ID
# how do I find this one out?
echo Attached $VOL_ID to $MY_INSTANCE_ID
export VOL_ID_NO_DASH=`echo $VOL_ID | sed s/-//g`
# export BLK_DEV="/dev/disk/by-id/nvme-Amazon_Elastic_Block_Store_"$VOL_ID_NO_DASH
sleep 5
export BLK=`lsblk | grep 30G  | awk '{print $1}'`
echo BLK is $BLK
export BLK_DEV="/dev/$BLK"
echo Going to mount $BLK_DEV
# seems we need to wait a bit...
sleep 5
sudo mkdir -p $MNT
sudo mount $BLK_DEV $MNT
sudo chown ubuntu.ubuntu $MNT
