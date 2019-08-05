#!/bin/bash
# Unmount BLK_DEV and VOL_ID from MY_INSTANCE_ID 
sudo sync
sudo umount -d $BLK_DEV
sleep 30
aws ec2 detach-volume --instance $MY_INSTANCE_ID --volume-id $VOL_ID
aws ec2 wait volume-available --volume-ids $VOL_ID
