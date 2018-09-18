#!/bin/bash
# Unmount BLK_DEV and VOL_ID from MY_INSTANCE_ID 
sudo sync
sudo umount $BLK_DEV
aws ec2 detach-volume --instance $MY_INSTANCE_ID --volume-id $VOL_ID
aws ec2 wait volume-available --volume-ids $VOL_ID
