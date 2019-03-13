#!/bin/bash
# Create and mount a standard volume on EC2 on $1 unless $1 is not provided
# First check to make sure a download-box does not exist already
MNT=${1:-/download-box}
export BLK=`lsblk | grep 30G  | awk '{print $1}'`
echo BLK is $BLK
export BLK_DEV="/dev/$BLK"
echo Going to mount $BLK_DEV
# seems we need to wait a bit...
sleep 5
sudo mkdir -p $MNT
if [ "$MNT" = "NOMNT" ]; then 
    sudo mount $BLK_DEV $MNT
    sudo chown ubuntu.ubuntu $MNT
fi
