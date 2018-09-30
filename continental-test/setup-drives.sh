#!/bin/bash
# Setup drives and create a swapfile
if grep -qs '/mnt' /proc/mounts; then
    echo Drives already setup, preserving data on them
    rm -f input
    rm -f tiles
else 
    echo Setting up drives and deleting all content
    sudo mkfs.xfs -f -K /dev/nvme1n1
    sudo mount /dev/nvme1n1 /mnt
    rm -rf input
    rm -rf tiles
    sudo chown ubuntu.ubuntu /mnt
    # The swapfile will be setup in the background while we are startup the downloading process
    ( dd if=/dev/zero of=/mnt/swapfile bs=1000000 count=16000 ; sudo chown root.root /mnt/swapfile ; sudo chmod 0600 /mnt/swapfile ; sudo mkswap /mnt/swapfile ; sudo swapon /mnt/swapfile ) &
fi
mkdir -p $PNGDIR
mkdir -p $OUTPUTDIR
mkdir -p $TILEDIR
