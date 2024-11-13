#!/bin/bash
# Setup drives and create a swapfile
if grep -qs '/mnt' /proc/mounts; then
    echo Drives already setup, preserving data on them
    rm -f input
    rm -f tiles
else 
    echo Setting up drives and deleting all content
    sudo rm -rf /mnt
    sudo mkdir /mnt
    if ( df | grep nvme0n1 ); then DRIVE=/dev/nvme1n1; else DRIVE=/dev/nvme0n1; fi;
    count=0
    until grep -qs '/mnt' /proc/mounts;
   	 do
	    sudo mkfs.xfs -f -K $DRIVE
	    sudo mount $DRIVE /mnt
	    if grep -qs '/mnt' /proc/mounts; then echo good; else sleep 1; fi;
	    if (( count++ >= 10 )); then
	      echo having trouble mounting drives
	      echo sudo shutdown -h now
	    fi
	done
    rm -rf input
    rm -rf tiles
    sudo chown ubuntu.ubuntu /mnt
    # The swapfile will be setup in the background while we are startup the downloading process
    ( dd if=/dev/zero of=/mnt/swapfile bs=1000000 count=16000 ; sudo chown root.root /mnt/swapfile ; sudo chmod 0600 /mnt/swapfile ; sudo mkswap /mnt/swapfile ; sudo swapon /mnt/swapfile ) &
fi
mkdir -p $PNGDIR
mkdir -p $OUTPUTDIR
mkdir -p $TILEDIR
