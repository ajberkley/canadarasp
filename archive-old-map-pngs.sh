#!/bin/bash
find /mnt/map-pngs/hrdps -name "-122:-120:48:50" -print > /tmp/file_list
find /mnt/map-pngs/hrdps -name "-122:-120:50:52" -print >> /tmp/file_list
find /mnt/map-pngs/hrdps -name "-120:-118:48:50" -print >> /tmp/file_list
find /mnt/map-pngs/hrdps -name "-120:-118:50:52" -print >> /tmp/file_list
cd /mnt/hrdps-map-archive
tar -c -T /tmp/file_list -O | tar xf - --strip-components=4
cp /mnt/map-pngs/hrdps/latest/*.png /mnt/hrdps-map-archive/
