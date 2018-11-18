#!/bin/bash
function deleteold {
 echo Scanning for old MAP PNGS from `pwd`
 NUMDIRS=`ls -1rt | wc | awk '{print $1}'`
 echo There are $NUMDIRS directories here, we only want 2
 while [ $NUMDIRS -gt 2 ]; do
  DIRNAME=`ls -1rt | head -1`
  echo Deleting MAP PNGS from $DIRNAME
  COMMAND="rm -rf $DIRNAME"
  echo Calling $COMMAND
  $COMMAND
  NUMDIRS=`ls -1rt | wc | awk '{print $1}'`
  echo There are now $NUMDIRS directories remaining
 done
}
(cd /mnt/map-pngs
  for DIR in * ; do
   (cd $DIR ; deleteold )
  done)
