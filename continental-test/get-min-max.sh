#!/bin/bash

VARNAME=$1
OUTPUTNAME=$2
WGRIB2=wgrib2
echo Calculating min/max of $VARNAME
F=`ls input/*$VARNAME*.grib2`
A=`cat $F | $WGRIB2 - -min | sort -t= -g -k2 | head -1`
B=`cat $F | $WGRIB2 - -max | sort -t= -gr -k2 | head -1`
MIN=${A#*=}
MAX=${B#*=}
echo "export $OUTPUTNAME"_MIN"=$MIN" > /mnt/$VARNAME
echo "export $OUTPUTNAME"_MAX"=$MAX" >> /mnt/$VARNAME
