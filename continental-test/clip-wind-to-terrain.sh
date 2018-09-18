#!/bin/bash
PARALLELTILE=15
WGRIB2=wgrib2
source ./model-parameters.sh
DIRECTORYNAME=$OUTPUTDIR
if [ -z $NOCLIP ]; then     # if string is NULL
    export OMP_NUM_THREADS=1
    echo "Starting clipping data below terrain elevation at `date`"
    echo "Processing times ${TIMES[*]}"
    rm -f /mnt/paralleljobs
    for H in ${TIMES[*]}
    do
        TOPO=`ls $DIRECTORYNAME"/$( filename HGT_SFC_0 00 )"`
        if [ -z ${TOPO:-""} ]; then
            echo TOPO undefined, exiting
            break
        fi
        for LEVEL in ${LEVELS[*]}
        do
#            echo "Clipping pressure altitude $LEVEL"
            VGRD=`ls $DIRECTORYNAME"/$( filename VGRD_ISBL_$LEVEL $H )"`
            UGRD=`ls $DIRECTORYNAME"/$( filename UGRD_ISBL_$LEVEL $H )"`
            HGT=`ls $DIRECTORYNAME"/$( filename HGT_ISBL_$LEVEL $H )"`
            if [ -z ${VGRD:-""} -o -z ${UGRD:-""} -o -z ${HGT:-""} ]; then
                echo "Could not find VGRD,UGRD, or HGT... break"
                break
            fi
            echo "( cat $TOPO $HGT $UGRD $VGRD | $WGRIB2 - -v0 -set_grib_type simple -if :HGT:surface: -rpn 'sto_1' -fi -if \":HGT:[0-9]+ mb:\" -rpn 'rcl_1:>:sto_2' -fi -if \":UGRD:[0-9]+ mb:\" -rpn 'rcl_2:mask' -grib_out $UGRD.masked  -fi -if \":VGRD:[0-9]+ mb:\" -rpn 'rcl_2:mask' -grib_out $VGRD.masked  -fi >& /dev/null ; mv $UGRD.masked $UGRD ; mv $VGRD.masked $VGRD )" >> /mnt/paralleljobs
        done
    done
    echo "running `wc -l paralleljobs`"
    parallel --gnu -j $PARALLELTILE < /mnt/paralleljobs
    export -n OMP_NUM_THREADS
    echo "Done clipping data below terrain elevation at `date`"
fi
rm -f /mnt/paralleljobs
