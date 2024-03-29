#!/bin/bash
# Usage: ./generate-single-file-plots.sh INFILE outdir headerfooter ([param1:param2:...] | [param1 param2 ...])

export NCARG_ROOT=/home/ubuntu/NCARG
export NCARG_RANGS=$NCARG_ROOT/database/rangs/
export ENV_NCL_FILENAME=$1
export ENV_NCL_OUTDIR=$2
export ENV_NCL_HEADERFOOTER=$3
export FMT="png"	#  could be "x11" for viewing
#export FMT="x11"	#  could be "x11" for viewing
export UNITS="metric"

# All possible parameters here, modify it you want
export ENV_NCL_PARAMS="bldepth:wstar:hwcritagl:sfcwind:sfcwind1:sfcwind2:sfcwind3:wind500:wind1000:wind1500:wind2000:wind2500:wind3000:sfcshf:sfctemp:sfcdewpt:cape:vwind0:vwind1:vwind2:cloud:rain"
# Override params with cmd-line args: can be space-separated OR ":" separated
if [ $# -gt 3 ]
then
    echo USING USER SUPPLIED PARAMS
    ENV_NCL_PARAMS=`echo ${*:3} | sed -e 's/ /:/g'`
    export ENV_NCL_PARAMS
    echo $ENV_NCL_PARAMS
fi

# Ensure Output Directories exist
if [ ! -d $ENV_NCL_OUTDIR ]
then
	mkdir -p $ENV_NCL_OUTDIR
fi

time $NCARG_ROOT/bin/ncl -n -p hrdps2gm-continental.ncl

exit
