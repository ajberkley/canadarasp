#!/bin/sh
INPUT1=$1
INPUT2=$2
OUTPUT=$3
echo Combining $1 and $2 into $3, rotating winds to be N/S, E/W
cat $1 $2 > $3.tmp
wgrib2 $3.tmp
GRID_DEFN=`./grid_defn.pl $3.tmp`
#GRID_DEFN="nps:252.000000:60.000000 231.9186:2576:2500.000000 35.6073:1456:2500.000000'"
# -set_grib_type ieee
# takes an extra 4 seconds to compress with jpeg... not worth it.
wgrib2 $3.tmp -set_grib_type ieee -new_grid_winds earth -new_grid_interpolation neighbor -new_grid $GRID_DEFN $3
rm $3.tmp
