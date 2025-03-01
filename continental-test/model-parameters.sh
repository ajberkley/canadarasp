#!/bin/bash
# usage: source model-parameters.sh [ rdps | hrdps | gdps | hrdps_rot]
MODEL=${1:-$MODEL} # Use parameter or environment variable MODEL
MODEL=${MODEL:-"hrdps"}
case $MODEL in
    rdps|hrdps|gdps|hrdps_west|hrdps_rot) echo Setting configuration parameters for MODEL $MODEL ;;
    *) echo MODEL $MODEL is not one of rdps hrdps gdps or hrdps_west; exit -1 ;;
esac

export MODEL=$MODEL
export FILETOPROBE="PRES_SFC_0" # how to tell if data is good at last hour

if [ $MODEL = "rdps" ]; then
  export WEBSERVER="dd.weather.gc.ca"
  export DIRECTORY="model_gem_regional/10km/grib2"
  export FILEHEADER="CMC_reg"
  export TIMESTART="0"
  export TIMESTEP="1"
  export TIMESTOP="54"
  export RESOLUTION="_ps10km_"
  export TAIL=".grib2"
  export FILE="RDPS-files.txt"
  export OUTPUTDIR="/mnt/input/rdps"  # Where input data is
  export TILEDIR="/mnt/windgram-tiles/rdps" # Where windgram grib2 tiles go
  export PNGDIR="/mnt/map-pngs/rdps" # Where map PNGs go
filename () {
 FILELABEL=$1;
 H=$2;
 echo $FILEHEADER"_$FILELABEL"$RESOLUTION$YEAR$MONTH$DAY$HOUR"_P0"$H$TAIL 
}
downloadfilename () {
    $( filename $1 $2 )
}

fi
if [ $MODEL = "hrdps" ]; then
#   export WEBSERVER="hpfx.collab.science.gc.ca"
#   export DIRECTORY="$YEAR$MONTH$DAY/WXO-DD/model_hrdps/continental/grib2"
   export WEBSERVER="dd.weather.gc.ca"
   export DIRECTORY="model_hrdps/continental/grib2"
   export FILEHEADER="CMC_hrdps_continental"
   export TIMESTART="1" # no prate data for zero
   export TIMESTEP="1"
   export TIMESTOP="48"
   export RESOLUTION="_ps2.5km_"
   export TAIL="-00.grib2"
   export FILE="HRDPS-files.txt"
   export OUTPUTDIR="/mnt/input/hrdps"
   export TILEDIR="/mnt/windgram-tiles/hrdps"
   export PNGDIR="/mnt/map-pngs/hrdps"
   export XMIN=-152
   export XMAX=-42
   export XSTEP=2
   export YMIN=26
   export YSTEP=2
   export YMAX=70
filename () {
 FILELABEL=$1;
 H=$2;
 echo $FILEHEADER"_$FILELABEL"$RESOLUTION$YEAR$MONTH$DAY$HOUR"_P0"$H$TAIL 
}
downloadfilename () {
    $( filename $1 $2 )
}

fi

if [ $MODEL = "hrdps_rot" ]; then
#   export WEBSERVER="hpfx.collab.science.gc.ca"
   export WEBSERVER="dd.weather.gc.ca"
   export DIRECTORY="$YEAR$MONTH$DAY/WXO-DD/model_hrdps/continental/2.5km"
#   export DIRECTORY="model_hrdps/continental/2.5km"
   export FILEHEADER="$YEAR$MONTH$DAY"T"$HOUR""Z_MSC_HRDPS"
   export TIMESTART="1" # no prate data for zero
   export TIMESTEP="1"
   export TIMESTOP="48"
   export RESOLUTION=""
   export TAIL=".grib2"
   export FILE="HRDPS-ROT-files.txt"
   export OUTPUTDIR="/mnt/input/hrdps"
   export TILEDIR="/mnt/windgram-tiles/hrdps"
   export PNGDIR="/mnt/map-pngs/hrdps"
   export XMIN=-152
   export XMAX=-42
   export XSTEP=2
   export YMIN=26
   export YSTEP=2
   export YMAX=70
   export FILETOPROBE="PRES_Sfc" # how to tell if data is good at last hour
filename () {
 FILELABEL=$1;
 H=$2;
 echo $FILEHEADER"_$FILELABEL"$RESOLUTION$YEAR$MONTH$DAY$HOUR"_PT0"$H"H"$TAIL
}
downloadfilename () {
 FILELABEL=$1;
 H=$2;
 echo $FILEHEADER"_$FILELABEL""_RLatLon0.0225_PT0"$H"H"$TAIL
}
fi

if [ $MODEL = "gdps" ]; then
   # export WEBSERVER="hpfx.collab.science.gc.ca"
   export DIRECTORY="$YEAR$MONTH$DAY/WXO-DD/model_gem_global/15km/grib2/lat_lon"
   export WEBSERVER="dd.weather.gc.ca"
   # export DIRECTORY="model_gem_global/15km/grib2/lat_lon"
   export FILEHEADER="CMC_glb"
   export TIMESTART="0"
   export TIMESTEP="3"
   export TIMESTOP="99"
   export RESOLUTION="_latlon.15x.15_"
   export TAIL=".grib2"
   export FILE="GDPS-files.txt"
   export OUTPUTDIR="/mnt/input/gdps"
   export TILEDIR="/mnt/windgram-tiles/gdps"
   export PNGDIR="/mnt/map-pngs/gdps"
   export XMIN=-180
   export XSTEP=10
   export XMAX=180
   export YMIN=-80
   export YSTEP=10
   export YMAX=80
filename () {
 FILELABEL=$1;
 H=$2;
 echo $FILEHEADER"_$FILELABEL"$RESOLUTION$YEAR$MONTH$DAY$HOUR"_P0"$H$TAIL 
}
downloadfilename () {
 FILELABEL=$1;
 H=$2;
 echo $FILEHEADER"_$FILELABEL"$RESOLUTION$YEAR$MONTH$DAY$HOUR"_P0"$H$TAIL
}


fi
if [ $MODEL = "hrdps_west" ]; then
   export WEBSERVER="dd.alpha.meteo.gc.ca"
   export DIRECTORY="model_hrdps/west/1km/grib2"
   export FILEHEADER="CMC_hrdps_west"
   export TIMESTART="1" # no prate data for zero
   export TIMESTEP="1"
   export TIMESTOP="48"
   export RESOLUTION="_rotated_latlon0.009x0.009_"
   export TAIL="-00.grib2"
   export FILE="HRDPS-files.txt"
   export OUTPUTDIR="/mnt/input/hrdps_west"
   export TILEDIR="/mnt/windgram-tiles/hrdps_west"
   export PNGDIR="/mnt/map-pngs/hrdps_west"
   export XMIN=-152
   export XMAX=-42
   export XSTEP=2
   export YMIN=26
   export YSTEP=2
   export YMAX=70
filename () {
 FILELABEL=$1;
 H=$2;
 echo $FILEHEADER"_$FILELABEL"$RESOLUTION$YEAR$MONTH$DAY"T"$HOUR"Z_P0"$H$TAIL 
}
downloadfilename () {
    $( filename $1 $2 )
}

fi
export XVALS=($(seq $XMIN $XSTEP $XMAX))
export YVALS=($(seq $YMIN $YSTEP $YMAX))

export TIMES=($(seq -w $TIMESTART $TIMESTEP $TIMESTOP))
if [ $MODEL = "hrdps" ]; then
       export LEVELS=(0550 0600 0650 0700 0750 0800 0850 0875 0900 0925 0950 0970 0985 1000 1015)
    else
       export LEVELS=(550 600 650 700 750 800 850 875 900 925 950 970 985 1000 1015)
fi
