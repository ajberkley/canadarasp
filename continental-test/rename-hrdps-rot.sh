#!/bin/bash
MODEL=${1:-$MODEL}
DOWNLOADDIRECTORY=${2:-/tmp}
if [ $MODEL == hrdps_rot ]; then
    echo Starting renaming HRDPS ROT files at `date`
    declare -A rewrite
    rewrite[CAPE_Sfc]=CAPE_ETAL_10000
    rewrite[DEPR_AGL-2m]=DEPR_TGL_2
    rewrite[DEPR_AGL-40m]=DEPR_TGL_40
    rewrite[DEPR_AGL-80m]=DEPR_TGL_80
    rewrite[DEPR_AGL-120m]=DEPR_TGL_120
    rewrite[HGT_Sfc]=HGT_SFC_0
    rewrite[PRMSL_MSL]=PRMSL_MSL_0
    rewrite[PRATE_Sfc]=PRATE_SFC_0
    rewrite[SHTFL_Sfc]=SHTFL_SFC_0
    rewrite[LHTFL_Sfc]=LHTFL_SFC_0
    rewrite[TCDC_Sfc]=TCDC_SFC_0
    rewrite[TMP_AGL-2m]=TMP_TGL_2
    rewrite[TMP_AGL-40m]=TMP_TGL_40
    rewrite[TMP_AGL-80m]=TMP_TGL_80
    rewrite[TMP_AGL-120m]=TMP_TGL_120
    rewrite[UGRD_AGL-10m]=UGRD_TGL_10
    rewrite[UGRD_AGL-40m]=UGRD_TGL_40
    rewrite[UGRD_AGL-80m]=UGRD_TGL_80
    rewrite[UGRD_AGL-120m]=UGRD_TGL_120
    rewrite[VGRD_AGL-10m]=VGRD_TGL_10
    rewrite[VGRD_AGL-40m]=VGRD_TGL_40
    rewrite[VGRD_AGL-80m]=VGRD_TGL_80
    rewrite[VGRD_AGL-120m]=VGRD_TGL_120
    rewrite[PRES_Sfc]=PRES_SFC_0
    rewrite[GUST-Max_AGL-10m]=GUST_MAX_TGL_10
    # target names are
    # CMC_hrdps_continental_DEPR_ISBL_0010_ps2.5km_2022122706_P001-00.grib2
    # source names are
    # 20221227T06Z_MSC_HRDPS_DEPR_ISBL_0950_RLatLon0.0225_P010.grib2
    # {YYYYMMDD}T{HH}Z_MSC_HRDPS_{VAR}_{LVLTYPE-LVL}_{Grid}{resolution}_PT{hhh}H.grib2



    for FILE in $DOWNLOADDIRECTORY/*.grib2; do
        JUSTNAME=`echo $FILE | sed -r 's/.+Z_MSC_HRDPS_((.+))_RLatLon0.0225_P.+/\1/'`
        # echo $JUSTNAME to ${rewrite[$JUSTNAME]:-$JUSTNAME}
        BLARG=`echo $FILE | sed -r 's/(\/.+\/)([[:digit:]]+)T([[:digit:]]+)Z_MSC_HRDPS_(.+)_RLatLon0.0225_PT([[:digit:]]+)H.grib2/\1\CMC_hrdps_continental_\4_ps2.5km_\2\3_P\5-00.grib2/'`
        TARGET=`echo $BLARG | sed -r "s/$JUSTNAME/${rewrite[$JUSTNAME]:-$JUSTNAME}/"`
        mv $FILE $TARGET
    done
    echo Done renaming HRDPS rot files at `date`
fi

