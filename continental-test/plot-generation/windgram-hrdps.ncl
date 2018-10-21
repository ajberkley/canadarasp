;; Setup variables for windgrams from HRDPS source
;; Variables are:
;;  locX, locY the index into the data arrays for the point at site_lon, site_lat

smooth = 1;
numdays=1; ;; 0 and 1
tstridemultiday=2
toffsetmultiday=1
function hour_to_idx (hour, roundup)
begin
 return hour
 ;; return(ind(taus.eq.hour))
end

function ij_from_lat_lon_generic (a, site_lat, site_lon)
begin
   XLAT=a[0]->gridlat_0
   XLONG=a[0]->gridlon_0
      locij:=wrf_latlon_to_ij(XLAT,XLONG,site_lat, site_lon)
      locY=locij(0)
      locX=locij(1)
      print(site_lon + " gives locX = "+locX  + " and actual lon = " + XLONG(locY,locX))
      print(site_lat + " gives locY = "+locY  + " and actual lat = " + XLAT(locY,locX))

 return((/locY, locX/))
end

procedure get_initial_info (a)
local ter, timesinfiles
begin
    ListSetType(a,"cat")
   ter := a[:]->HGT_P0_L1_GST0
   timesinfiles:=a[:]->HGT_P0_L1_GST0!0
      ;; WHAT TIMES ARE AVAILABLE?
   numtimes=dimsizes(timesinfiles)
   print("numtimes " + numtimes)
   taus:=ispan(0,numtimes-1,1)   ;coordinate variable for Time dimension

   initialization_date = str_get_field(ter@initial_time,1," ");
   run_str = str_get_field(ter@initial_time,2," ");
   ;; run_str is something like (06:00).  So we just want the first two
   run_str_array = stringtochar(run_str);
   run_time = chartostring(run_str_array(1:2))
   pmb:= a[0]->lv_ISBL0(::-1) ; in Pa
end

procedure load_variables_one_point (a, locY, locX)
local lasthour, windUr, windVr, windUrSfc, windVrSfc, numlevels
begin
    radian_rot=a[0]->gridrot_0(locY,locX)
    ListSetType(a,"cat")
    lasthour = ListCount(a)-1
    print("Last hour index is " + lasthour)
    h := a[lasthour]->HGT_P0_L1_GST0(locY,locX);
    ListSetType(a,"join")
    zloc := a[:]->HGT_P0_L100_GST0(:,::-1,locY,locX) ;; height at various ISBL versus time
    zloc@units = "m"
    zloc!0 = "Time"
    zloc!1 = "levels"
    agl := zloc ; copy metadata too
    agl = agl - h
    numlevels = dimsizes(pmb)
    windUr = a[:]->UGRD_P0_L100_GST0(:,::-1,locY,locX)  ; reverse vertical coordinate
    windVr = a[:]->VGRD_P0_L100_GST0(:,::-1,locY,locX)
    uloc:=new((/numtimes,numlevels/) ,float)
    vloc:=new((/numtimes,numlevels/) ,float)
    uloc = (sin(radian_rot)*windVr + cos(radian_rot)*windUr)*3.6  ; convert from m/s to km/h
    vloc = (cos(radian_rot)*windVr - sin(radian_rot)*windUr)*3.6  ; rotate to N/S grid
    uloc!0 = "Time"
    uloc!1 = "levels"
    vloc!0 = "Time"
    vloc!1 = "levels"
    
    windUrSfc = a[:]->UGRD_P0_L103_GST0(:,0,locY,locX) ; this is the lowest level... 10 m above the ground
    windVrSfc = a[:]->VGRD_P0_L103_GST0(:,0,locY,locX)

    ulocSfc:=new((/numtimes/) ,float)
    vlocSfc:=new((/numtimes/) ,float)
    ulocSfc = (sin(radian_rot)*windVrSfc + cos(radian_rot)*windUrSfc)*3.6  ; convert from m/s to km/h
    vlocSfc = (cos(radian_rot)*windVrSfc - sin(radian_rot)*windUrSfc)*3.6  ; rotate to N/S grid
    ulocSfc!0 = "Time"
    vlocSfc!0 = "Time"

    tkloc := a[:]->TMP_P0_L100_GST0(:,::-1,locY,locX)      ; temperature (K)
    tdloc := a[:]->DEPR_P0_L100_GST0(:,::-1,locY,locX)      ; dew point depression temperature (K)

    tkloc@_FillValue = defaultFillValue
    tkloc@units = "K"
    tkloc!0 = "Time"
    tkloc!1 = "levels"

    tdloc@_FillValue = defaultFillValue
    tdloc@units = "K"
    tdloc!0 = "Time"
    tdloc!1 = "levels"

      tSfc     := a[:]->TMP_P0_L103_GST0(:,0,locY,locX)  ; sfc temperature in K (2m elevation)
      dpSfc    := a[:]->DEPR_P0_L103_GST0(:,0,locY,locX) ; sfc dew point temperature in K (2m elevation)
      mslp     := a[:]->PRMSL_P0_L101_GST0(:,locY,locX)/1000       ; Sea Level Pressure
      shf      := a[:]->SHTFL_P0_L1_GST0(:,locY,locX)                ; sfc sensible heat flux
      lhf      := a[:]->LHTFL_P0_L1_GST0(:,locY,locX)                ; sfc latent heat flux
      rainloc  := a[:]->PRATE_P0_L1_GST0(:,locY,locX) * 3600.  ; precipitation rate mm/hour
      cloudloc := a[:]->TCDC_P0_L1_GST0(:,locY,locX)   ; cloud cover in %
    lclloc  := dpSfc*121. + h  ; quick and dirty cloud base calculation - result in meters
    tSfc!0 = "Time"
    dpSfc!0 = "Time"
    lclloc!0 = "Time"
    mslp!0 = "Time"
    shf!0 = "Time"
    lhf!0 = "Time"
    rainloc!0 = "Time"
    cloudloc!0 = "Time"

end
