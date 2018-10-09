;; Setup variables for windgrams from GDPS source
;; Variables are:
;;  locX, locY the index into the data arrays for the point at site_lon, site_lat

show_rain=0

function hour_to_idx (hour,round_up)
begin
 res = ind(taus.eq.hour);
 if(ismissing(res)) then
  ;print("Couldn't find hour " + hour);
  do j=0,numtimes-1
   ;print("j="+j+",taus(j)="+taus(j))
   if(taus(j).gt.hour)
    break;
   end if
  end do
  if(round_up) then
   j = j + 1;
  else
   j = j - 1;
  end if
  if(j .gt. numtimes-1) then
   j = numtimes-1;
  end if
  if(j .lt. 0) then
   j = 0
  end if
  ;;print("Returning index for hour " + taus(j))
  return(j);
 else
  return res;
 end if
end

function ij_from_lat_lon_generic (a, site_lat, site_lon)
begin
 print("NOT FULLY VERIFIED FOR GDPS")
 ;print("Looking for LAT: " + site_lat + " in " + a[0]->lat_0);
 locX=ind_nearest_coord(site_lat,a[0]->lat_0,0)
 if(site_lon.lt.0) then
  my_lon = 360+site_lon
  ;print("Looking for LON: " + my_lon + " in " + a[0]->lon_0);
  locY=ind_nearest_coord(my_lon,a[0]->lon_0,0)
  ;print("Checking... this should return 0 " + ind_nearest_coord(a[0]->lon_0(0),a[0]->lon_0,0) + " this should return 1 " + ind_nearest_coord(a[0]->lon_0(1),a[0]->lon_0,0))
 else
  locY=ind_nearest_coord(site_lon,a[0]->lon_0,0)
  ;print("Looking for LON: " + my_lon + " in " + a[0]->lon_0);
  ;print("Checking... this should return 0 " + ind_nearest_coord(a[0]->lon_0(0),a[0]->lon_0,0) + " this should return 1 " + ind_nearest_coord(a[0]->lon_0(1),a[0]->lon_0,0))
 end if
 return((/locX, locY/))
end
;; (0)	TMP_P0_L100_GLL0
;; (1)	TMP_P0_L103_GLL0
;; (2)	DEPR_P0_L100_GLL0
;; (3)	DEPR_P0_L103_GLL0
;; (4)	LHTFL_P0_L1_GLL0
;; (5)	SHTFL_P0_L1_GLL0
;; (6)	UGRD_P0_L100_GLL0
;; (7)	UGRD_P0_L103_GLL0
;; (8)	VGRD_P0_L100_GLL0
;; (9)	VGRD_P0_L103_GLL0
;; (10)	PRMSL_P0_L101_GLL0
;; (11)	HGT_P0_L1_GLL0
;; (12)	HGT_P0_L100_GLL0
;; (13)	TCDC_P0_L1_GLL0
;; (14)	lv_HTGL1
;; (15)	lat_0
;; (16)	lon_0
;; (17)	lv_ISBL0

procedure get_initial_info (a) ;; returns timesinfiles, initialization_date, run_time, pbm
local ter, timesinfiles
begin
   ListSetType(a,"cat")
   timesinfiles = a[:]->TMP_P0_L100_GLL0!0
   ;; WHAT TIMES ARE AVAILABLE?
   numtimes=dimsizes(timesinfiles)
   taus:=ispan(0,3*(numtimes-1),3)   ;coordinate variable for Time dimension
   ter := a[0]->HGT_P0_L1_GLL0
   initialization_date = str_get_field(ter@initial_time,1," ");
   ;   print("Initialization_date is " + initialization_date)
   run_str = str_get_field(ter@initial_time,2," ");
   ;; run_str is something like (06:00).  So we just want the first two
   run_str_array = stringtochar(run_str);
   run_time = chartostring(run_str_array(1:2))
   pmb:= a[0]->lv_ISBL0(::-1) ; in Pa

end


procedure load_variables_one_point (a, locY, locX)
begin
   print("Indices into data are " + locX + ", " + locY)
   radian_rot = 0;
   ListSetType(a,"cat")
   h := a[0]->HGT_P0_L1_GLL0(locY,locX)
   ListSetType(a,"join")
   zloc := a[:]->HGT_P0_L100_GLL0(:,::-1,locY,locX) ;; height at various ISBL versus time
   zloc@units = "m"
   zloc!0 = "Time"
   zloc!1 = "levels"
   agl := zloc ; copy metadata too
   agl = agl - h
   uloc := a[:]->UGRD_P0_L100_GLL0(:,::-1,locY,locX)*3.6  ; reverse vertical coordinate
   vloc := a[:]->VGRD_P0_L100_GLL0(:,::-1,locY,locX)
   uloc!0 = "Time"
   uloc!1 = "levels"
   vloc!0 = "Time"
   vloc!1 = "levels"
   ulocSfc := a[:]->UGRD_P0_L103_GLL0(:,0,locY,locX) ; this is the lowest level... 10 m above the ground
   vlocSfc := a[:]->VGRD_P0_L103_GLL0(:,0,locY,locX)
   ulocSfc!0 = "Time"
   vlocSfc!0 = "Time"

    tkloc := a[:]->TMP_P0_L100_GLL0(:,::-1,locY,locX)      ; temperature (K)
    tdloc := a[:]->DEPR_P0_L100_GLL0(:,::-1,locY,locX)      ; dew point depression temperature (K)

    tkloc@_FillValue = defaultFillValue
    tkloc@units = "K"
    tkloc!0 = "Time"
    tkloc!1 = "levels"
    ;tkloc&levels = znom
    ;tkloc&Time = taus

    tdloc@_FillValue = defaultFillValue
    tdloc@units = "K"
    tdloc!0 = "Time"
    tdloc!1 = "levels"

      tSfc     := a[:]->TMP_P0_L103_GLL0(:,locY,locX)  ; sfc temperature in K (2m elevation)
      dpSfc    := a[:]->DEPR_P0_L103_GLL0(:,locY,locX) ; sfc dew point temperature in K (2m elevation)
      mslp     := a[:]->PRMSL_P0_L101_GLL0(:,locY,locX)/1000       ; Sea Level Pressure
      shf      := a[:]->SHTFL_P0_L1_GLL0(:,locY,locX)                ; sfc sensible heat flux
      lhf      := a[:]->LHTFL_P0_L1_GLL0(:,locY,locX)                ; sfc latent heat flux
      rainloc  := a[:]->PRATE_P0_L1_GLL0(:,locY,locX) * 3600.  ; precipitation rate mm/hour
      cloudloc := a[:]->TCDC_P0_L1_GLL0(:,locY,locX)   ; cloud cover in %
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
