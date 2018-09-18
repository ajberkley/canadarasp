; WINDGRAM generator
;  Based on original code by TJ Olney
;  Modified by Peter Spear and Andrew Berkley

; USAGE:
;    ncl windgram-continental.ncl 'input_files="/mnt/tiles/-122:-120:49:51/hrdps_*.grib"'   ;; input information
;                                 [plot_day=0|1|2] output_files="woodsidewgram.png ; bridalwgram.png"
;                                 labels_lat_lons="Woodside, 49.0,-122.0 ; Bridal Falls, 49.5, -122.5 ; ..."
;                                 [max_altitude=4500 dp_cut=0.5 debug=1 sink_rate=1 unit_scheme=4 show_wind=1]
;    This will generate output files called "twoDay/2018-06-22/woodsidewgram.png" "oneDay/2018-06-22/..."

;; Load helper functions

NCARG_ROOT=getenv("NCARG_ROOT")
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "windgram-utilities.ncl"
load "windgram-continental-colormap.ncl"

; zTop is the number of levels, HRDPS is usually 13 (3500m)
; as is, it calculates lcl three different ways and you must change it in the gsn_csm_addpolymarkers = command if you want to try another
; it has not been completely "error trapped" but it catches some.

defaultFillValue = 999900026055400882176.
locations = "/home/ubuntu/continental-test/plot-generation/locations.txt"

begin

  operator="ajb & ps (ec2)"                       ; your name here :-)
  ;color choices must be in the palette being used or the closest one will be used.
  foreground_color= "white"
  background_color= (/.5, .5, .9 /) ;deep purple-blue
  label_color = foreground_color
  line_color = 13
  templine_color = 22

  if(.not.isvar("max_altitude")) then
     max_altitude = 4500
  end if
  if(.not.isvar("sink_rate")) then
     sink_rate=1.0
  end if
  if(.not.isvar("dp_cut")) then
     dp_cut=0.5
  end if
  debug=1
  
  if(.not.isvar("tmp_units"))then
    tmp_units = 0      ;ok  change to 0 for C for Celcius 1 for Farenheight --  only affects the show_temp variable.
  end if

  if(.not.isvar("show_condense"))then
    show_condense=1
  end if

  if(.not.isvar("show_cloud"))then
    show_cloud=1
  end if

  if(.not.isvar("show_wind"))then
    show_wind  = 1          ; windbarbs
  end if

  if(.not.isvar("color_wind_barbs"))then
    if(show_wind.eq.1)then
       color_wind_barbs=0     ; change this if you want the wind barbs to be colored by a scalar of their speed  note units
    else
     color_wind_barbs=1
    end if
  end if

  if(.not.isvar("show_rain"))then
    show_rain=1
  end if

  if(.not.isvar("show_lapse"))then
    show_lapse = 1          ;local level to level lapse rate
  end if
  if(.not.isvar("show_wing"))then
    show_wing  = 1          ; hg/pg 225ft/min 1.2m/s   soaring level markers
  end if
  if(.not.isvar("show_lcl"))then
    show_lcl   = 1          ; little clouds for lifted condensation level
  end if
  if(.not.isvar("show_snow"))then
    show_snow  = 0          ; lowest freezing level
  end if

  if(.not.isvar("show_pbl"))then
    show_pbl = 0    ; boundary layer top bar default to not show it.  Means nothing to most Foot launched pilots.
  end if


  if(.not.isvar("hide_lapse_colors"))then
    hide_lapse_colors=0     ; any other value will  make all colors  transparent  for lapse so it doesn't show but still provides scaffold for other stuff.
  end if

  if(.not.isvar("show_temp"))then
    show_temp  = 1          ; temperature contours
  end if
  if(.not.isvar("show_up_vel"))then
    show_up_vel= 1          ; show vertical velocity W* at top of time column  set to 0 if no access to drJack Fortran modules.
  end if
  if(.not.isvar("show_z_contour"))then
    show_z_contour= 0       ; draw a contour line for the altitude  attaching pressure level in the morning to corresponding altitude in evening
    z_stride=4              ; extra spacing between altitude contour lines
  end if
  if(.not.isvar("show_p_contour") .and. show_z_contour.ne.0 )then
    show_p_contour= 0       ; draw a contour line for the pressure level in the morning to corresponding altitude in evening
    z_stride=z_stride              ; extra spacing between altitude contour lines
  else
    show_p_contour= 0
  end if

  if(.not.isvar("show_pressure"))then
    show_pressure  = 1
  end if;

  ;; END ARGUMENT PARSING

  startTime = stringtolong(systemfunc("date +%s%3N")) ;; record this so we can see how long things take to run
  
  ;; LOAD FILES
  input_file_names = systemfunc("ls -1 " + input_files + " | sort") ;; to guarantee they are in increasing hour order, so a[47] is P048, a[0] is P001

  a = addfiles(input_file_names,"r");

  ;; FORECAST INITIALIZATION TIME DETECTION
  ter := a[:]->HGT_P0_L1_GST0 ;; Grab some variable so we can see when the model was initialized
  initialization_date = str_get_field(ter@initial_time,1," ")
  yyyy_mm_dd = systemfunc("date -u -d " + quote + initialization_date + quote + " +%Y-%m-%d")
  run_str = str_get_field(ter@initial_time,2," ");
  ;; run_str is something like (06:00).  So we just want the first two
  run_str_array = stringtochar(run_str);
  run_time = chartostring(run_str_array(1:2))
  print ("Plotting data for model initialized on date " + yyyy_mm_dd + " at " + run_time + ":00 UTC")
  ;; Now some complex conversions to local time.
  offset = stringtoint(systemfunc("date -d "+quote + yyyy_mm_dd + quote+ " +%z"))/100 ;; For this date (in UTC? local?), what is the offset in hours from UTC 
  pstpdt= systemfunc("date -d "+quote + yyyy_mm_dd + quote+ " +%Z") ;; what is our local time
  fcstday=systemfunc("date -u -d " + quote + yyyy_mm_dd + quote + " +%a-%b-%d")
  fcstYMD=systemfunc("date -u -d " + quote + yyyy_mm_dd + quote + " +%Y-%m-%d")
  date_str = "date "+quote+"+%a %d %b %Y %H:%M %Z" + quote+" -d " + quote + yyyy_mm_dd + " +" + tostring(stringtoint(run_time)+1) + "hours UTC" + quote
  local_start_time = systemfunc(date_str)
  print("First hour of the model " + local_start_time)

  ;; LEARN ABOUT OUR GRID
  XLAT=a[0]->gridlat_0
  XLONG=a[0]->gridlon_0
  rot=a[0]->gridrot_0

  ;; WHAT TIMES ARE AVAILABLE?
  timesinfiles:=a[:]->HGT_P0_L1_GST0!0  ;forecast_times
  numtimes=dimsizes(timesinfiles)
  print("numtimes " + numtimes)
  taus:=ispan(0,numtimes-1,1)   ;coordinate variable for Time dimension

  ;; WHAT LEVELS ARE AVAILABLE?
  pmb:= a[0]->lv_ISBL0(::-1) ; in Pa
  numlevels=dimsizes(pmb) ; How many levels we have predicted wind speeds at.
  print("num z levels is " + numlevels)
  ;; The HRDPS data has nominal output levels at
  ;; znom_meters = (/100, 230, 350, 480, 660, 880, 1100, 1300, 1600, 2100, 2600, 3100, 3700, 4300, 5000, 5700, 6500, 7400, 8300, 9400, 10000, 10600, 11300, 12000, 12800, 13800, 16300, 20600/)

  ; FIGURE OUT WHAT DAYS AND HOURS WILL BE USED IN THE PLOT
  hourFrom = 7 ; hours of the day used for the "day plots"
  hourTo = 21
  startHour = stringtoint(systemfunc("date +%H -d "+quote+local_start_time +quote))

  do plot_day = -1, 2
  if(plot_day.gt.-1)then ; only plot part of the data
    if(startHour.le.hourFrom) then  ; model starts early in the day. we can get both days OK
      if (plot_day .eq. 1) then  ; plot day 0
        hourFrom = hourFrom+24
        hourTo = hourTo+24
      end if
    else if(startHour.lt.hourTo) then  ; model starts in the middle of the day. Skip day 0 and only plot the day 1
      if (plot_day .eq. 0) then
       print("plot_day = 0 but only half the first day is available. Skipping day0 windgrams. Exiting")
       continue
      end if
      hourFrom = hourFrom+24
      hourTo = hourTo+24
    else ; model starts late in the day.
      if (plot_day .eq. 0) then  ; plot day 0
        hourFrom = hourFrom+24
        hourTo = hourTo+24
      else
        hourFrom = hourFrom+48
        hourTo = hourTo+48
      end if
    end if
    end if
    tFrom = hourFrom-startHour
    tTo = hourTo-startHour
  else
    tFrom = 0
    tTo=numtimes-1
  end if
  date_str = "date " + quote + "+%a, %b %d, %Y" + quote + " -d "+ quote + yyyy_mm_dd + " +" + (stringtointeger(run_time)+tFrom+1) + "hours UTC" + quote
  local_day = systemfunc(date_str)
  print ("local day is " + local_day)
  file_date_str = "date " + quote + "+%Y-%m-%d" + quote + " -d "+ quote + yyyy_mm_dd + " +" + (stringtointeger(run_time)+tFrom+1) + "hours UTC" + quote
  file_date = systemfunc(file_date_str)

  lstDay := new(numtimes,string,"")        ;put local time into this for labels
  lstNight := new(numtimes,string,"")        ;put local time into this for labels
  do i=0,numtimes-1
    tmp = (startHour+i)%24
    if(plot_day .gt. -1)then ; only plot part of the data
      lstDay(i)=tostring(tmp)
    else ; plot all hours
      if (tmp .lt. 8 .or. tmp .gt. 19) then
        lstNight(i) = tostring(tmp)
      else
        lstDay(i)= tostring(tmp)
      end if
    end if;
  end do
  print("plotting from hour " + hourFrom + " to hour " + hourTo + ", from index " + tFrom + " to index " + tTo)
  endTime = stringtolong(systemfunc("date +%s%3N"))
  delta = (endTime - startTime)/1000.0
  startTime = endTime
  print("Initialization took " + delta + "s")

  numsites = str_fields_count(labels_lats_lons,";");
  print(numsites + " sites to process")
  do site_idx=0,numsites-1               ; for one loxY locX at a time.
    site_info = str_get_field(labels_lats_lons,1+site_idx,";")
    site_name = str_get_field(site_info,1,",")
    site_lat = stringtofloat(str_get_field(site_info,2,","))
    site_lon = stringtofloat(str_get_field(site_info,3,","))
    outputfile = str_get_field(output_files,site_idx+1,";")
   if(plot_day.eq.-1) then
     outputfile = output_dir + "/twoDay/" + file_date + "/" + outputfile
   else
     outputfile = output_dir + "/oneDay/" + file_date + "/" + outputfile
   end if

    print("Processing " + site_name + " at " + site_lat + "," + site_lon + " and writing output to " + outputfile);

    locij:=wrf_latlon_to_ij(XLAT,XLONG,site_lat, site_lon)
    locX=locij(1)
    locY=locij(0)
    ;; Always use these to functions as locY, locX
    ; print(site_lon + " gives locX = "+locX  + " and actual lon = " + XLONG(locY,locX))
    ; print(site_lat + " gives locY = "+locY  + " and actual lat = " + XLAT(locY,locX))

    lasthour = ListCount(a)-1
    print("Last hour is " + lasthour)
    ListSetType(a,"cat")
    h = a[lasthour]->HGT_P0_L1_GST0(locY,locX)
    ListSetType(a,"join")
    zloc := a[:]->HGT_P0_L100_GST0(:,::-1,locY,locX) ;level altitudes ;; this is now hte reverse order?

    zloc@units = "m"
    zloc!0 = "Time"
    zloc!1 = "levels"

    agl := zloc ; copy metadata too
    agl = agl - h



    ; find minimum & max z index
    do zBot=0,numlevels-1
      if (zloc(0,zBot) .gt. (h*1.05+20)) then
        break
      end if
    end do
    do zTop=0,numlevels-1
      if (zloc(0,zTop) .gt. max_altitude) then
        break
      end if
    end do

    zloc0:=round(zloc(0,:),0)
    print("terrain " + h  + "m, first level " + zBot + ", at " + zloc0(zBot) + "m, next level " + zloc0(zBot+1) + "m")

    print ("Windgram for "+site_name+" at point "+locY+" "+locX+" "+site_lon+" "+site_lat+" model elevation:"+h+"m ASL")
    

    windUr = a[:]->UGRD_P0_L100_GST0(:,::-1,locY,locX)  ; reverse vertical coordinate
    windVr = a[:]->VGRD_P0_L100_GST0(:,::-1,locY,locX)
    uloc:=new((/numtimes,numlevels/) ,float)
    vloc:=new((/numtimes,numlevels/) ,float)
    radian_rot = rot(locY,locX)
    uloc = (sin(radian_rot)*windVr + cos(radian_rot)*windUr)*3.6  ; convert from m/s to km/h
    vloc = (cos(radian_rot)*windVr - sin(radian_rot)*windUr)*3.6  ; rotate to N/S grid
    delete(windUr)
    delete(windVr)
    uloc!0 = "Time"
    uloc!1 = "levels"
    ;uloc&levels = znom
    ;uloc&Time = taus
    vloc!0 = "Time"
    vloc!1 = "levels"
    ;vloc&levels = znom
    ;vloc&Time = taus
    windUr = a[:]->UGRD_P0_L103_GST0(:,0,locY,locX) ; this is the lowest level... 10 m above the ground
    windVr = a[:]->VGRD_P0_L103_GST0(:,0,locY,locX)

    ulocSfc:=new((/numtimes/) ,float)
    vlocSfc:=new((/numtimes/) ,float)
    ulocSfc = (sin(radian_rot)*windVr + cos(radian_rot)*windUr)*3.6  ; convert from m/s to km/h
    vlocSfc = (cos(radian_rot)*windVr - sin(radian_rot)*windUr)*3.6  ; rotate to N/S grid
    delete(windUr)
    delete(windVr)
    ulocSfc!0 = "Time"
    vlocSfc!0 = "Time"

    tkloc := a[:]->TMP_P0_L100_GST0(:,::-1,locY,locX)      ; temperature (K)
    tdloc := a[:]->DEPR_P0_L100_GST0(:,::-1,locY,locX)      ; dew point depression temperature (K)

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

    ;tdloc&levels = znom
    ;tdloc&Time = taus

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

    ; -------------            calculate "local" lapse rates                              ---------------------------;
    ;;---------------------------------------------------------------------------------------------------------------;
    ;;---------------------------------------------------------------------------------------------------------------;


    lapse := (tkloc(:,1:)-tkloc(:,:numlevels-2))/(zloc(:,1:)-zloc(:,:numlevels-2))  ; local lapse rates in C/m.
    lapse!0 = "Time"
    lapse!1 = "levels"
    ;lapse&levels = zloc0(zBot:zTop-1)
    ;lapse&Time = taus
    ;print(" delta z " + zdiffs(0,:) + ", delta T " +  tdiffs(0,:) + ", Lapse rate " +  lapse(0,:))


    blDepth:=new((/numtimes/) ,float)

    do j=tFrom,tTo
      do i=zBot,numlevels-1
        blDepth(j) = 0.0
                              ; dry adiabatic lapse rate is 9.8C/km
        if ((tSfc(j) - agl(j,i)*0.0098) .le. tkloc(j,i)) then ; if lifted packet is colder than surounding air and will stop rising
          if (i.gt.0) then
            blDepth(j) = (tSfc(j)-tkloc(j,i-1)+lapse(j,i-1)*agl(j,i-1))/(0.0098+lapse(j,i-1)) ; interpolate actual altitude (agl)
          ;print ("level=" + i + " time=" + j+ " agl=" + agl(j,i)+ " tSfc=" + (tSfc(j)-273.15)+ " tkloc=" + (tkloc(j,i-1)-273.15) + " lapse=" +lapse(j,i-1) + " blDepth=" + blDepth(j))
          end if
          break
        end if
      end do
    end do



  ; calculate the virtual temperature heat flux (see Garratt, Ch 2)
  ; vhf = shf + 0.61*Cp/L * T * lhf
  ; where Cp is the heat capacity of the air (1006J/kg K),
  ;       T is the surface temperature
  ;    and L is the heat of vaporization of water (2.0502MJ/kg
  ; The constant terms are: 0.61*cp/L=0.61*(1006J/Kkg)/(2.502x106J/kg)=0.000245268
  vhf := shf +  0.000245268*tSfc*lhf



  ;      ;see http://www.drjack.info/blip/INFO/parameter_details.html for w* calculation
  ;      ; w* =  [ (g/To) Qv D ]^1/3
  ;      ; g = 9.8m/s2
  ;      ; Qv = sfc virtual heat flux
  ;      ; D = boundary layer depth
  ;      ; To = potential temperature in the BL
  ;      ;    = Tsfc(Psfc/Psl)^R/Cp
  ;      ; R = 8.314J/mol/K
  ;      ; Cp = 29.19J/mol/K

  ; Peter Dec 2014: The units don't work!! The heat flux needs to be converted to a kinematic heat flux.
  ;                 The kinematic heat flux in in units of K m/s. which can be thought of as the "movement" or flux
  ;                 of temperature from the surface. For example, 1kW/m2 will heat a one meter section of air above the
  ;                 surface by roughly 1K every second - or 1K m/s
  ; see "Updraft Model for Development of Autonomous Soaring Uninhabited Air Vehicles" from NASA for
  ; a good description of the calculations.
  ;  w* =  [ (g/To) Qv/(Cp rho) D ]^1/3
  ;  where Cp is the heat capacity of the air (1006J/kg K)
  ;  and rho is the air density at the Po reference level (1.29kg/m3)
  ;
  ; the w* units are then:
  ;   [m/s2  1/K  J/s/m2  kgK/J  m3/kg  m]1/3 = [m3/s3]1/3 = m/s
  ;
  ; constant is 9.8/1006/1.29 = 0.0075516

  	 potTemp := tSfc*(pmb(0)/pmb(zBot))^0.28482
     wstar := new(numtimes,float)
    do j=tFrom,tTo
       ;print("vhf(j)=" + vhf(j) + " blDepth(j)=" + blDepth(j) )
       if (vhf(j) .le. 0) .or. (blDepth(j) .le. 0)  then
         wstar(j)= 0
       else
         wstar(j) = (0.0075516 / potTemp(j) * vhf(j) * blDepth(j))^0.333333
       end if
     end do

  ;print("taus=" + taus + " label=" + lstDay + " wstar="+wstar)

  ; the average updraft velocity as a function of altitude is roughly given by (Lenschow and Stephens, 1982)
  ; wavg = wstar * 1.34 * (z/D)^1/3 * (1-0.8z/D)
  ; The peak updraft (at the center of the core) is roughly 3 times greater
  ; hcrit, assuming a 1m/s sink rate is the highest we can get before the climb rate drops to zero
  ; find hcrit


  ; maximum updraft occurs at roughly 30% of blDepth. The maximum updraft will be roughly 2x wstar
  ; find that level and start there

    hcrit := new((/numtimes/) ,float)
    do j=tFrom,tTo
      if (wstar(j)*2.02 .lt. sink_rate) then ; the maximum updraft will be too small.
         hcrit(j) = 0.0
         continue
      end if
      updraft = wstar(j) * 1.97 ; initial values incase the first level triggers the loop break
      alt = 0.20 * blDepth(j)

      do i=zBot,numlevels-1
        if (agl(j,i) .lt. 0.25 * blDepth(j)) then  ; start at 25% of blDepth
          continue
        end if
        if (zloc(j,i) .gt. lclloc(j)) then
          hcrit(j) = lclloc(j)    ; good pilots don't fly in the clouds!
          break
        end if;

        lastUpdraft = updraft
        lastAlt = alt
        alt = agl(j,i)
        updraft = wstar(j) * 4.0 * (alt/blDepth(j))^0.33333 * (1.0-0.8*alt/blDepth(j))
        if (updraft .le. sink_rate) then
          ; interpolate the altitude
          hcrit(j) = zloc(j,i) + (lastAlt-alt)/(lastUpdraft-updraft)*sink_rate
  ;print ("i=" +i+" updraft=" +updraft+" lastUpdraft=" +lastUpdraft+" alt=" + alt + " lastAlt=" + lastAlt + " hcrit=" + hcrit(j))
          break
        end if
      end do
    end do
    ;print (hcrit)

; test to see the clouds appear correctly
;lclloc(0:10)=(/260, 385, 554, 771, 992, 1219, 1452 ,1935 ,2447, 2988, 3563/)

;  print ("lcl, blDepth, vhf, tSfc, wstar, hcrit")
;  print (lclloc + ", " + blDepth + ", " + vhf + ", " + (tSfc-273.15) +  ", " + wstar +  ", " + hcrit)


    ;find lowest freezing level  .lt. zTop
    if(show_snow.ne.0)
      freezelevel:=new(numtimes,float)
      do j=tFrom,tTo
        do i=zBot,zTop-1
          if (.not.ismissing(tkloc(j,i)) .and. tkloc(j,i).lt.273.15)                ; find the level where temp falls below freezing
            freezelevel(j)=zloc0(i)            ; assign it to the scalar freezelevel for this time
            break                                    ; once found do it for the next time
          end if
        end do;
      end do;
    end if





    ;;-----------------------------          end of lapse calculations  ---------------------------------------------;
    ;;---------------------------------------------------------------------------------------------------------------;

    ;======================        Set up the Workstation  for this site.            ================
    ; must be destroyed at end of loop  must be within the loop  color array can be outside though.
    type = "png"
    type@wkWidth = 880
    type@wkHeight = 880
    type@wkAntiAlias = "On"

    wks = gsn_open_wks(type, outputfile)


    gsn_define_colormap(wks, updcolors)   ; generate new color map
      setvalues wks
      "wkForegroundColor" : foreground_color              ; set at top
      "wkBackgroundColor" : background_color              ; set at top
    end setvalues



    ;;---------------------------------------------------------------------------------------------------------------;
    ;--------------------------start windgram  ----------------------------------------------------------------------


    if(plot_day .gt. -1)then ; only plot part of the data
      tstride=1
      toffset=0
      init_date_str = "date "+quote+"+%a %H:%M %Z" + quote+" -d " + quote + yyyy_mm_dd + " +" + tostring(stringtoint(run_time)+1) + "hours UTC" + quote
      date_initialized = systemfunc(init_date_str)
      MainTitle = "HRDPS Windgram for "+ site_name + " for " + local_day + " (initialized " + date_initialized + ")"
      lstDayColor = "white"
      barbLen = 0.03
    else
      tstride=2
      toffset=1

      MainTitle = "HRDPS Windgram for "+ site_name + ". First hour is " + local_start_time
      lstDayColor = "gold"
      barbLen = 0.023
      ; smooth the LCL and HCRIT results
      lclloc(1:numtimes-2) = (lclloc(0:numtimes-3) + 2*lclloc(1:numtimes-2) + lclloc(2:numtimes-1))/4.0
      hcrit(1:numtimes-2) = (hcrit(0:numtimes-3) + 2*hcrit(1:numtimes-2) + hcrit(2:numtimes-1))/4.0
    end if


    ; eliminate excessive clutter at the bottom of the plot and make lcl clouds hollow if they are above the boundary layer
    ;lclloc2:=new(numtimes ,float)

    do j=tFrom,tTo
      ;if (lclloc(j) .lt. zloc(j,zBot)) then
      ;  lclloc(j) = lclloc@_FillValue
      ;else
      if (lclloc(j) .gt. blDepth(j)+h) then
        ;lclloc2(j) = lclloc(j)
        lclloc(j) = lclloc@_FillValue
      end if
      ;end if
      if (.not.ismissing(hcrit(j)) .and. (hcrit(j) .lt. zloc(j,zBot))) then
        hcrit(j) = hcrit@_FillValue
      end if;
    end do

    locLevels = zTop-zBot+1
    locTimes = tTo-tFrom+1
    ;print (locTimes + " " + locTimes/tstride)
    ugrid:=new((/locLevels,locTimes/) ,float)
    vgrid:=new((/locLevels,locTimes/) ,float)
    lgrid:=new((/locLevels,locTimes/) ,float)
    cond_grid:=new((/locLevels,locTimes/) ,float)
    tcgrid:=new((/locLevels,locTimes/) ,float)
    ;printVarSummary(ugrid(1:,1::tstride))
    ;printVarSummary(uloc(levels|zBot:zTop-1,Time|tFrom+toffset:tTo:tstride))

    ugrid(1:,toffset::tstride)=uloc(levels|zBot:zTop-1,Time|tFrom+toffset:tTo:tstride)            ;reverse the dimensions so Time is on Xaxis
    ugrid(0,toffset::tstride)=ulocSfc(tFrom+toffset:tTo:tstride)            ;reverse the dimensions so Time is on Xaxis
    vgrid(1:,toffset::tstride)=vloc(levels|zBot:zTop-1,Time|tFrom+toffset:tTo:tstride)            ;reverse the dimensions so Time is on Xaxis
    vgrid(0,toffset::tstride)=vlocSfc(tFrom+toffset:tTo:tstride)            ;reverse the dimensions so Time is on Xaxis

    lgrid(1:,:)=lapse(levels|zBot:zTop-1,Time|tFrom:tTo)*303.0          ;reverse x and y axis & convert to C/1000ft
    lgrid(0,:)=(tkloc(levels|zBot,Time|tFrom:tTo)-tSfc(tFrom:tTo))/(zloc0(zBot)-h)*303.0

    cond_grid(1:,:)=-tdloc(levels|zBot:zTop-1,Time|tFrom:tTo)            ;reverse the dimensions so Time is on Xaxis
    cond_grid(0,:)=-dpSfc(tFrom:tTo)

    tcgrid(1:,:)=tkloc(levels|zBot:zTop-1,Time|tFrom:tTo)-273.15            ;reverse the dimensions so Time is on Xaxis
    tcgrid(0,:)=tSfc(tFrom:tTo)-273.15                                      ; Celcius
    if(tmp_units.eq.1)
      tcgrid=tcgrid*1.8+32         ; Farenheight
    end if

    zlevels:=new(locLevels,float)
    zlevels(1:)=zloc0(zBot:zTop-1)
    zlevels(0)=h
    feets:=round(zlevels*3.3,0)+"'"                             ;feets used for labels is a string by adding the '
    meters:=zlevels+"m"                          ;;string for labels
    ;locLevels = zTop - zBot
    ;print("znom=" + znom + ", meters="+ meters + ", feets=" +feets)
    ;print("zBot=" +zBot + ", zTop=" + zTop)
    ; these were created reversed so time in in 2nd position.
    vgrid!0="levels"
    vgrid!1="Time"
    vgrid&Time=taus(tFrom:tTo)
    vgrid&levels=zlevels

    ugrid!0="levels"
    ugrid!1="Time"
    ugrid&Time=taus(tFrom:tTo)
    ugrid&levels=zlevels

    lgrid!0="levels"
    lgrid!1="Time"
    lgrid&Time=taus(tFrom:tTo)
    lgrid&levels=zlevels

    cond_grid!0="levels"
    cond_grid!1="Time"
    cond_grid&Time=taus(tFrom:tTo)
    cond_grid&levels=zlevels

    tcgrid!0="levels"
    tcgrid!1="Time"
    tcgrid&Time=taus(tFrom:tTo)
    tcgrid&levels=zlevels


    ;                      resources for all 2D plots x--> time  y -->height
    ;xaxis_msg= "      Local Lapse Rate in Degrees C per 1000ft  "   ;default xaxis message becomes part of tiXAxisString
    xaxis_msg= "      Local Lapse Rate in ~F34~0~F~C per 1000ft.  "   ;default xaxis message becomes part of tiXAxisString
    ;;; SET BASIC TEXT RESOURCES
    lbtxtres = True
    lbtxtres@tiXAxisFont = 21 ;helvetica
    lbtxtres@txFuncCode    = "~"
    lbtxtres@txFontHeightF = 0.015
    lbtxtres@txFontHeightF = 0.015
    lbtxtres@txJust = "CenterLeft"

    res2D := True       ; res2D set basic resources for most other plots
    wstar_res = False       ; res2D set basic resources for most other plots
    cloud_res = False       ; res2D set basic resources for most other plots
    rain_res = False       ; res2D set basic resources for most other plots
    res2D@tiMainFont = 21
    res2D@tiXAxisFont = 21 ;helvetica
    res2D@tiYAxisFont = 21 ;helvetica
    res2D@lbTitleFont = 4
    res2D@lbAutoManage         = False      ;test tjo see discussion on ncl-talk
    res2D@lbRightMarginF = 0.0
    res2D@lbJustification = "topleft"
    res2D@lbLeftMarginF = 0.0
    res2D@lbTopMarginF = 0.0
    res2D@lbTitlePosition =  "Bottom"
    res2D@lbLabelPosition = "Top"
    res2D@lbLabelOffsetF = .01
    ; res2D@lbFontHeightF = .016  ;default is .02
    res2D@lbLabelFontColor = label_color  ;default is foreground (white here)
    res2D@lbBoxMinorExtentF = .23   ;.33 is default
    ; res2D@lbTitleOffsetF  = .05
    ; res2D@lbTitleExtentF = .05
    ; res2D@lbBoxMinorExtentF =  ;  default is 1.0
    res2D@pmLabelBarOrthogonalPosF = 0.010

    res2D@gsnDraw              = False      ; Don't draw individual plot.
    res2D@gsnFrame             = False      ; Don't advance frame.
    res2D@tiXAxisFuncCode    = "~"
    res2D@tmYRLabelFuncCode= "~"
    res2D@tmYLLabelFuncCode = "~"
    res2D@vpXF                 = 0.1       ; .1 x location
    vpYF_top = 0.94
    res2D@vpYF                 = vpYF_top       ; y location
    res2D@vpWidthF             = 0.82       ; .82 width big!
    res2D@vpHeightF            = 0.77       ; .8 was windgram alone height
    res2D@tiYAxisOffsetXF        =  -.06    ;moves it slightly to the left
    res2D@tiXAxisOffsetYF        =  -0.04   ;-.02 ;16       ;- moves it slightly down + up  .02 makes it nicely close to the time ticmarks


    resolution="~F22~833m~F21~"
    res2D@tiXAxisString        = "Time ("+pstpdt+"). Windbarbs(km/h). Lat,lon = "+site_lat+", "+site_lon+"~Z90Q~~C~"+xaxis_msg+" Ground level " +round(h,0)+"m asl" ;plottime   ;site_idx wlat is for this region wlats is all regions...
    res2D@tiYAxisString        = " "
    res2D@tiYAxisFontHeightF   = 0.015
    res2D@tiXAxisFontHeightF   = 0.015
    res2D@tiMainFontHeightF    = 0.015
    res2D@tiMainOn             = True
    res2D@tiMainString         = MainTitle
    res2D@tmEqualizeXYSizes    = False
    ;bottom tickmarks
    res2D@tmXUseBottom         = False                ;needed so top labels and ticks can be different from bottom
    ;res2D@tmXUseTop         = False                ;needed so top labels and ticks can be different from bottom
    res2D@tmXBMode             = "Explicit"
    res2D@tmXBValues           := taus(tFrom:tTo:tstride)
    res2D@tmXBLabelsOn         = True ; times
    res2D@tmXBLabels           := lstDay(tFrom:tTo:tstride)   ;          times   pst pdt??
    res2D@tmXBLabelFontColor   = lstDayColor
    res2D@tmYLLabelFont            = 21                ;helvetica http://www.ncl.ucar.edu/Document/Graphics/font_tables.shtml
    res2D@tmYRLabelFont            = 21
    res2D@tmXBLabelFont            = 21
    res2D@tmXBLabelJust        = "CenterCenter"
    res2D@tmXBLabelFontHeightF = .014
    res2D@tmYLLabelFontColor   =  label_color
    res2D@tmYLMajorLengthF     = .01
    res2D@tmYLLabelDeltaF     =  -1
    res2D@tmXBMajorLengthF     = .01
    res2D@tmYLOn               = True
    res2D@tmYLMode             = "Explicit"
    res2D@tmYLBorderOn         = True
    res2D@tmXTMinorOn         = False

    ystride=1
    res2D@tmYLValues           := zlevels(::ystride)
    res2D@tmYRValues           := zlevels(::ystride)
    res2D@tmYRLabels           := feets(::ystride)
    res2D@tmYLLabels           := meters(::ystride)
  	;print (znom(zBot:zTop-1:ystride))
    res2D@tmYUseLeft           = False   ; necessary for right labels to work at all
    res2D@tmYROn               = True
    res2D@tmYLLabelsOn = True
    res2D@tmYRLabelsOn         = True
    res2D@tmYRMode             = "Explicit"
    res2D@tmYRBorderOn         = True
    res2D@tmYLLabelFontHeightF = .014
    res2D@tmYRLabelFontHeightF = .014
    res2D@tmYRMajorLengthF     = .01
    res2D@tmYRLabelDeltaF     =  -1
    res2D@tmYLBorderOn         = True
    res2D@tmXTMajorLengthF     = .01
    res2D@tmXTMajorOutwardLengthF     = .01
    res2D@tmXTOn               = True
    res2D@trYTensionF          = 3 ; was getting errors with the default of 2

; Resource for the strip plots at the top
    stripHeight = 0.05
    strip_res = True
    strip_res@gsnDraw              = False      ; Don't draw individual plot.
    strip_res@gsnFrame             = False      ; Don't advance frame.
;view port size & posn
    strip_res@vpXF                 = res2D@vpXF       ; .1 x location
    strip_res@vpWidthF             = res2D@vpWidthF       ; .82 width big!
    strip_res@vpHeightF            = stripHeight       ; .1 was windgram alone height
;titles
    strip_res@tiXAxisOn            = False
    strip_res@tiMainOn             = True
    strip_res@tiMainFont           = res2D@tiMainFont
    strip_res@tiMainFontHeightF    = res2D@tiMainFontHeightF
    strip_res@tiMainString         = res2D@tiMainString
    strip_res@tiYAxisOn            = True
    strip_res@tiYAxisAngleF        = 0
    strip_res@tiYAxisJust          = "CenterLeft"
    strip_res@tiYAxisFontHeightF   = 0.016
    strip_res@tiYAxisFont          = 21 ;helvetica
; X tic marks
    strip_res@tmEqualizeXYSizes    = False
    strip_res@tmXBOn               = False
    strip_res@tmXTOn               = True
    strip_res@tmXUseBottom         = False
    strip_res@tmXTMinorOn         = False
    strip_res@tmXTMode             = "Explicit"
    strip_res@tmXTValues           := taus(tFrom:tTo:tstride)
    strip_res@tmXTLabelsOn         = False
    strip_res@tmXTMajorLengthF     = .01
    strip_res@tmXTMajorOutwardLengthF     = .01
; Y tic marks
    strip_res@tmYROn               = True
    strip_res@tmYLOn               = False
    strip_res@tmYUseLeft           = False
    strip_res@tmYRLabelsOn         = True
    strip_res@tmYRMajorLengthF     = .01
    strip_res@tmYRMajorOutwardLengthF     = 0.01
    strip_res@tmYRLabelJust        = "CenterRight"
    strip_res@tmYRBorderOn         = True
    strip_res@tmYRLabelFontHeightF = .014
    strip_res@tmYRMode               = "Manual"
    strip_res@tmYRMinorOn           = False
    strip_res@gsnYRefLine          = 0.0            ; create a reference line


        ;show_this_rain=0
    if(show_rain.ne.0) then
      if (max(rainloc(tFrom:tTo)) .gt. 0.02) then
        rain_res = strip_res
      end if;
    end if;

    if (rain_res) then
      ; adjust the size and appearance of the main viewport
      res2D@vpYF                 = res2D@vpYF - stripHeight
      res2D@vpHeightF            = res2D@vpHeightF - stripHeight
      res2D@tiMainOn    = False
      res2D@tmXTOn               = False
      res2D@pmLabelBarOrthogonalPosF = res2D@pmLabelBarOrthogonalPosF + stripHeight/3.333

      rain_res@vpYF                 = vpYF_top       ; y location
      rain_res@tiYAxisOffsetXF      = 0.068
      if (max(tcgrid) .lt. 0) then
        rain_res@tiYAxisString        = "Snow   ~Z80Q~(cm/hr)"
      else if (min(tcgrid(0,:)) .lt. 0) then
        rain_res@tiYAxisOffsetXF      = 0.110
        rain_res@tiYAxisString        = "Precip   ~Z80Q~(SWE mm/hr)"
      else
        rain_res@tiYAxisString        = "Rain   ~Z80Q~(mm/hr)"
      end if
      end if
      rain_res@tiYAxisFontColor      = "paleturquoise"
      rain_res@tmYRLabelFontColor = "paleturquoise"
      tempvar := ceil(max(rainloc(tFrom:tTo))*50.0)/50.0
      rain_res@tmYRTickStartF         = tempvar/2
      rain_res@tmYRTickEndF           = tempvar
      rain_res@tmYRTickSpacingF       = tempvar/2
      rain_res@gsnAboveYRefLineColor = "darkturquoise"        ; above ref line fill blue "gray75"
      rain_res@trYMinF              = 0
      rain_res@trYMaxF              = tempvar
    end if


    if(show_cloud .eq. 1)then
      cloud_res = strip_res

      ; adjust the size and appearance of the main viewport
      res2D@vpYF                 = res2D@vpYF - stripHeight
      res2D@vpHeightF            = res2D@vpHeightF - stripHeight
      res2D@tiMainOn    = False
      res2D@tmXTOn               = False
      res2D@pmLabelBarOrthogonalPosF = res2D@pmLabelBarOrthogonalPosF + stripHeight/3.333

      if(rain_res) then
        rain_res@vpYF                = rain_res@vpYF - stripHeight
        rain_res@tiMainOn            = False
        rain_res@tmXTOn              = False
      end if

      cloud_res@vpYF                 = vpYF_top       ; y location
      cloud_res@tiYAxisString        = "Cloud   ~Z80Q~(%)"
      cloud_res@tiYAxisFontColor      = "gray80"
      cloud_res@tiYAxisOffsetXF      = 0.034
      cloud_res@tmYRLabelFontColor    = "gray80"
      cloud_res@tmYRTickStartF         = 50
      cloud_res@tmYRTickEndF           = 100
      cloud_res@tmYRTickSpacingF       = 50
      cloud_res@gsnAboveYRefLineColor = "gray50"        ; above ref line fill blue "gray75"
      cloud_res@trYMinF              = 0
      cloud_res@trYMaxF              = 100
   end if

    if(show_up_vel .eq. 1) .and. (max(wstar) .ge. 0.2) then
      wstar_res = strip_res
      ; adjust the size and appearance of the main viewport
      res2D@vpYF                 = res2D@vpYF - stripHeight
      res2D@vpHeightF            = res2D@vpHeightF - stripHeight
      res2D@tiMainOn    = False
      res2D@tmXTOn               = False
      res2D@pmLabelBarOrthogonalPosF = res2D@pmLabelBarOrthogonalPosF + stripHeight/3.333
      if(cloud_res)then
        cloud_res@vpYF               = cloud_res@vpYF - stripHeight       ; y location
        cloud_res@tiMainOn            = False
        cloud_res@tmXTOn               = False
      end if
      if(rain_res)then
        rain_res@vpYF                = rain_res@vpYF - stripHeight
        rain_res@tiMainOn            = False
        rain_res@tmXTOn               = False
      end if

      wstar_res@vpYF                 = vpYF_top       ; y location
      wstar_res@tiYAxisString        = "Lift   ~Z80Q~(m/s)"
      wstar_res@tiYAxisFontColor      = "gold"
      wstar_res@tiYAxisOffsetXF      = 0.044
      wstar_res@tmYRLabelFontColor    = "gold"
      tempvar := ceil(max(wstar(tFrom:tTo))*5.0)/5.0
      wstar_res@tmYRTickStartF         = tempvar/2
      wstar_res@tmYRTickEndF           = tempvar
      wstar_res@tmYRTickSpacingF       = tempvar/2
      wstar_res@gsnAboveYRefLineColor = "gold3"
      wstar_res@trYMinF              = 0
      wstar_res@trYMaxF              = tempvar
    end if

    if(show_pressure .eq. 1) then
      pres_res = strip_res
      ; adjust the size and appearance of the main viewport
      res2D@vpYF                 = res2D@vpYF - stripHeight
      res2D@vpHeightF            = res2D@vpHeightF - stripHeight
      res2D@tiMainOn    = False
      res2D@tmXTOn               = False
      res2D@pmLabelBarOrthogonalPosF = res2D@pmLabelBarOrthogonalPosF + stripHeight/3.333
      if(cloud_res)then
        cloud_res@vpYF                = cloud_res@vpYF - stripHeight       ; y location
        cloud_res@tiMainOn            = False
        cloud_res@tmXTOn              = False
      end if
      if(rain_res)then
        rain_res@vpYF                 = rain_res@vpYF - stripHeight
        rain_res@tiMainOn             = False
        rain_res@tmXTOn               = False
      end if
      if(wstar_res)then
        wstar_res@vpYF                = wstar_res@vpYF - stripHeight
        wstar_res@tiMainOn            = False
        wstar_res@tmXTOn              = False
      end if
      pres_res@vpYF                 = vpYF_top       ; y location
      pres_res@tiYAxisString        = "pres.   ~Z80Q~(kPa)"
      pres_res@tiYAxisFontColor      = "coral"
      pres_res@tiYAxisOffsetXF      = 0.048
      pres_res@tmYRLabelFontColor    = "coral"
      to := ceil(max(mslp(tFrom:tTo))*5.0)/5.0
      from := floor(min(mslp(tFrom:tTo))*5.0)/5.0
      mid:=(to+from)/2
      pres_res@tmYRTickStartF         = round((to+from)*5,0)/10
      pres_res@tmYRTickEndF           = to
      pres_res@tmYRTickSpacingF       = round((to-from)*5,0)/10
      pres_res@gsnAboveYRefLineColor = "coral3"
      pres_res@trYMinF              = from
      pres_res@trYMaxF              = to
      ;print ("from="+from+" to="+to+"mid="+mid)
    end if





    ;;-------------------------------------------------------------------------------------------------------;
    ;;--------begin defining the relative humidity plot resources only plot if gt 95% -- use small cross hatch
    ;;-------------------------------------------------------------------------------------------------------;
    ;4test printVarSummary(rh)
    ; now just at the focal point

    ; define plotting resources for the relative humidity plot with only 2 levels below and above dpcut (0.5C)
    condmsg="~Z80Q~cross hatch~C~shows cloud~C~DPR <"+dp_cut+"~F34~0~F~C"
    hum_res=res2D
    hum_res@cnLevelSelectionMode = "ExplicitLevels"
    hum_res@cnLevels             = (/ 0.95 /)  ; 1 = crosshatched, 0 = not.
    hum_res@cnFillOn             = True
  ;    hum_res@cnFillColors         = (/-1,0/);(/-1,1/)       ; background color crosshatching
    hum_res@cnFillColors         = (/-1,22/);(/-1,1/)       ; transparent/black crosshatching
    hum_res@cnFillScaleF         = 1.5
    ;hum_res@cnFillScaleF         = .3
    hum_res@cnFillPattern        = 6              ;6 crosshatch  17 is stipple
    hum_res@cnFillDotSizeF       = 0.06           ;default is 0.0
    hum_res@tmXBOn               = False          ; turn off things that we don't want
    hum_res@tmYROn               = False
    hum_res@tmYLOn               = False
    hum_res@cnLinesOn            = False
    hum_res@cnLineLabelsOn       = False
    hum_res@tmXTLabelsOn         = False
    hum_res@tmXBLabelsOn         = False
    hum_res@cnInfoLabelOn        = False
    hum_res@tiMainString         = ""
    hum_res@tiXAxisString        = ""
    hum_res@tiYAxisString        = ""
    hum_res@lbLabelFontHeightF   = 0.002
    hum_res@cnConstFEnableFill = True
    hum_res@cnConstFLabelOn = False
    hum_res@cnLevels             = (/ -dp_cut /)  ; 1 = crosshatched, 0 = not.




    ;;-------------------------------------------------------------------------------------------------------;
    ;;-------------------------------------------------------------------------------------------------------;
    ;             resources for local lapse rate contour plot
    ;;-------------------------------------------------------------------------------------------------------;
    lapse_res = res2D
    ;lapse_res@trYMinF              = zloc0(zBot)
    ;lapse_res@trYMaxF              = zloc0(zTop-1)
    lapse_res@cnConstFEnableFill = True
    lapse_res@cnLevelSelectionMode = "ExplicitLevels"  ; -3 is condsidered absolutely unstable
    lapse_res@cnLevels =            (/ -3.,-2.5,-2.0,-1.5,-1.2, -.5, 0.,.5 /)

    if(hide_lapse_colors.eq.0)
      lapse_res@cnFillColors         = (/ 11,10, 7, 8, 3,2,2,13,14 /)      ;should work pink and purple reversed using updraft.rgb 12,13,14  gray
      ;lapse_res@cnFillColors         = (/ 11,10,9,7,8,2,2,13,14 /)      ;should work pink and purple reversed using updraft.rgb 12,13,14  gray
    else
      lapse_res@cnFillColors         = (/ -1,-1,-1,-1,-1,-1,-1,-1,-1 /)      ;all transparent so lapse rate doesn't show.
    end if


    lapse_res@cnFillOn             = True
    lapse_res@cnMonoLineColor       = False
    lapse_res@cnLineColors = lapse_res@cnFillColors(1:)
    lapse_res@cnLinesOn            = True
    ;lapse_res@cnLineColor = -1
    lapse_res@cnLineLabelsOn       = False
    lapse_res@tmXTLabelsOn         = False
    ;lapse_res@tmXBLabelsOn         = False
    lapse_res@cnInfoLabelOn        = False
    lapse_res@tiMainString          = ""
    lapse_res@tiXAxisString          = ""
    lapse_res@lbLabelFontHeightF   = 0.014
    lapse_res@lbBoxSeparatorLinesOn   = False
    lapse_res@tmXBLabelDeltaF = -0.64 ; compensate for the offset due to the tickmarks




    ;;-------------------------------------------------------------------------------------------------------;
    ;;-------------------------------------------------------------------------------------------------------;
    ;                       resources for lcl markers
    ;;-------------------------------------------------------------------------------------------------------;
    ; Add some cloud markers to plot all markers are plotted with a contour plot, not a vector plot:
    ;  - Create a cloud marker.
    ;  - Set some marker resources.
    ;  - Attach markers to "plot" using gsn_add_polymarker
    ;    Font 37 has a cloud for the letter k   19 / is a "paraglider",  20 E static= C ,20 / a blipspot, 20 6 a crescent moon angle 270 is a paraglider.
    ;    34 E is a blipspot  34 D is an Xspot, 36 ) is turbulence, 37 B  180deg a rogollo wing 37g raindropblown   35 < ...
    ;    37 F lightning  g and o Rain, q snowflake ,r fastcloud, k puffycloud
    mstring = "k"
    fontnum = 37
    xoffset = 0.0
    yoffset = 0.0
    tallyoffset = 0.5
    ratio   = .75
    tall  = 1.5
    if(plot_day .gt. -1) then 
      size    = 1.4
    else
      size    = 0.9
    end if
    angle   = 0.0

    cloud   = NhlNewMarker(wks, mstring, fontnum, xoffset, yoffset, ratio, size, angle)  ;puffy cloud
   ; hollowCloud   = NhlNewMarker(wks, mstring, 137, xoffset, yoffset, ratio, size, angle)  ;puffy cloud
    ;darkcloud   = NhlNewMarker(wks, mstring, fontnum, xoffset, yoffset, 1, 3, angle)  ; big puffy cloud for coloring dark dewpt=tmp
    cloudfast = NhlNewMarker(wks, "r", 37, xoffset, tallyoffset, tall, size, 0)          ;wind blown cloud
    paragl = NhlNewMarker(wks, "6", 20, xoffset, yoffset, tall, .5, 270)                 ;crescent moon tipped over
    ;ceilingmrk = NhlNewMarker(wks, "<", 35, xoffset, yoffset,  ratio, size, angle)       ;unused  three dots
    pbl_mark = NhlNewMarker(wks, "E", 20, xoffset, yoffset,  .3, 3*size, angle)            ; horizontal bar
    snowflake= NhlNewMarker(wks, "q", 37, xoffset, yoffset,   1, 1, 0)                   ; snowflake for freezing level
    ;raindrop=NhlNewMarker(wks, "g", 37, xoffset, yoffset,   1, .2, 0)      ; raindrop thing curved
    ;rogallo=NhlNewMarker(wks, "B", 37, xoffset, yoffset,   .5, .5, 180)        ; rogallo wing head on
    mkres                    = True
    ;mkres@gsClipOn           = False         ;symbols may overhang edges of the viewport.
    mkres@gsClipOn           = True         ;symbols may NOT overhang edges of the viewport.
    mkres@gsMarkerIndex      = cloud
    mkres@gsMarkerSizeF      = 0.03
    mkres@gsMarkerThicknessF = 2
    mkres@gsMarkerColor      = "white"
    ; xmarkers=taus

   ; mkres1 = mkres   ; copy mkres then change color and marker
   ; mkres1@gsMarkerColor      = "white"  ;12        ; hollow cloud for above boundary layer
   ; mkres1@gsMarkerIndex      = hollowCloud
   ; mkres1@gsMarkerThicknessF = 3


    mkres3 =mkres
    mkres3@gsMarkerThicknessF = 3
    mkres3@gsMarkerColor    ="blue"
    mkres3@gsMarkerIndex      = paragl


    mkres4 =mkres
    mkres4@gsMarkerColor      ="white"
    ; mkres4@gsMarkerIndex      = ceilingmrk
    mkres4@gsMarkerIndex      = snowflake    ;freezing level

    mkres5 =mkres
    mkres5@gsMarkerIndex      = pbl_mark     ;pbl_mark
    mkres5@gsMarkerColor      = "yellow"




    ;;----------------------------Vector plot resources for windgram ----------------------------------------;
    ;;----most tick mark labels come from here --------------------------------------------------------------;
    uv_res = res2D
    uv_res@tmYUseLeft             = False    ;
    uv_res@vcRefAnnoOn        = False       ; turns off the ref vector
    uv_res@vcRefLengthF       = barbLen ;  set length of ref vector
    uv_res@vcGlyphStyle       = "WindBarb"  ; turn on wind barbs
    uv_res@vcMinDistanceF       = 0.000           ; Min Distance between arrows
    ;control this below  uv_res@vcMonoWindBarbColor      = False ;True     ;False    colored barbs  get lost in the lapse colors only sho if hiding lapse
    uv_res@vcWindBarbColor         = "white"  ;gray48" ;(\ 0.9,0.9,0.9 \) ;    colored barbs  get lost in the lapse colors only sho if hiding lapse
    uv_res@vcLevelSelectionMode    = "ExplicitLevels" ; will explicitly set the levels
    uv_res@vcWindBarbLineThicknessF= 2.5           ; has been 3 set the wind barb thickness 3 works well for png output
    uv_res@vcLevels     = (/ 3.,6.,9.,12.,15.,18.,21. /)   ;knots 1.852km 1.1507mph
    uv_res@vcMapDirection = False      ; needed to disassociate the coordinate plane from the wind barbs thanks Andrea Barcellona
    uv_res@vcLevelColors    = (/2,3,4,5,6,7,8,9,10/)
    uv_res@tmYLOn      = False
    uv_res@tmYROn     = False
    ;uv_res@tmYRValues              := zloc0(zBot:zTop-1:ystride) ;  znom is plevels up to zTop-1
    ;uv_res@tmYLValues              := zloc0(zBot:zTop-1:ystride) ;
    uv_res@tmXTLabelsOn     = False
    uv_res@tmXBLabelsOn     = False
    ;uv_res@tiXAxisString        = ""
    ;uv_res@tiXAxisOffsetYF        =  -0.03   ;-.02 ;16       ;- moves it slightly down + up  .02 makes it nicely close to the time ticmarks

    uv_res@tmYLLabelsOn     = False
    uv_res@tmYRLabelsOn     = False

    ;uv_res@tmYRLabels              = feets(zBot:zTop-1:ystride)
    ;  uv_res@tmYLLabels              = stplevels(zBot:zTop:ystride)
    ;uv_res@tmYLLabels              = meters(zBot:zTop-1:ystride)
    ;   uv_res@tmYLLabels              = stplevels(zBot:zTop-1:ystride)+"~C~"+meters(zBot:zTop-1:ystride)
    ;uv_res@trYMinF       = zloc0(zBot) ;minp
    ;uv_res@trYMaxF       = zloc0(zTop-1) ;p

    ;-----------------------------------------------------------------------------------------------------;
    ;-----------------------------------------------------------------------------------------------------;
    ;-------------     the  end of main site loop plots what has been set up.  ---------------------------;
    ;-----------------------------------------------------------------------------------------------------;
    ;-----------------------------------------------------------------------------------------------------;




    ;;;       Create a Time Stamp for the plot -- when it was plotted.
    ;;      timestamp=systemfunc("date +%H:%M_%Z")
    ; ABC Oct 9, 2012 - Added date to timestamp and fixed extra space.
    timestamp=systemfunc("date +%Y-%m-%d' '%H:%M' '%Z")

    ;timestamp=get_script_name()+"~C~"+timestamp+"~C~"+operator
    timestamp="~F~~Z80Q~"+get_script_name()+"~C~"+timestamp+"~C~"+operator
    ;timestampyNDC = 5*lbtxtres@txFontHeightF  ;was 4*
    timestampyNDC = 0.075
    timestampxNDC = .005 ;lbtxtres@txFontHeightF
    ;draw the timestamp
    gsn_text_ndc ( wks, timestamp,timestampxNDC,timestampyNDC,lbtxtres )



    raspmsg="RASP~F35~d~F~~Z80Q~~C~TJ&DJ"          ;RASPtm italic DrJack
    gsn_text_ndc ( wks, raspmsg,.01,.98,lbtxtres )    ;put the message .94 from bottom and .01 from left 800x800 48px down 8px in

    stablemsg=""

    ;; if we are coloring in the lapse rate this section applies.
    if(show_lapse.ne.0)
      if((hide_lapse_colors.ne.0).or.(color_wind_barbs.eq.1))
        ;  uv_res@vcMonoFillArrowFillColor = False
        uv_res@vcMonoWindBarbColor      =  False ;True    ;False    they get lost in the lapse colors
        if(debug.ne.0) then
          print(" hide_lapse_colors = "+hide_lapse_colors+" or color_wind barbs ="+color_wind_barbs+" and therefore monoWindbarb color set to False there should have been colors")
        end if
      else
        if(debug.ne.0)
          print(" hide_lapse_colors = "+hide_lapse_colors+" or color_wind barbs ="+color_wind_barbs+" and therefore monoWindbarb color set to False there should NOT NOT have been colors")
        end if
        uv_res@vcMonoWindBarbColor      =  True     ; make default monochrome windbarbs False     ;False    they get lost in the lapse colors
        ;  uv_res@vcMonoFillArrowFillColor = False
      end if
      ;print ("doing lapse")
      lapse_plot=gsn_csm_contour(wks,lgrid,lapse_res)


      if(show_lcl.ne.0)
        ;print ("doing lcl")
        marker = gsn_add_polymarker(wks,lapse_plot,taus(tFrom+toffset:tTo:tstride),lclloc(tFrom+toffset:tTo:tstride),mkres) ; add little cloud markers
        ;marker2 = gsn_add_polymarker(wks,lapse_plot,taus(tFrom+toffset:tTo:tstride),lclloc2(tFrom+toffset:tTo:tstride),mkres1) ; add little hollow cloud markers
      end if

      if(show_wing.ne.0)
        marker3 = gsn_add_polymarker(wks,lapse_plot,taus(tFrom+toffset:tTo:tstride),hcrit(tFrom+toffset:tTo:tstride),mkres3)       ; hgldj_p max soar w/clouds  sideways crescent moons
      end if

      if(show_snow.ne.0)
        marker4 = gsn_add_polymarker(wks,lapse_plot,taus(tFrom:tTo),freezelevel(tFrom:tTo),mkres4)       ; lowest freezing level snowflakes
      end if

      if(show_pbl.ne.0)
        marker5 = gsn_add_polymarker(wks,lapse_plot,taus(tFrom:tTo),blDepth(tFrom:tTo),mkres5)       ; level of planetary boundary layer
      end if


      draw(lapse_plot)
    end if


      if(show_condense.ne.0)
      ;print ("doing condensation")
        condenselevels=gsn_contour(wks,cond_grid,hum_res)
        gsn_text_ndc ( wks, condmsg, .90, .09 ,lbtxtres )
        draw(condenselevels)
      end if


    if(show_wind.ne.0)
      ;print ("doing wind")
      wind_gram = gsn_vector(wks,ugrid,vgrid,uv_res)
    ; adjust the wind plot scale slightly so that the bottom and top windbarbs can be seen
      getvalues wind_gram
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues
       ;print ("trXMinF=" + trxmin + " trXMaxF=" + trxmax +  " trYMinF=" + trymin +  " trYMaxF=" + trymax)
      setvalues wind_gram
        "trYMinF"    : trymin-0.2
        "trYMaxF"    : trymax+0.2
      end setvalues

      draw(wind_gram)
    ;   delete(wind_gram)
    end if
      ;.102 .024 puts the stable msg at left border of colored bar.

      ;lbtxtres@txFontHeightF = 0.02
      ;-2.5,-2.0,-1.5,-1.2,

    ;stablemsg="Unstable      Conditional Instability               Stable            Inverted ";~C~-3C/1000ft      Local Lapse Rate           -1.2C/1000ft"
    ypos := 0.017
    gsn_text_ndc ( wks, "Unstable", .106, ypos, lbtxtres )  ; was .113, .024 for bottom of box
    gsn_text_ndc ( wks, "Conditional Instability", .24, ypos,lbtxtres )  ; was .113, .024 for bottom of box
    gsn_text_ndc ( wks, "Stable", .6, ypos,lbtxtres )  ; was .113, .024 for bottom of box
    gsn_text_ndc ( wks, "Inverted", .78, ypos,lbtxtres )  ; was .113, .024 for bottom of box

    ;;  ----------------  Temperature contour lines if desired.
    if(show_temp.ne.0)
      tc_res=res2D
      tc_res@cnLinesOn = True
      tc_res@cnLineLabelsOn = True
      tc_res@cnLineLabelBackgroundColor = -1
      tc_res@cnLineLabelFontColor = "navy"
      tc_res@cnLineColor = "navy"
      tc_res@cnInfoLabelOn = False
      tc_res@tmYLOn      = False
      tc_res@tmYROn     = False
      tc_res@tmYLLabelsOn     = False
      tc_res@tmYRLabelsOn     = False
      tc_res@tiMainString       = ""
      tc_res@tiXAxisString        = ""
      tc_res@tmXBLabelsOn = True
      tc_res@tmXBLabels           := lstNight(tFrom:tTo:tstride)   ;          times   pst pdt??
      tc_res@tmXBLabelFontColor = 22 ; "dark grey";          times   pst pdt??
      tc_res@cnFillOn = True     ; bullshit hack used to eliminate some wierd contour lines on the lower left
      tc_res@cnMonoFillColor = True ; adding a transparent fill fixes it.
      tc_res@cnFillColor = -1

      ;print ("doing temperature")

      showtemp=gsn_contour(wks,tcgrid,tc_res)
      draw(showtemp)
      delete(showtemp)
    end if

    if(rain_res)
      ;print ("doing rain")
      rain_plot = gsn_csm_xy(wks,taus(tFrom:tTo),rainloc(tFrom:tTo),rain_res)
      draw(rain_plot)
    end if

    if(cloud_res)
          ;print ("doing cloud plot")

      cloud_plot = gsn_csm_xy(wks,taus(tFrom:tTo),cloudloc(tFrom:tTo),cloud_res)
      draw(cloud_plot)
    end if

    if(wstar_res)
      ;print ("doing wstar")
      wstar_plot = gsn_csm_xy(wks,taus(tFrom:tTo),wstar(tFrom:tTo),wstar_res)
      draw(wstar_plot)
    end if

    if(pres_res)
      ;print ("doing pressure")
      wstar_plot = gsn_csm_xy(wks,taus(tFrom:tTo),mslp(tFrom:tTo),pres_res)
      draw(wstar_plot)
    end if

    frame(wks)                            ; now frame the plot

    delete(wks)

      convertCommand="convert "+outputfile+ " png8:"+outputfile
      print(" "+convertCommand)
      system(convertCommand)

    ;destroy(wks)

    endTime = stringtolong(systemfunc("date +%s%3N"))
    delta = (endTime - startTime)/1000.0
    startTime = endTime
    print("windgram took " + delta + "s")
    print(" ")


    ;;-----------------------------------------------------------------------------------------------------------------;
    ;;-----------------------------------------------------------------------------------------------------------------;
  end do
  end do
  status_exit(0)

end

