;; plot_hrdps_continental

domainid = new(1, string)
do_plots = new(1, string)
qdatimeinfo = new(1, string)

;;; PLOT CONTOUR
;;; Magnitude indicated by Colour

function plot_contour(wks, bg, mag, plot_param, tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, headerfooterfilename)
local opts_plot
begin

	if(isatt(mag, "long_name")) then
		delete_VarAtts(mag, "long_name")
	end if

	if(isatt(mag, "description")) then
		delete_VarAtts(mag, "description")
	end if
	if(isatt(mag, "units")) then
		delete_VarAtts(mag, "units")
	end if
	mag@lat2d = alat
	mag@lon2d = alon

	ldatafmt = 0	; May be overridden in rasp.site_load.countour-parameter.ncl
	opts_plot = True
	copy_VarAtts(opts_cn, opts_plot)

	fixedcolor = 0

        if (isvar("opts_contour")) then
          delete_VarAtts(opts_contour, -1)
        else
          opts_contour = True
        end if


	if(do_plots .eq. False) then
		return(ldatafmt)
	end if

	if( isatt(opts_bparam, "ContourParameters")) then
		levels = opts_bparam@ContourParameters
	end if
	if(isvar("levels")) then
		if(levels(1) .gt. levels(0)) then       ;  Min, Max & Intvl specified
			fixedcolor = 1
		end if
	else
		levels = 0				; Flag to indicate Not Specified
	end if

	; This does not allow for Min, Max to be specified - Why would you want to?
	if(plot_param .eq. "mslpress") then
		if(ismissing(opts_bparam@ContourParameters(2))) then
			step = 2.0
		else
			step = opts_bparam@ContourParameters(2)
		end if
		t1 = floor(min(mag) / step) * step  ; Display mslp on "step" boundaries
		t2 = ceil( max(mag) / step) * step
		levels = (/ t1 + step, t2 - step, step /)
	end if

	; Use new variable to avoid changing mag
	if((plot_param .eq. "sfctemp") .or. (plot_param .eq. "sfcdewpt")) then
		if( UNITS .eq. "american") then
			mag4plot = 32. + 1.8 * mag   ; Fahrenheit
			opts_bparam@ContourParameters = opts_bparam@ContourParameters*1.8 + 32.
		else
			mag4plot = mag
		end if
	else
		mag4plot = mag * opts_bparam@mult
	end if
	copy_VarAtts(mag, mag4plot)

	; Catch the "all missing" case - which blows up otherwise
	if( all(ismissing(mag4plot))) then
		print("    All values missing")
		delete(levels)
		levels = 0
		mag4plot = 0
	end if

	mn = min(mag4plot)
	mx = max(mag4plot)
	; Catch the all-same case when levels are not specified
	if(mn .eq. mx) then
		print("    All values the same")
		delete(levels)
		levels = 0
		mag4plot = 0
	end if

	print("    plot_contour: " + plot_param + " min = " + mn + " max = " + mx)

  if (isvar("opts_contour") .and. isatt(opts_contour, "cnLevelSelectionMode")) then
		opts = True
		copy_VarAtts(opts_contour, opts)
	else if( isatt(opts_bparam, "ExplicitLevels") .and. isatt(opts_bparam, "cnLevels")) then
		opts = True
		opts@cnLevelSelectionMode = "ExplicitLevels"
		opts@cnLevels = opts_bparam@cnLevels
	else
		opts = doColorLevels(wks, levels, mn, mx)
	end if
	end if

	getvalues wks
		"wkColorMap" : wkColorMap
	end getvalues
	copy_VarAtts(opts, opts_plot)
	fName = OutDir + plot_param + tail + ".body"
	wks_temp = create fName imageWorkstationClass defaultapp
		"wkFormat" : "newpng"
		"wkHeight" : img_height
		"wkWidth"  : img_width
		"wkBackgroundOpacityF" : 0.0
		"wkBackgroundColor" : "white"
		"wkForegroundColor" : "black"
		"wkColorMap"        : wkColorMap
	end create

	copy_VarAtts(opts_bg, opts_plot)
	opts_plot@mpOutlineOn         = False
        opts_plot@cnMonoLineColor       = False
        opts_plot@cnLineColors = opts_plot@cnFillColors
        opts_plot@cnLinesOn            = False ; True changed ajb
        opts_plot@cnConstFEnableFill = True ; added ajb
        opts_plot@cnConstFLabelOn  = False ; added ajb
;        opts_plot@cnFillMode = "RasterFill" ; added ajb, seems to slwo it down!
	;printVarSummary(mag4plot)
	;print(opts_plot)

	plot = gsn_csm_contour_map(wks_temp, mag4plot, opts_plot)
	draw(plot)

	; See http://www.ncl.ucar.edu/Support/talk_archives/2006/0750.html
	; for a not-very-obvious descripion of this twiddle
	NhlChangeWorkstation(bg, wks_temp)
	draw(bg)
	NhlChangeWorkstation(bg, wks)
	frame(wks_temp)
	delete(wks_temp)	; Close the file

	if(FMT .eq. "png") then
                cmd = CONVERT + " -trim "  + " " + fName + ".png +repage  png8:" + fName + ".png"
		system(cmd)
	end if
	headerfooter = getenv("ENV_NCL_HEADERFOOTER")
	if(headerfooter .eq. "1") then
	  print("    (also generating Header and footer)")
          doHeader(headerfooterfilename, opts_bparam@MainTitle, opts_bparam@SecondTitle, opts_bparam@ThirdTitle, FMT)
          doLabelBar(headerfooterfilename, opts, FMT, opts_bparam@unitString, fixedcolor)
        end if

	return(ldatafmt)
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PLOT CONTOUR with STREAMLINES
;;; Magnitude indicated by Colour
;;; Direction indicated by Streamline

function plot_streamline(wks, bg, u, v, mag, plot_param, tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, headerfooterfilename)
local opts_plot
begin
	;        print("Starting streamline")
	;	system("date --rfc-3339=ns")
	ldatafmt = 0	; May be overridden in rasp.site_load.wind.ncl
	
	u@lat2d = alat
	u@lon2d = alon
	v@lat2d = alat
	v@lon2d = alon
	mag@lat2d = alat
	mag@lon2d = alon

	opts_plot = True
	copy_VarAtts(opts_cn, opts_plot)

	getvalues wks
		"wkColorMap" : wkColorMap
	end getvalues

	load "opts_ws.ncl"      ;;;; Options for WindSpeed
        ;;; FOR SITE SUBSTITUTIONS
	opts_ws = True
	load "rasp.site_load.wind.ncl" ; Has opts_ws@ContourParameters = (...)

	if(do_plots .eq. False) then
		return(ldatafmt)
	end if


	; New vars to avoid changing originals
	mag4plot = mag * opts_bparam@mult
	u4plot = u * opts_bparam@mult
	v4plot = v * opts_bparam@mult
	copy_VarAtts(mag, mag4plot)
	copy_VarAtts(u, u4plot)
	copy_VarAtts(v, v4plot)

	fixedcolor = 0
  if (isvar("opts_ws") .and. isatt(opts_ws, "cnLevelSelectionMode")) then
		copy_VarAtts(opts_ws, opts_plot)
	else

	  fixedcolor = 0
	  if( isatt(opts_bparam, "ContourParameters")) then
			levels = opts_bparam@ContourParameters
	  end if
	  if( isatt(opts_ws, "ContourParameters")) then
			levels = opts_ws@ContourParameters
	  end if
		if(isvar("levels")) then
			if(levels(1) .gt. levels(0)) then       ;  Min, Max & Intvl specified
				fixedcolor = 1
			end if
		else
			levels = 0				; Flag to indicate Not Specified
		end if

		mn = min(mag4plot)
		mx = max(mag4plot)

		print("    plot_streamline: " + plot_param + " MIN,MAX = " + mn + " " + mx)
		opts = doColorLevels(wks, levels, mn, mx)

		copy_VarAtts(opts, opts_plot)
	end if
		;print (opts_plot)

  fName = OutDir + plot_param + tail + ".body"
  wks_temp = create fName imageWorkstationClass defaultapp
          "wkFormat" : "newpng"
          "wkHeight" : img_height
          "wkWidth"  : img_width
      		"wkBackgroundOpacityF" : 0.0
          "wkBackgroundColor" : "white"
          "wkForegroundColor" : "black"
          "wkColorMap"        : wkColorMap
  end create

  copy_VarAtts(opts_bg, opts_plot)
  opts_plot@mpOutlineOn         = False
  opts_plot@cnMonoLineColor       = False
  opts_plot@cnLineColors = opts_plot@cnFillColors
  opts_plot@cnLinesOn            = True
        opts_plot@cnConstFEnableFill = True
	plot = gsn_csm_contour_map(wks_temp, mag4plot, opts_plot) ; Do it!
	plot_st = gsn_csm_vector(wks_temp, u4plot, v4plot, opts_vc) ; Do it!
	;system("date --rfc-3339=ns")
	;print("Done with vector plot, starting overlay and draw")
	;system("date --rfc-3339=ns")
	overlay(plot, plot_st)
	draw(plot)
        ;system("date --rfc-3339=ns")	
        ;print("Overlay and draw done, writing the output")
	; See http://www.ncl.ucar.edu/Support/talk_archives/2006/0750.html
	; for a not-very-obvious descripion of this twiddle
	;        system("date --rfc-3339=ns")

        ;  Here we have a background that is shared by all the plots, say the coastline,
	;  we want to not have to recompute it, so we draw it here.  But, we are not using
	;  any background at all, so I removed the below three calls
	; NhlChangeWorkstation(bg, wks_temp)
	; draw(bg)
	; NhlChangeWorkstation(bg, wks)
	frame(wks_temp)
	delete(wks_temp)        ; Close the file
        ; system("date --rfc-3339=ns")
	if(FMT .eq. "png") then
                cmd = CONVERT + " -trim "  + " " + fName + ".png +repage  png8:" + fName + ".png"
		system(cmd)
	end if
	headerfooter = getenv("ENV_NCL_HEADERFOOTER")
	fName = OutDir + plot_param + tail + ".body"
	if(headerfooter .eq. "1") then
	  print("Generating Header and Footer")
          doHeader(headerfooterfilename, opts_bparam@MainTitle, opts_bparam@SecondTitle, opts_bparam@ThirdTitle, FMT)
          doLabelBar(headerfooterfilename, opts_plot, FMT, opts_bparam@unitString, fixedcolor)
	end if

    return(ldatafmt)
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


function utc_date_string_to_local_date_string(date_string, date_format)
local result, quote
begin
 quote = inttochar(34);
 command = "date -d " + quote + "`echo " + date_string + " | tr _ " \
                      + quote + " " + quote + "` UTC "\
                       + quote + " +"  + quote +  date_format + quote;
 result = systemfunc(command);
 return(result)
end


;;;;;;;;;;;;;;;;; PLOT PARAMETERS SPECIFIED IN "PARAMS" ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
procedure plot_parameters(filename, params)
local plot_param_chars,simulation_start, sim_minutes, simulation_valid_date,local_start_time
begin
    f = addfile( filename, "r" )
    names = str_split(filename, "/")
    lon1 = stringtointeger(str_get_field(names(2),1,":"))
    lon2 = stringtointeger(str_get_field(names(2),2,":"))
    lat1 = stringtointeger(str_get_field(names(2),3,":"))
    lat2 = stringtointeger(str_get_field(names(2),4,":"))
    ;    print(lon1 + " " + lon2 + " " + lat1 + " " + lat2);
    ; /tiles/-120:-118:45:47/hr

    z=f->HGT_P0_L100_GST0
    simulation_start = z@initial_time
    sim_minutes=z@forecast_time
    delete(z)
    print("Initialization Time: " + simulation_start + ", Forecast Minutes: " + sim_minutes)

    qt = inttochar(34)
    date_str = systemfunc("echo "+qt+simulation_start+qt+" | tr -d " +qt+ "()" + qt) + " UTC"
    local_start_time = systemfunc("date +%c -d "+ qt +  date_str + qt)

    date_str = "date +%c -d "+qt+local_start_time+" + "+sim_minutes+" minutes" +qt
    ;print(date_str)
    local_valid_time = systemfunc(date_str)
    ;print(local_valid_time)
    date_str = "date +%Y-%m-%d_%H%M -d "+qt+local_valid_time +qt
	  local_start_txt = systemfunc(date_str)
	  print("Local Init Time: " + local_start_time + ", Local Forecast Time: " + local_valid_time)

	  ;print(local_start_txt)

    ;local_start_time = utc_date_string_to_local_date_string( simulation_valid_date, "%Y-%m-%d_%H%M" )
    print("HRDPS model initialized " + simulation_start + " UTC, valid " + local_valid_time)
    qdatimeinfo = "Init = " + simulation_start + "UTC" ; , Valid = " + local_valid_time

        initmodel = "HRDPS"
        tail = "_"+local_start_txt;
	date_str = "date +%Y-%m-%d -d "+qt+local_valid_time +qt
	date_val = systemfunc(date_str);
        OutDir = OutDir  + date_val +"/"
        print("Output plots written to " + OutDir)
        blarg = systemfunc("mkdir -p " + OutDir) ; make sure it exists

        do_plots = True ; always plot

	load "getHRDPSvars.ncl"	; Inline code, so variables are local => discarded on exit from procedure
				; New wrfout file will reload

    load "opts_bg.ncl"	; Plotting options for Background Map
    load "opts_cn.ncl"	; Contour Options

         ;; MERCATOR PROJECTION IS DONE HERE
	        lon_min = lon1  ; min(alon)
                lon_max = lon2  ; max(alon)
                lon_ctr = (lon_min + lon_max) / 2.0
                lat_min = lat1  ; min(alat)
                lat_max = lat2  ; max(alat)
                lat_ctr = (lat_min + lat_max) / 2.0

                opts_bg@mpProjection        = "Mercator"
                opts_bg@mpLimitMode         = "Corners"             ; Set extent of map
                opts_bg@mpLeftCornerLatF    = lat_min
                opts_bg@mpLeftCornerLonF    = lon_min
                opts_bg@mpRightCornerLatF   = lat_max
                opts_bg@mpRightCornerLonF   = lon_max

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Build the Background - If you want contours, See wrf2gm.ncl
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	print("*** Making Background")
        print("    Width = " +  img_width + " Height = " + img_height)
	if(FMT .eq. "png") then
		wks = create "BG" imageWorkstationClass defaultapp
			"wkFormat" : "newpng"
			"wkHeight" : img_height
			"wkWidth"  : img_width
	  	"wkBackgroundOpacityF" : 0.0
			"wkBackgroundColor" : "white"
			"wkForegroundColor" : "black"
			"wkColorMap"        : "BlAqGrYeOrReVi200"      ; probably best by DrJack
		end create
	else
		if(FMT .eq. "x11") then
			wks = create "BG" xWorkstationClass defaultapp
				"wkHeight" : img_height
				"wkWidth"  : img_width
				"wkBackgroundColor" : "white"
				"wkForegroundColor" : "black"
				"wkColorMap"        : "BlAqGrYeOrReVi200"      ; probably best by DrJack
			end create
		end if
		if(FMT .eq. "ncgm") then
			wks = create "BG" ncgmWorkstationClass defaultapp
				"wkHeight" : img_height
				"wkWidth"  : img_width
				"wkBackgroundColor" : "white"
				"wkForegroundColor" : "black"
				"wkColorMap"        : "BlAqGrYeOrReVi200"      ; probably best by DrJack
			end create
		end if
	end if

	;;; INCREASE WORKSPACE SIZE ABOVE DEFAULTS
	; See http://www.ncl.ucar.edu/Document/Graphics/Resources/ws.shtml
	setvalues NhlGetWorkspaceObjectId()
		"wsMaximumSize":	1073741824	; 1.0 GiB (0x40000000) WORKSTATIONSPACE
		"wsThresholdSize":	 536870912	; 0.5 GiB (0x20000000) WORKSTATIONTHRESHOLD
	end setvalues

	; plot lat and lon lines
	; opts_bg@mpGridAndLimbOn = True
	; opts_bg@mpGridSpacingF = 0.2
	; opts_bg@mpGridLineColor  = "black"

	bg = gsn_csm_map(wks, opts_bg)                 ; Makes a Blank Map, with Coastline

	; Make opts_ter and opts_ter both "true" in wrf2gm.ncl if you want terrain contours
	if(isatt(opts_ter, "cnLinesOn") .and. opts_ter@cnLinesOn .eq. True) then
		opts_ter@cnLineDrawOrder = "PostDraw"
		opts_ter@gsnDraw         = False
		opts_ter@gsnFrame        = False

		bg_ter                   = ter
		bg_ter@lat2d             = alat
		bg_ter@lon2d             = alon

		if(UNITS .ne. "metric") then
			bg_ter = 3.28084 * ter
			opts_ter@cnLevelSpacingF = 500.
			opts_ter@cnMinLevelValF  = opts_ter@cnLevelSpacingF
		else
			opts_ter@cnMinLevelValF  = 200.
			opts_ter@cnMinLevelValF  = opts_ter@cnLevelSpacingF
		end if

		plot_ter = gsn_csm_contour(wks, bg_ter, opts_ter)
		overlay(bg, plot_ter)
	end if

	;ret = addBorder(wks, bg, alat, alon)           ; ret not used, but keeps return value
	print("*** Done")

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; create plots
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	mag = new ( (/ny, nx/), float)
	mag@lat2d = alat
	mag@lon2d = alon
	mag@_FillValue = defaultFillValue

	u = new ( (/ny, nx/), float)
	v = new ( (/ny, nx/), float)

	; Parameters for all params - Used for datafile write
	atts = True
	atts@nx = nx
	atts@ny = ny
	atts@dx = dx
	atts@dy = dy
	atts@projlat1 = projlat1
	atts@projlat2 = projlat2
	atts@projlon = projlon
	atts@centerlat = centerlat
	atts@centerlon = centerlon
	atts@tail = tail
	atts@gridspacingkm = gridspacingkm

;; MAIN LOOP OVER PARAMETERS
   do p = 0, dimsizes(params) - 1
    startTime = stringtolong(systemfunc("date +%s%3N"))
		print("")
		print("*** Doing parameter: " + params(p) + " at " + systemfunc("date"))

		if(isvar("plot_param_chars")) then
			delete(plot_param_chars)
		end if
		plot_param_chars = stringtocharacter(params(p))

		; Ensure no leftovers
		if(isvar("bparam")) then
			delete(bparam)
		end if

		if(isatt(atts, "plot_param_chars")) then
			delete(atts@plot_param_chars)
		end if
		atts@plot_param_chars = plot_param_chars

		if(isvar("opts_bparam")) then
			delete(opts_bparam)
		end if
		opts_bparam = True
		opts_bparam@mult = 1.0	; Default value; overidden as required
    opts_bparam@MainTitle = "Unknown";
    opts_bparam@SecondTitle = "Initialized " + simulation_start + " UTC" ; , valid " + local_valid_time
    opts_bparam@ThirdTitle = "Canadian Meteorological Service HRDPS West Model Data (2.5km grid spacing) (ec2)"




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; CLOUD
  if( params(p) .eq. "cloud" ) then
      opts_bparam@MainTitle = "Total Cloud Cover"
      opts_bparam@ContourParameters = (/ 10., 90., 10. /)
      opts_bparam@mult = 1.
      opts_bparam@unitString = "%"
      cldfra_mag = (/cldfra/)	; skip the metadata
      ldatafmt = plot_contour(wks, bg, cldfra_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg,params(p)+"_"+date_val)
  end if

  ;; SFCSHF
  if( params(p) .eq. "sfcshf" ) then
      opts_bparam@unitString = "W/m~S~2~N~"
      opts_bparam@MainTitle = "Sfc. Heating"
      global_min = stringtofloat(getenv("SHTFL_MIN"))
      global_max = stringtofloat(getenv("SHTFL_MAX"))
      if(ismissing(global_min) .or. (ismissing(global_min))) then
         opts_bparam@ContourParameters = (/ -400., 600., 100. /)
      else
      if(global_min .eq. global_max) then	; Catch the "constant field" case
         opts_bparam@ContourParameters = (/0., 0., 0./)
      else
         print("    SFC HEAT: Using global min " + global_min + " and global max " + global_max)
         opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
         opts_bparam@ContourParameters(0) = opts_bparam@ContourParameters(0) + opts_bparam@ContourParameters(2)
         opts_bparam@ContourParameters(1) = opts_bparam@ContourParameters(1) - opts_bparam@ContourParameters(2)
      end if
  end if

sfcshf_mag = (/hfx/)	; skip the metadata
ldatafmt = plot_contour(wks, bg, sfcshf_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

end if

; HBL
		if( params(p) .eq. "bldepth" ) then
			opts_bparam@MainTitle = "Boundary layer height AGL"
			if( UNITS .ne. "metric" ) then
				qparaminfo = "Param= "+params(p)+" Unit= ft Mult= 1"
				opts_bparam@mult = 3.28084 ; ft
				opts_bparam@unitString = "ft"
			else
				qparaminfo = "Param= "+params(p)+" Unit= m Mult= 1"
				; No default in rasp.bparam_resources.ncl!!
				opts_bparam@unitString = "m"
			end if
         print("bledpeth_min = " + getenv("BLDEPTH_MIN"))
         global_min = stringtofloat(getenv("BLDEPTH_MIN"))
         global_max = stringtofloat(getenv("BLDEPTH_MAX"))
         print("BLDEPTH_MIN " + global_min + " BLDEPTH_MAX " + global_max)
            opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)

			ldatafmt = plot_contour(wks, bg, bldepth, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

		end if

; WSTAR
		if( params(p) .eq. "wstar" ) then
			opts_bparam@ContourParameters = (/ 0., 5., 0.5 /)
			opts_bparam@MainTitle = "Thermal Updraft Velocity ~I~(W*)~P~"


				opts_bparam@mult = 1.0
				qparaminfo = "Param= "+params(p)+" Unit= m/s"
				opts_bparam@unitString = "m/sec"

			ldatafmt = plot_contour(wks, bg, wstar, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg,  params(p)+"_"+date_val)

		end if
;; CLOUDBASE
		if( params(p) .eq. "cloudbase" ) then
			opts_bparam@ContourParameters = (/ 0., 5., 0.5 /)
			opts_bparam@MainTitle = "Condensation cloud base (if any)"



				opts_bparam@mult = 1.0
				qparaminfo = "Param= "+params(p)+" Unit= m"
				opts_bparam@unitString = "m"

         global_min = stringtofloat(getenv("CLOUDBASE_MIN"))
         global_max = stringtofloat(getenv("CLOUDBASE_MAX"))
            opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
			ldatafmt = plot_contour(wks, bg, cloudbase, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg,  params(p)+"_"+date_val)

		end if



; HWCRIT
    if( params(p) .eq. "hwcritagl" ) then
	opts_bparam@MainTitle = "Height of Critical Updraft Strength AGL ~I~(Hcrit)~P~"
	qparaminfo = "Param= "+params(p)+" Unit= m Mult= 1"
	; No default in rasp.bparam_resources.ncl!!
	opts_bparam@unitString = "m"

        global_max = stringtofloat(getenv("HCRIT_SFC_MAX"))
        global_min = stringtofloat(getenv("HCRIT_SFC_MIN"))
        if(ismissing(global_min) .or. (ismissing(global_min))) then
          opts_bparam@ContourParameters = (/ 0., 250., 3000. /)
        else
          if(global_min .eq. global_max) then	; Catch the "constant field" case
            opts_bparam@ContourParameters = (/0., 0., 0./)
          else
            print("   HCRIT_AGL: Using global min " + global_min + " and global max " + global_max)
            opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
            opts_bparam@ContourParameters(0) = opts_bparam@ContourParameters(0) + opts_bparam@ContourParameters(2)
            opts_bparam@ContourParameters(1) = opts_bparam@ContourParameters(1) - opts_bparam@ContourParameters(2)
          end if
        end if
        ldatafmt = plot_contour(wks, bg, hwcrit_agl, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)
   end if

   ;; VWINDn for HRDPS n is (0..4)
   if(strlen(params(p)) .ge. 5 .and. plot_param_chars(0:4) .eq. "vwind") then
     if(strlen(params(p)) .eq. 5) then
       level_num := 0
     else
       level_num := stringtoint(charactertostring(plot_param_chars(5:)))
     end if
     opts_bparam@MainTitle = "Vertical Component of Wind at " +  wa_zlev(level_num) + "m"
     qparaminfo = "Param= "+params(p)+" Unit= m Mult= 1"
     opts_bparam@unitString = "m/s"
     global_max = stringtofloat(getenv("VVEL_MIN")) * wa_conv(level_num)
     global_min = stringtofloat(getenv("VVEL_MAX")) * wa_conv(level_num)
     if(ismissing(global_min) .or. (ismissing(global_min))) then
       opts_bparam@ContourParameters = (/ -2., 2., 0.2 /)
     else
       if(global_min .eq. global_max) then	; Catch the "constant field" case
         opts_bparam@ContourParameters = (/0., 0., 0./)
       else
         print("   VWind: Using global min " + global_min + " and global max " + global_max)
         opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
         opts_bparam@ContourParameters(0) = opts_bparam@ContourParameters(0) + opts_bparam@ContourParameters(2)
         opts_bparam@ContourParameters(1) = opts_bparam@ContourParameters(1) - opts_bparam@ContourParameters(2)
       end if
     end if
     wa_mag = (/wa(level_num,:,:)/)
     ldatafmt = plot_contour(wks, bg, wa_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)
   end if


; SFCWINDn for HRDPS n is (0..3)
		if(strlen(params(p)) .ge. 7 .and. plot_param_chars(0:6) .eq. "sfcwind") then
		  if(strlen(params(p)) .eq. 7) then
		    level_num := 0
		  else
			  level_num := stringtoint(charactertostring(plot_param_chars(7:)))
			end if
			sfc_levels = (/10., 40., 80., 120./)

			;opts_bparam@ContourParameters = (/0., 0., 2. /)

			opts_bparam@MainTitle = "Wind at " +  sfc_levels(level_num) + "m AGL"
			if(UNITS .ne. "metric") then
				opts_bparam@mult = 1.9438445
				opts_bparam@unitString = "kt"
			else
				opts_bparam@unitString = "km/h"
				opts_bparam@mult = 1.0
			end if
			u := uEWsurf(level_num,:,:)
			v := vNSsurf(level_num,:,:)
			spd:= sqrt(u*u+v*v)
			copy_VarAtts(mag, spd)
			copy_VarAtts(mag, u)
			copy_VarAtts(mag, v)
			ldatafmt = plot_streamline( wks, bg, u, v, spd, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)
		end if


; WINDn
		if(dimsizes(plot_param_chars) .gt. 3 .and. plot_param_chars(0:3) .eq. "wind") then
			alt = stringtofloat(charactertostring(plot_param_chars(4:)))
			; find the closest hrdps level
			min_diff = 10000.0
			do i=0,nz-1
				if (fabs(znom(i)-alt) .gt. min_diff) then
					break
				end if
				min_diff = fabs(znom(i)-alt)
			end do
			level_num = i-1
			print("   Plotting wind level at " +  alt + "m. Closest HRDPS level is " + level_num + " corresponding to " + znom(level_num) + "m")

			opts_bparam@MainTitle = "Wind at " + znom(level_num) + "m ASL"

			;opts_bparam@ContourParameters = (/0., 0., 2. /)
			if(UNITS .ne. "metric") then
				opts_bparam@mult = 1.9438445
				opts_bparam@unitString = "kt"
			else
				opts_bparam@unitString = "km/h"
				opts_bparam@mult = 1.0
			end if

      u := uEW(level_num,:,:)
      v := vNS(level_num,:,:)
			spd:= sqrt(u*u+v*v)


			copy_VarAtts(mag, spd)
			copy_VarAtts(mag, u)
			copy_VarAtts(mag, v)
			;print( sprintf("%f", spd(0,0:10)) + " " + sprintf("%f", spd(0,0:10)/1000.0))

			ldatafmt = plot_streamline( wks, bg, u, v, spd, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

		end if



; MSLPRESS
		if( params(p) .eq. "mslpress" ) then
			opts_bparam@MainTitle = "Mean Sea Level Pressure"
			opts_bparam@ContourParameters = (/ 0., 0., 2.0 /)
			opts_bparam@unitString = "mb"
			slvl_mag = (/slvl/)	; skip the metadata

			ldatafmt = plot_contour(wks, bg, slvl_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)
		end if

; CAPE
		if( params(p) .eq. "cape" ) then
			opts_bparam@MainTitle = "Convective Available Potential Energy (CAPE)"
			qparaminfo = "Param= "+params(p)+" Unit= J/kg Mult= 1"
			opts_bparam@unitString = "J/kg"

      global_min = stringtofloat(getenv("CAPE_MIN"))
      global_max = stringtofloat(getenv("CAPE_MAX"))
      if(ismissing(global_min) .or. (ismissing(global_min))) then
			  opts_bparam@ContourParameters = (/ 100., 2000., 200. /)
      else
    		if(global_min .eq. global_max) then	; Catch the "constant field" case
    			opts_bparam@ContourParameters = (/0., 0., 0./)
    		else
			    print("    CAPE: Using global min " + global_min + " and global max " + global_max)
    			opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
    			opts_bparam@ContourParameters(0) = opts_bparam@ContourParameters(0) + opts_bparam@ContourParameters(2)
    			opts_bparam@ContourParameters(1) = opts_bparam@ContourParameters(1) - opts_bparam@ContourParameters(2)
    		end if
      end if

			cape_mag = (/capeVar/)
			ldatafmt = plot_contour(wks, bg, cape_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

		end if

; RAIN
		if( params(p) .eq. "rain" ) then
			opts_bparam@MainTitle = "Precipitation Per Hour"
			qparaminfo = "Param= "+params(p)+" Unit= mm/hr"
			opts_bparam@unitString = "mm/hr"
			opts_bparam@mult = 1.0

      global_min = stringtofloat(getenv("PRATE_MIN")) * 3600.
      global_max = stringtofloat(getenv("PRATE_MAX")) * 3600.
      if(ismissing(global_min) .or. (ismissing(global_min))) then
			  opts_bparam@ContourParameters = (/ 0., 10., 0.5 /)
      else
    		if(global_min .eq. global_max) then	; Catch the "constant field" case
    			opts_bparam@ContourParameters = (/0., 0., 0./)
    		else
			    print("    PRATE: Using global min " + global_min + " and global max " + global_max)
    			opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
    			opts_bparam@ContourParameters(0) = opts_bparam@ContourParameters(0) + opts_bparam@ContourParameters(2)
    			opts_bparam@ContourParameters(1) = opts_bparam@ContourParameters(1) - opts_bparam@ContourParameters(2)
    		end if
      end if

			rain_mag = (/rainVar/)
			ldatafmt = plot_contour(wks, bg, rain_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

		end if

; SFCTEMP
		if( params(p) .eq. "sfctemp" ) then
			opts_bparam@MainTitle = "Surface Temperature ~I~(2m AGL)~P~"
			if( UNITS .eq. "american" ) then
				qparaminfo = "Param= "+params(p)+" Unit= F Mult= 1"
				opts_bparam@unitString = "F"
			else
				qparaminfo = "Param= "+params(p)+" Unit= C Mult= 1"
				opts_bparam@unitString = "C"
			end if

      global_min = stringtofloat(getenv("TMP_MIN")) - 273.15
      global_max = stringtofloat(getenv("TMP_MAX")) - 273.15
      if(ismissing(global_min) .or. (ismissing(global_min))) then
			  opts_bparam@ContourParameters = (/ -10., 30., 4. /)
      else
    		if(global_min .eq. global_max) then	; Catch the "constant field" case
    			opts_bparam@ContourParameters = (/0., 0., 0./)
    		else
			    print("   TEMP:  Using global min " + global_min + " and global max " + global_max)
    			opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
    			opts_bparam@ContourParameters(0) = opts_bparam@ContourParameters(0) + opts_bparam@ContourParameters(2)
    			opts_bparam@ContourParameters(1) = opts_bparam@ContourParameters(1) - opts_bparam@ContourParameters(2)
    		end if
      end if

			sfctemp = tc2 - 273.15
			ldatafmt = plot_contour(wks, bg, sfctemp, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

		end if

; SFCDEWPT
		if( params(p) .eq. "sfcdewpt" ) then
			opts_bparam@MainTitle = "Surface Dew Point Temperature ~I~(2m AGL)~P~"
			if( UNITS .eq. "american" ) then
				qparaminfo = "Param= "+params(p)+" Unit= F Mult= 1"
				opts_bparam@unitString = "F"
			else
				qparaminfo = "Param= "+params(p)+" Unit= C Mult= 1"
				opts_bparam@unitString = "C"
			end if

      global_min = stringtofloat(getenv("TMP_MIN")) - stringtofloat(getenv("DPT_MAX")) - 273.15
      global_max = stringtofloat(getenv("TMP_MAX")) - stringtofloat(getenv("DPT_MIN")) - 273.15
      if(ismissing(global_min) .or. (ismissing(global_min))) then
			  opts_bparam@ContourParameters = (/ -10., 30., 4. /)
      else
    		if(global_min .eq. global_max) then	; Catch the "constant field" case
    			opts_bparam@ContourParameters = (/0., 0., 0./)
    		else
			    print("    DPT: Using global min " + global_min + " and global max " + global_max)
    			opts_bparam@ContourParameters = nice_mnmxintvl(global_min, global_max, 13, False)
    			opts_bparam@ContourParameters(0) = opts_bparam@ContourParameters(0) + opts_bparam@ContourParameters(2)
    			opts_bparam@ContourParameters(1) = opts_bparam@ContourParameters(1) - opts_bparam@ContourParameters(2)
    		end if
      end if

			sfcdewpt = td2 - 273.15
			ldatafmt = plot_contour(wks, bg, sfcdewpt, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

		end if

; DBL
		if( params(p) .eq. "dbl" ) then
			opts_bparam@MainTitle = "BL Depth"
			if( UNITS .ne. "metric" ) then
				opts_bparam@mult = 3.28084 ; ft
				opts_bparam@unitString = "ft"
				qparaminfo = "Param= "+params(p)+" Unit= ft Mult= 1"
			else
				opts_bparam@unitString = "m"
				qparaminfo = "Param= "+params(p)+" Unit= m Mult= 1"
			end if

			dbl_mag = (/pblh/)
			ldatafmt = plot_contour(wks, bg, dbl_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)

		end if

; SFCSUN
;		if( params(p) .eq. "sfcsun" ) then
;			opts_bparam@unitString = "W/m2"
;			opts_bparam@MainTitle = "Sfc. Solar Radiation"
;			opts_bparam@ContourParameters = (/ -400., 600., 200. /)
;			qparaminfo = "Param= "+params(p)+" Unit= W/m2 Mult= 1"
;
;			sfcsun_mag = (/swnet/)
;			ldatafmt = plot_contour(wks, bg, sfcsun_mag, params(p), tail, alat, alon, gridspacingkm, opts_cn, opts_bparam, opts_bg, params(p)+"_"+date_val)
;
;		end if
;
    endTime = stringtolong(systemfunc("date +%s%3N"))
    delta = (endTime - startTime)/1000.0
    startTime = endTime
	  print("    Parameter " + params(p) + " took " + delta + "s")
    print(" ")

    end do
end

