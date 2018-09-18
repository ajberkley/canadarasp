; RAIN
function rain(filename, f, plot_param_chars, nx, ny)
begin
	;; Extract the accumulation time from the parameter name
	acctime = stringtointeger( chartostring( plot_param_chars(4:) ) )

	;; Get filename for the previous forecast. Needed to subtract earlier rain.
	filename_chars = stringtochar( filename )
	len_filename = dimsizes(filename_chars)-2

	; date of current file
	; sdate = string_target_replace(chartostring( filename_chars(len_filename-18:len_filename-6)), "_", "-" )
	sdate = str_sub_str(chartostring( filename_chars(len_filename-18:len_filename-6)), "_", "-" )

	; Allow for ".nc" on the end
	stime = filename_chars(len_filename-7:len_filename-3)

	; date  = stringtointeger(split_string_into_array( sdate,"-" ))
	date  = stringtointeger(str_split( sdate,"-" ))
	; date of previous file
	prevdate  = jul2greg( greg2jul(date(0),date(1),date(2),date(3)) - acctime/24. + 0.00001 )
	sprevdate = sprinti("%d-",prevdate(0)) + sprinti("%02d-",prevdate(1)) + sprinti("%02d_",prevdate(2)) + sprinti("%02d:", prevdate(3)) + stime 
	prevfilename = chartostring( filename_chars(0:len_filename-19)) + sprevdate

	rainc  = f->RAINC
	rainnc = f->RAINNC
	rainc  = rainc > 0.0
	rainnc = rainnc > 0.0

	rainTot = new ( (/ny, nx/), float)

	;; If the previous file exists -> load it, subtract earlier rain, plot it.
	if (isfilepresent(prevfilename)) then
		prevfile = addfile( prevfilename+".nc", "r" )

		prevrainc  = prevfile->RAINC
		prevrainnc = prevfile->RAINNC

		; Use NCL operator > to make sure all values >=0.0
		prevrainc  = prevrainc > 0.0
		prevrainnc = prevrainnc > 0.0

		rainTot = rainc + rainnc - prevrainc - prevrainnc
	else
		rainTot = rainc + rainnc
		print("    prevfilename = " + prevfilename )
		print("    WARNING: No previous file: showing total accumulated rain to " + date(3) + ":" + stime)
	end if
	return(rainTot)
end

; SFCSUN
function sfcsun(swdown)
begin
	bparam = swdown
	bparam@_FillValue = -999
	return(swdown)
end

; STARS
; Note that wind is sfcwind (10m) if HGflag==True else blavgwind
function stars(hglider_mag, ter, cu_pot, wind, bsratio_mag, HGflag) 
local hglider, stars
begin
	; WARNING: params here are in feet & Knots
	; hglider, ter, blavgwind & cu_pot need conversion

	hglider = 3.28084 * hglider_mag ; Convert to feet
	cupot  = 3.28084 * cu_pot      ; Convert to feet
	wind_local = 1.94386 * wind	; Convert to Kts

	; Start with hglider
	stars = hglider
	stars = where(hglider .gt. 4000.0, 5000, stars)
	stars = where(hglider .le. 4000.0, 4000, stars)
	stars = where(hglider .le. 3500.0, 3000, stars)
	stars = where(hglider .le. 3000.0, 2000, stars)
	stars = where(hglider .le. 2500.0, 1000, stars)
	stars = where(hglider .lt. 1500.0, 0000, stars)
	; print("stars(hglider): Min = " + min(stars) + " Max = " + max(stars) )

	; If it's the same as the Terrain, rating must be 0
	stars = where(stars .eq. ter * 3.28084, 0, stars)
	; print("stars(ter): Min = " + min(stars) + " Max = " + max(stars) )

	; Apply modification for "small puffy clouds"
	; Criterion is small(ish) clouds if 500 < cu_pot < 5000
	; If cu_pot < 0 => blue
	; print("stars - cupot: Min = " + min(cupot) + " Max = " + max(cupot) )
	stars = where(cupot .gt. 500.0 .and. cupot.lt.5000.0, stars + 500.0, stars)
	stars = where(cupot .lt. 0.0                        , stars - 500.0, stars)
	; print("stars(cu_pot): Min = " + min(stars) + " Max = " + max(stars) )

	; Wind Reduction and bsratio adjustment

	MAXWIND = 15.0	; Winds (Kt) less than this do not reduce rating

	; NB Star ratings are 0 - 5000
	if(HGflag) then
		sreduction = 1000.0 * (0.043 * (wind ^ 2.0) - 0.71 * wind + 3)
	else
		sreduction = where( wind .lt. MAXWIND, MAXWIND, wind)
		sreduction = 250 * ((sreduction - MAXWIND)^2.0) / sreduction
	end if
	; print ( " stars No Wind "+min(stars)+","+max(stars) ) 
	stars = stars - sreduction
	stars = stars > 0
	; print("stars(wind): Min = " + min(stars) + " Max = " + max(stars) )

	;;;;; bsratio Adjustment ;;;;;;
	; stars += (bsratio - 3) * 300
	; 3 is No Adjustment; +/-300 for each integer away from this
	; Max bsratio is 6
	; Min adjustment -900 @ bsratio = 0
	; Max adjustment +900 @ bsratio = 6
	; Note bsratio cannot be < 0!

	MAXBSRATIO = 6.0  ; Very seldom better than this in the UK :-)
	bsadjust = where(bsratio_mag .gt. MAXBSRATIO, MAXBSRATIO, bsratio_mag)
	stars = stars + ((bsadjust - 3.0) * 300)
	stars = stars > 0
	; print("stars(bsratio): Min = " + min(stars) + " Max = " + max(stars) )

	;;; End bsratio adjustment				

	; Spatially filter the Rating
	; Here you can choose your filter :-)
	; Only FIR filters supported
	; N_coeffs should be odd, to avoid shift

	; The coeffs should probably be selected by gridspacingkm,
	; To give similar results for each resolution (??)

	; wgt = (/ 1., 1, 1, 1, 1/) ; Simple (1/n, 1/n, ... ) filter

	; wgt = (/                                  1.,       1.                                      /)	; Don't use N_coeffs is even
	; wgt = (/                              1.,      2.,       1.                                 /)
	; wgt = (/                          1.,     3.,       3.,      1.                             /)	; Don't use N_coeffs is even
	; wgt = (/                      1.,     4.,       6.,      4.,      1.                        /)
	; wgt = (/                  1.,     5.,      10.,     10.,      5.,      1.                   /)	; Don't use N_coeffs is even
	  wgt = (/              1.,     6.,     15.,      20.,     15.,      6.,     1.               /)
	; wgt = (/          1.,    7.,     21.,      35.,      35.,      21.,    7.,      1.          /)	; Don't use N_coeffs is even
	; wgt = (/       1.,    8.,    28.,     56.,      70.,      56.,     28.,     8.,     1.      /)
	; wgt = (/   1.,    9.,    36.,     84.,     126.,     126.,     84.,     36.,    9.,    1.   /)	; Don't use N_coeffs is even
	; wgt = (/1.,   10.,   45.,    120.,    210.,     252.,     210.,    120.,    45.,   10.,   1./)

	; Ensure coeffs sum to 1.0 to avoid scale change
	wgt = wgt / sum(wgt)

	; Filter Horizontally
	; stars = wgt_runave(stars, wgt, -1)	; Cyclic conditions
	stars = wgt_runave(stars, wgt, 0)       ; Unsmoothed beginning and end pts to _FillValue
	; stars = wgt_runave(stars, wgt, 1)	; Reflective (symmetric) conditions
	; print ( " stars Filtered " + min(stars) + "," + max(stars) ) 

	; Transpose array
	stars!0 = "x"
	stars!1 = "y"
	stars_T = stars(y|:, x|:)

	; Filter transposed array (i.e. vertically)
	stars_T = wgt_runave(stars_T, wgt, 0)

	; Transpose back again
	stars_T!0 = "x_T"
	stars_T!1 = "y_T"
	stars_U = stars_T(y_T|:, x_T|:)

	; Normalize to 0 - 5
	stars_U = stars_U / 1000.0

	; print ( " stars_U Filtered " + min(stars_U) + "," + max(stars_U) ) 

	stars_U@_FillValue = 0.0001

	return(stars_U)
end

; SFCWIND0 (2m wind)
procedure sfcwind0(ua, va, uplot, vplot, mag)
begin
	uplot = ua(0,:,:)
	vplot = va(0,:,:)
	; mag = (uplot ^ 2.0 + vplot ^ 2.0) ^ 0.5
	mag = sqrt(uplot * uplot + vplot * vplot)
end

; SFCWIND (10m wind)
procedure sfcwind(ua, va, uplot, vplot, mag)
begin
	uplot = ua(1,:,:)
	vplot = va(1,:,:)
	; mag = (uplot ^ 2.0 + vplot ^ 2.0) ^ 0.5
	mag = sqrt(uplot * uplot + vplot * vplot)
end

; SFCWIND2 (30m wind)
procedure sfcwind2(ua, va, uplot, vplot, mag)
begin
	uplot = ua(2,:,:)
	vplot = va(2,:,:)
	; mag = (uplot ^ 2.0 + vplot ^ 2.0) ^ 0.5
	mag = sqrt(uplot * uplot + vplot * vplot)
end

; WRF=<param>
; Plot an arbitrary 2d/3d parameter name
;  2D paramname = "wrf=FILEparameterNAME" where FILEparamNAME is name used for NetCDF WRF output file
;  3D paramname = "wrf=FILEparameterNAME=level#" where level#=0 for lowest atm (highest soil) level
;     (note that 3D level is at constant model level, NOT constant MSL, AGL, or pressure)
function wrf(a, plot_param)
begin
	; param_array = split_string_into_array( plot_param, "=" )
	param_array = str_split( plot_param, "=" )
	dims = dimsizes(param_array)
        data = wrf_user_getvar(a, param_array(1), time)
        if( dims .eq. 2 ) then        ; 2D array
                bparam = data
        else ;;; **** NOT TESTED **** ;;; 3D array
                ;;; *NB* WARNING TOP-MOST LEVEL IN SOME ARRAYS HAS MISSING VALUES THERE ! eg qcloud
                ;;; 3D SIZES CHECKED SAME FOR Z,T,uz,wa,qcloud,
                datadims = dimsizes(data)
                kkdata = datadims(0)
                jjsize = datadims(1)
                iisize = datadims(2)
                if ( ismissing(iisize) ) then
                        print( "*** REQUESTED ARRAY "+plot_param+" IS NOT 3D - DIMS="+datadims )
                        exit
                else
                        level=  stringtointeger( param_array(2) )
                        bparam = data(level,:,:)
                end if
        end if
        delete ( data )

	return(bparam)
end

; ZBLCLDIF
function zblcldif(ter, pblh, zblcl_mag)
begin
	return(pblh + ter - zblcl_mag)
end

; ZSFCLCLDIF
function zsfclcldif(ter, pblh, zsfclcl_mag)
begin

	tmp = (pblh + ter - zsfclcl_mag)
	return(tmp)
end
