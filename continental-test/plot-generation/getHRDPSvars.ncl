; PART OF plot_parameter() - See plot_param.ncl
; Put in separate file for readability
;
; http://www.mmm.ucar.edu/wrf/users/docs/user_guide_V3/users_guide_chap5.htm#fields
; Shows what is available
; "ncdump - h <wrfout_file> shows what's actually there

;WGRIB2 is used to mask data below terrain elevation. That data is set to the following value
defaultFillValue = 999900026055400882176.
alat     = f->gridlat_0     ; Lat & lon for maps
alon     = f->gridlon_0
cldfra   = f->TCDC_P0_L1_GST0   ; cloud cover in %
hfx      =  f->SHTFL_P0_L1_GST0      ; for sfc. sensible heat flux in W/m2
;swnet    = f->NSWRS_P8_L1_GST0_acc/-3600.   ; Net short-wave radiation flux (surface) accumulated.  J/m2 -> W/m2 averaged over an hour
;pblh     = wrf_user_getvar(f, "PBLH", time)     ; bl depth (metres)
;qcloud   = wrf_user_getvar(f, "QCLOUD", time)   ; NB: sometimes also "rh"!!
;rh       = f->RH_P0_L100_GST0       ; relative humidity in percent - never used
;qvapor   = f->SPFH_P0_L100_GST0 ; water vapor mixing ratio in kg/kg (aka specific humidity)

slvl     = f->PRMSL_P0_L101_GST0       ; Sea Level Pressure
ter      =  f->HGT_P0_L1_GST0     ; terrain height
capeVar     = f->CAPE_P0_L111_GST0   ;
rainVar     =  f->PRATE_P0_L1_GST0 * 3600.  ; precipitation rate mm/hour


tc2      = f->TMP_P0_L103_GST0(0,:,:)      ; 2m temp in K
td2      = f->DEPR_P0_L103_GST0(0,:,:)      ; 2m dew point temp reduction in K
td2 = tc2 - td2

hwcrit_agl = f->HCRITAGL_P0_L1_GST0;
hwcrit_agl@units = "m"
;hwcrit_agl@long_name = "hwcrit"
wstar = f->WSTAR_P0_L1_GST0;
wstar@units = "m/s"
bldepth = f->BLDEPT_P0_L1_GST0;
bldepth@units = "m"
;cloudbase = f->CLOUDBASE_P0_L1_GST0;
;cloudbase@units = "m"


;printVarSummary(hwcrit_agl)
;print(hwcrit_agl)
;tc       = f->TMP_P0_L100_GST0       ; temperature (C)
;tc@_FillValue = defaultFillValue
;printVarSummary(tc)
;x       = f->DEPR_P0_L100_GST0       ; dew point depression temperature (C)
;x@_FillValue = defaultFillValue
;printVarSummary(x)
;td       = tc - x       ; dew point temperature (C)
;delete(x)


z = f->HGT_P0_L100_GST0(::-1,:,:) ;level altitudes
z@_FillValue = defaultFillValue
znom = (/100, 230, 350, 480, 660, 880, 1100, 1300, 1600, 2100, 2600, 3100, 3700, 4300, 5000, 5700, 6500, 7400, 8300, 9400, 10000, 10600, 11300, 12000, 12800, 13800, 16300, 20600/)
znom@units="m"
;printVarSummary(z)
;pmb      = f->$z!0$(::-1)  ; press is vertical coordinate in Pa
pmb      = z&$z!0$  ; press is vertical coordinate in Pa
;print(pmb)
;wa       = wrf_user_getvar(f, "wa", time)       ; vertical velocity, averaged to mass levels in m/s
wa       = f->VVEL_P0_L100_GST0(::-1,:,:)       ; vertical velocity for 5 levels in pa/s

;Convert Pa/s to m/s
wa_plev=wa&$wa!0$
;print(wa_plev)
;print(wa(:,0,0))

wa_nlev = dimsizes(wa_plev)
pmb_nlev = dimsizes(pmb)

wa_zlev = new (wa_nlev, float)
wa_conv = new (wa_nlev, float)
do i=0,wa_nlev-1
  do j=0,pmb_nlev-2
  	if  pmb(j) .eq. wa_plev(i) then
  		break
  	end if
  end do
  wa_zlev(i) = znom(j)
  wa_conv(i) = (avg(z(j,:,:))-avg(z(j+1,:,:)))/(pmb(j)-pmb(j+1))
  wa(i,:,:) = wa(i,:,:) * wa_conv(i)
end do
;print(wa_conv)


;print(wa_zlev)
;print(conv(0,0))
;print(wa(:,0,0))
;printVarSummary(wa)

windUr = f->UGRD_P0_L100_GST0(::-1,:,:)
windVr = f->VGRD_P0_L100_GST0(::-1,:,:)
rot = f->gridrot_0
windUr@_FillValue = defaultFillValue
windVr@_FillValue = defaultFillValue
rot@_FillValue = defaultFillValue
dims = dimsizes(windUr)
uEW = windUr
vNS = windVr
;print(sprintf("%.20f", windUr(0,0,0:10)))
do level = 0, dims(0)-1
 uEW(level,:,:) = (sin(rot)*windVr(level,:,:) + cos(rot)*windUr(level,:,:))*3.6
 vNS(level,:,:) = (cos(rot)*windVr(level,:,:) - sin(rot)*windUr(level,:,:))*3.6
end do
;print(sprintf("%.20f", uEW(0,0,0:10)))
;printVarSummary(uEW)
;print(sprintf("%.20f", uEW(0,0,0:10)/1000.0))
delete(windUr)
delete(windVr)

; surface winds at 10, 40, 80 and 120m AGL
;u1 = f->UGRD_P0_L103_GST0
;v1 = f->VGRD_P0_L103_GST0
;u2 = f->UGRD_P0_L111_GST0
;v2 = f->VGRD_P0_L111_GST0
;windUr = (/u1(0,:,:), u2(1,:,:), u1(1,:,:), u2(0,:,:)/)
;windVr = (/v1(0,:,:), v2(1,:,:), v1(1,:,:), v2(0,:,:)/)
;delete(u1)
;delete(v1)
;delete(u2)
;delete(v2)
;dims = dimsizes(windUr)
windUr = f->UGRD_P0_L103_GST0
windVr = f->VGRD_P0_L103_GST0
uEWsurf = windUr
vNSsurf = windVr
;print(sprintf("%.20f", windUr(0,0,0:10)))
dims = dimsizes(windUr)
do level = 0, dims(0)-1
 uEWsurf(level,:,:) = (sin(rot)*windVr(level,:,:) + cos(rot)*windUr(level,:,:))*3.6
 vNSsurf(level,:,:) = (cos(rot)*windVr(level,:,:) - sin(rot)*windUr(level,:,:))*3.6
end do
;print(sprintf("%.20f", uEWsurf(0,0,0:10)))
;print(sprintf("%.20f", uEWsurf(0,0,0:10)/1000.0))
delete(windUr)
delete(windVr)



paramdims= dimsizes(z)                                ; Get Dimensions
nz       = paramdims(0)
ny       = paramdims(1)
nx       = paramdims(2)
delete(paramdims)


dx        = alon@Dx
dy        = alat@Dy
gridspacing = sqrt(0.5 * (dx^2 + dy^2))
if( gridspacing .lt. 10000. ) then
	gridspacingkm = sprintf ( "%.1f", (0.001*gridspacing) )
else
	gridspacingkm = sprintf ( "%.0f", (0.001*gridspacing) )
end if


projlat1  = 44.0
projlat2  = 56.0
projlon   = -110
centerlat = alat(ny/2,nx/2)
centerlon = alon(ny/2,nx/2)
