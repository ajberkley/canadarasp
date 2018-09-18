load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;--------------------------------------------------------------------------------
; function hrdps_contour(nc_file:file,wks[1]: graphic, data[*][*]:numeric, \
;                      opt_args[1]:logical)
; function hrdps_vector(nc_file:file,wks[1]: graphic, data_u[*][*]:numeric, \
;                     data_v[*][*]:numeric, opt_args[1]:logical)
; function hrdps_wps_map(wks[1]:graphic,opt_args[1]:logical)
; function hrdps_map_overlays(wks:graphic,base[1]:graphic, plots[*]:graphic,\
;           opt_arg[1]:logical,mp_arg[1]:logical)
; function hrdps_overlays(wks:graphic, plots[*]:graphic, opt_arg[1]:logical)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("hrdpsFindIndex")
function hrdpsFindIndex(targetLat:float, targetLon:float, lats:float, lons:float)
local x, y, nlat, nlon, dims, lastX, lastY, i
begin
   dims=dimsizes(lats)
   nlat=dims(0)
   nlon=dims(1)

  x=0
  y=0
  i=0
  do while(True)
    lastX = x
    lastY = y
    y:= closest_val(targetLat,lats(:,x))
    if (y .ge. nlat) then
      y:= nlat-1;
    end if
    x:= closest_val(targetLon,lons(y,:))
    if (x .ge. nlon) then
      x := nlon-1;
    end if
    print("Xindex = " + x + ", Yindex= " + y)
    if ((abs(lastX-x).le.1) .and. (abs(lastY-y).le.1)) then
      break
    end if
    if (i.gt.100) then
      break
    end if
    i=i+1
  end do
  return ((/y,x/))
end



;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

undef("delete_attrs")
procedure delete_attrs(opts:logical)

; This procedure does some cleanup by removing unneeded attributes
; so they don't get passed to other routines by accident.

begin
  list_attrs = (/"MainTitle","MainTitlePos","MainTitlePosF", \
                 "InitTime","ValidTime","TimePos","TimePosF", \
                 "NoHeaderFooter","TimeLabel","LevelLabel", \
                 "FieldTitle","UnitLabel","NumVectors","AspectRatio", \
                 "SubFieldTitle","PlotOrientation","PlotLevelID", \
                 "mpNestTime","ContourParameters","FontHeightF","Footer", \
                 "start_lat","start_lon","end_lat","end_lon", \
                 "proj","map_proj","stand_lon","truelat1","truelat2","cenlat", \
                 "pole_lat","pole_lon","ref_lat","ref_lon","ref_x","ref_y", \
                 "e_we","e_sn","parent_id","parent_grid_ratio", \
                 "i_parent_start","j_parent_start", \
                 "dx","dy","max_dom","ContourLabel","ContourUnits" \
                 /)

  do i=0,dimsizes(list_attrs)-1
    if(isatt(opts,list_attrs(i))) then
      delete(opts@$list_attrs(i)$)
    end if
  end do
end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_cn_resources")
function set_cn_resources (data[*][*]:numeric, res:logical)

begin

  opts = res

; The ContourParameters resource can either be a scalar that
; represents the contour level spacing, or it can be an array
; of three elements that represent the minimum level, the maximum
; level, and the level spacing.
;
  mx = max(data)
  mn = min(data)

  if(mn.ne.mx.and.opts.and.isatt(opts,"ContourParameters")) then
    if(dimsizes(opts@ContourParameters) .eq. 1) then

; Only the contour interval is specified.
      nlev   = tointeger((mx-mn)/opts@ContourParameters)+1
      levels = nice_mnmxintvl(mn,mx,nlev,True)
      if(levels(0) .lt. 0.) then
        ; Set a zero contour.
        nlev = tointeger(levels(0)/opts@ContourParameters) - 1
        levels(0) = nlev*opts@ContourParameters
      end if
      nlev = tointeger((levels(1)-levels(0))/opts@ContourParameters)+1
      levels(1) = levels(0) + nlev*opts@ContourParameters
      levels(2) = opts@ContourParameters

; Min level, max level, and level spacing are specified by user.
    else
      if(dimsizes(opts@ContourParameters) .eq. 3) then
        levels = opts@ContourParameters
      else
        print("hrdps_contour: Warning: illegal setting for ContourParameters attribute")
      end if
    end if

  end if

; Contour levels
  if(isvar("levels")) then
    opts@cnLevelSelectionMode = get_res_value_keep(opts, "cnLevelSelectionMode", "ManualLevels")
    opts@cnMinLevelValF       = get_res_value_keep(opts, "cnMinLevelValF", levels(0))
    opts@cnMaxLevelValF       = get_res_value_keep(opts, "cnMaxLevelValF", levels(1))
    opts@cnLevelSpacingF      = get_res_value_keep(opts, "cnLevelSpacingF",levels(2))
    delete(levels)
  end if


; Set the default zero line thickness to 2, and the negative contour
; line dash pattern to 1 (0 is solid).
  opts@gsnContourZeroLineThicknessF = get_res_value_keep(opts, "gsnContourZeroLineThicknessF",2.0)
  opts@gsnContourNegLineDashPattern = get_res_value_keep(opts, "gsnContourNegLineDashPattern",1)


; Set resources that apply for both filled and line contour plots.
  opts@cnFillDrawOrder            = get_res_value_keep(opts,"cnFillDrawOrder", "PreDraw")


  opts@cnLineLabelAngleF          = get_res_value_keep(opts,"cnLineLabelAngleF", 0.0)
  opts@cnLineLabelFontHeightF     = get_res_value_keep(opts,"cnLineLabelFontHeightF", 0.012)
  opts@cnInfoLabelFontHeightF     = get_res_value_keep(opts,"cnInfoLabelFontHeightF", 0.012)
  opts@cnLineLabelPerimOn         = get_res_value_keep(opts,"cnLineLabelPerimOn", True)
  opts@cnInfoLabelPerimOn         = get_res_value_keep(opts,"cnInfoLabelPerimOn", False)
  opts@cnLineLabelBackgroundColor = get_res_value_keep(opts,"cnLineLabelBackgroundColor", -1)
  opts@cnHighLabelBackgroundColor = get_res_value_keep(opts,"cnHighLabelBackgroundColor", -1)
  opts@cnLowLabelBackgroundColor  = get_res_value_keep(opts,"cnLowLabelBackgroundColor", -1)
  opts@cnLineColor                = get_res_value_keep(opts,"cnLineColor", "Black")
  opts@cnLineLabelFontColor       = opts@cnLineColor
  opts@cnLineLabelPerimColor      = opts@cnLineColor
  opts@cnInfoLabelFontColor       = opts@cnLineColor
  opts@cnHighLabelFontColor       = opts@cnLineColor
  opts@cnLowLabelFontColor        = opts@cnLineColor


; Set field Title and levels if available
  if(isatt(opts,"ContourLabel")) then
    if(isatt(opts,"ContourUnits")) then
      opts@cnInfoLabelString     = opts@ContourLabel + " Contours: $CMN$"+opts@ContourUnits\
      + " to $CMX$"+opts@ContourUnits+" in steps of $CIU$"+opts@ContourUnits
    else
      opts@cnInfoLabelString     = opts@ContourLabel + " Contours: $CMN$ to $CMX$ in steps of $CIU$"
    end if
  else
    if(isatt(data,"description")) then
      opts@cnInfoLabelString     = data@description + " Contours: $CMN$ to $CMX$ by $CIU$"
    else
      opts@cnInfoLabelString     = " Contours: $CMN$ to $CMX$ by $CIU$"
    end if
  end if


  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_lb_resources")
function set_lb_resources (data[*][*]:numeric, res:logical)

begin

  opts = res


; Somewhat convoluted way to see if a labelbar is not desired.
  if(check_attr(opts,"pmTickMarkDisplayMode","Never",True).or.\
     check_attr(opts,"pmTickMarkDisplayMode",-1,False).or.\
     check_attr(opts,"pmTickMarkDisplayMode",0,False).or. \
     check_attr(opts,"lbLabelBarOn",False,False).or.\
     check_attr(opts,"lbLabelBarOn",0,False)) then
    lbar_on = False
  else
    lbar_on = True
  end if
  atmp = get_res_value(opts,"lbLabelBarOn",True) ; Remove this resource
  delete(atmp)                                   ; just in case.


; Possible title for the labelbar
  if(isatt(opts,"FieldTitle")) then
    lb_desc = opts@FieldTitle
    if(isatt(opts,"UnitLabel") ) then
      lb_desc = lb_desc + "  (" + opts@UnitLabel + ")"
    end if
  else if(isatt(data,"description")) then
    lb_desc = data@description
    if(isatt(data,"units") .and. .not.(data@units.eq."")) then
      lb_desc = lb_desc + "  (" + data@units + ")"
    end if
  else
    lb_desc = ""
  end if
  end if



  if(.not.isatt(opts,"cnFillColors")) then
    opts@gsnSpreadColors = get_res_value_keep(opts, "gsnSpreadColors", True)
  end if
  opts@cnInfoLabelOn   = get_res_value_keep(opts,"cnInfoLabelOn", False)
  opts@cnLinesOn       = get_res_value_keep(opts,"cnLinesOn", False)
  opts@cnLineLabelsOn  = get_res_value_keep(opts,"cnLineLabelsOn", False)

; Labelbar resources
  if(lbar_on) then
    opts@pmLabelBarDisplayMode    = get_res_value_keep(opts,"pmLabelBarDisplayMode", "Always")
    opts@pmLabelBarSide           = get_res_value_keep(opts,"pmLabelBarSide", "Bottom")
    opts@pmLabelBarOrthogonalPosF = get_res_value_keep(opts,"pmLabelBarOrthogonalPosF", -0.15)
    opts@lbAutoManage             = get_res_value_keep(opts,"lbAutoManage",False)
    opts@lbOrientation            = get_res_value_keep(opts,"lbOrientation", "Horizontal")
    opts@lbPerimOn                = get_res_value_keep(opts,"lbPerimOn", False)
    opts@lbLabelJust              = get_res_value_keep(opts,"lbLabelJust", "BottomCenter")
    opts@lbLabelAutoStride        = get_res_value_keep(opts,"lbLabelAutoStride",True)
    opts@lbBoxMinorExtentF        = get_res_value_keep(opts,"lbBoxMinorExtentF", 0.13)
    opts@lbTitleFontHeightF       = get_res_value_keep(opts,"lbTitleFontHeightF", 0.012)
    opts@lbLabelFontHeightF       = get_res_value_keep(opts,"lbLabelFontHeightF", 0.012)

    opts@lbTitleOn                = get_res_value_keep(opts,"lbTitleOn", True)
    if(lb_desc.ne."" .and. opts@lbTitleOn) then
      opts@lbTitleOn              = get_res_value_keep(opts,"lbTitleOn", True)
      opts@lbTitleString          = get_res_value_keep(opts,"lbTitleString", lb_desc)
      opts@lbTitleJust            = get_res_value_keep(opts,"lbTitleJust", "BottomCenter")
      opts@lbTitleOffsetF         = get_res_value_keep(opts,"lbTitleOffsetF", -0.5)
    else
      opts@lbTitleOn = False
    end if
  end if


  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_title_resources")
function set_title_resources (data[*][*]:numeric, res:logical)

begin

  opts = res

; Set field Title and levels if available
  if(isatt(opts,"FieldTitle")) then
    SubTitles = opts@FieldTitle
  else
    if(isatt(data,"description")) then
      SubTitles = data@description
    else
      SubTitles = "UnKnown"
    end if
  end if
  if (SubTitles .ne. "") then
    if(isatt(opts,"SubFieldTitle")) then
      SubTitles = SubTitles + " " +  opts@SubFieldTitle
    end if
    if(isatt(opts,"UnitLabel")) then
      SubTitles = SubTitles + " (" +  opts@UnitLabel + ")"
    else
       if(isatt(data,"units") .and. .not.(data@units.eq."")) then
        SubTitles = SubTitles + " (" +  data@units + ")"
      end if
    end if
    if (isatt(opts,"PlotLevelID") .and. opts@PlotLevelID .ne. "") then
      SubTitles = SubTitles + " at " +  opts@PlotLevelID
    else
      if (isatt(data,"PlotLevelID") .and. opts@PlotLevelID .ne. "") then
         SubTitles = SubTitles + " at " +  data@PlotLevelID
      end if
    end if
  end if
  opts@tiMainString = SubTitles

  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_vc_resources")
function set_vc_resources (res:logical)

begin

  opts = res

  if ( isatt(opts,"vpWidthF") ) then
  ; num_vectors is used for vcMinDistanceF and vcRefLengthF
    width = opts@vpWidthF
    num_vectors = get_res_value(opts,"NumVectors",25.0)
    opts@vcMinDistanceF   = get_res_value_keep(opts,"vcMinDistanceF", width/num_vectors)
    opts@vcRefLengthF     = get_res_value_keep(opts,"vcRefLengthF", width/num_vectors)
  else
    opts@vcMinDistanceF   = get_res_value_keep(opts,"vcMinDistanceF", 0.02)
    opts@vcRefLengthF     = get_res_value_keep(opts,"vcRefLengthF", 0.02)
  end if


  opts@vcGlyphStyle     = get_res_value_keep(opts,"vcGlyphStyle", "WindBarb")
  opts@vcWindBarbColor  = get_res_value_keep(opts,"vcWindBarbColor", "Blue")
  opts@vcRefAnnoOn      = get_res_value_keep(opts,"vcRefAnnoOn", False)
  opts@vcMinFracLengthF = get_res_value_keep(opts,"vcMinFracLengthF", .2)

  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_mp_resources")
function set_mp_resources (res:logical)

begin

  opts = res

; "LowRes" is the default that NCL uses, so you don't need to
; set it here. However, if you want a higher resolution, use
; "MediumRes". If you want higher resolution for the coastlines,
; then set it to "HighRes", but then you also need to download
; the RANGS-GSHHS database.  Higher resolutions take longer to
; draw.

      opts@mpDataBaseVersion     = get_res_value_keep(opts, "mpDataBaseVersion","Ncarg4_1") ; same as MediumRes
      opts@mpDataSetName         = get_res_value_keep(opts, "mpDataSetName","Earth..4")
     ;opts@mpOutlineBoundarySets = get_res_value_keep(opts, "mpOutlineBoundarySets", "AllBoundaries")
      ;opts@mpOutlineBoundarySets = get_res_value_keep(opts, "mpOutlineBoundarySets", "GeophysicalAndUSStates")
     ; opts@mpOutlineBoundarySets = get_res_value_keep(opts, "mpOutlineBoundarySets", "GeophysicalAndUSStates")
      opts@mpOutlineSpecifiers   = get_res_value_keep(opts, "mpOutlineSpecifiers", (/"Canada:provinces", "United States:states"/))
      opts@tmXBLabelFontHeightF  = get_res_value_keep(opts, "tmXBLabelFontHeightF", 0.008)
      opts@tmYLLabelFontHeightF  = get_res_value_keep(opts, "tmYLLabelFontHeightF", 0.008)

; Select portion of the map to view.
      opts@mpLimitMode           = get_res_value_keep(opts, "mpLimitMode","Corners")
      opts@mpLeftCornerLatF      = get_res_value_keep(opts, "mpLeftCornerLatF", opts@start_lat)
      opts@mpLeftCornerLonF      = get_res_value_keep(opts, "mpLeftCornerLonF", opts@start_lon)
      opts@mpRightCornerLatF     = get_res_value_keep(opts, "mpRightCornerLatF",opts@end_lat)
      opts@mpRightCornerLonF     = get_res_value_keep(opts, "mpRightCornerLonF",opts@end_lon)

      if ( opts@mpRightCornerLonF .lt. 0.0 ) then
        opts@mpRightCornerLonF  = opts@mpRightCornerLonF + 360.0
      end if

; Set some other resources for line colors and grid spacing.

      opts@mpGeophysicalLineColor      = get_res_value_keep(opts, "mpGeophysicalLineColor","Black")
      opts@mpGeophysicalLineThicknessF = get_res_value_keep(opts, "mpGeophysicalLineThicknessF",2.0)
      opts@mpGridLineColor             = get_res_value_keep(opts, "mpGridLineColor","Black")
      opts@mpGridLineThicknessF        = get_res_value_keep(opts, "mpGridLineThicknessF",2.0)
     ;opts@mpGridMaskMode              = get_res_value_keep(opts, "mpGridMaskMode",3)
      opts@mpGridSpacingF              = get_res_value_keep(opts, "mpGridSpacingF",5)
      opts@mpLimbLineColor             = get_res_value_keep(opts, "mpLimbLineColor","Black")
      opts@mpLimbLineThicknessF        = get_res_value_keep(opts, "mpLimbLineThicknessF",2.0)
      opts@mpNationalLineColor         = get_res_value_keep(opts, "mpNationalLineColor","Black")
      opts@mpNationalLineThicknessF    = get_res_value_keep(opts, "mpNationalLineThicknessF",2.0)
      opts@mpPerimLineColor            = get_res_value_keep(opts, "mpPerimLineColor","Black")
      opts@mpPerimLineThicknessF       = get_res_value_keep(opts, "mpPerimLineThicknessF", 2.0)
      opts@mpPerimOn                   = get_res_value_keep(opts, "mpPerimOn",True)
      opts@mpUSStateLineColor          = get_res_value_keep(opts, "mpUSStateLineColor","Black")
      opts@mpUSStateLineThicknessF     = get_res_value_keep(opts, "mpUSStateLineThicknessF",2.0)
      opts@pmTickMarkDisplayMode       = get_res_value_keep(opts, "pmTickMarkDisplayMode","Always")
      opts@mpGridAndLimbOn             = get_res_value_keep(opts, "mpGridAndLimbOn",False)
; Tick mark resources

     ;opts@tmXBMajorLengthF      = get_res_value(opts, "tmXBMajorLengthF",-0.03)
     ;opts@tmYLMajorLengthF      = get_res_value(opts, "tmYLMajorLengthF",-0.03)
      opts@tmXTOn                = get_res_value(opts,"tmXTOn",False)
      opts@tmYROn                = get_res_value(opts,"tmYROn",False)
      opts@tmYRLabelsOn          = get_res_value(opts,"tmYRLabelsOn",True)
      opts@tmXBBorderOn          = get_res_value(opts,"tmXBBorderOn",True)
      opts@tmXTBorderOn          = get_res_value(opts,"tmXTBorderOn",True)
      opts@tmYLBorderOn          = get_res_value(opts,"tmYLBorderOn",True)
      opts@tmYRBorderOn          = get_res_value(opts,"tmYRBorderOn",True)

  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

undef("_SetMainTitle")
procedure _SetMainTitle(wks[1]:graphic,cn[1]:graphic,opts)

; This procedure checks the input data for certain attributes, and
; based on those, sets MainTitle, InitTime and ValidTime
;
;  Attributes recognized by this procedure:
;     MainTitle                 (main title - top left)
;                               (with Init time top right)
;     TimeLabel                 (valid time  - right  under init time)
;     NoHeaderFooter            (switch all headers and footers off - mainly for panels)
;
; If the "NoHeaderFooter" attribute exists and is set True, then
; don't create any titles.

begin
;
  if(opts.and.isatt(opts,"NoHeaderFooter").and.opts@NoHeaderFooter) then
    return
  end if

  if(.not.isatt(opts,"MainTitle").or.opts@MainTitle.eq."") then
    return
  end if

;
; Set basic plot font
;
  font_height = get_res_value_keep(opts,"FontHeightF",0.01)
;
;
; If a MainTitle attribute hasn't been set, then set to "hrdps"
; Also set an Initial time
;
; MAIN Header of plot
  opts@MainTitle    = get_res_value_keep(opts,"MainTitle", " ")
  opts@MainTitlePos = get_res_value_keep(opts,"MainTitlePos", "Left")
  opts@ValidTime    = get_res_value_keep(opts,"ValidTime", True)
  opts@TimePos      = get_res_value_keep(opts,"TimePos", "Right")
  opts@Footer       = get_res_value_keep(opts,"Footer", True)


  if (opts@MainTitlePos .eq. "Left")
    opts@MainTitlePos  = "CenterLeft"
    opts@MainTitlePosF = 0.0
  end if
  if (opts@MainTitlePos .eq. "Center")
    opts@MainTitlePos  = "CenterCenter"
    opts@MainTitlePosF = 0.5
  end if
  if (opts@MainTitlePos .eq. "Right")
    opts@MainTitlePos  = "CenterRight"
    opts@MainTitlePosF = 1.0
  end if


  txt0 = create "MainPlotTitle" textItemClass wks
    "txString"      : opts@MainTitle
    "txFontHeightF" : font_height*1.5
    "txPerimOn" : True
    "txPerimColor" : "Background"
  end create
  anno = NhlAddAnnotation(cn,txt0)
  setvalues anno
    "amZone"           : 3
    "amSide"           : "Top"
    "amJust"           : opts@MainTitlePos
    "amParallelPosF"   : opts@MainTitlePosF
    "amOrthogonalPosF" : 0.0
    "amResizeNotify"   : False
  end setvalues

; Time information on plot
  if (opts@TimePos .eq. "Left")
    opts@TimePos  = "CenterLeft"
    opts@TimePosF = 0.0
  end if
  if (opts@TimePos .eq. "Right")
    opts@TimePos  = "CenterRight"
    opts@TimePosF = 1.0
  end if


  if( isatt(opts,"InitTime") ) then
    InitTime = "Model Run: " + opts@InitTime
    txt1 = create "InitTime" textItemClass wks
      "txString"      : InitTime
      "txFontHeightF" : font_height
      "txPerimOn" : True
      "txPerimColor" : "Background"
    end create
    anno = NhlAddAnnotation(cn,txt1)
    setvalues anno
      "amZone"           : 3
      "amSide"           : "Top"
      "amJust"           : opts@TimePos
      "amParallelPosF"   : opts@TimePosF
      "amOrthogonalPosF" : 0.0
      "amResizeNotify"   : False
    end setvalues
  end if

  plot_narrow = False
  if((opts).and.(isatt(opts,"vpWidthF")).and.(isatt(opts,"vpHeightF"))) then
     ph = opts@vpHeightF
     pw = opts@vpWidthF
     phw = ph/pw
     if ( phw .gt. 1.8 ) then
       plot_narrow = True
     end if
  end if

  if( opts@ValidTime .and. isatt(opts,"TimeLabel") ) then
    ValidTime = "Model Time : " + opts@TimeLabel

    txt2 = create "ValidTime" textItemClass wks
      "txString"      : ValidTime
      "txFontHeightF" : font_height
      "txPerimOn" : True
      "txPerimColor" : "Background"
    end create
    anno = NhlAddAnnotation(cn,txt2)
    setvalues anno
      "amZone"           : 2
      "amSide"           : "Top"
      "amJust"           : opts@TimePos
      "amParallelPosF"   : opts@TimePosF
      "amOrthogonalPosF" : 0.0
      "amResizeNotify"   : False
    end setvalues
  end if


; Add Footer if called for
  if( opts@Footer ) then
     Footer = " "
   else
     Footer = " "
   end if
    txt3 = create "Footer" textItemClass wks
      "txString"      : Footer
      "txFontHeightF" : font_height*.9
      "txPerimOn" : True
      "txPerimColor" : "Background"
    end create
    anno = NhlAddAnnotation(cn,txt3)
    setvalues anno
      "amZone"           : 1
;      "amZone"           : 7
      "amJust"           : "TopLeft"
      "amSide"           : "Bottom"
      "amParallelPosF"   : 0.0
      "amOrthogonalPosF" : -0.55
      "amResizeNotify"   : False
    end setvalues


; Add X-setion information if needed
  if(opts.and.isatt(opts,"PlotOrientation")) then
    ;Xsection = "Cross-Section Orientation : " + opts@PlotOrientation
    Xsection =  opts@PlotOrientation
    txt4 = create "Xsection" textItemClass wks
      "txString"      : Xsection
      "txFontHeightF" : font_height*.9
      "txPerimOn" : True
      "txPerimColor" : "Background"
    end create
    anno = NhlAddAnnotation(cn,txt4)
    setvalues anno
      "amZone"           : 2
      "amSide"           : "Top"
      "amJust"           : "CenterRight"
      "amParallelPosF"   : 1.0
      "amOrthogonalPosF" : 0.005
      "amResizeNotify"   : False
    end setvalues
  end if

end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

undef("hrdps_contour")
function hrdps_contour(wks[1]: graphic, data[*][*]:numeric, \
                     opt_args[1]:logical)

; This function creates a contour plot and adds some titles to it.
;
;   1. Determine width to height ratio of plot.
;
;   2. First determine if this is to be a filled or line
;      contour plot (fill_on)
;
;   3. If the ContourParameters attribute is set, then calculate
;      the contour levels.
;
;   4. Set two resources for setting the zero contour line to
;      a larger thickness, and for changing the negative contour
;      lines to a dashed pattern.
;
;   5. If doing a filled contour plot, set a title for the labelbar
;      based on whether a units attribute is set.
;
;   6. Make a copy of the resource list, and set some additional
;      resources for filled contour plots.
;
;   7. Create the contour plot, attach the titles, and draw
;      and advance the frame (if requested).

local dims
begin
  opts = opt_args      ; Make a copy of the resource list.

  if(opts.and.isatt(opts,"mpOutlineBoundarySets")) then
    delete(opts@mpOutlineBoundarySets)
  end if


; Calculate ratio of plot width and height. Note that this doesn't
; affect the setting of gsnMaximize to True, because gsnMaximize will
; retain the aspect ratio of the plot.

  if(opts.and.isatt(opts,"AspectRatio")) then
    ratio = opts@AspectRatio
  else
    dims  = dimsizes(data)
    ratio = 1.*dims(0)/dims(1)
    if(ratio .gt. 1.2) then
      ratio = 1.2
    end if
    if(ratio .lt. 0.6667) then
      ratio = 0.6667
    end if
  end if

  if(ratio .gt. 1)
    width  = 0.65 * 1.0/ratio
    height = 0.65
  else
    width  = 0.85
    height = 0.85 * ratio
  end if

  opts@vpWidthF = get_res_value_keep(opts,"vpWidthF",  width)
  opts@vpHeightF = get_res_value_keep(opts,"vpHeightF", height)


; Set some basic contour resources
  opts = set_cn_resources(data,opts)


; Find out if we are working with a contour or a shaded plot
; fill_on = False : line contour plot
; fill_on = True  : filled contour plot
  fill_on = get_res_value_keep(opts,"cnFillOn",False)
  if(fill_on) then      ; set lb resources if needed
    opts = set_lb_resources(data,opts)
    atmp = get_res_value(opts,"lbLabelBarOn",True) ; Remove this resource
    delete(atmp)                                   ; just in case.
  end if


; Set Title resources
  opts = set_title_resources(data,opts)


; Setting gsnScale to True ensures that the tickmark lengths and labels
; will be the same size on both axes.
  opts@gsnScale    = get_res_value_keep(opts,"gsnScale",    True)


; The default is not to draw the plot or advance the frame, and
; to maximize the plot in the frame.
  opts@gsnDraw          = False    ; Make sure don't draw or frame or,
  opts@gsnFrame         = False    ; maximize, b/c we'll do this later.
  opts@gsnMaximize      = False


  opts2 = opts
  delete_attrs(opts2)                       ; Clean up.
  cn = gsn_contour(wks,data,opts2)          ; Create the plot.
  _SetMainTitle(wks,cn,opts)        ; Set some titles

  opts2@gsnDraw     = get_res_value_keep(opts2,"gsnDraw",     False)
  opts2@gsnFrame    = get_res_value_keep(opts2,"gsnFrame",    False)
  opts2@gsnMaximize = get_res_value_keep(opts2,"gsnMaximize", True)
  draw_and_frame(wks,cn,opts2@gsnDraw,opts2@gsnFrame,False,opts2@gsnMaximize)

  return(cn)                                ; Return

end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

undef("hrdps_vector")
function hrdps_vector(wks[1]: graphic, data_u[*][*]:numeric, \
                    data_v[*][*]:numeric, opt_args[1]:logical)
;
; This function creates a vector plot and adds some titles to it.
;
;   1. Determine width to height ratio of plot. Will also be use
;      to calculate values for vector resources later.
;
;   2. Make a copy of the resource list, and set some additional
;      resources.
;
;   3. Create the vector plot, attach the titles, and draw
;      and advance the frame (if requested).

local dims
begin
  opts = opt_args      ; Make a copy of the resource list.


  if(opts.and.isatt(opts,"mpOutlineBoundarySets")) then
    delete(opts@mpOutlineBoundarySets)
  end if
;
; The ratio is used to determine the width and height of the
; plot, and also to determine the value for the vcMinDistanceF
; resource.
;
  if(opts.and.isatt(opts,"AspectRatio")) then
    ratio = get_res_value(opts,"AspectRatio",0.)
  else
    dims  = dimsizes(data_u)
    ratio = 1.*dims(0)/dims(1)
    if(ratio .gt. 1.2) then
      ratio = 1.2
    end if
    if(ratio .lt. 0.6667) then
      ratio = 0.6667
    end if
  end if

  if(ratio .gt. 1)
    width  = 0.65/ratio
    height = 0.65
  else
    width  = 0.95
    height = 0.95 * ratio
  end if

  opts@vpWidthF  = get_res_value_keep(opts,"vpWidthF",  width)
  opts@vpHeightF = get_res_value_keep(opts,"vpHeightF", height)


; Set Title resources
  opts = set_title_resources(data_u,opts)


; Set vector resources
  opts = set_vc_resources(opts)


; Setting gsnScale to True ensures that the tickmark lengths and labels
; will be the same size on both axes.
  opts@gsnScale = get_res_value_keep(opts,"gsnScale",    True)


; The default is not to draw the plot or advance the frame, and
; to maximize the plot in the frame.
  opts@gsnDraw          = False    ; Make sure don't draw or frame or,
  opts@gsnFrame         = False    ; maximize, b/c we'll do this later.
  opts@gsnMaximize      = False

  opts2 = opts
  delete_attrs(opts2)                             ; Clean up.
  vct = gsn_vector(wks,data_u,data_v,opts2)       ; Create vector plot.
  _SetMainTitle(wks,vct,opts)


  opts2@gsnDraw     = get_res_value_keep(opts2,"gsnDraw",     False)
  opts2@gsnFrame    = get_res_value_keep(opts2,"gsnFrame",    False)
  opts2@gsnMaximize = get_res_value_keep(opts2,"gsnMaximize", True)
  draw_and_frame(wks,vct,opts2@gsnDraw,opts2@gsnFrame,False, \
                 opts2@gsnMaximize)

  return(vct)                                     ; Return.
end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("hrdps_wps_map")
function hrdps_wps_map(wks[1]:graphic,opt_args[1]:logical)

begin
;
;   1. Make a copy of the resource list, and set some resources
;      common to all map projections.
;
;   2. Determine the projection being used, and set resources based
;      on that projection.
;
;   3. Create the map plot, and draw and advance the frame
;      (if requested).

    opts = opt_args      ; Make a copy of the resource list
    ;opts  = True

; Set some resources depending on what kind of map projection is
; chosen.
;
;   MAP_PROJ = 0 : "CylindricalEquidistant"
;   MAP_PROJ = 1 : "LambertConformal"
;   MAP_PROJ = 2 : "Stereographic"
;   MAP_PROJ = 3 : "Mercator"
;   MAP_PROJ = 6 : "Lat/Lon"

;   CylindricalEquidistant
      if(opts@map_proj .eq. 0)
        projection          = "CylindricalEquidistant"
        opts@mpGridSpacingF = 45
        opts@mpCenterLatF   = get_res_value_keep(opts, "mpCenterLatF", 0.0)
        opts@mpCenterLonF   = get_res_value_keep(opts, "mpCenterLonF",opts@stand_lon)
      end if

;   LambertConformal projection
      if(opts@map_proj .eq. 1)
        projection               = "LambertConformal"
        opts@mpLambertParallel1F = get_res_value_keep(opts, "mpLambertParallel1F",opts@truelat1)
        opts@mpLambertParallel2F = get_res_value_keep(opts, "mpLambertParallel2F",opts@truelat2)
        opts@mpLambertMeridianF  = get_res_value_keep(opts, "mpLambertMeridianF",opts@stand_lon)
      end if

;   Stereographic projection
      if(opts@map_proj .eq. 2)
        projection          = "Stereographic"
        if( isatt(opts,"cenlat") ) then
          opts@mpCenterLatF  = get_res_value_keep(opts, "mpCenterLatF",opts@cenlat)
        else
          opts@mpCenterLatF  = get_res_value_keep(opts, "mpCenterLatF",opts@ref_lat)
        end if
        opts@mpCenterLonF   = get_res_value_keep(opts, "mpCenterLonF",opts@stand_lon)
      end if

;   Mercator projection
      if(opts@map_proj .eq. 3)
        projection          = "Mercator"
        opts@mpCenterLatF   = get_res_value_keep(opts, "mpCenterLatF", 0.0)
        opts@mpCenterLonF   = get_res_value_keep(opts, "mpCenterLonF",opts@stand_lon)
      end if

;    global hrdps CylindricalEquidistant
      if(opts@map_proj .eq. 6)
        projection          = "CylindricalEquidistant"
        opts@mpGridSpacingF = 45
        if( isatt(opts,"cenlon") ) then
          opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",opts@cenlon)
        else
          opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",opts@ref_lon)
        end if
        if( isatt(opts,"pole_lat") ) then
          delete(opts@mpCenterLonF)
          opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF", - 190. )
          opts@mpCenterRotF = get_res_value_keep(opts, "mpCenterRotF", 90.0 - opts@pole_lat)
        else
          opts@mpCenterRotF = get_res_value_keep(opts, "mpCenterRotF", 0.0)
        end if
      end if

; Set some resources common to all map projections.
  opts = set_mp_resources(opts)

; The default is not to draw the plot or advance the frame, and
; to maximize the plot in the frame.

  opts@gsnDraw       = get_res_value_keep(opts,"gsnDraw",     False)
  opts@gsnFrame      = get_res_value_keep(opts,"gsnFrame",    False)
  opts@gsnMaximize   = get_res_value_keep(opts,"gsnMaximize", True)

  delete_attrs(opts)                             ; Clean up.
  mp = gsn_map(wks,projection,opts)              ; Create map plot.
  return(mp)                                     ; Return.

end


;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

undef("hrdps_map_overlays")
;function hrdps_map_overlays(fileArgs:logical, \
function hrdps_map_overlays(wks:graphic, \
                          plots[*]:graphic, \
                          opt_arg[1]:logical, \
                          opt_mp[1]:logical)

; This procedure takes an array of plots and overlays them on a
; base plot - map background.
;
; It will advance the plot and cleanup, unless you set the
; PanelPlot resource to True.
;
;  Attributes recognized by this procedure:
;     FramePlot
;     PanelPlot
;     NoTitles                  (don't do any titles)
;     CommonTitle & PlotTitle is used to overwrite field titles
;        CommonTitle will supercede NoTitles
;     LatLonOverlay
;
; If FramePlot False, then Draw the plot but do not Frame.
; In this case a user want to add to the drawing, and will
; have to advance the Frame manually in the script.
;
; If the "NoTitles" attribute exists and is set True, then
; don't create the top-left titles, and leave the main titles alone.
; This resource can be useful if you are planning to panel
; the plots.
;
; If PanelPlot is set to True, then this flags to hrdps_map_overlays
; that these plots are going to be eventually paneled (likely
; by gsn_panel), and hence 1) draw and frame should not be called
; (unless gsnDraw and/or gsnFrame are explicitly set to True),
; and 2) the overlays and titles should not be removed with
; NhlRemoveOverlay and NhlRemoveAnnotation.
;
; If LatLonOverlay is set to True, then this means the user is
; using the 2D lat/lon coordinates to do the overlay, and hence
; tfDoNDCOverlay should not be set to True. (The default is False.)
;
begin

  opts     = opt_arg         ; Make a copy of the resource lists
  opt_mp_2 = opt_mp


  ; Let's make the map first
;  base = hrdps_map(fileArgs,wks,opt_mp_2)
  base = hrdps_wps_map(wks,opt_mp_2)

  no_titles  = get_res_value(opts,"NoTitles",False)     ; Do we want field titles?
  com_title  = get_res_value(opts,"CommonTitle",False)     ; Do we have a common title?
  if ( com_title ) then
    plot_title = get_res_value(opts,"PlotTitle","  ")
    no_titles = True
  end if

  call_draw  = True
  call_frame = get_res_value(opts,"FramePlot",True)     ; Do we want to frame the plot?
  panel_plot = get_res_value(opts,"PanelPlot",False)    ; Are we paneling?
  latlon_overlay = get_res_value(opts,"LatLonOverlay",False) ; Lat/lon Overlay?
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  nplots = dimsizes(plots)
;  font_color = "Black"

  do i=0,nplots-1
    if(.not.ismissing(plots(i))) then
      if(.not.no_titles) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        if(i.eq.0) then
          SubTitles = SubTitle
        else  if(SubTitle .ne. "") then
          SubTitles = SubTitles + "~C~" + SubTitle
        end if
        end if;
      end if
      if(com_title .and. i .eq. nplots-1) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        SubTitles = plot_title
      end if
      if(.not.latlon_overlay) then
        setvalues plots(i)
          "tfDoNDCOverlay" : True
          "tiMainOn"       : False
        end setvalues
      else
        setvalues plots(i)
          "tiMainOn"       : False
        end setvalues
      end if
      overlay(base,plots(i))
    else
      print("hrdps_map_overlays: Warning: overlay plot #" + i + " is not valid.")
    end if
  end do

  if(.not.no_titles .or. com_title) then
    font_height = get_res_value_keep(opts,"FontHeightF",0.01)
    txt = create "map_titles" textItemClass wks
      "txString"      : SubTitles
      "txFontHeightF" : font_height
      "txPerimOn" : True
      "txPerimColor" : "Background"
     ;"txFontColor"   : font_color
    end create
    anno = NhlAddAnnotation(base,txt)
    setvalues anno
      "amZone"           : 2
      "amJust"           : "TopLeft"
      "amSide"           : "Top"
      "amParallelPosF"   : 0.005
      ;"amOrthogonalPosF" : 0.05
      "amOrthogonalPosF" : 0.0
      "amResizeNotify"   : False
    end setvalues
    base@map_titles = anno
  end if
;
; gsnDraw and gsnFrame default to False if panel plot.
;
  if(panel_plot) then
    call_draw = False
    call_frame= False
  end if


  opts@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     call_draw)
  opts@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    call_frame)

  draw_and_frame(wks,base,opts@gsnDraw,opts@gsnFrame,False, \
                 opts@gsnMaximize)

  if(.not.panel_plot) then
    do i=0,nplots-1
      if(.not.ismissing(plots(i))) then
        NhlRemoveOverlay(base,plots(i),False)
      else
        print("hrdps_remove_map_overlays: Warning: overlay plot #" + i + " is not valid.")
        print("                        Nothing to remove.")
      end if
    end do
  end if


  if(.not.no_titles.and..not.panel_plot) then
    if(isatt(base,"map_titles")) then
      NhlRemoveAnnotation(base,base@map_titles)
      delete(base@map_titles)
    end if
  end if

return(base)
end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

undef("hrdps_overlays")
function hrdps_overlays( wks:graphic, plots[*]:graphic, \
                     opt_arg[1]:logical)

; This procedure takes an array of plots and overlays them.
;
; It will advance the plot and cleanup, unless you set the
; PanelPlot resource to True.
;
;  Attributes recognized by this procedure:
;     FramePlot
;     PanelPlot
;     NoTitles                  (don't do any titles)
;     CommonTitle & PlotTile is used to overwrite field titles
;        CommonTitle will super-seed NoTitles
;
; If FramePlot False, then Draw the plot but do not Frame.
; In this case a user want to add to the drawing, and will
; have to advance the Frame manually in the script.
;
; If the "NoTitles" attribute exists and is set True, then
; don't create the top-left titles, and leave the main titles alone.
; This resource can be useful if you are planning to panel
; the plots.
;
; If PanelPlot is set to True, then this flags to hrdps_overlays
; that these plots are going to be eventually paneled (likely
; by gsn_panel), and hence 1) draw and frame should not be called
; (unless gsnDraw and/or gsnFrame are explicitly set to True),
; and 2) the overlays and titles should not be removed with
; NhlRemoveOverlay and NhlRemoveAnnotation.
;
; If LatLonOverlay is set to True, then this means the user is
; using the 2D lat/lon coordinates to do the overlay, and hence
; tfDoNDCOverlay should not be set to True. (The default is False.)
;
begin
  opts = opt_arg      ; Make a copy of the resource list.

  no_titles  = get_res_value(opts,"NoTitles",False)     ; Do we want field titles?
  com_title  = get_res_value(opts,"CommonTitle",False)     ; Do we have a common title?
  latlon_overlay = get_res_value(opts,"LatLonOverlay",False) ; Lat/lon Overlay?
  if ( com_title ) then
    plot_title = get_res_value(opts,"PlotTitle","  ")
    no_titles = True
  end if

  call_draw  = True
  call_frame = get_res_value(opts,"FramePlot",True)     ; Do we want to frame the plot?
  panel_plot = get_res_value(opts,"PanelPlot",False)    ; Are we paneling?
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  nplots = dimsizes(plots)

  base = plots(0)
  if(.not.no_titles) then
    getvalues plots(0)
      "tiMainString" : SubTitle
    end getvalues
    SubTitles = SubTitle
    if(.not.latlon_overlay) then
      setvalues plots(0)
        "tfDoNDCOverlay" : True
        "tiMainOn"       : False
      end setvalues
    else
      setvalues plots(0)
        "tiMainOn"       : False
      end setvalues
    end if
  else
    if(.not.latlon_overlay) then
      setvalues plots(0)
        "tfDoNDCOverlay" : True
      end setvalues
    end if
  end if

  if (nplots.eq.1) then
    blank = create "BlankPlot" logLinPlotClass wks
      ;"cnConstFLabelOn" : False
    end create
    overlay(base,blank)
  end if

  do i=1,nplots-1
    if(.not.ismissing(plots(i))) then
      if(.not.no_titles) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        if(i.eq.0) then
          SubTitles = SubTitle
        else
          SubTitles = SubTitles + "~C~" + SubTitle
        end if
      end if
      if(com_title .and. i .eq. nplots-1) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        SubTitles = plot_title
      end if
      if(.not.latlon_overlay) then
        setvalues plots(i)
          "tfDoNDCOverlay" : True
          "tiMainOn"       : False
        end setvalues
      else
        setvalues plots(i)
          "tiMainOn"       : False
        end setvalues
      end if
      overlay(base,plots(i))
    else
      print("hrdps_overlays: Warning: overlay plot #" + i + " is not valid.")
    end if
  end do

  if(.not.no_titles .or. com_title) then
    font_height = get_res_value_keep(opts,"FontHeightF",0.01)

    txt = create "map_titles" textItemClass wks
      "txString"      : SubTitles
      "txFontHeightF" : font_height
      "txPerimOn" : True
      "txPerimColor" : "Background"
    end create
    anno = NhlAddAnnotation(base,txt)
    setvalues anno
      "amZone"           : 2
      "amJust"           : "BottomLeft"
      "amSide"           : "Top"
      "amParallelPosF"   : 0.005
      "amOrthogonalPosF" : 0.03
      "amResizeNotify"   : False
    end setvalues
    base@map_titles = anno
  end if

;
; gsnDraw and gsnFrame should default to True if not a panel plot.
;
  if(panel_plot) then
    call_draw = False
    call_frame= False
  end if

  opts@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     call_draw)
  opts@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    call_frame)
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  draw_and_frame(wks,base,opts@gsnDraw,opts@gsnFrame,False, \
                 opts@gsnMaximize)

  if(.not.no_titles.and..not.panel_plot) then
    NhlRemoveAnnotation(base,base@map_titles)
    delete(base@map_titles)
  end if

  if(.not.panel_plot) then
    if ( nplots .ge. 2 ) then
      do i=1,nplots-1
        if(.not.ismissing(plots(i))) then
          NhlRemoveOverlay(base,plots(i),False)
        else
          print("hrdps_remove_overlays: Warning: overlay plot #" + i + " is not valid.")
          print("                        Nothing to remove.")
        end if
      end do
    end if
  end if

return(base)
end

