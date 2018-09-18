;;;---------------------------------------------------------------------------------

;*** WRITE LABELBAR INFO
;;; writes units and (if appropriate) "FIXED" to top / bottom of label bar
;;; "flag" signifies H or V labelBar
undef("add_labelbar_text" )
procedure add_labelbar_text( wks[1]:graphic, plot[1]:graphic, title:string, lfixedcolor:numeric, flag:string )
local vpXF,vpYF,vpWidthF,vpHeightF,libtxtres,xNDCleft,xNDCright,yNDCmid,xNDC,yNDC,unitTitle
begin
        ;;; SET BASIC TEXT RESOURCES
        lbtxtres = True
        lbtxtres@txFuncCode    = "~"
        ;;; DETERMINE LABEL BAR SIZE AND POSITION
        getvalues plot
                "vpXF" : vpXF
                "vpYF" : vpYF
                "vpWidthF" : vpWidthF
                "vpHeightF" : vpHeightF
        end getvalues

        ;;; ADD UNITS TEXT
        lbtxtres@txFontHeightF = 0.02
        lbtxtres@txFont = "helvetica"

        if(title .eq. "") then
                unitTitle = "~K~No units~I~"
	else
		unitTitle = "[" + title + "]"
        end if
        lbtxtres@txJust = "CenterCenter"          ; Default is "CenterCenter".

	; This would be a lot easier if NCL had a condional stmt "x = cond?true:false" !!
	if(flag .eq. "H") then
		;;; add right units text & (if apropriate) "FIXED COLOR"
		yNDC = 1.0 - vpHeightF / 2.0
		xNDC = 1.0 - (1.0 - vpWidthF) / 4.0
		gsn_text_ndc ( wks, unitTitle, xNDC, yNDC, lbtxtres )
		if( lfixedcolor .eq. 1 ) then
			gsn_text_ndc ( wks, "~I~FIXED~C~~C~SCALE", xNDC,yNDC, lbtxtres )
		end if

		;;; add bottom / right units text & (if appropriate) "FIXED COLOR"
		xNDC = (1.0 - vpWidthF) / 4.0
		gsn_text_ndc ( wks, unitTitle, xNDC, yNDC, lbtxtres )
		if( lfixedcolor .eq. 1 ) then
			gsn_text_ndc ( wks, "~I~FIXED~C~~C~SCALE", xNDC,yNDC, lbtxtres )
		end if
	else	; flag .eq. "V"
		xNDC = vpWidthF / 2.0
		yNDC = 1.0 - (1.0 - vpHeightF) / 4.0
		gsn_text_ndc ( wks, unitTitle, xNDC, yNDC, lbtxtres )
		if( lfixedcolor .eq. 1 ) then
			gsn_text_ndc ( wks, "~I~FIXED~C~~C~SCALE", xNDC,yNDC, lbtxtres )
		end if
		yNDC = (1.0 - vpHeightF) / 4.0
		gsn_text_ndc ( wks, unitTitle, xNDC, yNDC, lbtxtres )
		if( lfixedcolor .eq. 1 ) then
			gsn_text_ndc ( wks, "~I~FIXED~C~~C~SCALE", xNDC,yNDC, lbtxtres )
		end if
	end if

end
;;;---------------------------------------------------------------------------------





;;;;;; Do the LABELBAR ;;;;;;

procedure doLabelBar(plot_param, tail, opts, FMT, unitString, fixedcolor )
begin
	;;; How many opts is this!
	opts_lb                    = True
	opts_lb@lbAutoManage       = False		; I have control(?)
	opts_lb@lbOrientation      = "Vertical"
	opts_lb@lbTitleOn          = False

	opts_lb@lbPerimOn          = False  		; Line Border (Turn on for testing!)
	opts_lb@lbBoxMinorExtentF  = 0.4 		; Fraction of space for Coloured Bars

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Do the Vertical labelBar
	;
	opts_lb@vpHeightF          = 0.825   		; Must leave space for UnitString
	opts_lb@vpYF               = 0.5 * ( 1.0 + opts_lb@vpHeightF) ; Move closer to LH edge

	opts_lb@vpWidthF           = 0.09   		; Width of whole LabelBar

	opts_lb@lbBottomMarginF    = 0.0
	opts_lb@lbLeftMarginF      = 0.0
	opts_lb@lbRightMarginF     = 0.0
	opts_lb@lbTopMarginF       = 0.0

	opts_lb@lbLabelsOn         = True
	opts_lb@lbLabelFont        = "helvetica" 	; Font of colorbar numbers
	opts_lb@lbLabelFontHeightF = 0.012		; Height of colorbar numbers
	opts_lb@lbLabelOffsetF     = 0.05		; move numbers close to colorbar
	opts_lb@lbLabelAutoStride = True		; Auto elimination if overlap
	if( .not. ismissing(str_match_ic(plot_param, "stars"))) then
		opts_lb@lbLabelAlignment   = "BoxCenters"	; Put values in centre
	else
		opts_lb@lbLabelAlignment   = "ExternalEdges"	; Put values on bar boundaries
	end if

	opts_lb@lbMonoFillPattern  = True		; Coloured bars
	opts_lb@lbFillColors       = opts@cnFillColors

	; Make the array of label values, if possible
	if( isatt(opts, "cnMaxLevelValF") \
	   .and. isatt(opts, "cnMinLevelValF") \
	   .and. isatt(opts, "cnLevelSpacingF"))
		mn = opts@cnMinLevelValF
		mx = opts@cnMaxLevelValF
		st = opts@cnLevelSpacingF
		nLabels = floattointeger((mx - mn) / st) + 3
		lbLabels = new(nLabels,float)
		do i = 0, nLabels-1
			lbLabels(i) = mn + (i-1)*st
		end do
	else
		lbLabels = (/" ", " ", " "/)
		opts_lb@lbBoxLinesOn = False
	end if

	labels = "" + lbLabels	; Force conversion to string for gsn_create_labelbar

	wksLb = openWksLb(plot_param, tail, ".side")

	tmp = gsn_create_labelbar_ndc(wksLb, dimsizes(opts@cnFillColors), labels, 0.0, 0.0, opts_lb)

	add_labelbar_text(wksLb, tmp, unitString, fixedcolor, "V")

	draw(tmp)
	frame(wksLb)

	if(FMT .eq. "png") then
                getvalues wksLb
                	"wkFileName" : fName
                end getvalues

		Wd = wkWidth * opts_lb@vpWidthF
		Ht = wkHeight
		Xo = 0
		Yo = 0
		cmd = CONVERT + " -resize x800 -crop " + Wd + "x800+" + Xo + "+" + Yo + " " + fName + " " + fName + " &"
		print("    " + cmd)

		delete(wksLb)     ; Mary Haley: delete wks to close the file
		system(cmd)
	end if

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Now do the Horizontal labelBar
	;
	; opts mostly the same, except for H/V stuff

	opts_lb@lbOrientation      = "Horizontal"
        opts_lb@vpWidthF           = 0.825              ; Must leave space for UnitString
        opts_lb@vpXF               = 0.5 * (1.0 - opts_lb@vpWidthF) ; Offset from Left

        opts_lb@vpHeightF          = 0.06               ; Height of whole LabelBar
        opts_lb@vpYF               = 0.999              ; Position LabelBar at Top of page

	wksLb = openWksLb(plot_param, tail, ".foot")
	tmp = gsn_create_labelbar_ndc(wksLb, dimsizes(opts@cnFillColors), labels, 0.0, 0.0, opts_lb)
        add_labelbar_text(wksLb, tmp, unitString, fixedcolor, "H")

        draw(tmp)
        frame(wksLb)

        if(FMT .eq. "png") then
                getvalues wksLb
			"wkFileName" : fName
                end getvalues

                Wd = wkWidth
                Ht = wkHeight * opts_lb@vpHeightF
                Xo = 0
                Yo = 0
                cmd = CONVERT + " -resize 800 -crop 800x" + Ht + "+" + Xo + "+" + Yo + " " + fName + " " + fName + " &"
                print("    " + cmd)

                delete(wksLb)     ; Mary Haley: delete wks to close the file
		system(cmd)
	end if
end
