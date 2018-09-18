procedure doHeader(filename, title, title2, title3, FMT)
begin

	print("    *  " + title)
	print("    *  " + title2 )
	print("    *  " + title3)

	;;; More opts!
	opts_ti                       = True
	opts_ti@vpWidthF              = 1.0  ; Full Width
	opts_ti@vpXF                  = 0.0  ; No Offset from Left

	opts_ti@vpYF                  = 0.89 ; Position plot vertically as RASP
	opts_ti@vpHeightF             = 0.82

	opts_ti@pmTickMarkDisplayMode = "NoCreate"
	opts_ti@pmLabelBarDisplayMode = "NoCreate"
	opts_ti@pmLegendDisplayMode   = "NoCreate"
	opts_ti@cnConstFLabelOn       = False
	opts_ti@gsnDraw               = False
	opts_ti@gsnFrame              = False

	wksHd = openWksHd(filename)

	; Make a Dummy plot so can add Title
	dullData = (/ (/0.,0.,0./), (/0.,0.,0./), (/0.,0.,0./) /)

	; Turn off "ConstantField" warning - the data is dull :-)
	err = NhlGetErrorObjectId()
	getvalues err
		"errLevel" : errLevel
	end getvalues
	setvalues err
		"errLevel" : "Fatal"          ; Report only Fatal errors
	end setvalues

	tmp = gsn_csm_contour(wksHd, dullData, opts_ti)

	setvalues err
		"errLevel" : errLevel          ; Reinstate original reporting level
	end setvalues

	; Produce the 3 lines of the Title
	txt0 = create "line1" textItemClass wksHd
		"txFuncCode"	   : "~"
		"txString"	   : title
		"txFontHeightF"	   : 0.018
		"txFont"	   : "helvetica-bold"
	end create
	anno = NhlAddAnnotation(tmp, txt0)
	setvalues anno
		"amZone"           : 1           ; Measure offsets from Top
		"amSide"           : "Top"
		"amJust"           : "TopCenter"
		"amParallelPosF"   : 0.5         ; Horizontal Centre (of Top)
		"amOrthogonalPosF" : -(1 - 0.89) ; Place top of txt at top of plot
		                                 ; 0.89 == vpYF
	end setvalues

	txt1 = create "line2" textItemClass wksHd
		"txFuncCode"	   : "~"
		"txString"	   : title2
		"txFontHeightF"	   : 0.015
		"txFont"	   : "helvetica"
	end create
	anno = NhlAddAnnotation(tmp, txt1)
	setvalues anno
		"amZone"           : 1
		"amSide"           : "Top"
		"amJust"           : "TopCenter"
		"amParallelPosF"   : 0.5
		"amOrthogonalPosF" : -(1 - 0.89) + 0.018 + 0.015 ; Top - ht(txt0) - ht(txt1)
	end setvalues

	txt2 = create "line2" textItemClass wksHd
		"txFuncCode"	   : "~"
		"txString"	   : title3
		"txFontHeightF"	   : 0.010
		"txFont"	   : "helvetica"
	end create
	anno = NhlAddAnnotation(tmp, txt2)
	setvalues anno
		"amZone"           : 1
		"amJust"           : "TopCenter"
		"amSide"           : "Top"
		"amParallelPosF"   : 0.5
		"amOrthogonalPosF" : -(1 - 0.89) + 0.018 + 0.015 + 0.010 + 0.015
		                   ; Leave gap between 2nd & 3rd lines
	end setvalues

	draw(tmp)
	frame(wksHd)
	if(FMT .eq. "png") then
	    getvalues wksHd
	             "wkFileName" : fName
            end getvalues
	    print("Header filename is " +  fName)

	    delete(wksHd)     ; Mary Haley: delete wks to close the file
            cmd = CONVERT + " -trim " + fName + " +repage png8: " + fName + "&";
            system(cmd)
	end if
end
