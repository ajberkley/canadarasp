;
;        Plotting options for wind with barbs
;
; Resources for the Contour Plot are read from opts_cn.ncl

; Options for the WindBarbs

opts_vc                           = True
opts_vc@vcGlyphStyle              = "WindBarb"     ; WindBarbs, rather than coloured Bars
opts_vc@vcMinDistanceF            = 0.01           ; Min Distance between Barbs - 100 across
opts_vc@vcMonoLineArrowColor      = True           ; Make Barbs one colour (not colour of contour)
opts_vc@vcWindBarbColor           = "magenta"      ; Set Barb colour
opts_vc@vcRefAnnoOn               = False          ; Remove Reference Arrow
opts_vc@vcRefLengthF              = 0.01           ; Length of WindBarb - should be smaller than vcMinDistanceF
opts_vc@vcWindBarbLineThicknessF  = 1.0            ; Weight of Barb Line
opts_vc@gsnDraw                   = False
opts_vc@gsnFrame                  = False

