;
;        Plotting options for pressure level vertical velocity
;
; Resources for the Contour Plot are read from opts_cn.ncl

; Options for the WindBarbs

opts_vc                           = True
opts_vc@vcGlyphStyle              = "WindBarb"     ; WindBarbs, rather than coloured Bars
opts_vc@vcMinDistanceF            = 0.03           ; Min Distance between Barbs
opts_vc@vcMonoLineArrowColor      = True           ; Make Barbs one colour (not colour of contour)
opts_vc@vcWindBarbColor           = "navy blue"    ; Set Barb colour
opts_vc@vcRefAnnoOn               = False          ; Remove Reference Arrow
opts_vc@vcRefLengthF              = 0.02           ; Length of WindBarb
opts_vc@vcWindBarbLineThicknessF  = 4              ; Weight of Barb Line
opts_vc@gsnDraw                   = False
opts_vc@gsnFrame                  = False

