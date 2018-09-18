;;; TO ALTER WIND PLOT SECTION
;;; (cannot contain procedures or functions)
;;;  for additional information, see "Set Fixed Contour Levels" at
;;;  http://www.drjack.info/twiki/bin/view/RASPop/PlotFormatParameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

opts_vc                           = True
; opts_vc@vcGlyphStyle              = "CurlyVector"     ; WindBarbs, rather than coloured Bars
opts_vc@vcGlyphStyle              = "LineArrow"     ; WindBarbs, rather than coloured Bars
opts_vc@vcMinDistanceF            = 0.007           ; Min Distance between arrows
opts_vc@vcMonoLineArrowColor      = True           ; Make arrows one colour (not colour of contour)
opts_vc@vcLineArrowHeadMinSizeF   = 0.002           ; Make arrows have uniform head size
opts_vc@vcLineArrowHeadMaxSizeF   = 0.002           ;
opts_vc@vcLineArrowThicknessF    = 1.1            ; Weight of arrows Line
opts_vc@vcLineArrowColor           = "magenta"      ; Set arrows colour
opts_vc@vcRefAnnoOn               = False          ; Remove Reference Arrow
opts_vc@vcRefLengthF              = 0.03          ; Length of reference arrow
opts_vc@gsnDraw                   = False
opts_vc@gsnFrame                  = False

  opts_ws@cnLevelSelectionMode = "ManualLevels"
  opts_ws@cnLevelSpacingF = 5.0
  opts_ws@cnMaxLevelValF = 50.0
  opts_ws@cnMinLevelValF = 5.0
  opts_ws@cnFillColors =   (/(/0.99, 0.99, 0.99, 1.00/), \   ; 0.00/),  \
   (/0.80, 0.80, 0.80, 1.00/), \   ; 0.13/),  \
   (/0.60, 0.83, 0.58, 1.00/), \   ; 0.38/),  \
   (/0.46, 0.93, 0.38, 1.00/), \   ; 0.44/),  \
   (/0.76, 1.00, 0.32, 1.00/), \   ; 0.50/),  \
   (/0.96, 0.91, 0.36, 1.00/), \   ; 0.57/),  \
   (/0.96, 0.49, 0.19, 1.00/), \   ; 0.69/),  \
   (/0.86, 0.20, 0.16, 1.00/), \   ; 0.82/),  \
   (/0.57, 0.08, 0.10, 1.00/), \   ; 1.00/)/)
   (/0.50, 0.00, 0.50, 1.00/)/)    ; 1.00/)/)