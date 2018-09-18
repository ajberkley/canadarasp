;;;; Resources for Contours with Streamlines

opts_st                    = True
opts_st@stLineThicknessF   = 2.0          ; set line width
opts_st@stLineColor        = "magenta"    ; set line color
;;; for streamline color not in color map
; tintindex = NhlNewColor(wks,0.3,0.3,0.3)  ; gray
; opts_cn@stLineColor = tintindex

;;; set line spacing
opts_st@stMinDistanceF     = 0.02         ; prevent too-dense spacing for grids with very many grid points

;;; set arrow params
opts_st@stArrowStride      = 4          ; default=2
opts_st@stArrowLengthF     = 0.00625
opts_st@stMinArrowSpacingF = 0.05
opts_st@stMinLineSpacingF  = 0.003
opts_st@gsnDraw            = False
opts_st@gsnFrame           = False


