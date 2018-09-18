; Resources for BackGround Map
opts_bg                        = True
opts_bg@mpGridAndLimbOn        = False       ; No Lat/Lon lines 
; opts_bg@mpGridAndLimbOn        = True      
; opts_bg@mpGridLineThicknessF   = 2           ; Default = 1
; opts_bg@mpGridLineDashPattern  = 2           ; Lat/Lon lines as dashed (default = solid)
; opts_bg@mpGridSpacingF         = 1.0         ; At this spacing - if you have them
; opts_bg@mpGridLineColor        = "white"

opts_bg@pmTickMarkDisplayMode  = "NoCreate"  ; Don't create tickmarks

;opts_bg@mpDataBaseVersion      = "HighRes"   ; Use RANGS coastline
;opts_bg@mpDataResolution       = "Finest"    ; At best resolution
opts_bg@mpOutlineOn            = False        ; Draw continental outlines
;opts_bg@mpGeophysicalLineThicknessF = 2.0    ; Thickness of Land-Sea Boundary
opts_bg@mpFillOn               = False       ; Don't fill land

opts_bg@gsnMaximize            = True        ; Fill whole plot area - *either* Width *or* Height
opts_bg@gsnBoxMargin           = 0.0         ; Remove top space

opts_bg@pmTitleDisplayMode     = "NoCreate"     ; Don't create the Title

opts_bg@gsnDraw                = False                    ; Draw later, after plot is added
opts_bg@gsnFrame               = False

;;; Make display fill height (or width?)
;   Note that plot is always square (This cannot be changed)
;   Blank space L&R if map is "portrait", or T&B if "landscape"
opts_bg@vpHeightF              = 1.0
opts_bg@vpWidthF               = 1.0
opts_bg@vpXF                   = 0.0
opts_bg@vpYF                   = 1.0

; opts_bg@lbLabelBarOn          = False       ; Comment out for labelBar on Main Plot

