;;; Environment variables
;;;  ENV_NCL_FILENAME: the file we should open and process, should have all important variables in it
;;;  ENV_NCL_OUTDIR: the directory we should write out map files to
;;;  FMT: set to "png" for output files, could set to "x11" to view I think
;;   PARAMS: a colon separated list of parameters to calculate
;;; Things you might change here
;;;  img_height / img_width currently 1024
;;; AJB - developed from ...
;;; PAULS - developed from ...
;;; JACK - altered from http://www.ncl.ucar.edu/Support/talk_archives/2006/1026.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Determine Version of NCL we are running
nclv = get_ncl_version()                  ; returns string like 6.0.0 or 5.2.1
nclv_c = stringtochar(nclv)               ; create array of chars
vnum=stringtoint(chartostring(nclv_c(0))) ; turn first char turns into an int

;;; Load NCL scripts
;;; Must use loadscript() - not load() - as in a conditional Load Order is significant
if(vnum .lt. 6) then
	print("THIS CODE REQUIRES NCL VERSION HIGHER THAN 6, EXITING")
	exit
else
        loadscript("$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl")
	loadscript("$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl")
	loadscript("$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl")
end if

;;; *****************************************************************************************
;;; GLOBAL VARIABLES
;;; NB: Global variables MUST appear before any Functions / Procedures in which they are used
;;;     Beware: due to Call By Reference, the func/proc can change value(s)

;;; Constants for the Whole Run

CONVERT = "convert"
Files   = getenv("ENV_NCL_FILENAME")
OutDir  = getenv("ENV_NCL_OUTDIR")

if(ismissing(OutDir)) then
	print("WARNING: No ENV_NCL_OUTDIR")
	print("         Using '.' for OutDir")
	print("         Export ENV_NCL_OUTDIR to change")
	OutDir = "."
else
	print("Output files in " + OutDir)
end if
OutDir = OutDir + "/"

;;;; Get Output Type
;;   Options are "x11", "ncgm" or "png"
FMT = getenv("FMT")
if(ismissing(FMT)) then
	FMT = "png"
end if

; Specify Image Width and Height in Pixels
img_height = 1024
img_width = 1024
wkHeight = img_height
wkWidth = img_width

;; Other constants
time = 0	; Seems time is always 0 for DrJack's code
cdbl = 0.003	; Coefficient of Drag for Boundary Layer
cwbasecriteria = 0.000010	; Cloud Water criterion

UNITS = "metric"

opts_ter                 = True
; opts_ter@cnLinesOn       = True  ;; add terrain contour lines to the maps
; opts_ter@cnLineLabelsOn =  False ;; don't add terrain contour line labels

;;; Load our scripts order significant :-(

load "utils.ncl"
load "calc_funcs.ncl"
load "colourlevels.ncl"
load "wks.ncl"
load "labelbar.ncl"
load "header.ncl"
load "plot_hrdps_continental.ncl"

regionData = True
regionData@blsectionztop = 6.   ; in Km
UNITS = "metric"

ParamList = getParams( getenv("ENV_NCL_PARAMS"))
; print("ENV_NCL_PARAMS:")
; do p = 0,dimsizes(ParamList)-1
; 	print("    " + ParamList(p))
; end do
;****************************************************************
startTime = stringtolong(systemfunc("date +%s%2N"))

do fnum = 0, dimsizes(Files)-1
	print("*** Doing " + Files(fnum))

	plot_parameters(Files(fnum), ParamList)
  print("")
	print("*** Done " + Files(fnum))
end do
delta = stringtolong(systemfunc("date +%s%2N")) - startTime
print("Plots took " + delta/100.0 + "s")
print(" ")
print("NORMAL END of hrdps2gm-continental.ncl")
print("")

quit
