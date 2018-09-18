;---------------------------------------------------------------------------------
;;; EXTRACT STRING FIELD FROM INPUT STRING (null returned if string field non-existent)
;;;    delimiter = delimiterchar (must be single character) OR CR OR EndOfLine  (first field = 1 !)

undef("extract_string_field")

function extract_string_field( instring:string,fieldno:integer,delimiterchar:string )
local instring,fieldno,delimiterchar, string_chars,instring_length, kk,kfield,kfirst,klast,carriagereturnchar
begin
	string_chars = stringtochar( instring )
	instring_length = dimsizes(stringtochar(instring))-1
	;4test: print ( "string_chars,instring_length= "+string_chars+","+instring_length )
	carriagereturnchar = systemfunc( "echo ''" )
	kk = 0
	kfirst = 0
	klast = -2
	kfield = 0
	do while ( kk .lt. instring_length )
		;4test: print ( "kk,kfield,kfirst,klast= "+kk+","+kfield+","+kfirst+","+klast )
		;;; treat delimiter or CarriageReturn
		if( string_chars(kk:kk) .eq. delimiterchar .or. string_chars(kk:kk) .eq. carriagereturnchar ) then
			kfield = kfield +1
			kfirst = klast +2
			klast = kk -1
			if ( kfield .eq. fieldno ) then
				break
			end if
		end if
		;;; treat end of line reached
		if( kk .eq. (instring_length -1) ) then
			kfield = kfield +1
			kfirst = klast +2
			klast = instring_length -1
			if ( kfield .eq. fieldno ) then
				break
			end if
		end if
		kk = kk +1
	end do
	;4test: print ( "END kk,kfield,kfirst,klast= "+kk+","+kfield+","+kfirst+"="+klast+"=" )
	if ( kfield .eq. fieldno ) then
		outstring = chartostring( string_chars(kfirst:klast) )
	else
		outstring = ""
	end if
return (outstring)
end

;---------------------------------------------------------------------------------
;;; STRING GREP FUNCTION - count existences of "pattern" in "instring" (=> 0=false)
;;; THIS COULD BE REPLACED BY str_index_of_substr() ???
undef("string_pattern_grep")
function string_pattern_grep( instring:string,pattern:string )
	local instring,pattern,count, string_chars,instring_length,pattern_length
	begin
	count = 0
	string_chars = stringtochar( instring )
	instring_length = dimsizes(stringtochar(instring))-1
	pattern_length = dimsizes(stringtochar(pattern))-1
	do kk=0,(instring_length-pattern_length)
		; bombs here with "subscript out of range" if pattern not found
		if( string_chars(kk:kk-1+pattern_length).eq.pattern ) then
			count = count + 1
		end if
	end do
	return (count)
end
;;;---------------------------------------------------------------------------------

;;; EXTRACT DOMAIN NUMBER, DATE, TIME FROM FILENAME
procedure getfiledate(filename, jday, gmthr, domainid )
begin
	filechars =  stringtochar( filename )
	string_length = dimsizes( filechars )-1
	do kk=0,(string_length-1-9)
		;old do kk=0,150
		; bombs here with "subscript out of range" if wrfout not found in filename
		if( filechars(kk:kk+5).eq."wrfout" ) then
			;;; use character to get integer value
			kdomain = chartointeger( filechars(kk+9:kk+9) ) - 48
			datestring = chartostring( filechars(kk+11:kk+20) )
			timestring = chartostring( filechars(kk+22:kk+29) )
			YYYY = extract_string_field( datestring,1,"-" )
			MM = extract_string_field( datestring,2,"-" )
			DD = extract_string_field( datestring,3,"-" )
			hh = extract_string_field( timestring,1,":" )
			mm = extract_string_field( timestring,2,":" )
			ss = extract_string_field( timestring,3,":" )
			break
		end if
	end do

        ;;; get julian day and gmt
        monthlength = (/ 0,31,28,31,30,31,30,31,31,30,31,30,31 /)
        jday = 0
        kmon = stringtoint( MM )
        do imon=1,(kmon-1)
                jday = jday + monthlength( imon )
        end do
        jday = jday + stringtoint(DD)
        ;;; treat leap year
        if( ( stringtoint(YYYY) % 4 ) .eq. 0 .and. kmon .ge. 2 ) then
                jday = jday + 1
        end if
        gmthr = stringtofloat(hh) + ( stringtofloat(mm) + (stringtofloat(ss)/60.0) ) / 60.0
        ;4test: print ( "MM= "+MM+"  jday= "+jday+"  gmthr= "+gmthr )
	;;; SET DOMAINID
	if ( string_pattern_grep(filename,"-WINDOW").eq.0 ) then
		domainid = "d"+kdomain
	else
		domainid = "w"+kdomain
	end if
end
;---------------------------------------------------------------------------------

;;; Print Names & Values of all Attributes

procedure printAtts(var)
begin
	print(var)
        atts = getvaratts(var)
        if(.not.any(ismissing(atts))) then
                do i=0,dimsizes(atts)-1
                        print("Attribute " + atts(i) + " = " + var@$atts(i)$)
                end do
        end if
end

;;; READ PARAMETER LIST
function getParams(params)
begin
	if( .not. ismissing(params) .and. params .ne. "" ) then
		string_chars = stringtochar( params )
		string_max  = dimsizes(string_chars)-2
		;;; set PARAMS array size based on number of parameter names
		PARAMS = new( (string_pattern_grep(params,":")+1), string )
		ichar1=0
		kname=-1
		do ichar=1,string_max
			; allow for either colon or no-colon at end of string
			if( string_chars(ichar).eq.":" .or. ichar.eq.string_max) then
				kname = kname +1
				if( ichar.eq.string_max) then
					ichar2 = string_max
				else
					ichar2 = ichar-1
				end if
			PARAMS(kname) = chartostring( string_chars(ichar1:ichar2) )
			ichar1= ichar+1
			end if
		end do
	else
                print("You must specify PARAMS")
		exit
	end if
	return(PARAMS)
end

;;;---------------------------------------------------------------------------------

;;; MAKE BORDER
;;; Draw Border 5 points inside plot area

function addBorder(wks, plot, alat, alon)
begin
	; Can't specify just coreners, because map is re-projected
	; So need to specify every (grid) point

        siz=dimsizes(alat)
        nx = siz(0)
        ny = siz(1)
	; To parameterize border width, use this instead
	; border = 5

        ; Ybl2tl = (/alat(border:nx-1-border,border)/)	; BottomLeft  -> TopLeft
        ; Xbl2tl = (/alon(border:nx-1-border,border)/)

        ; Ytl2tr = (/alat(nx-1-border,border:ny-1-border)/)	; TopLeft     -> TopRight
        ; Xtl2tr = (/alon(nx-1-border,border:ny-1-border)/)

        ; Ybr2tr = (/alat(border:nx-1-border,ny-1-border)/)	; BottomRight -> TopRight
        ; Xbr2tr = (/alon(border:nx-1-border,ny-1-border)/)

        ; Ybl2br = (/alat(border,border:ny-1-border)/)	; BottomLeft  -> BottomRight
        ; Xbl2br = (/alon(border,border:ny-1-border)/)

        Ybl2tl = (/alat(5:nx-6,5)/)	; BottomLeft  -> TopLeft
        Xbl2tl = (/alon(5:nx-6,5)/)

        Ytl2tr = (/alat(nx-6,5:ny-6)/)	; TopLeft     -> TopRight
        Xtl2tr = (/alon(nx-6,5:ny-6)/)

        Ybr2tr = (/alat(5:nx-6,ny-6)/)	; BottomRight -> TopRight
        Xbr2tr = (/alon(5:nx-6,ny-6)/)

        Ybl2br = (/alat(5,5:ny-6)/)	; BottomLeft  -> BottomRight
        Xbl2br = (/alon(5,5:ny-6)/)

	S1 = dimsizes(Xbl2tl)
	S2 = dimsizes(Xtl2tr)
	S3 = dimsizes(Xbr2tr)
	S4 = dimsizes(Xbl2br)
	X = new( S1 + S2 + S3 + S4, float)
	Y = new( S1 + S2 + S3 + S4, float)

	; Go round c/w from BL
	X(0:S1-1)                 = Xbl2tl
	X(S1:(S1+S2-1))           = Xtl2tr
	X(S1+S2:S1+S2+S3-1)       = Xbr2tr(::-1)
	X(S1+S2+S3:S1+S2+S3+S4-1) = Xbl2br(::-1)

	Y(0:S1-1)                 = Ybl2tl
	Y(S1:(S1+S2-1))           = Ytl2tr
	Y(S1+S2:S1+S2+S3-1)       = Ybr2tr(::-1)
	Y(S1+S2+S3:S1+S2+S3+S4-1) = Ybl2br(::-1)

        opts_border = True
	col = 254./255.
	tintindex = NhlNewColor(wks,col,col,col)  ; very pale grey
	; opts_border@gsLineColor = tintindex       ; But distiguishable from white
	; Doesn't work - Why? It used to!?

	opts_border@gsLineColor = "white"       ; But distiguishable from white

        opts_border@gsLineDashPattern = 1
        opts_border@gsLineThicknessF = 4.0
        ; opts_border@tfPolyDrawOrder = "PostDraw"

	ret  = gsn_add_polyline(wks, plot, X, Y, opts_border)
	; Need to keep return value, to keep lines alive
	return(ret)
end
;;;---------------------------------------------------------------------------------

;-----------------------------------------------------------------------------
;
; substring.ncl -- Extract a substring from another string.
;
; 2007-mar-05	Original version.  By Dave Allured.
;
; Usage:  outstr = substring (instr, first, last)
;
; Input:  instr = input string
;	  first = starting index of substring (zero based)
;	  last = ending index of substring (zero based); if last is
;	     less than first, the entire right substring is returned
;
; Output: outstr = selected substring
;
; General notes:
;
; Range checking is not done in this version.  Specifying "first" or
; "last" out of range will result in a non-elegant subscript error.
;
; Organization:
; University of Colorado, CIRES Climate Diagnostics Center (CDC)
; NOAA/ESRL/PSD, Climate Analysis Branch (CAB)
;
;----------------------------------------------------------------------------

function substring (instr[1]:string, first[1]:numeric, last[1]:numeric)

local instr, first, last, main, p2

begin
   main = stringtochar (instr)		; convert input string to char array
					; terminal null character is included   
   
   if (last .ge. first) then		; check requested end position
      p2 = last				; go to last position specified
   else					; but if less than first:
      p2 = dimsizes (main) - 2		; go to last avail char in main string
   end if
   
   return (chartostring (main(first:p2)))	; extract substring
end
;;;---------------------------------------------------------------------------------
