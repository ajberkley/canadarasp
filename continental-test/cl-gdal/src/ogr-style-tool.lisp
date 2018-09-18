;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_Create" OGR_ST_Create) ogr-style-tool-h
  "OGRStyleTool factory.

This function is a constructor for OGRStyleTool derived classes.

 @argument[eClassId]{subclass of style tool to create. One of
 OGRSTCPen (1), OGRSTCBrush (2), OGRSTCSymbol (3) or OGRSTCLabel (4).}

 @return{an handle to the new style tool object or NULL if the creation failed.}"
  (eClassId ogr-st-class-id))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_Destroy" OGR_ST_Destroy) :void
  "Destroy Style Tool.

@argument[hST]{handle to the style tool to destroy.}"
  (hST ogr-style-tool-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_GetParamDbl" OGR_ST_GetParamDbl) :double
  "Get Style Tool parameter value as a double.

Maps to the OGRStyleTool subclasses' GetParamDbl() methods.

 @argument[hST]{handle to the style tool.}
 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[bValueIsNull]{pointer to an integer that will be set to
 TRUE or FALSE to indicate whether the parameter value is NULL.}

@return{the parameter value as double and sets bValueIsNull.}"
  (hST ogr-style-tool-h)
  (eParam :int)
  (bValueIsNull (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_GetParamNum" OGR_ST_GetParamNum) :int
  "Get Style Tool parameter value as an integer.

Maps to the OGRStyleTool subclasses' GetParamNum() methods.

 @argument[hST]{handle to the style tool.}
 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[bValueIsNull]{pointer to an integer that will be set to
 TRUE or FALSE to indicate whether the parameter value is NULL.}

 @return{the parameter value as integer and sets bValueIsNull.}"
  (hST ogr-style-tool-h)
  (eParam :int)
  (bValueIsNull (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_GetParamStr" OGR_ST_GetParamStr) :string
  "Get Style Tool parameter value as string.

Maps to the OGRStyleTool subclasses' GetParamStr() methods.

 @argument[hST]{handle to the style tool.}
 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[bValueIsNull]{pointer to an integer that will be set to
 TRUE or FALSE to indicate whether the parameter value is NULL.}

@return{the parameter value as string and sets bValueIsNull.}"
  (hST ogr-style-tool-h)
  (eParam :int)
  (bValueIsNull (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_GetRGBFromString" OGR_ST_GetRGBFromString) :int

  "Return the r,g,b,a components of a color encoded in #RRGGBB[AA] format.

Maps to OGRStyleTool::GetRGBFromString().

 @argument[hST]{handle to the style tool.}
 @argument[pszColor]{the color to parse}
 @argument[pnRed]{pointer to an int in which the red value will be returned}
 @argument[pnGreen]{pointer to an int in which the green value will be returned}
 @argument[pnBlue]{pointer to an int in which the blue value will be returned}
 @argument[pnAlpha]{pointer to an int in which the (optional) alpha
 value will be returned}

@return{TRUE if the color could be succesfully parsed, or FALSE in case of errors.}"
  (hST ogr-style-tool-h)
  (pszColor :string)
  (pnRed (:pointer :int))
  (pnGreen (:pointer :int))
  (pnBlue (:pointer :int))
  (pnAlpha (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_GetStyleString" OGR_ST_GetStyleString) :string
  "Get the style string for this Style Tool.

Maps to the OGRStyleTool subclasses' GetStyleString() methods.

@argument[hST]{handle to the style tool.}

@return{the style string for this style tool or \"\" if the hST is invalid.}"
  (hST ogr-style-tool-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_GetType" OGR_ST_GetType) ogr-st-class-id
  "Determine type of Style Tool.
@argument[hST]{handle to the style tool.}

 @return{the style tool type, one of OGRSTCPen (1), OGRSTCBrush (2),
 OGRSTCSymbol (3) or OGRSTCLabel (4). Returns OGRSTCNone (0) if the
 ogr-style-tool-h is invalid.}"
  (hST ogr-style-tool-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_GetUnit" OGR_ST_GetUnit) ogr-st-unit-id
  "Get Style Tool units.

@argument[hST]{handle to the style tool.}

@return{the style tool units.}"
  (hST ogr-style-tool-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_SetParamDbl" OGR_ST_SetParamDbl) :void
  "Set Style Tool parameter value from a double.

Maps to the OGRStyleTool subclasses' SetParamDbl() methods.

 @argument[hST]{handle to the style tool.}
 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[dfValue]{the new parameter value}"
  (hST ogr-style-tool-h)
  (eParam :int)
  (dfValue :double))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_SetParamNum" OGR_ST_SetParamNum) :void
  "Set Style Tool parameter value from an integer.

Maps to the OGRStyleTool subclasses' SetParamNum() methods.

 @argument[hST]{handle to the style tool.}
 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[nValue]{the new parameter value}"
  (hST ogr-style-tool-h)
  (eParam :int)
  (nValue :int))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_SetParamStr" OGR_ST_SetParamStr) :void
  "Set Style Tool parameter value from a string.

Maps to the OGRStyleTool subclasses' SetParamStr() methods.

 @argument[hST]{handle to the style tool.}
 @argument[eParam]{the parameter id from the enumeration corresponding
 to the type of this style tool (one of the OGRSTPenParam,
 OGRSTBrushParam, OGRSTSymbolParam or OGRSTLabelParam enumerations)}
 @argument[pszValue]{the new parameter value}"
  (hST ogr-style-tool-h)
  (eParam :int)
  (pszValue :string))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_ST_SetUnit" ogr-st-setunit) :void
  "Set Style Tool units.

This function is the same as OGRStyleTool::SetUnit()

 @argument[hST]{handle to the style tool.}
 @argument[eUnit]{the new unit.}
 @argument[dfGroundPaperScale]{ground to paper scale factor.}"
  (hST ogr-style-tool-h)
  (eUnit ogr-st-unit-id)
  (dfGroundPaperScale :double))

;; EOF
