;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_AddPart" OGR_SM_AddPart) :int
  "Add a part (style tool) to the current style.

This function is the same as the C++ method OGRStyleMgr::AddPart().
 @argument[hSM]{handle to the style manager.}
 @argument[hST]{the style tool defining the part to add.}

 @return{TRUE on success, FALSE on errors.}"
  (hSM ogr-style-mgr-h)
  (hST ogr-style-tool-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_AddStyle" OGR_SM_AddStyle) :int
  "Add a style to the current style table.

This function is the same as the C++ method OGRStyleMgr::AddStyle().

 @argument[hSM]{handle to the style manager.}
 @argument[pszStyleName]{the name of the style to add.}
 @argument[pszStyleString]{the style string to use, or NULL to use the
 style stored in the manager.}

@return{TRUE on success, FALSE on errors."
  (hSM ogr-style-mgr-h)
  (pszStyleName :string)
  (pszStyleString :string))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_Create" OGR_SM_Create) ogr-style-mgr-h
  "OGRStyleMgr factory.

 @argument[hStyleTable]{pointer to OGRStyleTable or NULL if not
 working with a style table.}

 @return{an handle to the new style manager object.}"
  (hStyleTable ogr-style-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_Destroy" OGR_SM_Destroy) :void
  "Destroy Style Manager.

@argument[hSM]{handle to the style manager to destroy.}"
  (hSM ogr-style-mgr-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_GetPart" OGR_SM_GetPart) ogr-style-tool-h
  "Fetch a part (style tool) from the current style.

 This function instanciates a new object that should be freed with
 OGR_ST_Destroy().

 @argument[hSM]{handle to the style manager.}
 @argument[nPartId]{the part number (0-based index).}
 @argument[pszStyleString]{(optional) the style string on which to
 operate. If NULL then the current style string stored in the style
 manager is used.}

 @return{ogr-style-tool-h of the requested part (style tools) or NULL on error.}"
  (hSM ogr-style-mgr-h)
  (nPartId :int)
  (pszStyleString :string))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_GetPartCount" OGR_SM_GetPartCount) :int
  "Get the number of parts in a style.

 @argument[hSM]{handle to the style manager.}
 @argument[pszStyleString]{(optional) the style string on which to
 operate. If NULL then the current style string stored in the style
 manager is used.}

@return{the number of parts (style tools) in the style.}"
  (hSM ogr-style-mgr-h)
  (pszStyleString :string))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_InitFromFeature" OGR_SM_InitFromFeature) :string
  "Initialize style manager from the style string of a feature.

 @argument[hSM]{handle to the style manager.}
 @argument[hFeat]{handle to the new feature from which to read the style.}

 @return{a reference to the style string read from the feature, or
 NULL in case of error.}"
  (hSM ogr-style-mgr-h)
  (hFeat ogr-feature-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_SM_InitStyleString" OGR_SM_InitStyleString) :int
  "Initialize style manager from the style string.

 @argument[hSM]{handle to the style manager.}
 @argument[pszStyleString]{the style string to use (can be NULL).}

@return{TRUE on success, FALSE on errors.}"
  (hSM ogr-style-mgr-h)
  (pszStyleString :string))

;; EOF
