;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_Create" OGR_STBL_Create) ogr-style-table-h
  "OGRStyleTable factory.

This function is the same as the C++ method OGRStyleTable::OGRStyleTable().
@return{an handle to the new style table object.}"
  )

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_Destroy" OGR_STBL_Destroy) :void
  "Destroy Style Table.
@argument[hSTBL]{handle to the style table to destroy.}"
  (hSTBL ogr-style-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_Find" OGR_STBL_Find) :string
  "Get a style string by name.

This function is the same as the C++ method OGRStyleTable::Find().

 @argument[hStyleTable]{handle to the style table.}
 @argument[pszName]{the name of the style string to find.}

 @return{the style string matching the name or NULL if not found or error.}"
  (hStyleTable ogr-style-table-h)
  (pszName :string))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_GetLastStyleName" OGR_STBL_GetLastStyleName) :string
  "Get the style name of the last style string fetched with
 OGR_STBL_GetNextStyle.

 This function is the same as the C++ method
 OGRStyleTable::GetStyleName().

@argument[hStyleTable]{handle to the style table.}

@return{the Name of the last style string or NULL on error.}"
  (hStyleTable ogr-style-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_GetNextStyle" OGR_STBL_GetNextStyle) :string
  "Get the next style string from the table.

This function is the same as the C++ method OGRStyleTable::GetNextStyle().
@argument[hStyleTable]{handle to the style table.}

@return{the next style string or NULL on error.}"
  (hStyleTable ogr-style-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_LoadStyleTable" OGR_STBL_LoadStyleTable) :int
  "Load a style table from a file.

 @argument[hStyleTable]{handle to the style table.}
 @argument[pszFilename]{the name of the file to load from.}

@return{TRUE on success, FALSE on error}"
  (hStyleTable ogr-style-table-h)
  (pszFilename :string))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_ResetStyleStringReading" OGR_STBL_ResetStyleStringReading) :void
  "Reset the next style pointer to 0.

@argument[hStyleTable]{handle to the style table.}"
  (hStyleTable ogr-style-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_STBL_SaveStyleTable" OGR_STBL_SaveStyleTable) :int
  "Save a style table to a file.

 @argument[hStyleTable]{handle to the style table.}
 @argument[pszFilename]{the name of the file to save to.}

@return{TRUE on success, FALSE on error}"
  (hStyleTable ogr-style-table-h)
  (pszFilename :string))

;; EOF
