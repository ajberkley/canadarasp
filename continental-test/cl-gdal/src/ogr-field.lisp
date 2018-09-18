;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_Create" ogr-fld-create) ogr-field-defn-h
  "Create a new field definition.

 This function is the same as the CPP method
 OGRFieldDefn::OGRFieldDefn().

 @argument[pszName]{the name of the new field definition.}
 @argument[eType]{the type of the new field definition.}

 @return{handle to the new field definition.}"
  (pszName :string)
  (eType ogr-field-type))
(export 'ogr-fld-create)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_Destroy" ogr-fld-destroy) :void
  "Destroy a field definition.

 @argument[hDefn]{handle to the field definition to destroy.}"
  (hDefn ogr-field-defn-h))
(export 'ogr-fld-destroy)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_GetJustify" ogr-fld-get-justify) ogr-justification
  "Get the justification for this field.

 This function is the same as the CPP method
 OGRFieldDefn::GetJustify().

 @argument[hDefn]{handle to the field definition to get justification
 from.}

@return{the justification.}"
  (hDefn ogr-field-defn-h))
(export 'ogr-fld-get-justify)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_GetNameRef" ogr-fld-get-name-ref) :string
  "Fetch name of this field.

 This function is the same as the CPP method
 OGRFieldDefn::GetNameRef().

 @argument[hDefn]{handle to the field definition.}

 @return{the name of the field definition.}"
  (hDefn ogr-field-defn-h))
(export 'ogr-fld-get-name-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_GetPrecision" ogr-fld-get-precision) :int
  "Get the formatting precision for this field. This should normally be
 zero for fields of types other than OFTReal.

 This function is the same as the CPP method
 OGRFieldDefn::GetPrecision().

 @argument[hDefn]{handle to the field definition to get precision from.}

 @return{the precision.}"
  (hDefn ogr-field-defn-h))
(export 'ogr-fld-get-precision)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_GetWidth" ogr-fld-get-width) :int
  "Get the formatting width for this field.

 This function is the same as the CPP method OGRFieldDefn::GetWidth().

 @argument[hDefn]{handle to the field definition to get width from.}

 @return{the width, zero means no specified width.}"
  (hDefn ogr-field-defn-h))
(export 'ogr-fld-get-width)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_IsIgnored" ogr-fld-is-ignored) :int
  "Return whether this field should be omitted when fetching features.

 This method is the same as the C++ method OGRFieldDefn::IsIgnored().

 @argument[hDefn]{handle to the field definition}

 @return{ignore state}"
  (hDefn ogr-field-defn-h))
(export 'ogr-fld-is-ignored)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_Set" ogr-fld-set) :void
  "Set defining parameters for a field in one call.

This function is the same as the CPP method OGRFieldDefn::Set().

 @argument[hDefn]{handle to the field definition to set to.}
 @argument[pszNameIn]{the new name to assign.}
 @argument[eTypeIn]{the new type (one of the OFT values like OFTInteger).}
 @argument[nWidthIn]{the preferred formatting width. Defaults to zero indicating undefined.}
 @argument[nPrecisionIn]{number of decimals places for formatting, defaults to zero indicating undefined.}
 @argument[eJustifyIn]{the formatting justification (OJLeft or OJRight), defaults to OJUndefined.}"
  (hDefn ogr-field-defn-h)
  (pszNameIn :string)
  (eTypeIn ogr-field-type)
  (nWidthIn :int)
  (nPrecisionIn :int)
  (eJustifyIn ogr-justification))
(export 'ogr-fld-set)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_SetIgnored" ogr-fld-set-ignored) :void
  "Set whether this field should be omitted when fetching features.

 This method is the same as the C++ method OGRFieldDefn::SetIgnored().

 @argument[hDefn]{handle to the field definition}
 @argument[ignore]{ignore state}"
  (hDefn ogr-field-defn-h)
  (ignore :int))
(export 'ogr-fld-set-ignored)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_SetJustify" ogr-fld-set-justify) :void
  "Set the justification for this field.

 This function is the same as the CPP method
 OGRFieldDefn::SetJustify().

 @argument[hDefn]{handle to the field definition to set justification to.}
 @argument[eJustify]{the new justification.}"
  (hDefn ogr-field-defn-h)
  (eJustify ogr-justification))
(export 'ogr-fld-set-justify)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_SetName" ogr-fld-set-name) :void
  "Reset the name of this field.

 This function is the same as the CPP method OGRFieldDefn::SetName().

 @argument[hDefn]{handle to the field definition to apply the new name to.}
 @argument[pszName]{the new name to apply.}"
  (hDefn ogr-field-defn-h)
  (pszName :string))
(export 'ogr-fld-set-name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_SetPrecision" ogr-fld-set-precision) :void
  "Set the formatting precision for this field in characters.

This should normally be zero for fields of types other than OFTReal.

This function is the same as the CPP method OGRFieldDefn::SetPrecision().

 @argument[hDefn]{handle to the field definition to set precision to.}
 @argument[nPrecision]{the new precision.}"
  (hDefn ogr-field-defn-h)
  (nPrecision :int))
(export 'ogr-fld-set-precision)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_SetType" ogr-fld-set-type) :void
  "Set the type of this field. This should never be done to an
 OGRFieldDefn that is already part of an OGRFeatureDefn.

 This function is the same as the CPP method OGRFieldDefn::SetType().

 @argument[hDefn]{handle to the field definition to set type to.}
 @argument[eType]{the new field type.}"
  (hDefn ogr-field-defn-h)
  (eType ogr-field-type))
(export 'ogr-fld-set-type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_SetWidth" ogr-fld-set-width) :void
  "Set the formatting width for this field in characters.

 This function is the same as the CPP method OGRFieldDefn::SetWidth().

 @argument[hDefn]{handle to the field definition to set width to.}
 @argument[nNewWidth]{the new width.}"
  (hDefn ogr-field-defn-h)
  (nNewWidth :int))
(export 'ogr-fld-set-width)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Fld_GetType" ogr-fld-get-type) :int
  "Fetch type of this field.

 This function is the same as the CPP method OGRFieldDefn::GetType().

 @argument[hDefn]{handle to the field definition to get type from.}

 @return{ogr-field-type field type.}"
  (hDefn ogr-field-defn-h))
(export 'ogr-fld-get-type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GetFieldTypeName" ogr-get-field-type-name) :string
  "Fetch human readable name for a field type.

 This function is the same as the CPP method OGRFieldDefn::GetFieldTypeName().

 @argument[eType]{the field type to get name for.}

 @return{the name.}"
  (eType ogr-field-type))
(export 'ogr-get-field-type-name)

;; EOF