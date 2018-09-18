;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_Create" ogr-gfld-create) ogr-geom-field-defn-h
  "Create a new field geometry definition.

 This function is the same as the CPP method
 OGRGeomFieldDefn::OGRGeomFieldDefn().

 @argument[pszName]{the name of the new field definition.}
 @argument[eType]{the type of the new field definition.}

 @return{handle to the new field definition.}
 Since: GDAL 2.0"
  (pszName :string)
  (eType ogr-wkb-geometry-type))
(export 'ogr-gfld-create)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_Destroy" ogr-gfld-destroy) :void
  "Destroy a geometry field definition.

 @argument[hDefn]{handle to the geometry field definition to destroy.}

 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h))
(export 'ogr-gfld-destroy)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_GetNameRef" ogr-gfld-get-name-ref) :string
  "Fetch name of this field.

 This function is the same as the CPP method
 OGRGeomFieldDefn::GetNameRef().

 @argument[hDefn]{handle to the geometry field definition.}

 @return{the name of the geometry field definition.}
 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h))
(export 'ogr-gfld-get-name-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_GetSpatialRef" ogr-gfld-get-spatial-ref) ogr-spatial-reference-h
  "Fetch spatial reference system of this field.

 This function is the same as the C++ method
 OGRGeomFieldDefn::GetSpatialRef().

 @argument[hDefn]{handle to the geometry field definition}

 @return{field spatial reference system.}
 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h))
(export 'ogr-gfld-get-spatial-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_GetType" ogr-gfld-get-type) ogr-wkb-geometry-type
  "Fetch geometry type of this field.

 This function is the same as the CPP method
 OGRGeomFieldDefn::GetType().

 @argument[hDefn]{handle to the geometry field definition to get type from.}

 @return{field geometry type.}
 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h))
(export 'ogr-gfld-get-type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_IsIgnored" ogr-gfld-is-ignored) :int
  "Return whether this field should be omitted when fetching features.

 This method is the same as the C++ method
 OGRGeomFieldDefn::IsIgnored().

 @argument[hDefn]{handle to the geometry field definition}

 @return{ignore state}
 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h))
(export 'ogr-gfld-is-ignored)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_SetIgnored" ogr-gfld-set-ignored) :void
  "Set whether this field should be omitted when fetching features.

 This method is the same as the C++ method
 OGRGeomFieldDefn::SetIgnored().

 @argument[hDefn]{handle to the geometry field definition}
 @argument[ignore]{ignore state}

 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h)
  (ignore :int))
(export 'ogr-gfld-set-ignored)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_SetName" ogr-gfld-set-name) :void
  "Reset the name of this field.

 This function is the same as the CPP method
 OGRGeomFieldDefn::SetName().

 @argument[hDefn]{handle to the geometry field definition to apply the new name to.}
 @argument[pszName]{the new name to apply.}

 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h)
  (pszName :string))
(export 'ogr-gfld-set-name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_SetSpatialRef" ogr-gfld-set-spatial-ref) :void
  "Set the spatial reference of this field.

 This function is the same as the C++ method
 OGRGeomFieldDefn::SetSpatialRef().

 This function drops the reference of the previously set SRS object
 and acquires a new reference on the passed object (if non-NULL).

 @argument[hDefn]{handle to the geometry field definition}
 @argument[hSRS]{the new SRS to apply.}

 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h)
  (hSRS ogr-spatial-reference-h))
(export 'ogr-gfld-set-spatial-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_GFld_SetType" ogr-gfld-set-type) :void
  "Set the geometry type of this field. This should never be done to an
 OGRGeomFieldDefn that is already part of an OGRFeatureDefn.

 This function is the same as the CPP method
 OGRGeomFieldDefn::SetType().

 @argument[hDefn]{handle to the geometry field definition to set type to.}
 @argument[eType]{the new field geometry type.}

 Since: GDAL 2.0"
  (hDefn ogr-geom-field-defn-h)
  (eType ogr-wkb-geometry-type))
(export 'ogr-gfld-set-type)

;; EOF