;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetFieldCount" ogr-fd-get-field-count) :int
  "Fetch number of fields on the passed feature definition.

 This function is the same as the C++ OGRFeatureDefn::GetFieldCount().

 @argument[hDefn]{handle to the feature definition to get the fields
 count from.}

 @return{count of fields.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-get-field-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetFieldDefn" ogr-fd-get-field-defn) :pointer
  "Fetch field definition of the passed feature definition.

 This function is the same as the C++ method
 OGRFeatureDefn::GetFieldDefn().

 Starting with GDAL 1.7.0, this method will also issue an error if the
 index is not valid.

 @argument[hDefn]{handle to the feature definition to get the field
 definition from.}
 @argument[iField]{the field to fetch, between 0 and GetFieldCount()-1.}

 @return{ogr-field-defn-h an handle to an internal field definition
 object or NULL if invalid index. This object should not be modified
 or freed by the application.}"
  (hDefn ogr-feature-defn-h)
  (iField :int))
(export 'ogr-fd-get-field-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_AddFieldDefn" ogr-fd-add-field-defn) :void
  "Add a new field definition to the passed feature definition.

 To add a new field definition to a layer definition, do not use this
 function directly, but use OGR_L_CreateField() instead.

 This function should only be called while there are no OGRFeature
 objects in existance based on this OGRFeatureDefn. The OGRFieldDefn
 passed in is copied, and remains the responsibility of the caller.

 This function is the same as the C++ method
 OGRFeatureDefn::AddFieldDefn().

 @argument[hDefn]{handle to the feature definition to add the field
 definition to.}

 @argument[hNewField]{handle to the new field definition.}"
  (hDefn ogr-feature-defn-h)
  (hNewField ogr-field-defn-h))
(export 'ogr-fd-add-field-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_AddGeomFieldDefn" ogr-fd-add-geom-field-defn) :void
  "Add a new field definition to the passed feature definition.

 To add a new field definition to a layer definition, do not use this
 function directly, but use OGR_L_CreateGeomField() instead.

 This function should only be called while there are no OGRFeature
 objects in existance based on this OGRFeatureDefn. The
 OGRGeomFieldDefn passed in is copied, and remains the responsibility
 of the caller.

 This function is the same as the C++ method
 OGRFeatureDefn::AddGeomFieldDefn().

 @argument[hDefn]{handle to the feature definition to add the geometry
 field definition to.}

 @argument[hNewGeomField]{handle to the new field definition.}

 Since: GDAL 2.0"
  (hDefn ogr-feature-defn-h)
  (hNewGeomField OGR-Geom-Field-Defn-H))
(export 'ogr-fd-add-geom-field-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_Create" ogr-fd-create) ogr-feature-defn-h
  "Create a new feature definition object to hold the field definitions.

 The OGRFeatureDefn maintains a reference count, but this starts at
 zero, and should normally be incremented by the owner.

 This function is the same as the C++ method
 OGRFeatureDefn::OGRFeatureDefn().

 @argument[pszName]{the name to be assigned to this layer/class. It
 does not need to be unique.}

 @return{handle to the newly created feature definition.}"
  (pszName :string))
(export 'ogr-fd-create)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_DeleteFieldDefn" ogr-fd-delete-field-defn) ogr-err
  "Delete an existing field definition.

 To delete an existing field definition from a layer definition, do
 not use this function directly, but use OGR_L_DeleteField() instead.

 This method should only be called while there are no OGRFeature
 objects in existance based on this OGRFeatureDefn.

 This method is the same as the C++ method
 OGRFeatureDefn::DeleteFieldDefn().

 @argument[hDefn]{handle to the feature definition.}
 @argument[iField]{the index of the field defintion.}

 @return{OGRERR_NONE in case of success.}

Since: OGR 1.9.0"
  (hDefn ogr-feature-defn-h)
  (iField :int))
(export 'ogr-fd-delete-field-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_DeleteGeomFieldDefn" ogr-fd-delete-geom-field-defn) ogr-err
  "Delete an existing geometry field definition.

 To delete an existing geometry field definition from a layer
 definition, do not use this function directly, but use
 OGR_L_DeleteGeomField() instead (*not implemented yet*)

 This method should only be called while there are no OGRFeature
 objects in existance based on this OGRFeatureDefn.

 This method is the same as the C++ method
 OGRFeatureDefn::DeleteGeomFieldDefn().

 @argument[hDefn]{handle to the feature definition.}
 @argument[iGeomField]{the index of the geometry field defintion.}

 @return{OGRERR_NONE in case of success.}

 Since: GDAL 2.0"
  (hDefn ogr-feature-defn-h)
  (iGeomField :int))
(export 'ogr-fd-delete-geom-field-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_Dereference" ogr-fd-dereference) :int
  "Decrements the reference count by one.

 This function is the same as the C++ method
 OGRFeatureDefn::Dereference().

 @argument[hDefn]{handle to the feature definition on witch OGRFeature
 are based on.}

 @return{the updated reference count.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-dereference)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_Destroy" ogr-fd-destroy) :void
  "Destroy a feature definition object and release all memory associated
 with it.

 This function is the same as the C++ method
 OGRFeatureDefn::~OGRFeatureDefn().

 @argument[hDefn]{handle to the feature definition to be destroyed.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-destroy)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetFieldIndex" ogr-fd-get-field-index) :int
  "Find field by name.

 The field index of the first field matching the passed field
 name (case insensitively) is returned.

 This function is the same as the C++ method
 OGRFeatureDefn::GetFieldIndex.

 @argument[hDefn]{handle to the feature definition to get field index
 from.}
 @argument[pszFieldName]{the field name to search for.}

 @return{the field index, or -1 if no match found.}"
  (hDefn ogr-feature-defn-h)
  (pszFieldName :string))
(export 'ogr-fd-get-field-index)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetGeomFieldCount" ogr-fd-get-geom-field-count) :int
  "Fetch number of geometry fields on the passed feature definition.

 This function is the same as the C++ OGRFeatureDefn::GetGeomFieldCount().

 @argument[hDefn]{handle to the feature definition to get the fields
 count from.}

 @return{count of geometry fields.}

 Since: GDAL 2.0"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-get-geom-field-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetGeomFieldDefn" ogr-fd-get-geom-field-defn) ogr-geom-field-defn-h
  "Fetch geometry field definition of the passed feature definition.

 This function is the same as the C++ method
 OGRFeatureDefn::GetGeomFieldDefn().

 @argument[hDefn]{handle to the feature definition to get the field
 definition from.}
 @argument[iGeomField]{the geometry field to fetch, between 0 and
 GetGeomFieldCount()-1.}

 @return{an handle to an internal field definition object or NULL if
 invalid index. This object should not be modified or freed by the
 application.}

 Since: GDAL 2.0"
  (hDefn ogr-feature-defn-h)
  (iGeomField :int))
(export 'ogr-fd-get-geom-field-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetGeomFieldIndex" ogr-fd-get-geom-field-index) :int
  "Find geometry field by name.

 The geometry field index of the first geometry field matching the
 passed field name (case insensitively) is returned.

 This function is the same as the C++ method
 OGRFeatureDefn::GetGeomFieldIndex.

 @argument[hDefn]{handle to the feature definition to get field index
 from.}
 @argument[pszGeomFieldName]{the geometry field name to search for.}

 @return{the geometry field index, or -1 if no match found.}"
  (hDefn ogr-feature-defn-h)
  (pszGeomFieldName :string))
(export 'ogr-fd-get-geom-field-index)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetGeomType" ogr-fd-get-geom-type) ogr-wkb-geometry-type
  "Fetch the geometry base type of the passed feature definition.

 This function is the same as the C++ method
 OGRFeatureDefn::GetGeomType().

 Starting with GDAL 2.0, this method returns
 GetGeomFieldDefn(0)->GetType().

 @argument[hDefn]{handle to the feature definition to get the geometry
 type from.}

 @return{the base type for all geometry related to this definition.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-get-geom-type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetName" ogr-fd-get-name) :string
  "Get name of the OGRFeatureDefn passed as an argument.

 This function is the same as the C++ method
 OGRFeatureDefn::GetName().

 @argument[hDefn]{handle to the feature definition to get the name
 from.}

 @return{the name. This name is internal and should not be modified,
 or freed.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-get-name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_GetReferenceCount" ogr-fd-get-reference-count) :int
  "Fetch current reference count.

 This function is the same as the C++ method
 OGRFeatureDefn::GetReferenceCount().

 @argument[hDefn]{handle to the feature definition on witch OGRFeature
 are based on.}

 @return{the current reference count.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-get-reference-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_IsGeometryIgnored" ogr-fd-is-geometry-ignored) :int
  "Determine whether the geometry can be omitted when fetching features.

 This function is the same as the C++ method
 OGRFeatureDefn::IsGeometryIgnored().

 Starting with GDAL 2.0, this method returns
 GetGeomFieldDefn(0)->IsIgnored().

 @argument[hDefn]{handle to the feature definition on witch OGRFeature
 are based on.}

 @return{ignore state}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-is-geometry-ignored)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_IsSame" ogr-fd-is-same) :int
  "Test if the feature definition is identical to the other one.

 @argument[hFDefn]{handle to the feature definition on witch
 OGRFeature are based on.}
 @argument[hOtherFDefn]{handle to the other feature definition to
 compare to.}

 @return{TRUE if the feature definition is identical to the other
 one.}

 Since: OGR 2.0"
  (hFDefn ogr-feature-defn-h)
  (hOtherFDefn ogr-feature-defn-h))
(export 'ogr-fd-is-same)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_IsStyleIgnored" ogr-fd-is-style-ignored) :int
  "Determine whether the style can be omitted when fetching features.

 This function is the same as the C++ method
 OGRFeatureDefn::IsStyleIgnored().

 @argument[hDefn]{handle to the feature definition on which OGRFeature
 are based on.}

 @return{ignore state}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-is-style-ignored)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_Reference" ogr-fd-reference) :int
  "Increments the reference count by one.

 The reference count is used keep track of the number of OGRFeature
 objects referencing this definition.

 This function is the same as the C++ method
 OGRFeatureDefn::Reference().

 @argument[hDefn]{handle to the feature definition on witch OGRFeature
 are based on.}

 @return{the updated reference count.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-reference)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_Release" ogr-fd-release) :void
  "Drop a reference, and destroy if unreferenced.

 This function is the same as the C++ method
 OGRFeatureDefn::Release().

 @argument[hDefn]{handle to the feature definition to be released.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-fd-release)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_SetGeometryIgnored" ogr-fd-set-geometry-ignored) :void
  "Set whether the geometry can be omitted when fetching features.

 This function is the same as the C++ method
 OGRFeatureDefn::SetGeometryIgnored().

 Starting with GDAL 2.0, this method calls
 GetGeomFieldDefn(0)->SetIgnored().

 @argument[hDefn]{handle to the feature definition on witch OGRFeature
 are based on.}
 @argument[bIgnore]{ignore state}"
  (hDefn ogr-feature-defn-h)
  (bIgnore :int))
(export 'ogr-fd-set-geometry-ignored)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_SetGeomType" ogr-fd-set-geom-type) :void
  "Assign the base geometry type for the passed layer (the same as the
 feature definition).

 All geometry objects using this type must be of the defined type or a
 derived type. The default upon creation is wkbUnknown which allows
 for any geometry type. The geometry type should generally not be
 changed after any OGRFeatures have been created against this
 definition.

 This function is the same as the C++ method
 OGRFeatureDefn::SetGeomType().

 Starting with GDAL 2.0, this method calls
 GetGeomFieldDefn(0)->SetType().

 @argument[hDefn]{handle to the layer or feature definition to set the
 geometry type to.}
 @argument[eType]{the new type to assign.}"
  (hDefn ogr-feature-defn-h)
  (eType ogr-wkb-geometry-type))
(export 'ogr-fd-set-geom-type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_FD_SetStyleIgnored" ogr-fd-set-style-ignored) :void
  "Set whether the style can be omitted when fetching features.

 This function is the same as the C++ method
 OGRFeatureDefn::SetStyleIgnored().

 @argument[hDefn]{handle to the feature definition on witch OGRFeature
 are based on.}
 @argument[bIgnore]{ignore state}"
  (hDefn ogr-feature-defn-h)
  (bIgnore :int))
(export 'ogr-fd-set-style-ignored)

;; --------------------------------------------------------
;; CLOS
;; --------------------------------------------------------

#+todo
(defmethod get-type ((fd <feature-defn>))
  (ogr-l-get-geom-type (pointer layer)))

;; EOF
