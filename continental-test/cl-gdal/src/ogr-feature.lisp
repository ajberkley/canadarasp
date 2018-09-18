;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_Create" ogr-f-create) ogr-feature-h
  "Feature factory.

 Note that the OGRFeature will increment the reference count of it's
 defining OGRFeatureDefn. Destruction of the OGRFeatureDefn before
 destruction of all OGRFeatures that depend on it is likely to result
 in a crash.

 This function is the same as the C++ method OGRFeature::OGRFeature().

 @argument[hDefn]{handle to the feature class (layer) definition to
 which the feature will adhere.}

 @return{an handle to the new feature object with null fields and no
 geometry.}"
  (hDefn ogr-feature-defn-h))
(export 'ogr-f-create)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_Destroy" ogr-f-destroy) :void
  "Destroy feature.

 The feature is deleted, but within the context of the GDAL/OGR
 heap. This is necessary when higher level applications use GDAL/OGR
 from a DLL and they want to delete a feature created within the
 DLL. If the delete is done in the calling application the memory will
 be freed onto the application heap which is inappropriate.

 This function is the same as the C++ method OGRFeature::DestroyFeature().

 @argument[hFeat]{handle to the feature to destroy.}"
  (hFeat ogr-feature-h))
(export 'ogr-f-destroy)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetDefnRef" ogr-f-get-defn-ref) ogr-feature-defn-h
  "Fetch feature definition.

 This function is the same as the C++ method OGRFeature::GetDefnRef().

 @argument[hFeat]{handle to the feature to get the feature definition from.}

 @return{an handle to the feature definition object on which feature depends.}"
  (hFeat ogr-feature-h))
(export 'ogr-f-get-defn-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetGeometryDirectly" ogr-f-set-geometry-directly) ogr-err
  "Set feature geometry.

 This function updates the features geometry, and operate exactly as
 SetGeometry(), except that this function assumes ownership of the
 passed geometry.

 This function is the same as the C++ method
 OGRFeature::SetGeometryDirectly.

 @argument[hFeat]{handle to the feature on which to apply the geometry.}
 @argument[hGeom]{handle to the new geometry to apply to feature.}

 @return{OGRERR_NONE if successful, or OGR_UNSUPPORTED_GEOMETRY_TYPE
 if the geometry type is illegal for the OGRFeatureDefn (checking not
 yet implemented).}"

  (hFeat ogr-feature-h)
  (hGeom ogr-geometry-h))
(export 'ogr-f-set-geometry-directly)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetGeometry" ogr-f-set-geometry) ogr-err
  "Set feature geometry.

 This function updates the features geometry, and operate exactly as
 SetGeometryDirectly(), except that this function does not assume
 ownership of the passed geometry, but instead makes a copy of it.

 This function is the same as the C++ OGRFeature::SetGeometry().

 @argument[hFeat]{handle to the feature on which new geometry is applied to.}
 @argument[hGeom]{handle to the new geometry to apply to feature.}

 @return{:NONE if successful, or :UNSUPPORTED_GEOMETRY_TYPE if the
 geometry type is illegal for the OGRFeatureDefn (checking not yet
 implemented).}"
  (hFeat ogr-feature-h)
  (hGeom ogr-geometry-h))
(export 'ogr-f-set-geometry)

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_F_GetGeometryRef" ogr-f-get-geometry-ref) ogr-geometry-h
  "Fetch an handle to feature geometry.

 This function is the same as the C++ method
 OGRFeature::GetGeometryRef().

 @argument[hFeat]{handle to the feature to get geometry from.}

 @return{an handle to internal feature geometry. This object should not
 be modified.}"
  (hFeat ogr-feature-h))
(export 'ogr-f-get-geometry-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_StealGeometry" ogr-f-steal-geometry) ogr-geometry-h
  "Take away ownership of geometry.

 Fetch the geometry from this feature, and clear the reference to the
 geometry on the feature. This is a mechanism for the application to
 take over ownship of the geometry from the feature without
 copying. Sort of an inverse to OGR_FSetGeometryDirectly().

 After this call the OGRFeature will have a NULL geometry.

 @return{the pointer to the geometry.}"
  (hFeat ogr-feature-h))
(export 'ogr-f-steal-geometry)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_Clone" ogr-f-clone) ogr-feature-h
  "Duplicate feature.

 The newly created feature is owned by the caller, and will have it's
 own reference to the OGRFeatureDefn.

 This function is the same as the C++ method OGRFeature::Clone().

 @argument[hFeat]{handle to the feature to clone.}

 @return{an handle to the new feature, exactly matching this feature.}"
  (hFeat ogr-feature-h))
(export 'ogr-f-clone)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_Equal" ogr-f-equal) :int
  "Test if two features are the same.

 Two features are considered equal if the share them (handle equality)
 same OGRFeatureDefn, have the same field values, and the same
 geometry (as tested by OGR_G_Equal()) as well as the same feature id.

 This function is the same as the C++ method OGRFeature::Equal().

 @argument[hFeat]{handle to one of the feature.}
 @argument[hOtherFeat]{handle to the other feature to test this one against.}

@return{TRUE if they are equal, otherwise FALSE.}"
  (hFeat ogr-feature-h)
  (hOtherFeat ogr-feature-h))
(export 'ogr-f-equal)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldCount" ogr-f-get-field-count) :int
  "Fetch number of fields on this feature This will always be the same
 as the field count for the OGRFeatureDefn.

 This function is the same as the C++ method
 OGRFeature::GetFieldCount().

 @argument[hFeat]{handle to the feature to get the fields count from.}

 @return{count of fields.}"
  (hFeat ogr-feature-h))
(export 'ogr-f-get-field-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldDefnRef" ogr-f-get-field-defn-ref) ogr-field-defn-h
  "Fetch definition for this field.

 This function is the same as the C++ method OGRFeature::GetFieldDefnRef().

 @argument[hFeat]{handle to the feature on which the field is found.}
 @argument[i]{the field to fetch, from 0 to GetFieldCount()-1.}

 @return{an handle to the field definition (from the
 OGRFeatureDefn). This is an internal reference, and should not be
 deleted or modified.}"
  (hFeat ogr-feature-h)
  (i :int))
(export 'ogr-f-get-field-defn-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldIndex" ogr-f-get-field-index) :int
  "Fetch the field index given field name.

 This is a cover for the OGRFeatureDefn::GetFieldIndex() method.

 This function is the same as the C++ method OGRFeature::GetFieldIndex().

 @argument[hFeat]{handle to the feature on which the field is found.}
 @argument[pszName]{the name of the field to search for.}

 @return{the field index, or -1 if no matching field is found.}"
  (hFeat ogr-feature-h)
  (pszName :string))
(export 'ogr-f-get-field-index)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_IsFieldSet" ogr-f-is-field-set) :int
  "Test if a field has ever been assigned a value or not.

 This function is the same as the C++ method OGRFeature::IsFieldSet().

 @argument[hFeat]{handle to the feature on which the field is.}
 @argument[iField]{the field to test.}

 @return{TRUE if the field has been set, otherwise false.}"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-is-field-set)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_UnsetField" ogr-f-unset-field) :void
  "Clear a field, marking it as unset.

 This function is the same as the C++ method OGRFeature::UnsetField().

 @argument[hFeat]{handle to the feature on which the field is.}
 @argument[iField]{the field to unset.}"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-unset-field)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetRawFieldRef" ogr-f-get-raw-field-ref) :pointer ; OGRField*
  "Fetch an handle to the internal field value given the index.

 This function is the same as the C++ method
 OGRFeature::GetRawFieldRef().

 @argument[hFeat]{handle to the feature on which field is found.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}

 @return{the returned handle is to an internal data structure, and
 should not be freed, or modified.}"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-get-raw-field-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsInteger" ogr-f-get-field-as-integer) :int
  "Fetch field value as integer.

 OFTString features will be translated using atoi(). OFTReal fields
 will be cast to integer. Other field types, or errors will result in
 a return value of zero.

 This function is the same as the C++ method OGRFeature::GetFieldAsInteger().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}

 @return{the field value.}"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-get-field-as-integer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsDouble" ogr-f-get-field-as-double) :double
  "Fetch field value as a double.

 OFTString features will be translated using atof(). OFTInteger fields
 will be cast to double. Other field types, or errors will result in a
 return value of zero.

 This function is the same as the C++ method
 OGRFeature::GetFieldAsDouble().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}

 @return{the field value.}"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-get-field-as-double)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsString" ogr-f-get-field-as-string) :string
  "Fetch field value as a string.

 OFTReal and OFTInteger fields will be translated to string using
 sprintf(), but not necessarily using the established formatting
 rules. Other field types, or errors will result in a return value of
 zero.

 This function is the same as the C++ method
 OGRFeature::GetFieldAsString().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}

 @return{the field value. This string is internal, and should not be
 modified, or freed. Its lifetime may be very brief.}"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-get-field-as-string)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsIntegerList" ogr-f-get-field-as-integer-list) (:pointer :int) ; const int*
  "Fetch field value as a list of integers.

 Currently this function only works for OFTIntegerList fields.

 This function is the same as the C++ method
 OGRFeature::GetFieldAsIntegerList().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[pnCount]{an integer to put the list count (number of integers) into.}

 @return{the field value. This list is internal, and should not be
 modified, or freed. Its lifetime may be very brief. If *pnCount is
 zero on return the returned pointer may be NULL or non-NULL.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (pnCount (:pointer :int)))		; int *
(export 'ogr-f-get-field-as-integer-list)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsDoubleList" ogr-f-get-field-as-double-list) (:pointer :double) ; const double*
  "Fetch field value as a list of doubles.

 Currently this function only works for OFTRealList fields.

 This function is the same as the C++ method OGRFeature::GetFieldAsDoubleList().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[pnCount]{an integer to put the list count (number of doubles) into.}

 @return{the field value. This list is internal, and should not be
 modified, or freed. Its lifetime may be very brief. If *pnCount is
 zero on return the returned pointer may be NULL or non-NULL.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (pnCount (:pointer :int)))
(export 'ogr-f-get-field-as-double-list)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsStringList" ogr-f-get-field-as-string-list) (:pointer :string) ; char**
  "Fetch field value as a list of strings.

 Currently this method only works for OFTStringList fields.

 The returned list is terminated by a NULL pointer. The number of
 elements can also be calculated using CSLCount().

 This function is the same as the C++ method OGRFeature::GetFieldAsStringList().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}

 @return{the field value. This list is internal, and should not be
 modified, or freed. Its lifetime may be very brief.}"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-get-field-as-string-list)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsBinary" ogr-f-get-field-as-binary) :pointer ; GByte*
  "Fetch field value as binary.

 Currently this method only works for OFTBinary fields.

 This function is the same as the C++ method OGRFeature::GetFieldAsBinary().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[pnBytes]{location to place count of bytes returned.}

 @return{the field value. This list is internal, and should not be
 modified, or freed. Its lifetime may be very brief.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (pnBytes (:pointer :int)))		; int *
(export 'ogr-f-get-field-as-binary)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFieldAsDateTime" ogr-f-get-field-as-date-time) :int
  "Fetch field value as date and time.

 Currently this method only works for OFTDate, OFTTime and OFTDateTime fields.

 This function is the same as the C++ method OGRFeature::GetFieldAsDateTime().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[pnYear]{(including century)}
 @argument[pnMonth]{(1-12)}
 @argument[pnDay]{(1-31)}
 @argument[pnHour]{(0-23)}
 @argument[pnMinute]{(0-59)}
 @argument[pnSecond]{(0-59)}
 @argument[pnTZFlag]{(0=unknown, 1=localtime, 100=GMT, see data model for details)}

 @return{TRUE on success or FALSE on failure.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (pnYear (:pointer :int))
  (pnMonth (:pointer :int))
  (pnDay (:pointer :int))
  (pnHour (:pointer :int))
  (pnMinute (:pointer :int))
  (pnSecond (:pointer :int))
  (pnTZFlag (:pointer :int)))
(export 'ogr-f-get-field-as-date-time)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldInteger" ogr-f-set-field-integer) :void
  "Set field to integer value.

 OFTInteger and OFTReal fields will be set directly. OFTString fields
 will be assigned a string representation of the value, but not
 necessarily taking into account formatting constraints on this
 field. Other field types may be unaffected.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[nValue]{the value to assign.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (nValue :int))
(export 'ogr-f-set-field-integer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldDouble" ogr-f-set-field-double) :void
  "Set field to double value.

 OFTInteger and OFTReal fields will be set directly. OFTString fields
 will be assigned a string representation of the value, but not
 necessarily taking into account formatting constraints on this
 field. Other field types may be unaffected.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[dfValue]{the value to assign.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (dfValue :double))
(export 'ogr-f-set-field-double)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldString" ogr-f-set-field-string) :void
  "Set field to string value.

 OFTInteger fields will be set based on an atoi() conversion of the
 string. OFTReal fields will be set based on an atof() conversion of
 the string. Other field types may be unaffected.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[pszValue]{the value to assign.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (pszValue :string))
(export 'ogr-f-set-field-string)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldIntegerList" ogr-f-set-field-integer-list) :void
  "Set field to list of integers value.

 This function currently on has an effect of OFTIntegerList fields.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to set, from 0 to GetFieldCount()-1.}
 @argument[nCount]{the number of values in the list being assigned.}
 @argument[panValues]{the values to assign.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (nCount :int)
  (panValues (:pointer :int)))
(export 'ogr-f-set-field-integer-list)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldDoubleList" ogr-f-set-field-double-list) :void

  "Set field to list of doubles value.

 This function currently on has an effect of OFTRealList fields.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to set, from 0 to GetFieldCount()-1.}
 @argument[nCount]{the number of values in the list being assigned.}
 @argument[padfValues]{the values to assign.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (nCount :int)
  (padfValues (:pointer :double)))
(export 'ogr-f-set-field-double-list)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldStringList" ogr-f-set-field-string-list) :void
  "Set field to list of strings value.

This function currently on has an effect of OFTStringList fields.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to set, from 0 to GetFieldCount()-1.}
 @argument[papszValues]{the values to assign.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (papszValues (:pointer :string)))
(export 'ogr-f-set-field-string-list)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldRaw" ogr-f-set-field-raw) :void
  "Set field.

 The passed value OGRField must be of exactly the same type as the
 target field, or an application crash may occur. The passed value is
 copied, and will not be affected. It remains the responsibility of
 the caller.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to fetch, from 0 to GetFieldCount()-1.}
 @argument[psValue]{handle on the value to assign.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (psValue :pointer))			; OGRField *
(export 'ogr-f-set-field-raw)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldBinary" ogr-f-set-field-binary) :void
  "Set field to binary data.

 This function currently on has an effect of OFTBinary fields.

 This function is the same as the C++ method OGRFeature::SetField().

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to set, from 0 to GetFieldCount()-1.}
 @argument[nBytes]{the number of bytes in pabyData array.}
 @argument[pabyData]{the data to apply.}"
  (hFeat ogr-feature-h)
  (iField :int)
  (nBytes :int)
  (pabyData :pointer))			; GByte *
(export 'ogr-f-set-field-binary)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFieldDateTime" ogr-f-set-field-date-time) :void
  "Set field to datetime.

 This method currently only has an effect for OFTDate, OFTTime and
 OFTDateTime fields.

 @argument[hFeat]{handle to the feature that owned the field.}
 @argument[iField]{the field to set, from 0 to GetFieldCount()-1.}
 @argument[nYear]{(including century)}
 @argument[nMonth]{(1-12)}
 @argument[nDay]{(1-31)}
 @argument[nHour]{(0-23)}
 @argument[nMinute]{(0-59)}
 @argument[nSecond]{(0-59)}
 @argument[nTZFlag]{(0=unknown, 1=localtime, 100=GMT, see data model for details)}"
  (hFeat ogr-feature-h)
  (iField :int)
  (nYear :int)
  (nMonth :int)
  (nDay :int)
  (nHour :int)
  (nMinute :int)
  (nSecond :int)
  (nTZFlag :int))
(export 'ogr-f-set-field-date-time)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetGeomFieldCount" ogr-f-get-geom-field-count) :int
  "Fetch number of geometry fields on this feature This will always be
 the same as the geometry field count for the OGRFeatureDefn.

 This function is the same as the C++ method
 OGRFeature::GetGeomFieldCount().

 @argument[hFeat]{handle to the feature to get the geometry fields
 count from.}

 @return{count of geometry fields.}

 Since: GDAL 2.0"
  (hFeat ogr-feature-h))
(export 'ogr-f-get-geom-field-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetGeomFieldDefnRef" ogr-f-get-geom-field-defn-ref) ogr-geom-field-defn-h
  "Fetch definition for this geometry field.

 This function is the same as the C++ method OGRFeature::GetGeomFieldDefnRef().

 @argument[hFeat]{handle to the feature on which the field is found.}
 @argument[i]{the field to fetch, from 0 to GetGeomFieldCount()-1.}

 @return{an handle to the field definition (from the
 OGRFeatureDefn). This is an internal reference, and should not be
 deleted or modified.}

Since: GDAL 2.0"
  (hFeat ogr-feature-h)
  (i :int))
(export 'ogr-f-get-geom-field-defn-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetGeomFieldIndex" ogr-f-get-geom-field-index) :int
  "Fetch the geometry field index given geometry field name.

 This is a cover for the OGRFeatureDefn::GetGeomFieldIndex() method.

 This function is the same as the C++ method
 OGRFeature::GetGeomFieldIndex().

 @argument[hFeat]{handle to the feature on which the geometry field is found.}
 @argument[pszName]{the name of the geometry field to search for.}

 @return{the geometry field index, or -1 if no matching geometry field
 is found.}

Since: GDAL 2.0"
  (hFeat ogr-feature-h)
  (pszName :string))
(export 'ogr-f-get-geom-field-index)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetGeomFieldRef" ogr-f-get-geom-field-ref) ogr-geometry-h
  "Fetch an handle to feature geometry.

 This function is the same as the C++ method OGRFeature::GetGeomFieldRef().

 @argument[hFeat]{handle to the feature to get geometry from.}
 @argument[iField]{geometry field to get.}

 @return{an handle to internal feature geometry. This object should
 not be modified.}

Since: GDAL 2.0"
  (hFeat ogr-feature-h)
  (iField :int))
(export 'ogr-f-get-geom-field-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetGeomFieldDirectly" ogr-f-set-geom-field-directly) ogr-err
  "Set feature geometry of a specified geometry field.

 This function updates the features geometry, and operate exactly as
 SetGeomField(), except that this function assumes ownership of the
 passed geometry.

 This function is the same as the C++ method OGRFeature::SetGeomFieldDirectly.

 @argument[hFeat]{handle to the feature on which to apply the geometry.}
 @argument[iField]{geometry field to set.}
 @argument[hGeom]{handle to the new geometry to apply to feature.}

 @return{:NONE if successful, or :FAILURE if the index is
 invalid, or :UNSUPPORTED_GEOMETRY_TYPE if the geometry type is
 illegal for the OGRFeatureDefn (checking not yet implemented).}

Since: GDAL 2.0"
  (hFeat ogr-feature-h)
  (iField :int)
  (hGeom ogr-geometry-h))
(export 'ogr-f-set-geom-field-directly)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetGeomField" ogr-f-set-geom-field) ogr-err
  "Set feature geometry of a specified geometry field.

 This function updates the features geometry, and operate exactly as
 SetGeometryDirectly(), except that this function does not assume
 ownership of the passed geometry, but instead makes a copy of it.

This function is the same as the C++ OGRFeature::SetGeomField().

 @argument[hFeat]{handle to the feature on which new geometry is applied to.}
 @argument[iField]{geometry field to set.}
 @argument[hGeom]{handle to the new geometry to apply to feature.}

 @return{:NONE if successful, or :UNSUPPORTED_GEOMETRY_TYPE
 if the geometry type is illegal for the OGRFeatureDefn (checking not
 yet implemented).}"
  (hFeat ogr-feature-h)
  (iField :int)
  (hGeom ogr-geometry-h))
(export 'ogr-f-set-geom-field)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetFID" ogr-f-get-fid) :long
  "Get feature identifier.

 This function is the same as the C++ method OGRFeature::GetFID().

 @argument[hFeat]{handle to the feature from which to get the feature
 identifier.}

 @return{feature id or OGRNullFID if none has been assigned.}"
  (hFeat ogr-feature-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFID" ogr-f-set-fid) ogr-err
  "Set the feature identifier.

 For specific types of features this operation may fail on illegal
 features ids. Generally it always succeeds. Feature ids should be
 greater than or equal to zero, with the exception of OGRNullFID (-1)
 indicating that the feature id is unknown.

 This function is the same as the C++ method OGRFeature::SetFID().

 @argument[hFeat]{handle to the feature to set the feature id to.}
 @argument[nFID]{the new feature identifier value to assign.}

 @return{On success OGRERR_NONE, or on failure some other value.}"
  (hFeat ogr-feature-h)
  (nFID :long))
(export 'ogr-f-set-fid)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_DumpReadable" ogr-f-dump-readable) :void
  "Dump this feature in a human readable form.

 This dumps the attributes, and geometry; however, it doesn't
 definition information (other than field types and names), nor does
 it report the geometry spatial reference system.

 This function is the same as the C++ method
 OGRFeature::DumpReadable().

 @argument[hFeat]{handle to the feature to dump.}
 @argument[fpOut]{the stream to write to, such as strout.}"
  (hFeat ogr-feature-h)
  (fpOut :pointer))			; FILE *
(export 'ogr-f-dump-readable)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFrom" ogr-f-set-from) ogr-err
  "Set one feature from another.

 Overwrite the contents of this feature from the geometry and
 attributes of another. The hOtherFeature does not need to have the
 same OGRFeatureDefn. Field values are copied by corresponding field
 names. Field types do not have to exactly match. OGR_F_SetField*()
 function conversion rules will be applied as needed.

 This function is the same as the C++ method OGRFeature::SetFrom().

 @argument[hFeat]{handle to the feature to set to.}
 @argument[hOtherFeat]{handle to the feature from which geometry, and
 field values will be copied.}
 @argument[bForgiving]{TRUE if the operation should continue despite
 lacking output fields matching some of the source fields.}

 @return{OGRERR_NONE if the operation succeeds, even if some values
 are not transferred, otherwise an error code.}"
  (hFeat ogr-feature-h)
  (hOtherFeat ogr-feature-h)
  (bForgiving :int))
(export 'ogr-f-set-from)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetFromWithMap" ogr-f-set-from-with-map) ogr-err
  "Set one feature from another.

 Overwrite the contents of this feature from the geometry and
 attributes of another. The hOtherFeature does not need to have the
 same OGRFeatureDefn. Field values are copied according to the
 provided indices map. Field types do not have to exactly
 match. OGR_F_SetField*() function conversion rules will be applied as
 needed. This is more efficient than OGR_F_SetFrom() in that this
 doesn't lookup the fields by their names. Particularly useful when
 the field names don't match.

 This function is the same as the C++ method OGRFeature::SetFrom().

 @argument[hFeat]{handle to the feature to set to.}
 @argument[hOtherFeat]{handle to the feature from which geometry, and
 field values will be copied.}
 @argument[panMap]{Array of the indices of the destination feature's
 fields stored at the corresponding index of the source feature's
 fields. A value of -1 should be used to ignore the source's
 field. The array should not be NULL and be as long as the number of
 fields in the source feature.}
 @argument[bForgiving]{TRUE if the operation should continue despite
 lacking output fields matching some of the source fields.}

 @return{OGRERR_NONE if the operation succeeds, even if some values
 are not transferred, otherwise an error code.}"
  (hFeat ogr-feature-h)
  (hOtherFeat ogr-feature-h)
  (bForgiving :int)
  (panMap (:pointer :int)))
(export 'ogr-f-set-from-with-map)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_GetStyleString" ogr-f-get-style-string) :string
  "Fetch style string for this feature.

 Set the OGR Feature Style Specification for details on the format of
 this string, and ogr_featurestyle.h for services available to parse
 it.

 This function is the same as the C++ method OGRFeature::GetStyleString().

 @argument[hFeat]{handle to the feature to get the style from.}

 @return{a reference to a representation in string format, or NULL if
 there isn't one.}"
  (hFeat ogr-feature-h))
(export 'ogr-f-get-style-string)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetStyleString" ogr-f-set-style-string) :void
  "Set feature style string. This method operate exactly as
 OGR_F_SetStyleStringDirectly() except that it does not assume
 ownership of the passed string, but instead makes a copy of it.

 This function is the same as the C++ method OGRFeature::SetStyleString().

 @argument[hFeat]{handle to the feature to set style to.}
 @argument[pszStyle]{the style string to apply to this feature, cannot
 be NULL.}"
  (hFeat ogr-feature-h)
  (pszStyle :string))
(export 'ogr-f-set-style-string)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_F_SetStyleStringDirectly" ogr-f-set-style-string-directly) :void
  "Set feature style string. This method operate exactly as
 OGR_F_SetStyleString() except that it assumes ownership of the passed
 string.

 This function is the same as the C++ method
 OGRFeature::SetStyleStringDirectly().

 @argument[hFeat]{handle to the feature to set style to.}
 @argument[pszStyle]{the style string to apply to this feature, cannot
 be NULL.}"
  (hFeat ogr-feature-h)
  (pszStyle :string))
(export 'ogr-f-set-style-string-directly)

;; --------------------------------------------------------
;; CLOS
;; --------------------------------------------------------

(defmethod get-geometry ((feature <feature>) &optional idx)
  (declare (ignore idx))
  ;; todo!
  (let* ((ref (ogr-f-get-geometry-ref (pointer feature)))
	 (geom-type (ogr-g-get-geometry-type ref)))
    (dispatch-geometry-construction geom-type ref)))

;; EOF
