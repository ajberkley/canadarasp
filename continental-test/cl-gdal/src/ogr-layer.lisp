;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetName" ogr-l-get-name) :string
  "Return the layer name.

 This returns the same content as
 OGR_FD_GetName(OGR_L_GetLayerDefn(hLayer)), but for a few drivers,
 calling OGR_L_GetName() directly can avoid lengthy layer definition
 initialization.

 This function is the same as the C++ method OGRLayer::GetName().

 @argument[hLayer]{handle to the layer.}

 @return{the layer name (must not been freed)}

 Since: OGR 1.8.0"
  (hLayer ogr-layer-h))
(export 'ogr-l-get-name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetGeomType" ogr-l-get-geom-type) ogr-wkb-geometry-type
  "Return the layer geometry type.

 This returns the same result as
 OGR_FD_GetGeomType(OGR_L_GetLayerDefn(hLayer)), but for a few
 drivers, calling OGR_L_GetGeomType() directly can avoid lengthy layer
 definition initialization.

 This function is the same as the C++ method OGRLayer::GetGeomType().

 @argument[hLayer]{handle to the layer.}

 @return{the geometry type}

 Since: OGR 1.8.0"
  (hLayer ogr-layer-h))
(export 'ogr-l-get-geom-type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetSpatialFilter" ogr-l-get-spatial-filter) ogr-geometry-h
  "This function returns the current spatial filter for this layer.

 The returned pointer is to an internally owned object, and should not
 be altered or deleted by the caller.

 This function is the same as the C++ method OGRLayer::GetSpatialFilter().

 @argument[hLayer]{handle to the layer to get the spatial filter
 from.}

 @return{an handle to the spatial filter geometry.}"
  (hLayer ogr-layer-h))
(export 'ogr-l-get-spatial-filter)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetSpatialFilter" ogr-l-set-spatial-filter) :void
  "Set a new spatial filter.

 This function set the geometry to be used as a spatial filter when
 fetching features via the OGR_L_GetNextFeature() function. Only
 features that geometrically intersect the filter geometry will be
 returned.

 Currently this test is may be inaccurately implemented, but it is
 guaranteed that all features who's envelope (as returned by
 OGR_G_GetEnvelope()) overlaps the envelope of the spatial filter will
 be returned. This can result in more shapes being returned that
 should strictly be the case.

 This function makes an internal copy of the passed geometry. The
 passed geometry remains the responsibility of the caller, and may be
 safely destroyed.

 For the time being the passed filter geometry should be in the same
 SRS as the layer (as returned by OGR_L_GetSpatialRef()). In the
 future this may be generalized.

 This function is the same as the C++ method
 OGRLayer::SetSpatialFilter.

 @argument[hLayer]{handle to the layer on which to set the spatial filter.}
 @argument[hGeom]{handle to the geometry to use as a filtering
 region. NULL may be passed indicating that the current spatial filter
 should be cleared, but no new one instituted.}"
  (hLayer ogr-layer-h)
  (hGeom OGR-Geometry-H))
(export 'ogr-l-set-spatial-filter)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetSpatialFilterRect" ogr-l-set-spatial-filter-rect) :void
  "Set a new rectangular spatial filter.

 This method set rectangle to be used as a spatial filter when
 fetching features via the OGR_L_GetNextFeature() method. Only
 features that geometrically intersect the given rectangle will be
 returned.

 The x/y values should be in the same coordinate system as the layer
 as a whole (as returned by OGRLayer::GetSpatialRef()). Internally
 this method is normally implemented as creating a 5 vertex closed
 rectangular polygon and passing it to
 OGRLayer::SetSpatialFilter(). It exists as a convenience.

 The only way to clear a spatial filter set with this method is to
 call OGRLayer::SetSpatialFilter(NULL).

 This method is the same as the C++ method OGRLayer::SetSpatialFilterRect().

 @argument[hLayer]{handle to the layer on which to set the spatial filter.}
 @argument[dfMinX]{the minimum X coordinate for the rectangular region.}
 @argument[dfMinY]{the minimum Y coordinate for the rectangular region.}
 @argument[dfMaxX]{the maximum X coordinate for the rectangular region.}
 @argument[dfMaxY]{the maximum Y coordinate for the rectangular region.}"
  (hLayer ogr-layer-h)
  (dfMinX :double)
  (dfMinY :double)
  (dfMaxX :double)
  (dfMaxY :double))
(export 'ogr-l-set-spatial-filter-rect)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetSpatialFilterEx" ogr-l-set-spatial-filter-ex) :void
  "Set a new spatial filter.

 This function set the geometry to be used as a spatial filter when
 fetching features via the OGR_L_GetNextFeature() function. Only
 features that geometrically intersect the filter geometry will be
 returned.

 Currently this test is may be inaccurately implemented, but it is
 guaranteed that all features who's envelope (as returned by
 OGR_G_GetEnvelope()) overlaps the envelope of the spatial filter will
 be returned. This can result in more shapes being returned that
 should strictly be the case.

 This function makes an internal copy of the passed geometry. The
 passed geometry remains the responsibility of the caller, and may be
 safely destroyed.

 For the time being the passed filter geometry should be in the same
 SRS as the geometry field definition it corresponds to (as returned
 by GetLayerDefn()->GetGeomFieldDefn(iGeomField)->GetSpatialRef()). In
 the future this may be generalized.

 Note that only the last spatial filter set is applied, even if
 several successive calls are done with different iGeomField values.

 This function is the same as the C++ method
 OGRLayer::SetSpatialFilter.

 @argument[hLayer]{handle to the layer on which to set the spatial
 filter.}
 @argument[iGeomField]{index of the geometry field on which the
 spatial filter operates.}
 @argument[hGeom]{handle to the geometry to use as a filtering
 region. NULL may be passed indicating that the current spatial filter
 should be cleared, but no new one instituted.}

 Since: GDAL 2.0"
  (hLayer ogr-layer-h)
  (iGeomField :int)
  (hGeom ogr-geometry-h))
(export 'ogr-l-set-spatial-filter-ex)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetSpatialFilterRectEx" ogr-l-set-spatial-filter-rect-ex) :void
  "Set a new rectangular spatial filter.

 This method set rectangle to be used as a spatial filter when
 fetching features via the OGR_L_GetNextFeature() method. Only
 features that geometrically intersect the given rectangle will be
 returned.

 The x/y values should be in the same coordinate system as as the
 geometry field definition it corresponds to (as returned by
 GetLayerDefn()->GetGeomFieldDefn(iGeomField)->GetSpatialRef()). Internally
 this method is normally implemented as creating a 5 vertex closed
 rectangular polygon and passing it to
 OGRLayer::SetSpatialFilter(). It exists as a convenience.

 The only way to clear a spatial filter set with this method is to
 call OGRLayer::SetSpatialFilter(NULL).

 This method is the same as the C++ method
 OGRLayer::SetSpatialFilterRect().

 @argument[hLayer]{handle to the layer on which to set the spatial filter.}
 @argument[iGeomField]{index of the geometry field on which the spatial filter operates.}
 @argument[dfMinX]{the minimum X coordinate for the rectangular region.}
 @argument[dfMinY]{the minimum Y coordinate for the rectangular region.}
 @argument[dfMaxX]{the maximum X coordinate for the rectangular region.}
 @argument[dfMaxY]{the maximum Y coordinate for the rectangular region.}

 Since: GDAL 2.0"
  (hLayer ogr-layer-h)
  (iGeomField :int)
  (dfMinX :double)
  (dfMinY :double)
  (dfMaxX :double)
  (dfMaxY :double))
(export 'ogr-l-set-spatial-filter-rect-ex)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetAttributeFilter" ogr-l-set-attribute-filter) ogr-err
  "Set a new attribute query.

 This function sets the attribute query string to be used when
 fetching features via the OGR_L_GetNextFeature() function. Only
 features for which the query evaluates as true will be returned.

 The query string should be in the format of an SQL WHERE clause. For
 instance \"population > 1000000 and population < 5000000\" where
 population is an attribute in the layer. The query format is a
 restricted form of SQL WHERE clause as
 defined \"eq_format=restricted_where\" about half way through this
 document:

 http://ogdi.sourceforge.net/prop/6.2.CapabilitiesMetadata.html

 Note that installing a query string will generally result in
 resetting the current reading position (ala OGR_L_ResetReading()).

 This function is the same as the C++ method
 OGRLayer::SetAttributeFilter().

 @argument[hLayer]{handle to the layer on which attribute query will
 be executed.}
 @argument[pszQuery]{query in restricted SQL WHERE format, or NULL to
 clear the current query.}

 @return{OGRERR_NONE if successfully installed, or an error code if
 the query expression is in error, or some other failure occurs.}"
  (hLayer ogr-layer-h)
  (pszQuery :string))			    ; const char *
(export 'ogr-l-set-attribute-filter)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_ResetReading" ogr-l-reset-reading) :void
  "Reset feature reading to start on the first feature.

 This affects GetNextFeature().

 This function is the same as the C++ method OGRLayer::ResetReading().

 @argument[hLayer]{handle to the layer on which features are read.}"
  (hLayer ogr-layer-h))
(export 'ogr-l-reset-reading)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetNextFeature" ogr-l-get-next-feature) ogr-feature-h
  "Fetch the next available feature from this layer.

 The returned feature becomes the responsiblity of the caller to delete
 with OGR_F_Destroy(). It is critical that all features associated with
 an OGRLayer (more specifically an OGRFeatureDefn) be deleted before
 that layer/datasource is deleted.

 Only features matching the current spatial filter (set with
 SetSpatialFilter()) will be returned.

 This function implements sequential access to the features of a
 layer. The OGR_L_ResetReading() function can be used to start at the
 beginning again.

 This function is the same as the C++ method
 OGRLayer::GetNextFeature().

 @argument[hLayer]{handle to the layer from which feature are read.}

 @return{an handle to a feature, or NULL if no more features are available.}"
  (hLayer ogr-layer-h))
(export 'ogr-l-get-next-feature)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetNextByIndex" ogr-l-set-next-by-index) ogr-err
  "Move read cursor to the nIndex'th feature in the current resultset.

 This method allows positioning of a layer such that the
 GetNextFeature() call will read the requested feature, where nIndex
 is an absolute index into the current result set. So, setting it to 3
 would mean the next feature read with GetNextFeature() would have
 been the 4th feature to have been read if sequential reading took
 place from the beginning of the layer, including accounting for
 spatial and attribute filters.

 Only in rare circumstances is SetNextByIndex() efficiently
 implemented. In all other cases the default implementation which
 calls ResetReading() and then calls GetNextFeature() nIndex times is
 used. To determine if fast seeking is available on the current layer
 use the TestCapability() method with a value of
 OLCFastSetNextByIndex.

 This method is the same as the C++ method OGRLayer::SetNextByIndex()

 @argument[hLayer]{handle to the layer}
 @argument[nIndex]{the index indicating how many steps into the result
 set to seek.}

 @return{OGRERR_NONE on success or an error code.}"

  (hLayer ogr-layer-h)
  (nIndex :long))
(export 'ogr-l-set-next-by-index)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetFeature" ogr-l-get-feature) ogr-feature-h
  "Fetch a feature by its identifier.

 This function will attempt to read the identified feature. The nFID
 value cannot be OGRNullFID. Success or failure of this operation is
 unaffected by the spatial or attribute filters.

 If this function returns a non-NULL feature, it is guaranteed that
 its feature id (OGR_F_GetFID()) will be the same as nFID.

 Use OGR_L_TestCapability(OLCRandomRead) to establish if this layer
 supports efficient random access reading via OGR_L_GetFeature();
 however, the call should always work if the feature exists as a
 fallback implementation just scans all the features in the layer
 looking for the desired feature.

 Sequential reads are generally considered interrupted by a
 OGR_L_GetFeature() call.

 The returned feature should be free with OGR_F_Destroy().

 This function is the same as the C++ method OGRLayer::GetFeature( ).

 @argument[hLayer]{handle to the layer that owned the feature.}
 @argument[nFeatureId]{the feature id of the feature to read.}

 @return{an handle to a feature now owned by the caller, or NULL on
 failure.}"
  (hLayer ogr-layer-h)
  (nFeatureId :long))
(export 'ogr-l-get-feature)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetFeature" ogr-l-set-feature) ogr-err
  "Rewrite an existing feature.

 This function will write a feature to the layer, based on the feature
 id within the OGRFeature.

 Use OGR_L_TestCapability(OLCRandomWrite) to establish if this layer
 supports random access writing via OGR_L_SetFeature().

 This function is the same as the C++ method OGRLayer::SetFeature().

 @argument[hLayer]{handle to the layer to write the feature.}
 @argument[hFeat]{the feature to write.}

 @return{OGRERR_NONE if the operation works, otherwise an appropriate
 error code.}"
  (hLayer ogr-layer-h)
  (hFeat ogr-feature-h))
(export 'ogr-l-set-feature)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_CreateFeature" ogr-l-create-feature) ogr-err
  "Create and write a new feature within a layer.

 The passed feature is written to the layer as a new feature, rather
 than overwriting an existing one. If the feature has a feature id
 other than OGRNullFID, then the native implementation may use that as
 the feature id of the new feature, but not necessarily. Upon
 successful return the passed feature will have been updated with the
 new feature id.

 This function is the same as the C++ method
 OGRLayer::CreateFeature().

 @argument[hLayer]{handle to the layer to write the feature to.}
 @argument[hFeat]{the handle of the feature to write to disk.}

 @return{OGRERR_NONE on success.}"
  (hLayer ogr-layer-h)
  (hFeat ogr-feature-h))
(export 'ogr-l-create-feature)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_DeleteFeature" ogr-l-delete-feature) ogr-err
  "Delete feature from layer.

 The feature with the indicated feature id is deleted from the layer
 if supported by the driver. Most drivers do not support feature
 deletion, and will return OGRERR_UNSUPPORTED_OPERATION. The
 OGR_L_TestCapability() function may be called with OLCDeleteFeature
 to check if the driver supports feature deletion.

 This method is the same as the C++ method OGRLayer::DeleteFeature().

 @argument[hLayer]{handle to the layer}
 @argument[nFID]{the feature id to be deleted from the layer}

 @return{OGRERR_NONE on success.}"
  (hLayer ogr-layer-h)
  (nFID :long))
(export 'ogr-l-delete-feature)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetLayerDefn" ogr-l-get-layer-defn) ogr-feature-defn-h
  "Fetch the schema information for this layer.

 The returned handle to the OGRFeatureDefn is owned by the OGRLayer,
 and should not be modified or freed by the application. It
 encapsulates the attribute schema of the features of the layer.

 This function is the same as the C++ method OGRLayer::GetLayerDefn().

 @argument[hLayer]{handle to the layer to get the schema information.}

 @return{OGRFeatureDefnH an handle to the feature definition.}"
  (hLayer :pointer))
(export 'ogr-l-get-layer-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetSpatialRef" ogr-l-get-spatial-ref) ogr-spatial-reference-h
  "Fetch the spatial reference system for this layer.

 The returned object is owned by the OGRLayer and should not be
 modified or freed by the application.

 This function is the same as the C++ method OGRLayer::GetSpatialRef().

 @argument[hLayer]{handle to the layer to get the spatial reference from.}

 @return{spatial reference, or NULL if there isn't one.}"
  (hLayer ogr-layer-h))
(export 'ogr-l-get-spatial-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetFeatureCount" ogr-l-get-feature-count) :int
  "Fetch the feature count in this layer.

 Returns the number of features in the layer. For dynamic databases
 the count may not be exact. If bForce is FALSE, and it would be
 expensive to establish the feature count a value of -1 may be
 returned indicating that the count isn't know. If bForce is TRUE some
 implementations will actually scan the entire layer once to count
 objects.

 The returned count takes the spatial filter into account.

 Note that some implementations of this method may alter the read
 cursor of the layer.

 This function is the same as the CPP OGRLayer::GetFeatureCount().

 @argument[hLayer]{handle to the layer that owned the features.}
 @argument[bForce]{Flag indicating whether the count should be
 computed even if it is expensive.}

 @return{feature count, -1 if count not known.}"
  (hLayer ogr-layer-h)
  (bForce :int))
(export 'ogr-l-get-feature-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetExtent" ogr-l-get-extent) ogr-err
  "Fetch the extent of this layer.

 Returns the extent (MBR) of the data in the layer. If bForce is
 FALSE, and it would be expensive to establish the extent then
 OGRERR_FAILURE will be returned indicating that the extent isn't
 know. If bForce is TRUE then some implementations will actually scan
 the entire layer once to compute the MBR of all the features in the
 layer.

 Depending on the drivers, the returned extent may or may not take the
 spatial filter into account. So it is safer to call OGR_L_GetExtent()
 without setting a spatial filter.

 Layers without any geometry may return OGRERR_FAILURE just indicating
 that no meaningful extents could be collected.

 Note that some implementations of this method may alter the read
 cursor of the layer.

 This function is the same as the C++ method OGRLayer::GetExtent().

 @argument[hLayer]{handle to the layer from which to get extent.}
 @argument[psExtent]{the structure in which the extent value will be
 returned.}
 @argument[bForce]{Flag indicating whether the extent should be
 computed even if it is expensive.}

 @return{OGRERR_NONE on success, OGRERR_FAILURE if extent not known.}"
  (hLayer ogr-layer-h)
  (psExtent (:pointer (:struct ogr-envelope)))
  (bForce :int))
(export 'ogr-l-get-extent)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetExtentEx" ogr-l-get-extent-ex) ogr-err
  "Fetch the extent of this layer, on the specified geometry field.

 Returns the extent (MBR) of the data in the layer. If bForce is
 FALSE, and it would be expensive to establish the extent then
 OGRERR_FAILURE will be returned indicating that the extent isn't
 know. If bForce is TRUE then some implementations will actually scan
 the entire layer once to compute the MBR of all the features in the
 layer.

 Depending on the drivers, the returned extent may or may not take the
 spatial filter into account. So it is safer to call OGR_L_GetExtent()
 without setting a spatial filter.

 Layers without any geometry may return OGRERR_FAILURE just indicating
 that no meaningful extents could be collected.

 Note that some implementations of this method may alter the read
 cursor of the layer.

 This function is the same as the C++ method OGRLayer::GetExtent().

 @argument[hLayer]{handle to the layer from which to get extent.}
 @argument[iGeomField]{the index of the geometry field on which to
 compute the extent.}
 @argument[psExtent]{the structure in which the extent value will be
 returned.}
 @argument[bForce]{Flag indicating whether the extent should be
 computed even if it is expensive.}

 @return{OGRERR_NONE on success, OGRERR_FAILURE if extent not known.}"

  (hLayer ogr-layer-h)
  (iGeomField :int)
  (psExtent :pointer)			; OGREnvelope *
  (bForce :int))
(export 'ogr-l-get-extent-ex)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_TestCapability" ogr-l-test-capability) :int
  "Test if this layer supported the named capability.

The capability codes that can be tested are represented as strings, but #defined constants exists to ensure correct spelling. Specific layer types may implement class specific capabilities, but this can't generally be discovered by the caller.


OLCRandomRead / \"RandomRead\": TRUE if the GetFeature() method is implemented in an optimized way for this layer, as opposed to the default implementation using ResetReading() and GetNextFeature() to find the requested feature id.

OLCSequentialWrite / \"SequentialWrite\": TRUE if the CreateFeature() method works for this layer. Note this means that this particular layer is writable. The same OGRLayer class may returned FALSE for other layer instances that are effectively read-only.

OLCRandomWrite / \"RandomWrite\": TRUE if the SetFeature() method is operational on this layer. Note this means that this particular layer is writable. The same OGRLayer class may returned FALSE for other layer instances that are effectively read-only.

OLCFastSpatialFilter / \"FastSpatialFilter\": TRUE if this layer implements spatial filtering efficiently. Layers that effectively read all features, and test them with the OGRFeature intersection methods should return FALSE. This can be used as a clue by the application whether it should build and maintain its own spatial index for features in this layer.

OLCFastFeatureCount / \"FastFeatureCount\": TRUE if this layer can return a feature count (via OGR_L_GetFeatureCount()) efficiently ... ie. without counting the features. In some cases this will return TRUE until a spatial filter is installed after which it will return FALSE.

OLCFastGetExtent / \"FastGetExtent\": TRUE if this layer can return its data extent (via OGR_L_GetExtent()) efficiently ... ie. without scanning all the features. In some cases this will return TRUE until a spatial filter is installed after which it will return FALSE.

OLCFastSetNextByIndex / \"FastSetNextByIndex\": TRUE if this layer can perform the SetNextByIndex() call efficiently, otherwise FALSE.

OLCCreateField / \"CreateField\": TRUE if this layer can create new fields on the current layer using CreateField(), otherwise FALSE.

OLCCreateGeomField / \"CreateGeomField\": (GDAL >= 2.0) TRUE if this layer can create new geometry fields on the current layer using CreateGeomField(), otherwise FALSE.

OLCDeleteField / \"DeleteField\": TRUE if this layer can delete existing fields on the current layer using DeleteField(), otherwise FALSE.

OLCReorderFields / \"ReorderFields\": TRUE if this layer can reorder existing fields on the current layer using ReorderField() or ReorderFields(), otherwise FALSE.

OLCAlterFieldDefn / \"AlterFieldDefn\": TRUE if this layer can alter the definition of an existing field on the current layer using AlterFieldDefn(), otherwise FALSE.

OLCDeleteFeature / \"DeleteFeature\": TRUE if the DeleteFeature() method is supported on this layer, otherwise FALSE.

OLCStringsAsUTF8 / \"StringsAsUTF8\": TRUE if values of OFTString fields are assured to be in UTF-8 format. If FALSE the encoding of fields is uncertain, though it might still be UTF-8.

OLCTransactions / \"Transactions\": TRUE if the StartTransaction(), CommitTransaction() and RollbackTransaction() methods work in a meaningful way, otherwise FALSE.

This function is the same as the C++ method OGRLayer::TestCapability().

@argument[hLayer]{handle to the layer to get the capability from.}
@argument[pszCap]{the name of the capability to test.}

 @return{TRUE if the layer has the requested capability, or FALSE
 otherwise. OGRLayers will return FALSE for any unrecognised
 capabilities.}"
  (hLayer ogr-layer-h)
  (pszCap :string))
(export 'ogr-l-test-capability)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_CreateField" ogr-l-create-field) ogr-err
  "Create a new field on a layer.

 You must use this to create new fields on a real layer. Internally
 the OGRFeatureDefn for the layer will be updated to reflect the new
 field. Applications should never modify the OGRFeatureDefn used by a
 layer directly.

 This function should not be called while there are feature objects in
 existance that were obtained or created with the previous layer
 definition.

 Not all drivers support this function. You can query a layer to check
 if it supports it with the OLCCreateField capability. Some drivers
 may only support this method while there are still no features in the
 layer. When it is supported, the existings features of the backing
 file/database should be updated accordingly.

 This function is the same as the C++ method OGRLayer::CreateField().

 @argument[hLayer]{handle to the layer to write the field definition.}
 @argument[hField]{handle of the field definition to write to disk.}
 @argument[bApproxOK]{If TRUE, the field may be created in a slightly
 different form depending on the limitations of the format driver.}

 @return{OGRERR_NONE on success.}"
  (hLayer ogr-layer-h)
  (hField ogr-field-defn-h)
  (bApproxOK :int))
(export 'ogr-l-create-field)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_CreateGeomField" ogr-l-create-geom-field) ogr-err
  "Create a new geometry field on a layer.

 You must use this to create new geometry fields on a real
 layer. Internally the OGRFeatureDefn for the layer will be updated to
 reflect the new field. Applications should never modify the
 OGRFeatureDefn used by a layer directly.

 This function should not be called while there are feature objects in
 existance that were obtained or created with the previous layer
 definition.

 Not all drivers support this function. You can query a layer to check
 if it supports it with the OLCCreateField capability. Some drivers
 may only support this method while there are still no features in the
 layer. When it is supported, the existings features of the backing
 file/database should be updated accordingly.

 This function is the same as the C++ method OGRLayer::CreateField().

 @argument[hLayer]{handle to the layer to write the field definition.}
 @argument[hField]{handle of the geometry field definition to write to
 disk.}
 @argument[bApproxOK]{If TRUE, the field may be created in a slightly
 different form depending on the limitations of the format driver.}

 @return{OGRERR_NONE on success.}

 Since: OGR 2.0"
  (hLayer ogr-layer-h)
  (hField ogr-geom-field-defn-h)
  (bApproxOK :int))
(export 'ogr-l-create-geom-field)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_DeleteField" ogr-l-delete-field) ogr-err
  "Create a new field on a layer.

 You must use this to delete existing fields on a real
 layer. Internally the OGRFeatureDefn for the layer will be updated to
 reflect the deleted field. Applications should never modify the
 OGRFeatureDefn used by a layer directly.

 This function should not be called while there are feature objects in
 existance that were obtained or created with the previous layer
 definition.

 Not all drivers support this function. You can query a layer to check
 if it supports it with the OLCDeleteField capability. Some drivers
 may only support this method while there are still no features in the
 layer. When it is supported, the existings features of the backing
 file/database should be updated accordingly.

 This function is the same as the C++ method OGRLayer::DeleteField().

 @argument[hLayer]{handle to the layer.}
 @argument[iField]{index of the field to delete.}

 @return{:NONE on success.}

 Since: OGR 1.9.0"
  (hLayer ogr-layer-h)
  (iField :int))
(export 'ogr-l-delete-field)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_ReorderFields" ogr-l-reorder-fields) ogr-err
  "Reorder all the fields of a layer.

 You must use this to reorder existing fields on a real
 layer. Internally the OGRFeatureDefn for the layer will be updated to
 reflect the reordering of the fields. Applications should never
 modify the OGRFeatureDefn used by a layer directly.

 This function should not be called while there are feature objects in
 existance that were obtained or created with the previous layer
 definition.

 panMap is such that,for each field definition at position i after
 reordering, its position before reordering was panMap[i].

 For example, let suppose the fields were \"0\",\"1\",\"2\",\"3\",\"4\"
 initially. ReorderFields([0,2,3,1,4]) will reorder them
 as \"0\",\"2\",\"3\",\"1\",\"4\".

 Not all drivers support this function. You can query a layer to check
 if it supports it with the OLCReorderFields capability. Some drivers
 may only support this method while there are still no features in the
 layer. When it is supported, the existings features of the backing
 file/database should be updated accordingly.

 This function is the same as the C++ method
 OGRLayer::ReorderFields().

 @argument[hLayer]{handle to the layer.}
 @argument[panMap]{an array of GetLayerDefn()->GetFieldCount()
 elements which is a permutation of [0,
 GetLayerDefn()->GetFieldCount()-1].}

 @return{OGRERR_NONE on success.}

 Since: OGR 1.9.0"
  (hLayer ogr-layer-h)
  (panMap (:pointer :int)))
(export 'ogr-l-reorder-fields)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_ReorderField" ogr-l-reorder-field) ogr-err
  "Reorder an existing field on a layer.

 This function is a conveniency wrapper of OGR_L_ReorderFields()
 dedicated to move a single field.

 You must use this to reorder existing fields on a real
 layer. Internally the OGRFeatureDefn for the layer will be updated to
 reflect the reordering of the fields. Applications should never
 modify the OGRFeatureDefn used by a layer directly.

 This function should not be called while there are feature objects in
 existance that were obtained or created with the previous layer
 definition.

 The field definition that was at initial position iOldFieldPos will
 be moved at position iNewFieldPos, and elements between will be
 shuffled accordingly.

 For example, let suppose the fields were
 \"0\",\"1\",\"2\",\"3\",\"4\" initially. ReorderField(1, 3) will
 reorder them as \"0\",\"2\",\"3\",\"1\",\"4\".

 Not all drivers support this function. You can query a layer to check
 if it supports it with the OLCReorderFields capability. Some drivers
 may only support this method while there are still no features in the
 layer. When it is supported, the existings features of the backing
 file/database should be updated accordingly.

 This function is the same as the C++ method OGRLayer::ReorderField().

 @argument[hLayer]{handle to the layer.}
 @argument[iOldFieldPos]{previous position of the field to move. Must
 be in the range [0,GetFieldCount()-1].}
 @argument[iNewFieldPos]{new position of the field to move. Must be in
 the range [0,GetFieldCount()-1].}

 @return{:NONE on success.}

 Since: OGR 1.9.0"
  (hLayer ogr-layer-h)
  (iOldFieldPos :int)
  (iNewFieldPos :int))
(export 'ogr-l-reorder-field)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_AlterFieldDefn" ogr-l-alter-field-defn) ogr-err
  "Alter the definition of an existing field on a layer.

 You must use this to alter the definition of an existing field of a
 real layer. Internally the OGRFeatureDefn for the layer will be
 updated to reflect the altered field. Applications should never
 modify the OGRFeatureDefn used by a layer directly.

 This function should not be called while there are feature objects in
 existance that were obtained or created with the previous layer
 definition.

 Not all drivers support this function. You can query a layer to check
 if it supports it with the OLCAlterFieldDefn capability. Some drivers
 may only support this method while there are still no features in the
 layer. When it is supported, the existings features of the backing
 file/database should be updated accordingly. Some drivers might also
 not support all update flags.

 This function is the same as the C++ method OGRLayer::AlterFieldDefn().

 @argument[hLayer]{handle to the layer.}
 @argument[iField]{index of the field whose definition must be altered.}
 @argument[hNewFieldDefn]{new field definition}
 @argument[nFlags]{combination of ALTER_NAME_FLAG, ALTER_TYPE_FLAG and
 ALTER_WIDTH_PRECISION_FLAG to indicate which of the name and/or type
 and/or width and precision fields from the new field definition must
 be taken into account.}

 @return{OGRERR_NONE on success.}

 Since: OGR 1.9.0"
  (hLayer ogr-layer-h)
  (iField :int)
  (hNewFieldDefn ogr-field-defn-h)
  (nFlags :int))
(export 'ogr-l-alter-field-defn)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_StartTransaction" ogr-l-start-transaction) ogr-err
  "For datasources which support transactions, StartTransaction creates
 a transaction.

 If starting the transaction fails, will return
 OGRERR_FAILURE. Datasources which do not support transactions will
 always return OGRERR_NONE.

 This function is the same as the C++ method OGRLayer::StartTransaction().

 @argument[hLayer]{handle to the layer}

 @return{OGRERR_NONE on success.}"
  (hLayer ogr-layer-h))
(export 'ogr-l-start-transaction)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_CommitTransaction" ogr-l-commit-transaction) ogr-err
  "For datasources which support transactions, CommitTransaction commits
 a transaction.

 If no transaction is active, or the commit fails, will return
 OGRERR_FAILURE. Datasources which do not support transactions will
 always return OGRERR_NONE.

 This function is the same as the C++ method
 OGRLayer::CommitTransaction().

 @argument[hLayer]{handle to the layer}

 @return{OGRERR_NONE on success.}"
  (hLayer ogr-layer-h))
(export 'ogr-l-commit-transaction)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_RollbackTransaction" ogr-l-rollback-transaction) ogr-err

  "For datasources which support transactions, RollbackTransaction will
  roll back a datasource to its state before the start of the current
  transaction. If no transaction is active, or the rollback fails, will
  return OGRERR_FAILURE. Datasources which do not support transactions
  will always return OGRERR_NONE.

  This function is the same as the C++ method
  OGRLayer::RollbackTransaction().

  @argument[hLayer]{handle to the layer}

  @return{OGRERR_NONE on success.}"
  (hLayer ogr-layer-h))
(export 'ogr-l-rollback-transaction)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SyncToDisk" ogr-l-sync-to-disk) ogr-err
  "Flush pending changes to disk.

 This call is intended to force the layer to flush any pending writes
 to disk, and leave the disk file in a consistent state. It would not
 normally have any effect on read-only datasources.

 Some layers do not implement this method, and will still return
 OGRERR_NONE. The default implementation just returns OGRERR_NONE. An
 error is only returned if an error occurs while attempting to flush
 to disk.

 In any event, you should always close any opened datasource with
 OGR_DS_Destroy() that will ensure all data is correctly flushed.

 This method is the same as the C++ method OGRLayer::SyncToDisk()

 @argument[hLayer]{handle to the layer}

 @return{OGRERR_NONE if no error occurs (even if nothing is done) or
 an error code.}"
  (hLayer ogr-layer-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetFIDColumn" ogr-l-get-fid-column) :string
  "This method returns the name of the underlying database column being
 used as the FID column, or \"\" if not supported.

 This method is the same as the C++ method OGRLayer::GetFIDColumn()

 @argument[hLayer]{handle to the layer}

 @return{fid column name.}"
  (hLayer ogr-layer-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_GetGeometryColumn" ogr-l-get-geometry-column) :string
  "This method returns the name of the underlying database column being
 used as the geometry column, or \"\" if not supported.

 This method is the same as the C++ method OGRLayer::GetGeometryColumn()

 @argument[hLayer]{handle to the layer}

 @return{geometry column name.}"
  (hLayer ogr-layer-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SetIgnoredFields" ogr-l-set-ignored-fields) ogr-err
  "Set which fields can be omitted when retrieving features from the
 layer.

 If the driver supports this functionality (testable using
 OLCIgnoreFields capability), it will not fetch the specified fields
 in subsequent calls to GetFeature() / GetNextFeature() and thus save
 some processing time and/or bandwidth.

 Besides field names of the layers, the following special fields can
 be passed: \"OGR_GEOMETRY\" to ignore geometry and \"OGR_STYLE\" to
 ignore layer style.

 By default, no fields are ignored.

 This method is the same as the C++ method OGRLayer::SetIgnoredFields()

 @argument[papszFields]{an array of field names terminated by NULL
 item. If NULL is passed, the ignored list is cleared.}

 @return{OGRERR_NONE if all field names have been resolved (even if
 the driver does not support this method)}"
  (hLayer ogr-layer-h)
  (papszFields (:pointer :string)))	; const char **
(export 'ogr-l-set-ignored-fields)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_Intersection" ogr-l-intersection) ogr-err
  "Intersection of two layers.

 The result layer contains features whose geometries represent areas
 that are common between features in the input layer and in the method
 layer. The features in the result layer have attributes from both
 input and method layers. The schema of the result layer can be set by
 the user or, if it is empty, is initialized to contain all fields in
 the input and method layers.

 Note: If the schema of the result is set by user and contains fields
 that have the same name as a field in input and in method layer, then
 the attribute in the result feature will get the value from the
 feature of the method layer.

 For best performance use the minimum amount of features in the method
 layer and copy it into a memory layer.

 This method relies on GEOS support. Do not use unless the GEOS
 support is compiled in.

The recognized list of options is :

 SKIP_FAILURES=YES/NO. Set it to YES to go on, even when a feature
   could not be inserted.

 PROMOTE_TO_MULTI=YES/NO. Set it to YES to convert Polygons into
   MultiPolygons, or LineStrings to MultiLineStrings.

 INPUT_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the input layer.

 METHOD_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the method layer.

 This function is the same as the C++ method OGRLayer::Intersection().

 @argument[pLayerInput]{the input layer. Should not be NULL.}
 @argument[pLayerMethod]{the method layer. Should not be NULL.}
 @argument[pLayerResult]{the layer where the features resulting from
 the operation are inserted. Should not be NULL. See above the note
 about the schema.}
 @argument[papszOptions]{NULL terminated list of options (may be NULL).}
 @argument[pfnProgress]{a GDALProgressFunc() compatible callback
 function for reporting progress or NULL.}
 @argument[pProgressArg]{argument to be passed to pfnProgress. May be NULL.}

 @return{an error code if there was an error or the execution was
 interrupted, OGRERR_NONE otherwise.}

 Since: OGR 1.10"
  (pLayerInput ogr-layer-h)
  (pLayerMethod ogr-layer-h)
  (pLayerResult ogr-layer-h)
  (papszOptions (:pointer :string))		 ; char **
  (pfnProgress :pointer)			 ; GDALProgressFunc
  (pProgressArg :pointer))			 ; void *
(export 'ogr-l-intersection)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_Union" ogr-l-union) ogr-err
  "Union of two layers.

 The result layer contains features whose geometries represent areas
 that are in either in the input layer or in the method layer. The
 features in the result layer have attributes from both input and
 method layers. For features which represent areas that are only in
 the input or in the method layer the respective attributes have
 undefined values. The schema of the result layer can be set by the
 user or, if it is empty, is initialized to contain all fields in the
 input and method layers.

 Note: If the schema of the result is set by user and contains fields
 that have the same name as a field in input and in method layer, then
 the attribute in the result feature will get the value from the
 feature of the method layer (even if it is undefined).

 For best performance use the minimum amount of features in the method
 layer and copy it into a memory layer.

 This method relies on GEOS support. Do not use unless the GEOS
 support is compiled in.

The recognized list of options is :
 SKIP_FAILURES=YES/NO. Set it to YES to go on, even when a feature could not be inserted.
 PROMOTE_TO_MULTI=YES/NO. Set it to YES to convert Polygons into MultiPolygons, or LineStrings to MultiLineStrings.
 INPUT_PREFIX=string. Set a prefix for the field names that will be created from the fields of the input layer.
 METHOD_PREFIX=string. Set a prefix for the field names that will be created from the fields of the method layer.

 This function is the same as the C++ method OGRLayer::Union().

 @argument[pLayerInput]{the input layer. Should not be NULL.}
 @argument[pLayerMethod]{the method layer. Should not be NULL.}
 @argument[pLayerResult]{the layer where the features resulting from
 the operation are inserted. Should not be NULL. See above the note
 about the schema.}
 @argument[papszOptions]{NULL terminated list of options (may be NULL).}
 @argument[pfnProgress]{a GDALProgressFunc() compatible callback
 function for reporting progress or NULL.}
 @argument[pProgressArg]{argument to be passed to pfnProgress. May be NULL.}

 @return{an error code if there was an error or the execution was
 interrupted, OGRERR_NONE otherwise.}

 Since: OGR 1.10"

  (pLayerInput ogr-layer-h)
  (pLayerMethod ogr-layer-h)
  (pLayerResult ogr-layer-h)
  (papszOptions (:pointer :string))		 ; char **
  (pfnProgress :pointer)			 ; GDALProgressFunc
  (pProgressArg :pointer))			 ; void *
(export 'ogr-l-union)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_SymDifference" ogr-l-sym-difference) ogr-err
  "Symmetrical difference of two layers.

 The result layer contains features whose geometries represent areas
 that are in either in the input layer or in the method layer but not
 in both. The features in the result layer have attributes from both
 input and method layers. For features which represent areas that are
 only in the input or in the method layer the respective attributes
 have undefined values. The schema of the result layer can be set by
 the user or, if it is empty, is initialized to contain all fields in
 the input and method layers.

 Note: If the schema of the result is set by user and contains fields
 that have the same name as a field in input and in method layer, then
 the attribute in the result feature will get the value from the
 feature of the method layer (even if it is undefined).

 For best performance use the minimum amount of features in the method
 layer and copy it into a memory layer.

 This method relies on GEOS support. Do not use unless the GEOS
 support is compiled in.

The recognized list of options is :
 SKIP_FAILURES=YES/NO. Set it to YES to go on, even when a feature could not be inserted.
 PROMOTE_TO_MULTI=YES/NO. Set it to YES to convert Polygons into MultiPolygons, or LineStrings to MultiLineStrings.
 INPUT_PREFIX=string. Set a prefix for the field names that will be created from the fields of the input layer.
 METHOD_PREFIX=string. Set a prefix for the field names that will be created from the fields of the method layer.

 This function is the same as the C++ method
 OGRLayer::SymDifference().

 @argument[pLayerInput]{the input layer. Should not be NULL.}
 @argument[pLayerMethod]{the method layer. Should not be NULL.}
 @argument[pLayerResult]{the layer where the features resulting from
 the operation are inserted. Should not be NULL. See above the note
 about the schema.}
 @argument[papszOptions]{NULL terminated list of options (may be NULL).}
 @argument[pfnProgress]{a GDALProgressFunc() compatible callback
 function for reporting progress or NULL.}
 @argument[pProgressArg]{argument to be passed to pfnProgress. May be NULL.}

 @return{an error code if there was an error or the execution was
 interrupted, OGRERR_NONE otherwise.}

 Since: OGR 1.10"
  (pLayerInput ogr-layer-h)
  (pLayerMethod ogr-layer-h)
  (pLayerResult ogr-layer-h)
  (papszOptions (:pointer :string))	; char **
  (pfnProgress :pointer)		; GDALProgressFunc
  (pProgressArg :pointer))		; void *
(export 'ogr-l-sym-difference)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_Identity" ogr-l-identity) ogr-err
  "Identify the features of this layer with the ones from the identity
 layer.

 The result layer contains features whose geometries represent areas
 that are in the input layer. The features in the result layer have
 attributes from both input and method layers. The schema of the
 result layer can be set by the user or, if it is empty, is
 initialized to contain all fields in input and method layers.

 Note: If the schema of the result is set by user and contains fields
 that have the same name as a field in input and in method layer, then
 the attribute in the result feature will get the value from the
 feature of the method layer (even if it is undefined).

 For best performance use the minimum amount of features in the method
 layer and copy it into a memory layer.

 This method relies on GEOS support. Do not use unless the GEOS
 support is compiled in.

The recognized list of options is :
 SKIP_FAILURES=YES/NO. Set it to YES to go on, even when a feature could not be inserted.
 PROMOTE_TO_MULTI=YES/NO. Set it to YES to convert Polygons into MultiPolygons, or LineStrings to MultiLineStrings.
 INPUT_PREFIX=string. Set a prefix for the field names that will be created from the fields of the input layer.
 METHOD_PREFIX=string. Set a prefix for the field names that will be created from the fields of the method layer.

 This function is the same as the C++ method OGRLayer::Identity().

 @argument[pLayerInput]{the input layer. Should not be NULL.}
 @argument[pLayerMethod]{the method layer. Should not be NULL.}
 @argument[pLayerResult]{the layer where the features resulting from
 the operation are inserted. Should not be NULL. See above the note
 about the schema.}
 @argument[papszOptions]{NULL terminated list of options (may be NULL).}
 @argument[pfnProgress]{a GDALProgressFunc() compatible callback
 function for reporting progress or NULL.}
 @argument[pProgressArg]{argument to be passed to pfnProgress. May be NULL.}

 @return{an error code if there was an error or the execution was
 interrupted, OGRERR_NONE otherwise.}

 Since: OGR 1.10"
  (pLayerInput ogr-layer-h)
  (pLayerMethod ogr-layer-h)
  (pLayerResult ogr-layer-h)
  (papszOptions (:pointer :string))	; char **
  (pfnProgress :pointer)		; GDALProgressFunc
  (pProgressArg :pointer))		; void *
(export 'ogr-l-identity)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_Update" ogr-l-update) ogr-err
  "Update this layer with features from the update layer.

 The result layer contains features whose geometries represent areas
 that are either in the input layer or in the method layer. The
 features in the result layer have areas of the features of the method
 layer or those ares of the features of the input layer that are not
 covered by the method layer. The features of the result layer get
 their attributes from the input layer. The schema of the result layer
 can be set by the user or, if it is empty, is initialized to contain
 all fields in the input layer.

 Note: If the schema of the result is set by user and contains fields
 that have the same name as a field in the method layer, then the
 attribute in the result feature the originates from the method layer
 will get the value from the feature of the method layer.

 For best performance use the minimum amount of features in the method
 layer and copy it into a memory layer.

 This method relies on GEOS support. Do not use unless the GEOS
 support is compiled in.

The recognized list of options is :

 SKIP_FAILURES=YES/NO. Set it to YES to go on, even when a feature
   could not be inserted.

 PROMOTE_TO_MULTI=YES/NO. Set it to YES to convert Polygons into
   MultiPolygons, or LineStrings to MultiLineStrings.

 INPUT_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the input layer.

 METHOD_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the method layer.

 This function is the same as the C++ method OGRLayer::Update().

 @argument[pLayerInput]{the input layer. Should not be NULL.}
 @argument[pLayerMethod]{the method layer. Should not be NULL.}
 @argument[pLayerResult]{the layer where the features resulting from
 the operation are inserted. Should not be NULL. See above the note
 about the schema.}
 @argument[papszOptions]{NULL terminated list of options (may be NULL).}
 @argument[pfnProgress]{a GDALProgressFunc() compatible callback
 function for reporting progress or NULL.}
 @argument[pProgressArg]{argument to be passed to pfnProgress. May be
 NULL.}

 @return{an error code if there was an error or the execution was
 interrupted, OGRERR_NONE otherwise.}

 Since: OGR 1.10"
  (pLayerInput ogr-layer-h)
  (pLayerMethod ogr-layer-h)
  (pLayerResult ogr-layer-h)
  (papszOptions (:pointer :string))	; char **
  (pfnProgress :pointer)		; GDALProgressFunc
  (pProgressArg :pointer))		; void *
(export 'ogr-l-update)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_Clip" ogr-l-clip) ogr-err
  "Clip off areas that are not covered by the method layer.

 The result layer contains features whose geometries represent areas
 that are in the input layer and in the method layer. The features in
 the result layer have the (possibly clipped) areas of features in the
 input layer and the attributes from the same features. The schema of
 the result layer can be set by the user or, if it is empty, is
 initialized to contain all fields in the input layer.

 Note: For best performance use the minimum amount of features in the
 method layer and copy it into a memory layer.

 This method relies on GEOS support. Do not use unless the GEOS
 support is compiled in.

The recognized list of options is :

 SKIP_FAILURES=YES/NO. Set it to YES to go on, even when a feature
   could not be inserted.

 PROMOTE_TO_MULTI=YES/NO. Set it to YES to convert Polygons into
   MultiPolygons, or LineStrings to MultiLineStrings.

 INPUT_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the input layer.

 METHOD_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the method layer.

 This function is the same as the C++ method OGRLayer::Clip().

 @argument[pLayerInput]{the input layer. Should not be NULL.}
 @argument[pLayerMethod]{the method layer. Should not be NULL.}
 @argument[pLayerResult]{the layer where the features resulting from
 the operation are inserted. Should not be NULL. See above the note
 about the schema.}
 @argument[papszOptions]{NULL terminated list of options (may be NULL).}
 @argument[pfnProgress]{a GDALProgressFunc() compatible callback
 function for reporting progress or NULL.}
 @argument[pProgressArg]{argument to be passed to pfnProgress. May be
 NULL.}

 @return{an error code if there was an error or the execution was
 interrupted, OGRERR_NONE otherwise.}

 Since: OGR 1.10"

  (pLayerInput ogr-layer-h)
  (pLayerMethod ogr-layer-h)
  (pLayerResult ogr-layer-h)
  (papszOptions (:pointer :string))	; char **
  (pfnProgress :pointer)		; GDALProgressFunc
  (pProgressArg :pointer))		; void *
(export 'ogr-l-clip)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_L_Erase" ogr-l-erase) ogr-err
  "Remove areas that are covered by the method layer.

 The result layer contains features whose geometries represent areas
 that are in the input layer but not in the method layer. The features
 in the result layer have attributes from the input layer. The schema
 of the result layer can be set by the user or, if it is empty, is
 initialized to contain all fields in the input layer.

 Note: For best performance use the minimum amount of features in the
 method layer and copy it into a memory layer.

 This method relies on GEOS support. Do not use unless the GEOS
 support is compiled in.

 The recognized list of options is :

 SKIP_FAILURES=YES/NO. Set it to YES to go on, even when a feature
   could not be inserted.

 PROMOTE_TO_MULTI=YES/NO. Set it to YES to convert Polygons into
   MultiPolygons, or LineStrings to MultiLineStrings.

 INPUT_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the input layer.

 METHOD_PREFIX=string. Set a prefix for the field names that will be
   created from the fields of the method layer.

 This function is the same as the C++ method OGRLayer::Erase().

 @argument[pLayerInput]{the input layer. Should not be NULL.}
 @argument[pLayerMethod]{the method layer. Should not be NULL.}
 @argument[pLayerResult]{the layer where the features resulting from
 the operation are inserted. Should not be NULL. See above the note
 about the schema.}
 @argument[papszOptions]{NULL terminated list of options (may be NULL).}
 @argument[pfnProgress]{a GDALProgressFunc() compatible callback
 function for reporting progress or NULL.}
 @argument[pProgressArg]{argument to be passed to pfnProgress. May be NULL.}

 @return{an error code if there was an error or the execution was
 interrupted, OGRERR_NONE otherwise.}

 Since: OGR 1.10"
  (pLayerInput ogr-layer-h)
  (pLayerMethod ogr-layer-h)
  (pLayerResult ogr-layer-h)
  (papszOptions (:pointer :string))	; char **
  (pfnProgress :pointer)		; GDALProgressFunc
  (pProgressArg :pointer))		; void *
(export 'ogr-l-erase)

;; --------------------------------------------------------
;; CLOS
;; --------------------------------------------------------

(defmethod get-name ((layer <layer>))
  (ogr-l-get-name (pointer layer)))

;; --------------------------------------------------------

(defmethod get-type ((layer <layer>))
  (ogr-l-get-geom-type (pointer layer)))

;; --------------------------------------------------------

(defgeneric get-next-feature (l)
  (:documentation "")
  (:method ((layer <layer>))
    (let ((feature-h (ogr-l-get-next-feature (pointer layer))))
      (unless (cffi:null-pointer-p feature-h)
	(make-instance '<feature>' :pointer feature-h)))))
(export 'get-next-feature)

;; --------------------------------------------------------

(defgeneric get-feature (l idx)
  (:documentation "")
  (:method ((layer <layer>) (idx integer))
    (make-instance '<feature>'
		   :pointer (ogr-l-get-feature (pointer layer)
					       idx))))
(export 'get-feature)

;; --------------------------------------------------------

(defgeneric get-feature-count (l &optional bf)
  (:documentation "BF is for brute-force.")
  (:method ((layer <layer>) &optional bf)
    (let ((count (ogr-l-get-feature-count (pointer layer)
					  (if bf 1 0))))
      (when (>= count 0)
	count))))
(export 'get-feature-count)

;; --------------------------------------------------------

(defgeneric get-geometry-column (l)
  (:documentation "")
  (:method ((layer <layer>))
    (ogr-l-get-geometry-column (pointer layer))))
(export 'get-geometry-column)

;; --------------------------------------------------------

(defmethod get-spatial-ref ((l <layer>))
  ;; return NIL if there is no spatial-ref
  (let ((ref (ogr-l-get-spatial-ref (pointer l))))
    (unless (cffi:null-pointer-p ref)
      (make-instance '<spatial-ref>
                     :pointer ref))))

;; --------------------------------------------------------

(defmethod get-extent ((l <layer>))
  (cffi:with-foreign-object (envelope '(:struct ogr-envelope))
    (let ((ret (ogr-l-get-extent (ogr:pointer l) envelope 0)))
      ;; ogr-err checking
      (unless (eql ret :none)
	(error (make-condition '<ogr-error> :error-code ret))))
    (cffi:with-foreign-slots ((minx maxx miny maxy)
			      envelope
			      (:struct ogr-envelope))
      (make-instance '<envelope>
		     :min-x minx
		     :min-y miny
		     :max-x maxx
		     :max-y maxy))))

;; EOF
