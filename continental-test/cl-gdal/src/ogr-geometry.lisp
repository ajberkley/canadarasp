;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

;; OGRGeometry <ogr_api.h>

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetX" ogr-g-getx) :double
  (geom ogr-geometry-h)
  (i :int))
(export 'ogr-g-getx)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetY" ogr-g-gety) :double
  (geom :pointer)
  (i :int))
(export 'ogr-g-gety)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetZ" ogr-g-getz) :double
  (geom :pointer)
  (i :int))
(export 'ogr-g-getz)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CreateFromWkb" ogr-g-create-from-wkb) ogr-err
  "Create a geometry object of the appropriate type from it's well
 known binary representation.

 Note that if nBytes is passed as zero, no checking can be done on
 whether the pabyData is sufficient. This can result in a crash if the
 input data is corrupt. This function returns no indication of the
 number of bytes from the data source actually used to represent the
 returned geometry object. Use OGR_G_WkbSize() on the returned
 geometry to establish the number of bytes it required in WKB format.

 The OGRGeometryFactory::createFromWkb() CPP method is the same as this function.

 @argument[pabyData]{pointer to the input BLOB data.}

 @argument[hSRS]{handle to the spatial reference to be assigned to the
 created geometry object. This may be NULL.}

 @argument[phGeometry]{the newly created geometry object will be
 assigned to the indicated handle on return. This will be NULL in case
 of failure. If not NULL, *phGeometry should be freed with
 OGR_G_DestroyGeometry() after use.}

 @argument[nBytes]{the number of bytes of data available in pabyData,
 or -1 if it is not known, but assumed to be sufficient.}

 @return{:NONE if all goes well, otherwise any of
 :NOT_ENOUGH_DATA, :UNSUPPORTED_GEOMETRY_TYPE, or
 :CORRUPT_DATA may be returned.}"
  (pabyData :string)
  (hSRS ogr-spatial-reference-h)
  (phGeometry (:pointer ogr-geometry-h))
  (nBytes :int))
(export 'ogr-g-create-from-wkb)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CreateFromWkt" ogr-g-create-from-wkt) ogr-err
  "Create a geometry object of the appropriate type from it's well
 known text representation.

 The OGRGeometryFactory::createFromWkt CPP method is the same as this
 function.

 @argument[ppszData]{input zero terminated string containing well
 known text representation of the geometry to be created. The pointer
 is updated to point just beyond that last character consumed.}

 @argument[hSRS]{handle to the spatial reference to be assigned to the
 created geometry object. This may be NULL.}

 @argument[phGeometry]{the newly created geometry object will be
 assigned to the indicated handle on return. This will be NULL if the
 method fails. If not NULL, *phGeometry should be freed with
 OGR_G_DestroyGeometry() after use.}

 @return{NONE if all goes well, otherwise any of
 NOT_ENOUGH_DATA, UNSUPPORTED_GEOMETRY_TYPE, or
 CORRUPT_DATA may be returned.}"
  (ppszData (:pointer :string))
  (hSRS ogr-spatial-reference-h)
  (phGeometry (:pointer ogr-geometry-h)))
(export 'OGR-G-Create-From-Wkt)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_DestroyGeometry" ogr-g-destroy-geometry) :void
  "Destroy geometry object."
  (geom :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CreateGeometry" ogr-g-create-geometry) ogr-geometry-h ; OGRGeometryH
"Create an empty geometry of desired type.

 This is equivalent to allocating the desired geometry with new, but
 the allocation is guaranteed to take place in the context of the
 GDAL/OGR heap.

 This function is the same as the CPP method
 OGRGeometryFactory::createGeometry.

 @argument[eGeometryType]{the type code of the geometry to be created.}

 @return{handle to the newly create geometry or NULL on
 failure. Should be freed with OGR_G_DestroyGeometry() after use.}"
  (eGeometryType OGR-wkb-Geometry-Type))			; OGR-wkb-Geometry-Type
(export 'ogr-g-create-geometry)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ApproximateArcAngles" OGR-G-Approximate-Arc-Angles) ogr-geometry-h ;OGRGeometryH
  "Stroke arc to linestring.

Stroke an arc of a circle to a linestring based on a center point,
radius, start angle and end angle, all angles in degrees.

If the dfMaxAngleStepSizeDegrees is zero, then a default value will be
used. This is currently 4 degrees unless the user has overridden the
value with the OGR_ARC_STEPSIZE configuration variable.

See also:
CPLSetConfigOption()

@argument[dfCenterX]{center X}
@argument[dfCenterY]{center Y}
@argument[fZ]{center Z}
@argument[fPrimaryRadius]{X radius of ellipse.}
@argument[fSecondaryRadius]{Y radius of ellipse.}
@argument[fRotation]{rotation of the ellipse clockwise.}
@argument[fStartAngle]{angle to first point on arc (clockwise of X-positive)}
@argument[fEndAngle]{angle to last point on arc (clockwise of X-positive)}
@argument[fMaxAngleStepSizeDegrees]{the largest step in degrees along the arc, zero to use the default setting.}

Returns:
OGRLineString geometry representing an approximation of the arc.
Since:
OGR 1.8.0"

  (dfCenterX :double)
  (dfCenterY :double)
  (dfZ :double)
  (dfPrimaryRadius :double)
  (dfSecondaryRadius :double)
  (dfRotation :double)
  (dfStartAngle :double)
  (dfEndAngle :double)
  (dfMaxAngleStepSizeDegrees :double))
(export 'ogr-g-approximate-arc-angles)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToPolygon" ogr-g-force-to-polygon) ogr-geometry-h ; OGRGeometryH
  "Convert to polygon.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToPolygon().

 @argument[hGeom]{handle to the geometry to convert (ownership
 surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom :pointer))			; OGRGeometryH
(export 'OGR-G-Force-To-Polygon)

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_G_ForceToMultiLineString" OGR_G_ForceToMultiLineString) ogr-geometry-h
  "Convert to multilinestring.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiLineString().

 @argument[hGeom]{handle to the geometry to convert (ownership surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom ogr-geometry-h))			; OGRGeometryH

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToMultiPolygon" OGR-G-Force-To-Multi-Polygon) ogr-geometry-h
  "Convert to multipolygon.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiPolygon().

 @argument[hGeom]{handle to the geometry to convert (ownership
 surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom ogr-geometry-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToMultiPoint" ogr-g-force-to-multi-point) ogr-geometry-h
  "Convert to multipoint.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiPoint().

 @argument[hGeom]{handle to the geometry to convert (ownership
 surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom ogr-geometry-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ForceToMultiLineString" OGR-G-Force-To-Multi-Line-String) ogr-geometry-h ; OGRGeometryH
  "Convert to multilinestring.

 This function is the same as the C++ method
 OGRGeometryFactory::forceToMultiLineString().

 @argument[hGeom]{handle to the geometry to convert (ownership surrendered).}

 @return{the converted geometry (ownership to caller).}

 Since: GDAL/OGR 1.8.0"
  (hGeom :pointer))			; OGRGeometryH

;; --------------------------------------------------------


(cffi:defcfun ("OGR_G_GetDimension" OGR-G-Get-Dimension) :int
  "Get the dimension of this geometry.

 This function corresponds to the SFCOM IGeometry::GetDimension()
 method. It indicates the dimension of the geometry, but does not
 indicate the dimension of the underlying space (as indicated by
 OGR_G_GetCoordinateDimension() function).

 This function is the same as the CPP method
 OGRGeometry::getDimension().

 @argument[hGeom]{handle on the geometry to get the dimension from.}
 @return{0 for points, 1 for lines and 2 for surfaces.}"
  (hGeom ogr-geometry-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetCoordinateDimension" ogr-g-get-coordinate-dimension) :int
  "Get the dimension of the coordinates in this geometry.

 This function corresponds to the SFCOM IGeometry::GetDimension()
 method.

 This function is the same as the CPP method
 OGRGeometry::getCoordinateDimension().

 @argument[hGeom]{handle on the geometry to get the dimension of the
 coordinates from.}

 @return{in practice this will return 2 or 3. It can also return 0 in
 the case of an empty point.}"
  (hGeom ogr-geometry-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_SetCoordinateDimension" ogr-g-set-coordinate-dimension) :void
  "Set the coordinate dimension.

 This method sets the explicit coordinate dimension. Setting the
 coordinate dimension of a geometry to 2 should zero out any existing
 Z values. Setting the dimension of a geometry collection will not
 necessarily affect the children geometries.

 @argument[hGeom]{handle on the geometry to set the dimension of the
 coordinates.}
 @argument[nNewDimension]{New coordinate dimension value, either 2 or
 3.}"
  (hGeom ogr-geometry-h)
  (nNewDimension :int))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Clone" ogr-g-clone) ogr-geometry-h
  "Make a copy of this object.

 This function relates to the SFCOM IGeometry::clone() method.

 This function is the same as the CPP method OGRGeometry::clone().

 @argument[hGeom]{handle on the geometry to clone from.}

 @return{an handle on the copy of the geometry with the spatial
 reference system as the original.}"
  (hGeom ogr-geometry-h))
(export 'ogr-g-clone)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetEnvelope" ogr-g-get-envelope) :void
  "Computes and returns the bounding envelope for this geometry in
 the passed psEnvelope structure.

 This function is the same as the CPP method OGRGeometry::getEnvelope().

 @argument[hGeom]{handle of the geometry to get envelope from.}
 @argument[psEnvelope]{the structure in which to place the results.}"

  (hGeom ogr-geometry-h)
  (psEnvelope (:pointer (:struct ogr-envelope))))
(export 'ogr-g-get-envelope)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetEnvelope3D" OGR-G-Get-Envelope3D) :void
  "Computes and returns the bounding envelope (3D) for this geometry in
 the passed psEnvelope structure.

 This function is the same as the CPP method
 OGRGeometry::getEnvelope().

 @argument[hGeom]{handle of the geometry to get envelope from.}
 @argument[psEnvelope]{the structure in which to place the results.}

 Since: OGR 1.9.0"
  (hGeom ogr-geometry-h)
  (psEnvelope :pointer))		; OGREnvelope3D*


;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ImportFromWkb" %OGR-G-Import-From-Wkb) ogr-err
  (hGeom ogr-geometry-h)
  (pabyData :string)			; unsigned char *
  (nSize :int))

(defun OGR-G-Import-From-Wkb (hGeom pabyData nSize)
  "Assign geometry from well known binary data.

 The object must have already been instantiated as the correct derived
 type of geometry object to match the binaries type.

 This function relates to the SFCOM IWks::ImportFromWKB() method.

 This function is the same as the CPP method OGRGeometry::importFromWkb().

 @argument[hGeom]{handle on the geometry to assign the well know
 binary data to.}
 @argument[pabyData]{the binary input data.}
 @argument[nSize]{the size of pabyData in bytes, or zero if not known.}

 @return{NONE if all goes well, otherwise any
 of :NOT_ENOUGH_DATA, :UNSUPPORTED_GEOMETRY_TYPE, or :CORRUPT_DATA may
 be returned.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-G-Import-From-Wkb hGeom pabyData nSize)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToWkb" %OGR-G-Export-To-Wkb) ogr-err
  (hGeom ogr-geometry-h)
  (eOrder :int)				; OGRwkbByteOrder
  (pabyDstBuffer :string))		; unsigned char *

(defun OGR-G-Export-To-Wkb (hGeom eOrder pabyDstBuffer)
  "Convert a geometry into well known binary format.

 This function relates to the SFCOM IWks::ExportToWKB() method.

 This function is the same as the CPP method OGRGeometry::exportToWkb().

 @argument[hGeom]{handle on the geometry to convert to a well know
 binary data from.}
 @argument[eOrder]{One of wkbXDR or wkbNDR indicating MSB or LSB byte
 order respectively.}
 @argument[pabyDstBuffer]{a buffer into which the binary
 representation is written. This buffer must be at least
 OGR_G_WkbSize() byte in size.}
 @return{Currently :NONE is always returned.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-G-Export-To-Wkb hGeom eOrder pabyDstBuffer)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_WkbSize" OGR-G-Wkb-Size) :int
  "Returns size of related binary representation.

 This function returns the exact number of bytes required to hold the
 well known binary representation of this geometry object. Its
 computation may be slightly expensive for complex geometries.

 This function relates to the SFCOM IWks::WkbSize() method.

 This function is the same as the CPP method OGRGeometry::WkbSize().

 @argument[hGeom]{handle on the geometry to get the binary size from.}

 @return{size of binary representation in bytes.}"
  (hGeom ogr-geometry-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ImportFromWkt" %OGR-G-Import-From-Wkt) :int ; OGRErr
  (hGeom :pointer)			; OGRGeometryH
  (ppszSrcText (:pointer :string)))	; char **

(defun OGR-G-Import-From-Wkt (hGeom ppszSrcText)
  "Assign geometry from well known text data.

 The object must have already been instantiated as the correct derived
 type of geometry object to match the text type.

 This function relates to the SFCOM IWks::ImportFromWKT() method.

 This function is the same as the CPP method OGRGeometry::importFromWkt().

 @argument[hGeom]{handle on the geometry to assign well know text data to.}

 @argument[ppszSrcText]{pointer to a pointer to the source text. The
 pointer is updated to pointer after the consumed text.}

 @return{:NONE if all goes well, otherwise any of
 :NOT_ENOUGH_DATA, :UNSUPPORTED_GEOMETRY_TYPE, or
 :CORRUPT_DATA may be returned.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-G-Import-From-Wkt hGeom ppszSrcText)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToWkt" %OGR-G-Export-To-Wkt) :int ; OGRErr
  (hGeom :pointer)			; OGRGeometryH
  (ppszSrcText (:pointer :string)))	; char **

(defun OGR-G-Export-To-Wkt (hGeom ppszSrcText)
  "Convert a geometry into well known text format.

 This function relates to the SFCOM IWks::ExportToWKT() method.

 This function is the same as the CPP method OGRGeometry::exportToWkt().

 @argument[hGeom]{handle on the geometry to convert to a text format from.}

 @argument[ppszSrcText]{a text buffer is allocated by the program, and
 assigned to the passed pointer. After use, *ppszDstText should be
 freed with OGRFree().}

 @return{Currently :NONE is always returned.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-G-Export-To-Wkt hGeom ppszSrcText)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetGeometryType" ogr-g-get-geometry-type) ogr-wkb-geometry-type
  "Fetch geometry type.

 Note that the geometry type may include the 2.5D flag. To get a 2D
 flattened version of the geometry type apply the wkbFlatten() macro
 to the return result.

 This function is the same as the CPP method
 OGRGeometry::getGeometryType().

 @argument[hGeom]{handle on the geometry to get type from.}

 @return{the geometry type code.}"
  (hGeom ogr-geometry-h))
(export 'ogr-g-get-geometry-type)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetGeometryName" ogr-g-get-geometry-name) :string ; const char*
  "Fetch WKT name for geometry type.

 There is no SFCOM analog to this function.

 This function is the same as the CPP method OGRGeometry::getGeometryName().

 @argument[hGeom]{handle on the geometry to get name from.}

 @return{name used for this geometry type in well known text format.}"
  (hGeom :pointer))			; OGRGeometryH
(export 'OGR-G-Get-Geometry-Name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_FlattenTo2D" OGR-G-Flatten-To-2D) :void
  "Convert geometry to strictly 2D. In a sense this converts all Z coordinates to 0.0.

 This function is the same as the CPP method OGRGeometry::flattenTo2D().

 @argument[hGeom]{handle on the geometry to convert.}"
  (hGeom :pointer))			; OGRGeometryH
(export 'OGR-G-Flatten-To-2D)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CloseRings" OGR-G-Close-Rings) :void
  "Force rings to be closed.

 If this geometry, or any contained geometries has polygon rings that
 are not closed, they will be closed by adding the starting point at
 the end.

 @argument[hGeom]{handle to the geometry.}"
  (hGeom :pointer))			; OGRGeometryH
(export 'OGR-G-Close-Rings)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_CreateFromGML" OGR-G-Create-From-GML) :pointer ; OGRGeometryH
  "Create geometry from GML.

 This method translates a fragment of GML containing only the geometry
 portion into a corresponding OGRGeometry. There are many limitations
 on the forms of GML geometries supported by this parser, but they are
 too numerous to list here.

 The following GML2 elements are parsed : Point, LineString, Polygon,
 MultiPoint, MultiLineString, MultiPolygon, MultiGeometry.

 (OGR >= 1.8.0) The following GML3 elements are parsed : Surface,
 MultiSurface, PolygonPatch, Triangle, Rectangle, Curve, MultiCurve,
 CompositeCurve, LineStringSegment, Arc, Circle, CompositeSurface,
 OrientableSurface, Solid, Tin, TriangulatedSurface.

 Arc and Circle elements are stroked to linestring, by using a 4
 degrees step, unless the user has overridden the value with the
 OGR_ARC_STEPSIZE configuration variable.

 The C++ method OGRGeometryFactory::createFromGML() is the same as
 this function.

 @argument[pszGML]{The GML fragment for the geometry.}

 @return{a geometry on succes, or NULL on error.}"
  (pszGML :string))			; const char *
(export 'OGR-G-Create-From-GML)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToGML" OGR-G-Export-To-GML) :string ; char*
  "Convert a geometry into GML format.

 The GML geometry is expressed directly in terms of GML basic data
 types assuming the this is available in the gml namespace. The
 returned string should be freed with CPLFree() when no longer
 required.

 This method is the same as the C++ method OGRGeometry::exportToGML().

 @argument[hGeometry]{handle to the geometry.}

 @return{A GML fragment or NULL in case of error.}"
  (hGeometry :pointer))			; OGRGeometryH
(export 'OGR-G-Export-To-GML)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToGMLEx" OGR-G-Export-To-GML-Ex) :string ; char*
  "Convert a geometry into GML format.

 The GML geometry is expressed directly in terms of GML basic data
 types assuming the this is available in the gml namespace. The
 returned string should be freed with CPLFree() when no longer
 required.

 The supported options in OGR 1.8.0 are :

 FORMAT=GML3. Otherwise it will default to GML 2.1.2 output.

 GML3_LINESTRING_ELEMENT=curve. (Only valid for FORMAT=GML3) To use
 gml:Curve element for linestrings. Otherwise gml:LineString will be
 used .

 GML3_LONGSRS=YES/NO. (Only valid for FORMAT=GML3) Default to YES. If
 YES, SRS with EPSG authority will be written with
 the \"urn:ogc:def:crs:EPSG::\" prefix. In the case, if the SRS is a
 geographic SRS without explicit AXIS order, but that the same SRS
 authority code imported with ImportFromEPSGA() should be treated as
 lat/long, then the function will take care of coordinate order
 swapping. If set to NO, SRS with EPSG authority will be written with
 the \"EPSG:\" prefix, even if they are in lat/long order.

 GMLID=astring. If specified, a gml:id attribute will be written in
 the top-level geometry element with the provided value. Required for
 GML 3.2 compatibility.

 This method is the same as the C++ method OGRGeometry::exportToGML().

 @argument[hGeometry]{handle to the geometry.}
 @argument[papszOptions]{NULL-terminated list of options.}

 @return{A GML fragment or NULL in case of error.}
 Since: OGR 1.8.0"
  (hGeometry :pointer)			; OGRGeometryH
  (papszOptions (:pointer :string)))	; char **
(export 'OGR-G-Export-To-GML-Ex)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToKML" OGR-G-Export-To-KML) :string ; char*
  "Convert a geometry into KML format.

 The returned string should be freed with CPLFree() when no longer
 required.

 This method is the same as the C++ method OGRGeometry::exportToKML().

 @argument[hGeometry]{handle to the geometry.}

 @argument[pszAltitudeMode]{value to write in altitudeMode element, or
 NULL.}

 @return{A KML fragment or NULL in case of error.}"
  (hGeometry :pointer)			; OGRGeometryH
  (pszAltitudeMode :string))		; const char *
(export 'OGR-G-Export-To-KML)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToJson" OGR-G-Export-To-Json) :string ; char*
  "Convert a geometry into GeoJSON format.

 The returned string should be freed with CPLFree() when no longer
 required.

 This method is the same as the C++ method
 OGRGeometry::exportToJson().

 @argument[hGeometry]{handle to the geometry.}

 @return{A GeoJSON fragment or NULL in case of error.}"
  (hGeometry :pointer))			; OGRGeometryH
(export 'OGR-G-Export-To-Json)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ExportToJsonEx" OGR-G-Export-To-Json-Ex) :string ; char*
  "Convert a geometry into GeoJSON format.

 The returned string should be freed with CPLFree() when no longer required.

 This method is the same as the C++ method OGRGeometry::exportToJson().

 @argument[hGeometry]{handle to the geometry.}

 @argument[papszOptions]{a null terminated list of options. For now,
 only COORDINATE_PRECISION=int_number where int_number is the maximum
 number of figures after decimal separator to write in coordinates.}

 @return{A GeoJSON fragment or NULL in case of error.}

 Since: OGR 1.9.0"
  (hGeometry :pointer)			; OGRGeometryH
  (papszOptions (:pointer :string)))	; char **
(export 'OGR-G-Export-To-Json-Ex)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_AssignSpatialReference" ogr-g-assign-spatial-reference) :void
  "Assign spatial reference to this object.

 Any existing spatial reference is replaced, but under no
 circumstances does this result in the object being reprojected. It is
 just changing the interpretation of the existing geometry. Note that
 assigning a spatial reference increments the reference count on the
 OGRSpatialReference, but does not copy it.

 This is similar to the SFCOM IGeometry::put_SpatialReference()
 method.

 This function is the same as the CPP method
 OGRGeometry::assignSpatialReference.

 @argument[hGeom]{handle on the geometry to apply the new spatial
 reference system.}

 @argument[hSRS]{handle on the new spatial reference system to apply.}"
  (hGeom ogr-geometry-h)
  (hSRS ogr-spatial-reference-h))
(export 'ogr-g-assign-spatial-reference)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetSpatialReference" ogr-g-get-spatial-reference) ogr-spatial-reference-h
  "Returns spatial reference system for geometry.

 This function relates to the SFCOM IGeometry::get_SpatialReference()
 method.

 This function is the same as the CPP method
 OGRGeometry::getSpatialReference().

 @argument[hGeom]{handle on the geometry to get spatial reference
 from.}

 @return{a reference to the spatial reference geometry.}"
  (hGeom ogr-geometry-h))
(export 'ogr-g-get-spatial-reference)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Transform" %ogr-g-transform) ogr-err
  (hGeom ogr-geometry-h)
  (hTransform :pointer))		; OGRCoordinateTransformationH

(defun OGR-G-Transform (hGeom hTransform)
  "Apply arbitrary coordinate transformation to geometry.

 This function will transform the coordinates of a geometry from their
 current spatial reference system to a new target spatial reference
 system. Normally this means reprojecting the vectors, but it could
 include datum shifts, and changes of units.

 Note that this function does not require that the geometry already
 have a spatial reference system. It will be assumed that they can be
 treated as having the source spatial reference system of the
 OGRCoordinateTransformation object, and the actual SRS of the
 geometry will be ignored. On successful completion the output
 OGRSpatialReference of the OGRCoordinateTransformation will be
 assigned to the geometry.

 This function is the same as the CPP method OGRGeometry::transform.

 @argument[hGeom]{handle on the geometry to apply the transform to.}

 @argument[hTransform]{handle on the transformation to apply.}

 @return{:NONE on success or an error code.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-G-Transform hGeom hTransform)))
(export 'ogr-g-transform)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_TransformTo" %OGR-G-Transform-To) :int ; OGRErr
  (hGeom :pointer)			; OGRGeometryH
  (hSRS :pointer))			; OGRSpatialReferenceH

(defun OGR-G-Transform-To (hGeom hSRS)
  "Transform geometry to new spatial reference system.

 This function will transform the coordinates of a geometry from their
 current spatial reference system to a new target spatial reference
 system. Normally this means reprojecting the vectors, but it could
 include datum shifts, and changes of units.

 This function will only work if the geometry already has an assigned
 spatial reference system, and if it is transformable to the target
 coordinate system.

 Because this function requires internal creation and initialization
 of an OGRCoordinateTransformation object it is significantly more
 expensive to use this function to transform many geometries than it
 is to create the OGRCoordinateTransformation in advance, and call
 transform() with that transformation. This function exists primarily
 for convenience when only transforming a single geometry.

 This function is the same as the CPP method OGRGeometry::transformTo.

 @argument[hGeom]{handle on the geometry to apply the transform to.}

 @argument[hSRS]{handle on the spatial reference system to apply.}

 @return{:NONE on success, or an error code.}"
  (%OGR-G-Transform-To hGeom hSRS))
(export 'OGR-G-Transform-To)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Simplify" ogr-g-simplify) ogr-geometry-h
  "Compute a simplified geometry.

 This function is the same as the C++ method OGRGeometry::Simplify().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry.}

 @argument[dTolerance]{the distance tolerance for the simplification.}

 @return{the simplified geometry or NULL if an error occurs.}

 Since: OGR 1.8.0"
  (hThis ogr-geometry-h)
  (dTolerance :double))			; double
(export 'OGR-G-Simplify)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_SimplifyPreserveTopology" ogr-g-simplify-preserve-topology) ogr-geometry-h
  "Simplify the geometry while preserving topology.

 This function is the same as the C++ method
 OGRGeometry::SimplifyPreserveTopology().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry.}

 @argument[dTolerance]{the distance tolerance for the simplification.}

 @return{the simplified geometry or NULL if an error occurs.}

 Since: OGR 1.9.0"
  (hThis ogr-geometry-h)
  (dTolerance :double))
(export 'OGR-G-Simplify-Preserve-Topology)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Segmentize" OGR-G-Segmentize) :void
  "Modify the geometry such it has no segment longer then the given
 distance.

 Interpolated points will have Z and M values (if needed) set to
 0. Distance computation is performed in 2d only

 This function is the same as the CPP method OGRGeometry::segmentize().

 @argument[hGeom]{handle on the geometry to segmentize}

 @argument[dfMaxLength]{the maximum distance between 2 points after
 segmentization}"
  (hGeom :pointer)			; OGRGeometryH
  (dfMaxLength :double))		; double
(export 'OGR-G-Segmentize)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Intersects" OGR-G-Intersects) :int
  "Do these features intersect?

 Currently this is not implemented in a rigerous fashion, and
 generally just tests whether the envelopes of the two features
 intersect. Eventually this will be made rigerous.

 This function is the same as the CPP method OGRGeometry::Intersects.

 @argument[hGeom]{handle on the first geometry.}

 @argument[hOtherGeom]{handle on the other geometry to test against.}

 @return{TRUE if the geometries intersect, otherwise FALSE.}"
  (hGeom :pointer)			       ; OGRGeometryH
  (hOtherGeom :pointer))		       ; OGRGeometryH
(export 'OGR-G-Intersects)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Equals" OGR-G-Equals) :int
  "Returns TRUE if two geometries are equivalent.

 This function is the same as the CPP method OGRGeometry::Equals()
 method.

 @argument[hGeom]{handle on the first geometry.}

 @argument[hOther]{handle on the other geometry to test against.}

 @return{TRUE if equivalent or FALSE otherwise.}"
  (hGeom :pointer)			; OGRGeometryH
  (hOther :pointer))			; OGRGeometryH
(export 'OGR-G-Equals)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Disjoint" OGR-G-Disjoint) :int
  "Test for disjointness.

 Tests if this geometry and the other geometry are disjoint.

 This function is the same as the C++ method OGRGeometry::Disjoint().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry to compare.}

 @argument[hOther]{the other geometry to compare.}

 @return{TRUE if they are disjoint, otherwise FALSE.}"
  (hThis :pointer)			; OGRGeometryH
  (hOther :pointer))			; OGRGeometryH
(export 'OGR-G-Disjoint)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Touches" OGR-G-Touches) :int
  "Test for touching.

 Tests if this geometry and the other geometry are touching.

 This function is the same as the C++ method OGRGeometry::Touches().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry to compare.}

 @argument[hOther]{the other geometry to compare.}

 @return{TRUE if they are touching, otherwise FALSE.}"
  (hThis :pointer)			; OGRGeometryH
  (hOther :pointer))			; OGRGeometryH
(export 'OGR-G-Touches)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Crosses" OGR-G-Crosses) :int
  "Test for crossing.

 Tests if this geometry and the other geometry are crossing.

 This function is the same as the C++ method OGRGeometry::Crosses().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry to compare.}

 @argument[hOther]{the other geometry to compare.}

 @return{TRUE if they are crossing, otherwise FALSE.}"
  (hThis :pointer)			; OGRGeometryH
  (hOther :pointer))			; OGRGeometryH
(export 'OGR-G-Crosses)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Within" OGR-G-Within) :int
  "Test for containment.

 Tests if this geometry is within the other geometry.

 This function is the same as the C++ method OGRGeometry::Within().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry to compare.}

 @argument[hOther]{the other geometry to compare.}

 @return{TRUE if hThis is within hOther, otherwise FALSE.}"
  (hThis :pointer)			; OGRGeometryH
  (hOther :pointer))			; OGRGeometryH
(export 'OGR-G-Within)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Contains" ogr-g-contains) :int
  "Test for containment.

 Tests if this geometry contains the other geometry.

 This function is the same as the C++ method OGRGeometry::Contains().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry to compare.}

 @argument[hOther]{the other geometry to compare.}

 @return{TRUE if hThis contains hOther geometry, otherwise FALSE.}"
  (hThis ogr-geometry-h)
  (hOther ogr-geometry-h))
(export 'ogr-g-contains)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Overlaps" OGR-G-Overlaps) :int
  "Test for overlap.

 Tests if this geometry and the other geometry overlap, that is their
 intersection has a non-zero area.

 This function is the same as the C++ method OGRGeometry::Overlaps().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry to compare.}

 @argument[hOther]{the other geometry to compare.}

 @return{TRUE if they are overlapping, otherwise FALSE.}"
  (hThis :pointer)			; OGRGeometryH
  (hOther :pointer))			; OGRGeometryH
(export 'OGR-G-Overlaps)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Boundary" OGR-G-Boundary) :pointer ; OGRGeometryH
  "Compute boundary.

 A new geometry object is created and returned containing the boundary
 of the geometry on which the method is invoked.

 This function is the same as the C++ method OGR_G_Boundary().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hTarget]{The Geometry to calculate the boundary of.}

 @return{a handle to a newly allocated geometry now owned by the
 caller, or NULL on failure.}

 Since: OGR 1.8.0"
  (hTarget :pointer))			; OGRGeometryH
(export 'OGR-G-Boundary)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_ConvexHull" OGR-G-Convex-Hull) :pointer ; OGRGeometryH
  "Compute convex hull.

 A new geometry object is created and returned containing the convex
 hull of the geometry on which the method is invoked.

 This function is the same as the C++ method OGRGeometry::ConvexHull().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hTarget]{The Geometry to calculate the convex hull of.}

 @return{a handle to a newly allocated geometry now owned by the
 caller, or NULL on failure.}"
  (hTarget :pointer))			; OGRGeometryH
(export 'OGR-G-Convex-Hull)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Buffer" OGR-G-Buffer) :pointer ; OGRGeometryH
  "Compute buffer of geometry.

 Builds a new geometry containing the buffer region around the
 geometry on which it is invoked. The buffer is a polygon containing
 the region within the buffer distance of the original geometry.

 Some buffer sections are properly described as curves, but are
 converted to approximate polygons. The nQuadSegs parameter can be
 used to control how many segements should be used to define a 90
 degree curve - a quadrant of a circle. A value of 30 is a reasonable
 default. Large values result in large numbers of vertices in the
 resulting buffer geometry while small numbers reduce the accuracy of
 the result.

 This function is the same as the C++ method OGRGeometry::Buffer().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hTarget]{the geometry.}
 @argument[dfDist]{the buffer distance to be applied.}
 @argument[nQuadSegs]{the number of segments used to approximate a 90
 degree (quadrant) of curvature.}

 @return{the newly created geometry, or NULL if an error occurs.}"
  (hTarget :pointer)			; OGRGeometryH
  (dfDist :double)
  (nQuadSegs :int))
(export 'OGR-G-Buffer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Intersection" OGR-G-Intersection) :pointer ; OGRGeometryH
  "Compute intersection.

 Generates a new geometry which is the region of intersection of the
 two geometries operated on. The OGR_G_Intersects() function can be
 used to test if two geometries intersect.

 This function is the same as the C++ method OGRGeometry::Intersection().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry.}
 @argument[hOther]{the other geometry.}

 @return{a new geometry representing the intersection or NULL if there
 is no intersection or an error occurs.}"
  (hThis :pointer)			   ; OGRGeometryH
  (hOther :pointer))			   ; OGRGeometryH
(export 'OGR-G-Intersection)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Union" OGR-G-Union) :pointer ; OGRGeometryH
  "Compute union.

 Generates a new geometry which is the region of union of the two
 geometries operated on.

 This function is the same as the C++ method OGRGeometry::Union().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry.}

 @argument[hOther]{the other geometry.}

 @return{a new geometry representing the union or NULL if an error
 occurs.}"
  (hThis :pointer)			; OGRGeometryH
  (hOther :pointer))			; OGRGeometryH
(export 'OGR-G-Union)

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_G_UnionCascaded" OGR-G-Union-Cascaded) :pointer ; OGRGeometryH
  "Compute union using cascading.

 This function is the same as the C++ method
 OGRGeometry::UnionCascaded().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry.}

 @return{a new geometry representing the union or NULL if an error
 occurs.}"
  (hThis :pointer))			; OGRGeometryH
(export 'OGR-G-Union-Cascaded)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_PointOnSurface" ogr-g-point-on-surface) ogr-geometry-h
  "Returns a point guaranteed to lie on the surface.

 This method relates to the SFCOM ISurface::get_PointOnSurface()
 method however the current implementation based on GEOS can operate
 on other geometry types than the types that are supported by
 SQL/MM-Part 3 : surfaces (polygons) and
 multisurfaces (multipolygons).

 This method is built on the GEOS library, check it for the definition
 of the geometry operation. If OGR is built without the GEOS library,
 this method will always fail, issuing a CPLE_NotSupported error.

 @argument[hGeom]{the geometry to operate on.}

 @return{a point guaranteed to lie on the surface or NULL if an error
 occured.}

 Since: OGR 1.10"
  (hGeom ogr-geometry-h))
(export 'ogr-g-point-on-surface)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Difference" ogr-g-difference) ogr-geometry-h
  "Compute difference.

 Generates a new geometry which is the region of this geometry with
 the region of the other geometry removed.

 This function is the same as the C++ method
 OGRGeometry::Difference().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry.}

 @argument[hOther]{the other geometry.}

 @return{a new geometry representing the difference or NULL if the
 difference is empty or an error occurs.}"
  (hThis ogr-geometry-h)
  (hOther ogr-geometry-h))
(export 'OGR-G-Difference)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_SymDifference" ogr-g-sym-difference) ogr-geometry-h
  "Compute symmetric difference.

 Generates a new geometry which is the symmetric difference of this
 geometry and the other geometry.

 This function is the same as the C++ method
 OGRGeometry::SymmetricDifference().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hThis]{the geometry.}

 @argument[hOther]{the other geometry.}

 @return{a new geometry representing the symmetric difference or NULL
 if the difference is empty or an error occurs.}

 Since: OGR 1.8.0"
  (hThis ogr-geometry-h)
  (hOther ogr-geometry-h))
(export 'ogr-g-sym-difference)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Distance" ogr-g-distance) :double
  "Compute distance between two geometries.

 Returns the shortest distance between the two geometries.

 This function is the same as the C++ method OGRGeometry::Distance().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hFirst]{the first geometry to compare against.}

 @argument[hOther]{the other geometry to compare against.}

 @return{the distance between the geometries or -1 if an error
 occurs.}"
  (hFirst ogr-geometry-h)
  (hOther ogr-geometry-h))
(export 'OGR-G-Distance)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Length" ogr-g-length) :double
  "Compute length of a geometry.

 Computes the area for OGRCurve or MultiCurve objects. Undefined for
 all other geometry types (returns zero).

 This function utilizes the C++ get_Length() method.

 @argument[hGeom]{the geometry to operate on.}

 @return{the lenght or 0.0 for unsupported geometry types.}

 Since:OGR 1.8.0"
  (hGeom ogr-geometry-h))
(export 'OGR-G-Length)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Area" ogr-g-area) :double
  "Compute geometry area.

 Computes the area for an OGRLinearRing, OGRPolygon or
 OGRMultiPolygon. Undefined for all other geometry types (returns
 zero).

 This function utilizes the C++ get_Area() methods such as
 OGRPolygon::get_Area().

 @argument[hGeom]{the geometry to operate on.}

 @return{the area or 0.0 for unsupported geometry types.}

 Since: OGR 1.8.0"
  (hGeom ogr-geometry-h))
(export 'OGR-G-Area)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Centroid" ogr-g-centroid) ogr-err
  "Compute the geometry centroid.

 The centroid location is applied to the passed in OGRPoint
 object. The centroid is not necessarily within the geometry.

 This method relates to the SFCOM ISurface::get_Centroid() method
 however the current implementation based on GEOS can operate on other
 geometry types such as multipoint, linestring, geometrycollection
 such as multipolygons. OGC SF SQL 1.1 defines the operation for
 surfaces (polygons). SQL/MM-Part 3 defines the operation for surfaces
 and multisurfaces (multipolygons).

 This function is the same as the C++ method OGRGeometry::Centroid().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @return{:NONE on success or :FAILURE on error.}"
  (hGeom ogr-geometry-h)
  (hCentroidPoint ogr-geometry-h))
(export 'ogr-g-centroid)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Empty" ogr-g-empty) :void
  "Clear geometry information. This restores the geometry to it's
 initial state after construction, and before assignment of actual
 geometry.

 This function relates to the SFCOM IGeometry::Empty() method.

 This function is the same as the CPP method OGRGeometry::empty().

 @argument[hGeom]{handle on the geometry to empty.}"
  (hGeom ogr-geometry-h))
(export 'OGR-G-Empty)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_IsEmpty" OGR-G-Is-Empty) :int
  "Test if the geometry is empty.

 This method is the same as the CPP method OGRGeometry::IsEmpty().

 @argument[hGeom]{The Geometry to test.}

 @return{TRUE if the geometry has no points, otherwise FALSE.}"
  (hGeom ogr-geometry-h))
(export 'OGR-G-Is-Empty)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_IsValid" OGR-G-Is-Valid) :int
  "Test if the geometry is valid.

 This function is the same as the C++ method OGRGeometry::IsValid().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always return FALSE.

 @argument[hGeom]{The Geometry to test.}

 @return{TRUE if the geometry has no points, otherwise FALSE.}"
  (hGeom ogr-geometry-h))
(export 'OGR-G-Is-Valid)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_IsSimple" OGR-G-Is-Simple) :int
  "Returns TRUE if the geometry is simple.

 Returns TRUE if the geometry has no anomalous geometric points, such
 as self intersection or self tangency. The description of each
 instantiable geometric class will include the specific conditions
 that cause an instance of that class to be classified as not simple.

 This function is the same as the c++ method OGRGeometry::IsSimple()
 method.

 If OGR is built without the GEOS library, this function will always
 return FALSE.

 @argument[hGeom]{The Geometry to test.}

 @return{TRUE if object is simple, otherwise FALSE.}"
  (hGeom ogr-geometry-h))
(export 'OGR-G-Is-Simple)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_IsRing" OGR-G-Is-Ring) :int
  "Test if the geometry is a ring.

 This function is the same as the C++ method OGRGeometry::IsRing().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always return FALSE.

 @argument[hGeom]{The Geometry to test.}

 @return{TRUE if the geometry has no points, otherwise FALSE.}"
  (hGeom ogr-geometry-h))
(export 'OGR-G-Is-Ring)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_Polygonize" OGR-G-Polygonize) :pointer ; OGRGeometryH
  "Polygonizes a set of sparse edges.

 A new geometry object is created and returned containing a collection
 of reassembled Polygons: NULL will be returned if the input
 collection doesn't corresponds to a MultiLinestring, or when
 reassembling Edges into Polygons is impossible due to topogical
 inconsistencies.

 This function is the same as the C++ method OGRGeometry::Polygonize().

 This function is built on the GEOS library, check it for the
 definition of the geometry operation. If OGR is built without the
 GEOS library, this function will always fail, issuing a
 CPLE_NotSupported error.

 @argument[hTarget]{The Geometry to be polygonized.}

 @return{a handle to a newly allocated geometry now owned by the
 caller, or NULL on failure.}

 Since: OGR 1.9.0"
  (hTarget ogr-geometry-h))
(export 'OGR-G-Polygonize)

;; --------------------------------------------------------

#| deprecated
cffi:defcfun {"OGR_G_SymmetricDifference" OGR-G-Symmetric-Difference} OGRGeometryH
cffi:defcfun double OGR_G_GetArea
cffi:defcfun OGRGeometryH OGR_G_GetBoundary
|#

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetPointCount" ogr-g-get-point-count) :int
  "Fetch number of points from a geometry.

 Only wkbPoint[25D] or wkbLineString[25D] may return a valid
 value. Other geometry types will silently return 0.

 @argument[hGeom]{handle to the geometry from which to get the number
 of points.}

 @return{the number of points.}"
  (hGeom ogr-geometry-h))
(export 'ogr-g-get-point-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetPoints" ogr-g-get-points) :int
  "Returns all points of line string.

 This method copies all points into user arrays. The user provides the
 stride between 2 consecutives elements of the array.

 On some CPU architectures, care must be taken so that the arrays are
 properly aligned.

 @argument[hGeom]{handle to the geometry from which to get the
 coordinates.}

 @argument[pabyX]{a buffer of at least (sizeof(double) * nXStride *
 nPointCount) bytes, may be NULL.}

 @argument[nXStride]{the number of bytes between 2 elements of pabyX.}

 @argument[pabyY]{a buffer of at least (sizeof(double) * nYStride *
 nPointCount) bytes, may be NULL.}

 @argument[nYStride]{the number of bytes between 2 elements of pabyY.}

 @argument[pabyZ]{a buffer of at last size (sizeof(double) * nZStride
 * nPointCount) bytes, may be NULL.}

 @argument[nZStride]{the number of bytes between 2 elements of pabyZ.}

 @return{the number of points}

 Since: OGR 1.9.0"
  (hGeom ogr-geometry-h)		; OGRGeometryH
  (pabyX :pointer)			; void *
  (nXStride :int)
  (pabyY :pointer)			; void *
  (nYStride :int)
  (pabyZ :pointer)			; void *
  (nZStride :int))
(export 'ogr-g-get-points)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetPoint" ogr-g-get-point) :void
  "Fetch a point in line string or a point geometry.

 @argument[hGeom]{handle to the geometry from which to get the
 coordinates.}
 @argument[i]{the vertex to fetch, from 0 to getNumPoints()-1, zero
 for a point.}
 @argument[pdfX]{value of x coordinate.}
 @argument[pdfY]{value of y coordinate.}
 @argument[pdfZ]{value of z coordinate.}"
  (hGeom ogr-geometry-h)
  (i :int)
  (pdfX (:pointer :double))			   ; double *
  (pdfY (:pointer :double))			   ; double *
  (pdfZ (:pointer :double)))			   ; double *
(export 'ogr-g-get-point)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_SetPoint" OGR-G-Set-Point) :void
  "Set the location of a vertex in a point or linestring geometry.

 If iPoint is larger than the number of existing points in the
 linestring, the point count will be increased to accomodate the
 request.

 @argument[hGeom]{handle to the geometry to add a vertex to.}
 @argument[i]{the index of the vertex to assign (zero based) or zero
 for a point.}
 @argument[dfX]{input X coordinate to assign.}
 @argument[dfY]{input Y coordinate to assign.}
 @argument[dfZ]{input Z coordinate to assign (defaults to zero).}"
  (hGeom :pointer)			; OGRGeometryH
  (i :int)
  (dfX :double)
  (dfY :double)
  (dfZ :double))
(export 'OGR-G-Set-Point)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_SetPoint_2D" OGR-G-Set-Point-2D) :void
  "Set the location of a vertex in a point or linestring geometry.

 If iPoint is larger than the number of existing points in the
 linestring, the point count will be increased to accomodate the
 request.

 @argument[hGeom]{handle to the geometry to add a vertex to.}
 @argument[i]{the index of the vertex to assign (zero based) or
 zero for a point.}
 @argument[dfX]{input X coordinate to assign.}
 @argument[dfY]{input Y coordinate to assign.}"
  (hGeom :pointer)			; OGRGeometryH
  (i :int)
  (dfX :double)
  (dfY :double))
(export 'OGR-G-Set-Point-2D)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_AddPoint" OGR-G-Add-Point) :void
  "Add a point to a geometry (line string or point).

 The vertex count of the line string is increased by one, and assigned
 from the passed location value.

 @argument[hGeom]{handle to the geometry to add a point to.}
 @argument[dfX]{x coordinate of point to add.}
 @argument[dfY]{y coordinate of point to add.}
 @argument[dfZ]{z coordinate of point to add.}"
  (hGeom :pointer)			; OGRGeometryH
  (dfX :double)
  (dfY :double)
  (dfZ :double))
(export 'OGR-G-Add-Point)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_AddPoint_2D" OGR-G-Add-Point-2D) :void
  "Add a point to a geometry (line string or point).

 The vertex count of the line string is increased by one, and assigned
 from the passed location value.

 @argument[hGeom]{handle to the geometry to add a point to.}

 @argument[dfX]{x coordinate of point to add.}

 @argument[dfY]{y coordinate of point to add.}"
  (hGeom :pointer)			; OGRGeometryH
  (dfX :double)
  (dfY :double))
(export 'OGR-G-Add-Point-2D)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetGeometryCount" ogr-g-get-geometry-count) :int
  "Fetch the number of elements in a geometry or number of geometries in
 container.

 Only geometries of type wkbPolygon[25D], wkbMultiPoint[25D],
 wkbMultiLineString[25D], wkbMultiPolygon[25D] or
 wkbGeometryCollection[25D] may return a valid value. Other geometry
 types will silently return 0.

 For a polygon, the returned number is the number of rings (exterior
 ring + interior rings).

 @argument[hGeom]{single geometry or geometry container from which to
 get the number of elements.}

 @return{the number of elements.}"
  (hGeom ogr-geometry-h))
(export 'ogr-g-get-geometry-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_GetGeometryRef" ogr-g-get-geometry-ref) ogr-geometry-h
  "Fetch geometry from a geometry container.

 This function returns an handle to a geometry within the
 container. The returned geometry remains owned by the container, and
 should not be modified. The handle is only valid untill the next
 change to the geometry container. Use OGR_G_Clone() to make a copy.

 This function relates to the SFCOM
 IGeometryCollection::get_Geometry() method.

 This function is the same as the CPP method
 OGRGeometryCollection::getGeometryRef().

 For a polygon, OGR_G_GetGeometryRef(iSubGeom) returns the exterior
 ring if iSubGeom == 0, and the interior rings for iSubGeom > 0.

 @argument[hGeom]{handle to the geometry container from which to get a
 geometry from.}

 @argument[iSubGeom]{the index of the geometry to fetch, between 0 and
 getNumGeometries() - 1.}

 @return{handle to the requested geometry.}"
  (hGeom ogr-geometry-h)
  (iSubGeom :int))
(export 'ogr-g-get-geometry-ref)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_AddGeometry" ogr-g-add-geometry) ogr-err
  "Add a geometry to a geometry container.

 Some subclasses of OGRGeometryCollection restrict the types of
 geometry that can be added, and may return an error. The passed
 geometry is cloned to make an internal copy.

 There is no SFCOM analog to this method.

 This function is the same as the CPP method
 OGRGeometryCollection::addGeometry.

 For a polygon, hNewSubGeom must be a linearring. If the polygon is
 empty, the first added subgeometry will be the exterior ring. The
 next ones will be the interior rings.

 @argument[hGeom]{existing geometry container.}

 @argument[hNewSubGeom]{geometry to add to the container.}

 @return{OGRERR_NONE if successful, or
 OGRERR_UNSUPPORTED_GEOMETRY_TYPE if the geometry type is illegal for
 the type of existing geometry.}"
  (hGeom ogr-geometry-h)
  (hNewSubGeom ogr-geometry-h))
(export 'OGR-G-Add-Geometry)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_AddGeometryDirectly" OGR-G-Add-Geometry-Directly) ogr-err ; OGRErr
  "Add a geometry directly to an existing geometry container.

 Some subclasses of OGRGeometryCollection restrict the types of
 geometry that can be added, and may return an error. Ownership of the
 passed geometry is taken by the container rather than cloning as
 addGeometry() does.

 This function is the same as the CPP method
 OGRGeometryCollection::addGeometryDirectly.

 There is no SFCOM analog to this method.

 For a polygon, hNewSubGeom must be a linearring. If the polygon is
 empty, the first added subgeometry will be the exterior ring. The
 next ones will be the interior rings.

 @argument[hGeom]{existing geometry.}
 @argument[hNewSubGeom]{geometry to add to the existing geometry.}

 @return{:NONE if successful, or
 :UNSUPPORTED_GEOMETRY_TYPE if the geometry type is illegal for the
 type of geometry container.}"
  (hGeom ogr-geometry-h)
  (hNewSubGeom ogr-geometry-h))
(export 'OGR-G-Add-Geometry-Directly)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_G_RemoveGeometry" OGR-G-Remove-Geometry) ogr-err
  "Remove a geometry from an exiting geometry container.

 Removing a geometry will cause the geometry count to drop by one, and
 all \"higher\" geometries will shuffle down one in index.

 There is no SFCOM analog to this method.

 This function is the same as the CPP method
 OGRGeometryCollection::removeGeometry().

 @argument[hGeom]{the existing geometry to delete from.}
 @argument[iGeom]{the index of the geometry to delete. A value of -1
 is a special flag meaning that all geometries should be removed.}
 @argument[bDelete]{if TRUE the geometry will be destroyed, otherwise
 it will not. The default is TRUE as the existing geometry is
 considered to own the geometries in it.}

 @return{:NONE if successful, or :FAILURE if the index is out of
 range.}"
  (hGeom ogr-geometry-h)
  (iGeom :int)
  (bDelete :int))
(export 'ogr-g-remove-geometry)

;; --------------------------------------------------------

(cffi:defcfun ("OGRBuildPolygonFromEdges" ogr-build-polygon-from-edges) ogr-geometry-h
  "Build a ring from a bunch of arcs.

 @argument[hLines]{handle to an OGRGeometryCollection (or
 OGRMultiLineString) containing the line string geometries to be built
 into rings.}

 @argument[bBestEffort]{not yet implemented???.}
 @argument[bAutoClose]{indicates if the ring should be close when first and last points of the ring are the same.}
 @argument[dfTolerance]{tolerance into which two arcs are considered close enough to be joined.}
 @argument[peErr]{OGRERR_NONE on success, or OGRERR_FAILURE on failure.}

 @return{an handle to the new geometry, a polygon.}"
  (hLines ogr-geometry-h)
  (bBestEffort :int)
  (bAutoClose :int)
  (dfTolerance :double)
  (peErr (:pointer ogr-err)))
(export 'ogr-build-polygon-from-edges)

;; --------------------------------------------------------
;; CLOS
;; --------------------------------------------------------

(defgeneric get-point-count (g)
  (:documentation "")
  (:method ((geom <geometry>))
    (ogr-g-get-point-count (pointer geom))))
(export 'get-point-count)

;; --------------------------------------------------------

(defgeneric get-points (g)
  (:documentation "")
  (:method ((geom <geometry>))
    (let* ((count (get-point-count geom))
	   (dim (ogr-g-get-coordinate-dimension (pointer geom)))
	   (padxy (cffi:foreign-alloc :double :count (* 2 count)))
	   (in-3d (= dim 3))
	   (padz (if in-3d
		     (cffi:foreign-alloc :double :count count)
		     (cffi:null-pointer)))
	   (sizeof-double (cffi:foreign-type-size :double)))
      ;; instead of allocating three arrays, we allocate only two. The
      ;; PADXY array will contain coordinates for both X and Y
      ;; direction one after another, and the PADZ array is used only
      ;; when we are dealing in a 3-dimensional space
      (ogr-g-get-points
       (pointer geom)
       padxy (* 2 sizeof-double)
       (cffi:inc-pointer padxy sizeof-double) (* 2 sizeof-double)
       padz sizeof-double)
      (let ((ret (if in-3d
		     (make-array (list count count count)
				 :element-type 'double-float)
		     (make-array (list count count)
				 :element-type 'double-float))))
	(if in-3d
	    (loop for i from 0 below count
	       do (setf (aref ret i 0) (cffi:mem-aref padxy :double (* i 2))
			(aref ret i 1) (cffi:mem-aref padxy :double (+ (* i 2) 1))
			(aref ret i 2) (cffi:mem-aref padz :double i)))
	    (loop for i from 0 below count
	       do (setf (aref ret i 0) (cffi:mem-aref padxy :double (* i 2))
			(aref ret i 1) (cffi:mem-aref padxy :double (+ (* i 2) 1)))))
	ret))))
(export 'get-points)

;; --------------------------------------------------------

(defgeneric get-x (g idx)
  (:documentation "")
  (:method ((geom <geometry>) (idx integer))
    (ogr-g-getx (pointer geom) idx)))
(export 'get-x)

(defgeneric get-y (g idx)
  (:documentation "")
  (:method ((geom <geometry>) (idx integer))
    (ogr-g-gety (pointer geom) idx)))
(export 'get-y)

(defgeneric get-z (g idx)
  (:documentation "")
  (:method ((geom <geometry>) (idx integer))
    (ogr-g-getz (pointer geom) idx)))
(export 'get-z)

;; --------------------------------------------------------

(defgeneric get-point (g idx)
  (:documentation "")
  (:method ((geom <geometry>) (idx integer))
    (values (ogr-g-getx (pointer geom) idx)
	    (ogr-g-gety (pointer geom) idx)
	    (ogr-g-getz (pointer geom) idx)
	    )
    #+ignore
    (cffi:with-foreign-objects ((fnx :double)
				(fny :double)
				(fnz :double))
      (ogr-g-get-point (pointer geom) idx fnx fny fnz)
      (values (cffi:mem-ref fnx :double)
	      (cffi:mem-ref fny :double)
	      (cffi:mem-ref fnz :double)))))
(export 'get-point)

;; --------------------------------------------------------

(defmacro with-points ((x y z count) geom &body body)
  "Handles creation of native variables for coordinate arrays,
extracts point information from the given geometry object and then
frees allocated memory resources with cpl-free.

X, Y, Z will be holding native CFFI arrays for the point coordinates
and COUNT will be assigned the number returned by ogr-g-get-points."
  (let ((point-count (gensym))
        (sizeof-double (gensym)))
    `(let ((,point-count (get-point-count ,geom))
           (,sizeof-double (cffi:foreign-type-size :double)))
       (cffi:with-foreign-objects ((,x :double ,point-count)
                                   (,y :double ,point-count)
                                   (,z :double ,point-count))
         (let ((,count (ogr-g-get-points (pointer ,geom)
                                         ,x ,sizeof-double
                                         ,y ,sizeof-double
                                         ,z ,sizeof-double)))
           ,@body)))))
(export 'with-points)

;; --------------------------------------------------------

(defmethod get-type ((geom <geometry>))
    (ogr-g-get-geometry-type (pointer geom)))

;; --------------------------------------------------------

(defgeneric get-geometry-count (g)
  (:documentation "")
  (:method ((geom <geometry>))
    (ogr-g-get-geometry-count (pointer geom))))
(export 'get-geometry-count)

;; --------------------------------------------------------

(defmethod get-geometry ((geom <geometry>) &optional idx)
  (check-type idx (or null integer))
  ;; todo!
  (let* ((ref (ogr-g-get-geometry-ref (pointer geom)
				      (if idx idx 0)))
	 (geom-type (ogr-g-get-geometry-type ref)))
    (dispatch-geometry-construction geom-type ref)))

;; --------------------------------------------------------

(defgeneric get-geometry-ref (g idx)
  (:documentation "")
  (:method ((geom <geometry>) (idx integer))
    (make-instance '<geometry-ref>
		   :pointer (ogr-g-get-geometry-ref
			     (pointer geom)
			     idx))))
(export 'get-geometry-ref)

;; --------------------------------------------------------

(defmethod get-spatial-ref ((geom <geometry>))
  (make-instance '<spatial-ref>
		 :pointer (ogr-g-get-spatial-reference (pointer geom))))


;; EOF
