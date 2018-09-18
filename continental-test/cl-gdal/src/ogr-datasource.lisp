;;; -*- package: OGR; Syntax: Common-lisp; Base: 10 -*-

;; OGRDataSource <ogr_api.h>

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGROpen" ogr-open) ogr-data-source-h
  (psz-name :string)
  (p-update :int)
  (path-driver-list :pointer)) ;; OGRSFDriverH
(export 'ogr-open)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_GetLayerCount" ogr-ds-get-layer-count) :int
  "Get the number of layers in this data source.

 This function is the same as the C++ method
 OGRDataSource::GetLayerCount().

 @argument[hDS]{handle to the data source from which to get the number
 of layers.}

 @return{layer count.}"
  (ds :pointer))			; OGRDataSourceH
(export 'ogr-ds-get-layer-count)

;; --------------------------------------------------------

(cffi:defcfun  ("OGR_DS_GetLayerByName" ogr-ds-get-layer-by-name) :pointer ; OGRLayerH
  "Fetch a layer by name.

 The returned layer remains owned by the OGRDataSource and should not
 be deleted by the application.

 This function is the same as the C++ method OGRDataSource::GetLayerByName().

 @argument[hDS]{handle to the data source from which to get the layer.}

 @argument[pszLayerName]{Layer the layer name of the layer to fetch.}

 @return{an handle to the layer, or NULL if the layer is not found or
 an error occurs.}"
  (hDS :pointer)
  (pszLayerName :string))
(export 'ogr-ds-get-layer-by-name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_GetLayer" ogr-ds-get-layer) :pointer ; OGRLayerH
  "Fetch a layer by index.

 The returned layer remains owned by the OGRDataSource and should not
 be deleted by the application.

 This function is the same as the C++ method
 OGRDataSource::GetLayer().

 @argument[hDS]{handle to the data source from which to get the layer.}
 @argument[iLayer]{a layer number between 0 and OGR_DS_GetLayerCount()-1.}

 @return{an handle to the layer, or NULL if iLayer is out of range or
 an error occurs.}"
  (hDS :pointer)			; OGRDataSourceH
  (i :int))
(export 'ogr-ds-get-layer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_Destroy" ogr-ds-destroy) :void
  "Closes opened datasource and releases allocated resources.

 This method is the same as the C++ method OGRDataSource::DestroyDataSource().

 @argument[hDataSource]{handle to allocated datasource object.}"
  (hDataSource :pointer))		; OGRDataSourceH
(export 'OGR-DS-Destroy)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_GetName" ogr-ds-get-name) :string ; const char *
  "Returns the name of the data source.

 This string should be sufficient to open the data source if passed to
 the same OGRSFDriver that this data source was opened with, but it
 need not be exactly the same string that was used to open the data
 source. Normally this is a filename.

 This function is the same as the C++ method OGRDataSource::GetName().

 @argument[hDS]{handle to the data source to get the name from.}

 @return{pointer to an internal name string which should not be
 modified or freed by the caller.}"
  (hDS :pointer))			; OGRDataSourceH
(export 'OGR-DS-Get-Name)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_DeleteLayer" %OGR-DS-Delete-Layer) :int ; OGRErr
  (hDS :pointer)			; OGRDataSourceH
  (iLayer :int))

(defun ogr-ds-delete-layer (hDS iLayer)
  "Delete the indicated layer from the datasource.

 If this method is supported the ODsCDeleteLayer capability will test
 TRUE on the OGRDataSource.

 This method is the same as the C++ method
 OGRDataSource::DeleteLayer().

 @argument[hDS]{handle to the datasource}
 @argument[iLayer]{the index of the layer to delete.}

 @return{:NONE on success, or :UNSUPPORTED_OPERATION if deleting
 layers is not supported for this datasource.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-DS-Delete-Layer hDS iLayer)))
(export 'ogr-ds-delete-layer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_GetDriver" ogr-ds-get-driver) :pointer ; OGRSFDriverH
  "Returns the driver that the dataset was opened with.

 This method is the same as the C++ method OGRDataSource::GetDriver()

 @argument[hDS]{handle to the datasource}

 @return{NULL if driver info is not available, or pointer to a driver
 owned by the OGRSFDriverManager.}"
  (hDS :pointer))			; OGRDataSourceH
(export 'ogr-ds-get-driver)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_CreateLayer" ogr-ds-create-layer) ogr-layer-h
  "This function attempts to create a new layer on the data source with
 the indicated name, coordinate system, geometry type.

 The papszOptions argument can be used to control driver specific
 creation options. These options are normally documented in the format
 specific documentation.

 This function is the same as the C++ method
 OGRDataSource::CreateLayer().

Parameters:	hDS 	The dataset handle.
	pszName 	the name for the new layer. This should ideally not match any existing layer on the datasource.
	hSpatialRef 	handle to the coordinate system to use for the new layer, or NULL if no coordinate system is available.
	eType 	the geometry type for the layer. Use wkbUnknown if there are no constraints on the types geometry to be written.
	papszOptions 	a StringList of name=value options. Options are driver specific, and driver information can be found at the following url: http://www.gdal.org/ogr/ogr_formats.html

 @return{NULL is returned on failure, or a new OGRLayer handle on success.}

Example:
...
@begin{pre}
        OGRLayerH *hLayer;
        char     **papszOptions;

        if( OGR_DS_TestCapability( hDS, ODsCCreateLayer ) )
        (
            ...
        )

        papszOptions = CSLSetNameValue( papszOptions, \"DIM\", \"2\" );
        hLayer = OGR_DS_CreateLayer( hDS, \"NewLayer\", NULL, wkbUnknown,
                                     papszOptions );
        CSLDestroy( papszOptions );

        if( hLayer == NULL )
        (
            ...
        )
@end{pre}"
  (hDS :pointer)			; OGRDataSourceH
  (pszName :string)			; const char *
  (hSpatialRef :pointer)		; OGRSpatialReferenceH
  (eType ogr-wkb-geometry-type)				; OGR-wkb-Geometry-Type
  (papszOptions (:pointer :string)))	; char **
(export 'OGR-DS-Create-Layer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_CopyLayer" ogr-ds-copy-layer) ogr-layer-h
  "Duplicate an existing layer.

 This function creates a new layer, duplicate the field definitions of
 the source layer and then duplicate each features of the source
 layer. The papszOptions argument can be used to control driver
 specific creation options. These options are normally documented in
 the format specific documentation. The source layer may come from
 another dataset.

 This function is the same as the C++ method OGRDataSource::CopyLayer

 @argument[hDS]{handle to the data source where to create the new layer}
 @argument[hSrcLayer]{handle to the source layer.}
 @argument[pszNewName]{the name of the layer to create.}
 @argument[papszOptions]{a StringList of name=value options. Options
 are driver specific.}

 @return{an handle to the layer, or NULL if an error occurs.}"
  (hDS :pointer)			; OGRDataSourceH
  (hSrcLayer :pointer)			; OGRLayerH
  (pszNewName :string)			; const char *
  (papszOptions (:pointer :string)))	; char **
(export 'OGR-DS-Copy-Layer)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_TestCapability" ogr-ds-test-capability) :int
  "Test if capability is available.

 One of the following data source capability names can be passed into
 this function, and a TRUE or FALSE value will be returned indicating
 whether or not the capability is available for this object.

 ODsCCreateLayer: True if this datasource can create new layers.

 ODsCDeleteLayer: True if this datasource can delete existing layers.

 ODsCCreateGeomFieldAfterCreateLayer: True if the layers of this
 datasource support CreateGeomField() just after layer creation.

 The #define macro forms of the capability names should be used in
 preference to the strings themselves to avoid mispelling.

 This function is the same as the C++ method
 OGRDataSource::TestCapability().

 @argument[hDS]{handle to the data source against which to test the
 capability.}
 @argument[pszCapability]{the capability to test.}

 @return{TRUE if capability available otherwise FALSE.}"
  (hDS ogr-data-source-h)
  (pszCapability :string))
(export 'ogr-ds-test-capability)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_ExecuteSQL" ogr-ds-execute-sql) ogr-layer-h
  "Execute an SQL statement against the data store.

 The result of an SQL query is either NULL for statements that are in
 error, or that have no results set, or an OGRLayer handle
 representing a results set from the query. Note that this OGRLayer is
 in addition to the layers in the data store and must be destroyed
 with OGR_DS_ReleaseResultSet() before the data source is
 closed (destroyed).

 For more information on the SQL dialect supported internally by OGR
 review the OGR SQL document. Some drivers (ie. Oracle and PostGIS)
 pass the SQL directly through to the underlying RDBMS.

 Starting with OGR 1.10, the SQLITE dialect can also be used.

 This function is the same as the C++ method
 OGRDataSource::ExecuteSQL();

 @argument[hDS]{handle to the data source on which the SQL query is
 executed.}
 @argument[pszSQLCommand]{the SQL statement to execute.}
 @argument[hSpatialFilter]{handle to a geometry which represents a
 spatial filter. Can be NULL.}
 @argument[pszDialect]{allows control of the statement dialect. If set
 to NULL, the OGR SQL engine will be used, except for RDBMS drivers
 that will use their dedicated SQL engine, unless OGRSQL is
 explicitely passed as the dialect. Starting with OGR 1.10, the SQLITE
 dialect can also be used.}

 @return{an handle to a OGRLayer containing the results of the
 query. Deallocate with OGR_DS_ReleaseResultSet().}"
  (hDS ogr-data-source-h)
  (pszSQLCommand :string)
  (hSpatialFilter ogr-geometry-h)
  (pszDialect :string))
(export 'OGR-DS-Execute-SQL)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_ReleaseResultSet" ogr-ds-release-result-set) :void
  "Release results of OGR_DS_ExecuteSQL().

 This function should only be used to deallocate OGRLayers resulting
 from an OGR_DS_ExecuteSQL() call on the same OGRDataSource. Failure
 to deallocate a results set before destroying the OGRDataSource may
 cause errors.

 This function is the same as the C++ method
 OGRDataSource::ReleaseResultSet().

 @argument[hDS]{an handle to the data source on which was executed an
 SQL query.}
 @argument[hLayer]{handle to the result of a previous
 OGR_DS_ExecuteSQL() call.}"
  (hDS :pointer)			; OGRDataSourceH
  (hLayer :pointer))			; OGRLayerH
(export 'OGR-DS-Release-Result-Set)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_DS_SyncToDisk" %OGR-DS-Sync-To-Disk) :int ; OGRErr
  (hDS :pointer))			; OGRDataSourceH

(defun ogr-ds-sync-to-disk (hDS)
  "Flush pending changes to disk.

 This call is intended to force the datasource to flush any pending
 writes to disk, and leave the disk file in a consistent state. It
 would not normally have any effect on read-only datasources.

 Some data sources do not implement this method, and will still return
 OGRERR_NONE. An error is only returned if an error occurs while
 attempting to flush to disk.

 The default implementation of this method just calls the SyncToDisk()
 method on each of the layers. Conceptionally, calling SyncToDisk() on
 a datasource should include any work that might be accomplished by
 calling SyncToDisk() on layers in that data source.

 In any event, you should always close any opened datasource with
 OGR_DS_Destroy() that will ensure all data is correctly flushed.

 This method is the same as the C++ method OGRDataSource::SyncToDisk()

 @argument[hDS]{handle to the data source}

 @return{:NONE if no error occurs (even if nothing is done) or an
 error code.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-DS-Sync-To-Disk hDS)))
(export 'ogr-ds-sync-to-disk)

;; --------------------------------------------------------

(cffi:defcfun ("OGRReleaseDataSource" %OGR-Release-Data-Source) :int ; OGRErr
  (hDS :pointer))			; OGRDataSourceH

(defun ogr-release-data-source (hDS)
  "Drop a reference to this datasource, and if the reference count
 drops to zero close (destroy) the datasource.

 Internally this actually calls the
 OGRSFDriverRegistrar::ReleaseDataSource() method. This method is
 essentially a convenient alias.

 This method is the same as the C++ method OGRDataSource::Release()

 @argument[hDS]{handle to the data source to release}

 @return{:NONE on success or an error code.}"
  (cffi:foreign-enum-keyword
   'ogr-err
   (%OGR-Release-Data-Source hDS)))
(export 'ogr-release-data-source)

;; --------------------------------------------------------

(cffi:defcfun ("OGRRegisterDriver" ogr-register-driver) :void
  "Add a driver to the list of registered drivers.

 If the passed driver is already registered (based on handle
 comparison) then the driver isn't registered. New drivers are added
 at the end of the list of registered drivers.

 This function is the same as the C++ method
 OGRSFDriverRegistrar::RegisterDriver().

 @argument[hDriver]{handle to the driver to add.}"
  (hDriver :pointer))			; OGRSFDriverH
(export 'OGR-Register-Driver)

;; --------------------------------------------------------

(cffi:defcfun ("OGRDeregisterDriver" OGR-Deregister-Driver) :void
  "Remove the passed driver from the list of registered drivers.

 This function is the same as the C++ method
 OGRSFDriverRegistrar::DeregisterDriver().

 @argument[hDriver]{handle to the driver to deregister.}

 Since: GDAL 1.8.0"
  (hDriver :pointer))			; OGRSFDriverH
(export 'OGR-Deregister-Driver)

;; --------------------------------------------------------

(cffi:defcfun ("OGRGetDriverCount" ogr-get-driver-count) :int
  "Fetch the number of registered drivers.

 This function is the same as the C++ method
 OGRSFDriverRegistrar::GetDriverCount().

 @return{the drivers count.}")
(export 'ogr-get-driver-count)

;; --------------------------------------------------------

(cffi:defcfun ("OGRGetDriver" ogr-get-driver) ogr-sf-driver-h
  "Fetch the indicated driver.

 This function is the same as the C++ method
 OGRSFDriverRegistrar::GetDriver().

 @argument[iDriver]{the driver index, from 0 to GetDriverCount()-1.}

 @return{handle to the driver, or NULL if iDriver is out of range.}"
  (iDriver :int))
(export 'OGR-Get-Driver)

;; --------------------------------------------------------

(cffi:defcfun ("OGRGetDriverByName" ogr-get-driver-by-name) ogr-sf-driver-h
  "Fetch the indicated driver.

 This function is the same as the C++ method
 OGRSFDriverRegistrar::GetDriverByName()

 @argument[pszName]{the driver name}

 @return{the driver, or NULL if no driver with that name is found}"
  (pszName :string))			; const char *
(export 'ogr-get-driver-by-name)

;; --------------------------------------------------------

(cffi:defcfun ("OGRGetOpenDSCount" OGR-Get-Open-DS-Count) :int
  "Return the number of opened datasources.

 This function is the same as the C++ method
 OGRSFDriverRegistrar::GetOpenDSCount()

 @return{the number of opened datasources.}")
(export 'OGR-Get-Open-DS-Count)

;; --------------------------------------------------------

(cffi:defcfun ("OGRGetOpenDS" ogr-get-open-ds) ogr-data-source-h
  "Return the iDS th datasource opened.

 This function is the same as the C++ method
 OGRSFDriverRegistrar::GetOpenDS.

 @argument[iDS]{the index of the dataset to return (between 0 and
 GetOpenDSCount() - 1)}"
  (iDS :int))
(export 'ogr-get-open-ds)

;; --------------------------------------------------------

(cffi:defcfun ("OGRRegisterAll" ogr-register-all) :void
  "Register all drivers.")
(export 'ogr-register-all)

;; --------------------------------------------------------
;; CLOS
;; --------------------------------------------------------

(defun open-data-source (name &optional (update 0) (drivers (cffi:null-pointer)))
  "Returns DATA-SOURCE instance in case of success, NIL otherwise."
  (let ((ds-pointer (ogr-open name update drivers)))
    (unless (cffi:null-pointer-p ds-pointer)
      (make-instance '<data-source>
		     :pointer ds-pointer))))
(export 'open-data-source)

;; --------------------------------------------------------

(defgeneric destroy (ds)
  (:documentation "")
  (:method ((ds <data-source>))
    (ogr-ds-destroy (pointer ds))))
(export 'destroy)

;; --------------------------------------------------------

(defgeneric release (ds)
  (:documentation "")
  (:method ((ds <data-source>))
    (ogr-release-data-source (pointer ds))))
(export 'release)

;; --------------------------------------------------------

(defgeneric get-layer-count (ds)
  (:documentation "")
  (:method ((ds <data-source>))
    (ogr-ds-get-layer-count (pointer ds))))
(export 'get-layer-count)

;; --------------------------------------------------------

(defgeneric get-layer-by-name (ds name)
  (:method ((ds <data-source>) (name string))
    (let ((layer-pointer (ogr-ds-get-layer-by-name (pointer ds)
						   name)))
      (unless (cffi:null-pointer-p layer-pointer)
	(make-instance '<layer>
		       :pointer layer-pointer
		       :data-source ds)))))
(export 'get-layer-by-name)

;; --------------------------------------------------------

(defgeneric get-layer (ds idx)
  (:method ((ds <data-source>) (idx fixnum))
    (let ((layer-pointer (ogr-ds-get-layer (pointer ds)
					   idx)))
      (unless (cffi:null-pointer-p layer-pointer)
	(make-instance '<layer>
		       :pointer layer-pointer
		       :data-source ds)))))
(export 'get-layer)

;; --------------------------------------------------------

(defgeneric get-name (ds)
  (:method ((ds <data-source>))
    (ogr-ds-get-name (pointer ds))))
(export 'get-name)

;; --------------------------------------------------------

(defgeneric delete-layer (ds iLayer)
  (:method ((ds <data-source>) (iLayer fixnum))
    (ogr-ds-delete-layer ds iLayer)))
(export 'delete-layer)

;; --------------------------------------------------------

;; EOF
