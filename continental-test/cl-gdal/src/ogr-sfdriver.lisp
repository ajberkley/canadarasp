;;; -*- package: CL-OGR; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-ogr)

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Dr_CopyDataSource" ogr-dr-copy-data-source) ogr-data-source-h
  "This function creates a new datasource by copying all the layers from
 the source datasource.

 It is important to call OGR_DS_Destroy() when the datasource is no
 longer used to ensure that all data has been properly flushed to
 disk.

 This function is the same as the C++ method
 OGRSFDriver::CopyDataSource().

 @argument[hDriver]{handle to the driver on which data source creation is based.}
 @argument[hSrcDS]{source datasource}
 @argument[pszNewName]{the name for the new data source.}
 @argument[papszOptions]{a StringList of name=value options. Options
 are driver specific, and driver information can be found at the
 following url: http://www.gdal.org/ogr/ogr_formats.html}

 @return{NULL is returned on failure, or a new OGRDataSource handle on
 success.}"
  (hDriver ogr-sf-driver-h)
  (hSrcDS ogr-data-source-h)
  (pszNewName :string)
  (papszOptions (:pointer :string)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Dr_CreateDataSource" OGR_Dr_CreateDataSource) ogr-data-source-h
  "This function attempts to create a new data source based on the
 passed driver.

 The papszOptions argument can be used to control driver specific
 creation options. These options are normally documented in the format
 specific documentation.

 It is important to call OGR_DS_Destroy() when the datasource is no
 longer used to ensure that all data has been properly flushed to
 disk.

 This function is the same as the C++ method
 OGRSFDriver::CreateDataSource().

 @argument[hDriver]{handle to the driver on which data source creation is based.}
 @argument[pszName]{the name for the new data source. UTF-8 encoded.}
 @argument[papszOptions]{a StringList of name=value options. Options
 are driver specific, and driver information can be found at the
 following url: http://www.gdal.org/ogr/ogr_formats.html}

 @return{NULL is returned on failure, or a new OGRDataSource handle on
 success.}"
  (hDriver ogr-sf-driver-h)
  (pszName :string)
  (papszOptions (:pointer :string)))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Dr_DeleteDataSource" OGR_Dr_DeleteDataSource) ogr-err
  "Delete a datasource.

 Delete (from the disk, in the database, ...) the named
 datasource. Normally it would be safest if the datasource was not
 open at the time.

 Whether this is a supported operation on this driver case be tested
 using TestCapability() on ODrCDeleteDataSource.

 This method is the same as the C++ method
 OGRSFDriver::DeleteDataSource().

 @argument[hDriver]{handle to the driver on which data source deletion is based.}
 @argument[pszDataSource]{the name of the datasource to delete.}

 @return{OGRERR_NONE on success, and OGRERR_UNSUPPORTED_OPERATION if
 this is not supported by this driver.}"
  (hDriver ogr-sf-driver-h)
  (pszDataSource :string))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Dr_GetName" OGR_Dr_GetName) :string
  "Fetch name of driver (file format). This name should be relatively
 short (10-40 characters), and should reflect the underlying file
 format. For instance \"ESRI Shapefile\".

 This function is the same as the C++ method OGRSFDriver::GetName().

 @argument[hDriver]{handle to the the driver to get the name from.}

 @return{driver name. This is an internal string and should not be
 modified or freed.}"
  (hDriver ogr-sf-driver-h))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Dr_Open" OGR_Dr_Open) ogr-data-source-h
  "Attempt to open file with this driver.

 This function is the same as the C++ method OGRSFDriver::Open().

 @argument[hDriver]{handle to the driver that is used to open file.}
 @argument[pszName]{the name of the file, or data source to try and open.}
 @argument[bUpdate]{TRUE if update access is required, otherwise FALSE (the default).}

 @return{NULL on error or if the pass name is not supported by this
 driver, otherwise an handle to an OGRDataSource. This OGRDataSource
 should be closed by deleting the object when it is no longer needed.}"
  (hDriver ogr-sf-driver-h)
  (pszName :string)
  (bUpdate :int))

;; --------------------------------------------------------

(cffi:defcfun ("OGR_Dr_TestCapability" OGR_Dr_TestCapability) :int
  "Test if capability is available.

 One of the following data source capability names can be passed into
 this function, and a TRUE or FALSE value will be returned indicating
 whether or not the capability is available for this object.


ODrCCreateDataSource: True if this driver can support creating data sources.

ODrCDeleteDataSource: True if this driver supports deleting data sources.


 The #define macro forms of the capability names should be used in
 preference to the strings themselves to avoid mispelling.

 This function is the same as the C++ method
 OGRSFDriver::TestCapability().

 @argument[hDriver]{handle to the driver to test the capability against.}
 @argument[pszCap]{the capability to test.}

 @return{TRUE if capability available otherwise FALSE.}"
  (hDriver ogr-sf-driver-h)
  (pszCap :string))

;; EOF
