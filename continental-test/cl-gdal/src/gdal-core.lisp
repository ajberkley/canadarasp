;;; -*- package: CL-GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------

(cffi:defctype gdal-raster-band-h :pointer "GDALRasterBandH")
(cffi:defctype gdal-color-table-h :pointer "GDALColorTableH")
(cffi:defctype gdal-major-object-h :pointer "GDALMajorObjectH")
(cffi:defctype gdal-color-table-h :pointer "GDALColorTableH")
(cffi:defctype gdal-driver-h :pointer "GDALDriverH")
(cffi:defctype gdal-dataset-h :pointer "GDALDatasetH")
(cffi:defctype gbyte :uint8 "GByte")
(cffi:defctype gdal-raster-attribute-table-h :pointer "GDALRasterAttributeTableH")
(cffi:defctype g-int-big :long-long "GIntBig")

;; --------------------------------------------------------

;; CPLErr
(cffi:defcenum cpl-err
    "More functions are defined in the cpl_error.h file."
  (:CE_None 0)
  (:CE_Debug 1)
  (:CE_Warning 2)
  (:CE_Failure 3)
  (:CE_Fatal 4))

;; --------------------------------------------------------

(cffi:defcenum gdal-access
  "Flag indicating read/write, or read-only access to data."
  (:GA_ReadOnly 0)                      ; Read only (no update) access
  (:GA_Update 1))                       ; Read/write access.
(export 'gdal-access)

;; --------------------------------------------------------

(cffi:defcenum gdal-async-status-type
"status of the asynchronous stream"
  (:GARIO_PENDING 0)
  (:GARIO_UPDATE 1)
  (:GARIO_ERROR 2)
  (:GARIO_COMPLETE 3) 
  (:GARIO_TypeCount 4))
(export 'gdal-async-status-type)

;; --------------------------------------------------------

(cffi:defcenum gdal-color-interp
  "Types of color interpretation for raster bands."
  :GCI_GrayIndex	       ; Greyscale
  :GCI_PaletteIndex	       ; Paletted (see associated color table)
  :GCI_RedBand		       ; Red band of RGBA image
  :GCI_GreenBand	       ; Green band of RGBA image
  :GCI_BlueBand		       ; Blue band of RGBA image
  :GCI_AlphaBand	       ; Alpha (0=transparent, 255=opaque)
  :GCI_HueBand		       ; Hue band of HLS image
  :GCI_SaturationBand	       ; Saturation band of HLS image
  :GCI_LightnessBand	       ; Lightness band of HLS image
  :GCI_CyanBand		       ; Cyan band of CMYK image
  :GCI_MagentaBand	       ; Magenta band of CMYK image
  :GCI_YellowBand	       ; Yellow band of CMYK image
  :GCI_BlackBand	       ; Black band of CMLY image
  :GCI_YCbCr_YBand	       ; Y Luminance
  :GCI_YCbCr_CbBand	       ; Cb Chroma
  :GCI_YCbCr_CrBand	       ; Cr Chroma
  :GCI_Max)		       ; Max current value
(export 'gdal-color-interp)

;; --------------------------------------------------------

(cffi:defcenum gdal-data-type
  "Pixel data types"
  :GDT_Unknown			     ; Unknown or unspecified type
  :GDT_Byte			     ; Eight bit unsigned integer
  :GDT_UInt16			     ; Sixteen bit unsigned integer
  :GDT_Int16			     ; Sixteen bit signed integer
  :GDT_UInt32			     ; Thirty two bit unsigned integer
  :GDT_Int32			     ; Thirty two bit signed integer
  :GDT_Float32			     ; Thirty two bit floating point
  :GDT_Float64			     ; Sixty four bit floating point
  :GDT_CInt16			     ; Complex Int16
  :GDT_CInt32			     ; Complex Int32
  :GDT_CFloat32			     ; Complex Float32
  :GDT_CFloat64)		     ; Complex Float64
(export 'gdal-data-type)

;; --------------------------------------------------------

(cffi:defcenum gdal-palette-interp
  "Types of color interpretations for a GDALColorTable."
  :GPI_Gray   ; Grayscale (in GDALColorEntry.c1)
  :GPI_RGB    ; Red, Green, Blue and Alpha in (in c1, c2, c3 and c4)
  :GPI_CMYK   ; Cyan, Magenta, Yellow and Black (in c1, c2, c3 and c4)
  :GPI_HLS)   ; Hue, Lightness and Saturation (in c1, c2, and c3)
(export 'gdal-palette-interp)

;; --------------------------------------------------------

(cffi:defcenum gdal-rat-field-type
  "Field type of raster attribute table."
  :GFT_Integer			       ; Integer field
  :GFT_Real			       ; Floating point (double) field
  :GFT_String)			       ; String field
(export 'gdal-rat-field-type)

;; --------------------------------------------------------

(cffi:defcenum gdal-rat-field-usage
  "Field usage of raster attribute table."
  :GFU_Generic			    ; General purpose field.
  :GFU_PixelCount		    ; Histogram pixel count
  :GFU_Name			    ; Class name
  :GFU_Min			    ; Class range minimum
  :GFU_Max			    ; Class range maximum
  :GFU_MinMax			    ; Class value (min=max)
  :GFU_Red			    ; Red class color (0-255)
  :GFU_Green			    ; Green class color (0-255)
  :GFU_Blue			    ; Blue class color (0-255)
  :GFU_Alpha			    ; Alpha (0=transparent,255=opaque)
  :GFU_RedMin			    ; Color Range Red Minimum
  :GFU_GreenMin			    ; Color Range Green Minimum
  :GFU_BlueMin			    ; Color Range Blue Minimum
  :GFU_AlphaMin			    ; Color Range Alpha Minimum
  :GFU_RedMax			    ; Color Range Red Maximum
  :GFU_GreenMax			    ; Color Range Green Maximum
  :GFU_BlueMax			    ; Color Range Blue Maximum
  :GFU_AlphaMax			    ; Color Range Alpha Maximum
  :GFU_MaxCount)		    ; Maximum GFU value
(export 'gdal-rat-field-usage)

;; --------------------------------------------------------

(cffi:defcenum gdal-rw-flag
  "Read/Write flag for RasterIO() method"
  :GF_Read				; Read data
  :GF_Write)				; Write data
(export 'gdal-rw-flag)

;; --------------------------------------------------------

(cffi:defcstruct gdal-color-entry
    "Color tuple."
  (c1 :short)
  (c2 :short)
  (c3 :short)
  (c4 :short))


;; --------------------------------------------------------

(cffi:defcstruct gdal-gcp
  "GDAL_GCP Ground Control Point."
  (pszId :string)          ; Unique identifier, often numeric.
  (pszInfo :string)        ; Informational message or "".
  (dfGCPPixel :double)     ; Pixel (x) location of GCP on raster.
  (dfGCPLine :double)      ; Line (y) location of GCP on raster.
  (dfGCPX :double)         ; X position of GCP in georeferenced space.
  (dfGCPY :double)         ; Y position of GCP in georeferenced space
  (dfGCPZ :double))        ; Elevation of GCP, or zero if not known.

;; --------------------------------------------------------

(cffi:defcfun ("GDALAllRegister" gdal-all-register) :void
  "Register all known configured GDAL drivers.

 This function will drive any of the following that are configured
 into GDAL. Many others as well haven't been updated in this
 documentation (see full list):

 GeoTIFF (GTiff)
 Geosoft GXF (GXF)
 Erdas Imagine (HFA)
 CEOS (CEOS)
 ELAS (ELAS)
 Arc/Info Binary Grid (AIGrid)
 SDTS Raster DEM (SDTS)
 OGDI (OGDI)
 ESRI Labelled BIL (EHdr)
 PCI .aux Labelled Raw Raster (PAux)
 HDF4 Hierachal Data Format Release 4
 HDF5 Hierachal Data Format Release 5
 GSAG Golden Software ASCII Grid
 GSBG Golden Software Binary Grid

 This function should generally be called once at the beginning of the
 application." )
(export 'gdal-all-register)

;; EOF
