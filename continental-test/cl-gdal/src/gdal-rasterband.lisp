;;; -*- package: CL-GDAL; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-gdal)

;; --------------------------------------------------------

(cffi:defcfun ("GDALComputeRasterMinMax" GDAL-Compute-Raster-Min-Max) :void
  "Compute the min/max values for a band."
  (hBand gdal-raster-band-h)
  (bApproxOK :int)
  (adfMinMax (:pointer :double)) ;; array of 2
  )

;; --------------------------------------------------------

(cffi:defcfun ("GDALComputeRasterStatistics" gdal-compute-raster-statistics) cpl-err
  "Compute image statistics."
  (hBand gdal-raster-band-h)
  (bApproxOK :int)
  (pdfMin (:pointer :double))
  (pdfMax (:pointer :double))
  (pdfMean (:pointer :double))
  (pdfStdDev (:pointer :double))
  (pfnProgress :pointer) ;; GDALProgressFunc
  (pProgressData :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("GDALCreateMaskBand" GDALCreateMaskBand) cpl-err
  "Adds a mask band to the current band."
  (hBand gdal-raster-band-h)
  (nFlags :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALFillRaster" GDAL-Fill-Raster) cpl-err
  "Fill this band with a constant value."
  (hBand gdal-raster-band-h)
  (dfRealValue :double)
  (dfImaginaryValue :double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALFlushRasterCache" GDAL-Flush-Raster-Cache) cpl-err
  "Flush raster data cache."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBandDataset" GDAL-Get-Band-Dataset) gdal-dataset-h
  "Fetch the owning dataset handle."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBandNumber" gdal-get-band-number) :int
  "Fetch the band number."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetBlockSize" gdal-get-block-size) :void
  "Fetch the \"natural\" block size of this band."
  (hBand gdal-raster-band-h)
  (pnXSize (:pointer :int))
  (pnYSize (:pointer :int)))

(cffi:defcfun ("GDALGetActualBlockSize" gdal-get-actual-block-size) :void
  "Fetch the \"natural\" block size of this band."
  (hBand gdal-raster-band-h)
  (blk-idx-x :int)
  (blk-idx-y :int)
  (actual-block-size-x (:pointer :int))
  (actual-block-size-y (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDefaultHistogram" gdal-get-default-histogram) cpl-err
  "Fetch default raster histogram."
  (hBand gdal-raster-band-h)
  (pdfMin (:pointer :double))
  (pdfMax (:pointer :double))
  (pnBuckets (:pointer :int))
  (ppanHistogram (:pointer (:pointer :int)))
  (bForce :int)
  (pfnProgress :pointer) ;; GDALProgressFunc
  (pProgressData :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetDefaultRAT" gdal-get-default-rat) gdal-raster-attribute-table-h
  "Fetch default Raster Attribute Table."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMaskBand" gdal-get-mask-band) gdal-raster-band-h
  "Return the mask band associated with the band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetMaskFlags" gdal-get-mask-flags) :int
  "Return the status flags of the mask band associated with the band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetOverview" gdal-get-overview) gdal-raster-band-h
  "Fetch overview raster band object."
  (hBand gdal-raster-band-h)
  (i :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetOverviewCount" gdal-get-overview-count) :int
  "Return the number of overview layers available."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterAccess" gdal-get-raster-access) gdal-access
  "Find out if we have update permission for this band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterBandXSize" gdal-get-raster-band-x-size) :int
  "Fetch XSize of raster."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterBandYSize" gdal-get-raster-band-y-size) :int
  "Fetch YSize of raster."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterCategoryNames" gdal-get-raster-category-names) (:pointer :string)
  "Fetch the list of category names for this raster."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterColorInterpretation" gdal-get-raster-color-interpretation) gdal-color-interp
  "How should this band be interpreted as color?"
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterColorTable" gdal-get-raster-color-table) gdal-color-table-h
  "Fetch the color table associated with band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterDataType" gdal-get-raster-data-type) gdal-data-type
  "Fetch the pixel data type for this band."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterHistogram" gdal-get-raster-histogram) cpl-err
  "Compute raster histogram."
  (hBand gdal-raster-band-h)
  (dfMin :double)
  (dfMax :double)
  (nBuckets :int)
  (panHistogram (:pointer :int))
  (bIncludeOutOfRange :int)
  (bApproxOK :int)
  (pfnProgress :pointer) ;; GDALProgressFunc
  (pProgressData :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterMaximum" gdal-get-raster-maximum) :double
  "Fetch the maximum value for this band."
  (hBand gdal-raster-band-h)
  (pbSuccess (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterMinimum" gdal-get-raster-minimum) :double
  "Fetch the minimum value for this band."
  (hBand gdal-raster-band-h)
  (pbSuccess (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterNoDataValue" gdal-get-raster-no-data-value) :double
  "Fetch the no data value for this band."
  (hBand gdal-raster-band-h)
  (pbSuccess (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterOffset" gdal-get-raster-offset) :double
  "Fetch the raster value offset."
  (hBand gdal-raster-band-h)
  (pbSuccess (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterSampleOverview" gdal-get-raster-sample-overview) gdal-raster-band-h
  "Fetch best sampling overview."
  (hBand gdal-raster-band-h)
  (nDesiredSamples :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterScale" gdal-get-raster-scale) :double
  "Fetch the raster value scale."
  (hBand gdal-raster-band-h)
  (pbSuccess (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterStatistics" gdal-get-raster-statistics) cpl-err
  "Fetch image statistics."
  (hBand gdal-raster-band-h)
  (bApproxOK :int)
  (bForce :int)
  (pdfMin (:pointer :double))
  (pdfMax (:pointer :double))
  (pdfMean (:pointer :double))
  (pdfStdDev (:pointer :double)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALGetRasterUnitType" gdal-get-raster-unit-type) :string
"Return raster unit type."
(hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALHasArbitraryOverviews" gdal-has-arbitrary-overviews) :int
  "Check for arbitrary overviews."
  (hBand gdal-raster-band-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRasterAdviseRead" gdal-raster-advise-read) cpl-err
  "Advise driver of upcoming read requests."
  (hBand gdal-raster-band-h)
  (nXOff :int)
  (nYOff :int)
  (nXSize :int)
  (nYSize :int)
  (nBufXSize :int)
  (nBufYSize :int)
  (eDT gdal-data-type)
  (papszOptions (:pointer :string)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALRasterIO" gdal-raster-io) cpl-err
  "Read/write a region of image data for this band."
  (hBand gdal-raster-band-h)
  (eRWFlag gdal-rw-flag)
  (nXOff :int)
  (nYOff :int)
  (nXSize :int)
  (nYSize :int)
  (pData :pointer)
  (nBufXSize :int)
  (nBufYSize :int)
  (eBufType gdal-data-type)
  (nPixelSpace :int)
  (nLineSpace :int))

;; --------------------------------------------------------

(cffi:defcfun ("GDALReadBlock" gdal-read-block) cpl-err
  "Read a block of image data efficiently."
  (hBand gdal-raster-band-h)
  (nXOff :int)
  (nYOff :int)
  (pData :pointer))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetDefaultHistogram" gdal-set-default-histogram) cpl-err
  "Set default histogram."
  (hBand gdal-raster-band-h)
  (dfMin :double)
  (dfMax :double)
  (nBuckets :int)
  (panHistogram (:pointer :int)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetDefaultRAT" gdal-set-default-rat) cpl-err
  "Set default Raster Attribute Table."
  (hBand gdal-raster-band-h)
  (hRAT gdal-raster-attribute-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterCategoryNames" gdal-set-raster-category-names) cpl-err
  "Set the category names for this band."
  (hBand gdal-raster-band-h)
  (papszNames (:pointer :string)))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterColorInterpretation" gdal-set-raster-color-interpretation) cpl-err
  "Set color interpretation of a band."
  (hBand gdal-raster-band-h)
  (eColorInterp gdal-color-interp))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterColorTable" gdal-set-raster-color-table) cpl-err
  "Set the raster color table."
  (hBand gdal-raster-band-h)
  (hCT gdal-color-table-h))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterNoDataValue" gdal-set-raster-no-data-value) cpl-err
  "Set the no data value for this band."
  (hBand gdal-raster-band-h)
  (dfValue :double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterOffset" gdal-set-raster-offset) cpl-err
  "Set scaling offset."
  (hBand gdal-raster-band-h)
  (dfNewOffset :double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterScale" gdal-set-raster-scale) cpl-err
  "Set scaling ratio."
  (hBand gdal-raster-band-h)
  (dfNewOffset :double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterStatistics" gdal-set-raster-statistics) cpl-err
  "Set statistics on band."
  (hBand gdal-raster-band-h)
  (dfMin :double)
  (dfMax :double)
  (dfMean :double)
  (dfStdDe :double))

;; --------------------------------------------------------

(cffi:defcfun ("GDALSetRasterUnitType" gdal-set-raster-unit-type) cpl-err
  "Set unit type.

Since: GDAL 1.8.0"
  (hBand gdal-raster-band-h)
  (pszNewValue :string))

;; --------------------------------------------------------

(cffi:defcfun ("GDALWriteBlock" gdal-write-block) cpl-err
  "Write a block of image data efficiently."
  (hBand gdal-raster-band-h)
  (nXOff :int)
  (nYOff :int)
  (pDat :pointer))

;; EOF
