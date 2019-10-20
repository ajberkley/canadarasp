/****************************************************************
 *  [ Style Class Name, Value, Description, Full Description ]  *
 *  NOTE: Style Class Names are defined in the .html file       *
 ****************************************************************/
var paramListFull = new Array();
paramListFull[ 0  ]= ["optionBoldRed","nope1","- WIND PARAMETERS -","noclick"];
paramListFull[ 1  ]= ["","sfcwind","Sfc.Wind (27m)","The speed and direction of the wind at 27m above the ground."];
paramListFull[ 2  ]= ["","sfcwind1","Sfc.Wind (95m)","The speed and direction of the wind at 95m above the ground."];
paramListFull[ 3  ]= ["optionBoldBlue","sfcwind2","Sfc.Wind (187m)","The speed and direction of the wind at 187m above the ground."];
paramListFull[ 4  ]= ["","sfcwind","Sfc.Wind (10m)", "The speed and direction of the wind at 10m above the ground."];
paramListFull[ 5  ]= ["","sfcwind1","Sfc.Wind (40m)", "The speed and direction of the wind at 40m above the ground."];
paramListFull[ 6  ]= ["","sfcwind2","Sfc.Wind (80m)", "The speed and direction of the wind at 80m above the ground."];
paramListFull[ 7  ]= ["optionBoldBlue", "sfcwind3","Sfc.Wind (120m)", "The speed and direction of the wind at 120m above the ground."];
paramListFull[ 8  ]= ["", "wind500","Wind at 500m", "The speed and direction of the wind at 500m above sea level."];
paramListFull[ 9  ]= ["optionBoldBlue","wind1000","Wind at 1000m", "The speed and direction of the wind at 1000m above sea level."];
paramListFull[ 10 ]= ["optionBoldBlue","wind1500","Wind at 1500m", "The speed and direction of the wind at 1500m above sea level."];
paramListFull[ 11 ]= ["optionBoldBlue","wind2000","Wind at 2000m", "The speed and direction of the wind at 2000m above sea level."];
paramListFull[ 12 ]= ["optionBoldBlue","wind2500","Wind at 2500m", "The speed and direction of the wind at 2500m above sea level."];
paramListFull[ 13 ]= ["optionBoldBlue","wind3000","Wind at 3000m", "The speed and direction of the wind at 3000m above sea level."];
paramListFull[ 14 ]= ["", "wblmaxmin","BL Max. Up/Down (Convergence)", "Maximum grid-area-averaged extensive upward or downward motion within the BL as created by horizontal wind convergence.  Positive convergence is associated with local small-scale convergence lines (often called 'shear lines' by pilots, which are horizontal changes of wind speed/direction) - however, the actual size of such features is much smaller than can be resolved by the model so only stronger ones will be forecast and their predictions are subject to much error.  If CAPE is also large, thunderstorms can be triggered.  Negative convergence (divergence) produces subsiding vertical motion, creating low-level inversions which limit thermaling heights.  This parameter can be noisy, so users should be wary.  For a grid resolution of 12km or better convergence lines created by terrain are commonly predicted - sea-breeze predictions can also be found for strong cases, though they are best resolved by smaller-resolution grids."];
paramListFull[ 15 ]= ["", "vwind0","Vertical winds at 230m", "Vertical wind speed at 230m due to ridge lift and convergence. Not sure what this means when the terrain is > 230m ;)"];
paramListFull[ 16 ]= ["", "vwind1","Vertical winds at 1600m", "Vertical wind speed at 1600 due to ridge lift and convergence"];
paramListFull[ 17 ]= ["", "vwind2","Vertical winds at 3100m", "Vertical wind speed at 3100m due to ridge lift and convergence"];
paramListFull[ 18 ]= ["optionBoldRed","nope1","- THERMAL PARAMETERS -", "noclick"];
paramListFull[ 19 ]= ["optionBoldBlue","wstar","Thermal Updraft Velocity (W*)", "Average dry thermal updraft strength near mid-BL height.  Subtract glider descent rate to get average vario reading for cloudless thermals.  Updraft strengths will be stronger than this forecast if convective clouds are present, since cloud condensation adds buoyancy aloft (i.e. this neglects cloudsuck).  W* depends upon both the surface heating and the BL depth"];
paramListFull[ 20 ]= ["","bsratio","Buoyancy/Shear Ratio", "Dry thermals may be broken up by vertical wind shear (i.e. wind changing with height) and unworkable if B/S ratio is 5 or less.  [Though hang-gliders can soar with smaller B/S values than can sailplanes.]  If convective clouds are present, the actual B/S ratio will be larger than calculated here due to the neglect of 'cloudsuck'.  [This parameter is truncated at 10 for plotting.]"];
paramListFull[ 21 ]= ["optionBoldBlue", "hwcritagl","Top of Lift AGL", "This parameter estimates the height at which the average dry updraft strength drops below 1.0 m/s and is expected to give better quantitative numbers for the maximum cloudless thermaling height than the BL Top height forecast, especially when mixing results from vertical wind shear rather than thermals.  (Note: the present assumptions tend to under-predict the max. thermaling height for dry conditions.) In the presence of clouds the maximum thermaling height may instead be limited by the cloud base. Being for dry thermals, this parameter omits the effect of cloudsuck.  The height is above ground level"];
paramListFull[ 22 ]= ["",		 "hbl",						"BL Top", "Height of the top of the mixing layer, which for thermal convection is the average top of a dry thermal.  Over flat terrain, maximum thermaling heights will be lower due to the glider descent rate and other factors.  In the presence of clouds (which release additional buoyancy aloft, creating cloudsuck) the updraft top will be above this forecast, but the maximum thermaling height will then be limited by the cloud base. Further, when the mixing results from shear turbulence rather than thermal mixing this parameter is not useful for glider flying.  NB: this BL Top is not the height where the Thermal Index (TI) is zero, which is a criteria used by many simple determinations of the BL top - instead, the RASP BL Top uses a more sophisticated BL Top criteria based on turbulent fluxes"];
paramListFull[ 23 ]= ["",		 "bldepth",						"BL Depth", "Depth of the layer mixed by thermals or (vertical) wind shear.  This parameter can be useful in determining which flight direction allows better thermaling conditions when average surface elevations vary greatly in differing directions.  (But the same cautions mentioned under Height of BL Top also apply.)  It is also an important determinant of thermals strength (as is the Surface Heating)"];
paramListFull[ 24 ]= ["",		 "zwblmaxmin",		"MSL Height of max/min Wbl", "Height at which the max / min of the vertical velocity in the Boundary Layer occurs, i.e. of \"BL Max Up/Down (Convergence)\" (qv)"];
paramListFull[ 25 ]= ["",		 "sfcshf",				"Sfc.Heating", "Heat transferred into the atmosphere due to solar heating of the ground, creating thermals.  This parameter is an important determinant of thermal strength (as is the BL depth).  This parameter is obtained directly from WRF model output and not from a BLIPMAP computation."];
paramListFull[ 26 ]= ["", "sfcsun",			"Solar radiation", "The 'Solar Radiation' incident at the surface.  This parameter indicates the degree of cloudiness, i.e. where clouds limit the sunlight reaching the surface."];
paramListFull[ 27 ]= ["",		 "sfctemp",				"Sfc.Temperature", "The temperature at a height of 2m above ground level.  This can be compared to observed surface temperatures as an indication of model simulation accuracy; e.g. if observed surface temperatures are significantly below those forecast, then soaring conditions will be poorer than forecast.  This parameter is obtained directly from WRF model output and not from a BLIPMAP computation."];
paramListFull[ 28 ]= ["",		 "sfcdewpt", 			"Sfc.DewpointDepr.", "The dew point depression (temperature at a height of 2m above ground level - dew point temperature).  This can be compared to observed surface dew point temperatures as an indication of model simulation accuracy; e.g. if observed surface dew point temperatures are significantly below those forecast, then BL cloud formation will be poorer than forecast.  This parameter is obtained directly from WRF model output and not from a BLIPMAP computation."];
paramListFull[ 29 ]= ["optionBoldRed","nope1","- CLOUD PARAMETERS -", "noclick"];
paramListFull[ 30 ]= ["optionBoldBlue",		 "blcloudpct", 		"BL Cloud Cover", "This parameter provides an additional means of evaluating the formation of clouds within the BL and might be used either in conjunction with or instead of the other cloud prediction parameters.  It assumes a very simple relationship between cloud cover percentage and the maximum relative humidity within the BL.  The cloud base height is not predicted, but is expected to be below the BL Top height.  DrJack does not have a lot of faith in this prediction, since the formula used is so simple, and expects its predictions to be very approximate - but other meteorologists have used it and it is better than nothing.  Note: Since The the 'BL Cloud Cover', 'Cumulus Potential', and 'BL Extensive CloudBase' are based upon fundamentally different model predictions -- respectively the predicted maximum moisture in the BL, the predicted surface moisture, and an explicit cloud-water prediction -- they can yield somewhat differing predictions, e.g. the 'Cumulus Potential' can predict puffy cloud formation when the 'BL Cloud Cover' is zero or vice versa."];
paramListFull[ 31 ]= ["optionBoldBlue",		 "cloud", 		"Cloud Cover", "Percentage cloud coverage fall all levels"];
paramListFull[ 32 ]= ["", "lowcloud", 		"Low elevation cloud cover", "Percentage cloud coverage at elevations below 3500 m"];
paramListFull[ 33 ]= ["", "midcloud", 		"Mid elevation cloud cover", "Percentage cloud coverage at elevations above 3500 m and below 6500 m"];
paramListFull[ 34 ]= ["", "highcloud", 		"High elevation cloud cover", "Percentage cloud coverage at elevations above 6500 m"];
paramListFull[ 35 ]= ["optionBoldBlue",		 "rain", 				"Rain", "Rain accumulated over the last hour. Note that this requires a forecast for the previous hour, so it is not possible to plot this parameter until 1 hour after the first forecast for the day."];
paramListFull[ 36 ]= ["optionBoldBlue",		 "CAPE", 					"CAPE", "Convective Available Potential Energy indicates the atmospheric stability affecting deep convective cloud formation above the BL.  A higher value indicates greater potential instability, larger updraft velocities within deep convective clouds, and greater potential for thunderstorm development (since a trigger is needed to release that potential).  Note that thunderstorms may develop in regions of high CAPE and then get transported downwind to regions of lower CAPE.  Also, locations where both convergence and CAPE values are high can be subject to explosive thunderstorm development."];
paramListFull[ 37 ]= ["",		 "sfcpres", 					"Surface Pressure MSL", "Surface Pressure MSL (mb)"];
paramListFull[ 38 ]= ["optionBoldBlue","wind3600","Wind at 3600m", "The speed and direction of the wind at 3600m above sea level."];
paramListFull[ 39 ]= ["optionBoldBlue","wind4200","Wind at 4200m", "The speed and direction of the wind at 4200m above sea level."];
paramListFull[ 40 ]= ["",		 "cloudbase", 					"Cloud base", "If there are clouds, this shows cloudbase in m."];
paramListFull[ 41 ]= ["",		 "terrain", 					"Terrain Height", "Height of model terrain"];
paramListFull[ 42  ]= ["optionBoldRed","nope1","- OTHER -","noclick"];
paramListFull[ 43 ]= ["optionBoldBlue",		 "maxgust", 					"Max wind gust", "Maximum predicted wind gust at the surface"];


var HRDPSparamListFull = [
	paramListFull[0],
	paramListFull[4],
	paramListFull[5],
	paramListFull[6],
	paramListFull[43],
	paramListFull[7],
	paramListFull[8],
	paramListFull[9],
	paramListFull[10],
	paramListFull[11],
	paramListFull[12],
	paramListFull[13],
	paramListFull[38],
	paramListFull[39],
	paramListFull[15],
	paramListFull[16],
	paramListFull[17],
	paramListFull[18],
	paramListFull[19],
	paramListFull[21],
	paramListFull[23],
	paramListFull[25],
	paramListFull[27],
//	paramListFull[28],
	paramListFull[29],
	paramListFull[31],
	paramListFull[35],
        paramListFull[36],
        paramListFull[42],
	paramListFull[37],
	paramListFull[41]
];

var GDPSparamListFull = [
	paramListFull[0],
	paramListFull[4],
	paramListFull[5],
	paramListFull[6],
	paramListFull[43],
	paramListFull[7],
	paramListFull[8],
	paramListFull[9],
	paramListFull[10],
	paramListFull[11],
	paramListFull[12],
	paramListFull[13],
	paramListFull[38],
	paramListFull[39],
	paramListFull[16],
	paramListFull[17],
	paramListFull[18],
	paramListFull[19],
	paramListFull[21],
	paramListFull[23],
	paramListFull[25],
	paramListFull[27],
//	paramListFull[28],
	paramListFull[29],
	paramListFull[31],
	paramListFull[35],
        paramListFull[42],
	paramListFull[37],
	paramListFull[41]



];
