/* Canada RASP tiled map viewer
    ajb March 2019, added archive HRDPS maps for BC
    ajb September 2018, added GDPS
    ajb August 2018, continental coverage!
    ajb May-June 2018
    based on work by
    ajb Mar 2013 heavily modified
    based on original work by:
    p.scorer_at_leedsmet_dot_ac_dot_uk
    Based on, and with many ideas taken from, gogo's RASPviewer
    And thanks to the Uncountable and Invaluable contributions from <the_author_of> http://glidemet.co.uk,
    to both this viewer and RASP-UK parametrisation.
*/

var opacity_control = "N"; // keep track of whether we initialized the opacity control or not
var archive_mode = false;  // Whether we are browsing archives or not
var oldParam;

var map;           // stores the google.map
var opacity = 50;  // default opacity
var OPACITY_MAX_PIXELS = 57; // Width of opacity control image

var regions = [ "Lower Mainland", "Sea to Sky", "Vancouver Island", "Kamloops Area","North Okanagan","South Okanagan","Kootenays","Smithers Area","Jasper Area","Alberta","Washington","Ski","Cariboo", "Montreal", "Yukon", "Saskatchewan", "Manitoba", "Montana"];
var windgrammarkers = Array();
var windgramsinitialized = false;

function displayWindgrams() {
    if(windgramsinitialized) {
	for(i = 0 ; i < windgrammarkers.length ; i++ ) { windgrammarkers[i].setVisible(true); }
    } else {	
	// Create markers.
	regions.forEach(function(region) {
            var mylocationidx = 0;
            var myregionidx = regions.indexOf(region);
            locations.forEach(function(location) {
		if(location[0]==region) {
		    var text = 'Click to view: ' + location[0] + ' ' + location[1];
		    var marker = new google.maps.Marker({
			position: new google.maps.LatLng(location[2],location[3]),
			map: map,
			title: text
		    });
		    windgrammarkers.push(marker);
		    var mylocationidxlocal = mylocationidx++; 
		    google.maps.event.addListener(marker,'click', function() { window.open('windgrams/?region='+myregionidx+'&location='+mylocationidxlocal+'&Model='+model())});
		}
            });
	});
	windgramsinitialized=true;
    }
}

function clearWindgrams () {
    for(i = 0 ; i < windgrammarkers.length ; i++ ) { windgrammarkers[i].setVisible(false); }
}

function onDemandWindgram(lat, lon) {
	window.open('windgram?lat='+lat+'&lon='+lon);
}

function padwithzero(string) {
    string = string+"";
    if (string.length < 2) {
	return("0" + string + "");
    } else {
	return(string);
    }
}

function setSize() {
    return;
    // var titleBox = document.getElementById("topTitle");
    // var zoomBox = document.getElementById("zoomBox");;
    // var scaleBox = document.getElementById("botScale");;
    // var height = window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight;
    // var width = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
    // titleBox.style.height   =  "68px";
    // titleBox.style.overflow = "hidden" ;
    // titleBox.style.position = "relative" ;

    // zoomBox = document.getElementById("zoomBox");
    // zoomBox.style.width = "100vw";
    // zoomBox.style.overflow = "hidden";
    // zoomBox.style.position = "relative" ;

    // scaleBox = document.getElementById("botScale");
    // titleBox.style.height = "50px";
    // scaleBox.style.overflow = "hidden";
    // scaleBox.style.position = "relative" ;
}

function model_is_hrdps() {
    return(model() == "hrdps" || model() == "hrdps archive")
}

function setupParamset () {
    var paramel = document.getElementById("Param");
    var idx;
    var newParams;
    if(model_is_hrdps()) { newParams=HRDPSparamListFull; } else { newParams=GDPSparamListFull; };
    paramel.options = true;   
    for (var i = 0; i < newParams.length; i++) {
	paramel.options[i] = new Option(newParams[i][2], newParams[i][1]);
	paramel.options[i].className = newParams[i][0];
    }
    if(paramel.options.length > newParams.length){
	for(i = newParams.length; i < paramel.length; i++){
	    paramel.options[i] = null;
	}
    }
    paramel.options.length = newParams.length;
    for(var i = 0; i < paramel.options.length; i++){
	if(paramel.options[i].value == oldParam) break;
    }
    if(i == paramel.options.length){
	paramel.options[1].selected = true;  // Not available
    } else{
	paramel.options[i].selected =true;
    }
}

var windgram_checkbox;

function model () {
    return((document.getElementById("model").selectedOptions)[0].value)
}

function step () {
    if(model_is_hrdps()) {
	return 2;
    } else if(model() === "gdps") {
	return 10;
    }
}

function callWithTimeZone(callback) {
    var pos = map.getCenter();
    var timestamp = Math.round(Date.now()/1000);
    //var request = "/timezone?location="+pos.lat()+","+pos.lng()+"&timestamp="+timestamp+"&key=AIzaSyAEkxYNkm8Vjuw0HguSNMn4j39QoI8-rks";
    var offset = 60*Math.round(pos.lng() * 24 / 360) + 60; //dst hardcoded
    callback(offset);
    // var xhttp = new XMLHttpRequest();
    // xhttp.timeout = 1000; // 1 second before timeout
    // xhttp.onreadystatechange = function() {
    // 	if (this.readyState == 4 && this.status == 200) {
    // 	    var result = JSON.parse(this.responseText);
    // 	    if(result.status == "ZERO_RESULTS") {
    // 		var offset = 60*Math.round(pos.lng() * 24 / 360);
    // 		//console.log("Didn't get an answer from google about time zone offset, so using natural offset of " + offset);
    // 		callback(offset)
    // 	    } else {
    // 		//console.log(this.responseText);
    // 		//console.log("Got an answer, rawOffset in minutes is + " + result.rawOffset/60 + " dstOffset is " + result.dstOffset/60 );
    // 		callback(result.rawOffset/60 + result.dstOffset/60);
    // 	    }
    // 	}
    // };
    // xhttp.ontimeout = function (e) { console.log("Didn't get timezone information, leaving it unchanged");
    // 				     callback(undefined); };
    // xhttp.onerror = function (e) { console.log("Error getting timezone"); console.log(e); callback(undefined); };
    // xhttp.open("GET", request, true);
    // xhttp.send(); 
}

function set_datetime_options (offset_in) {
    var hourstep = 1;
    var hourpast = -24;
    var hourfuture = 99;
    var msperhour = 60 * 60 * 1000; // milliseconds in an hour
    var nowDate = new Date();
    var now = msperhour*Math.round(nowDate.getTime()/msperhour);  // Time now in milliseconds rounded to an hour
    var msperminute = 60 * 1000;
    var j = 0;
    var selected = false;
    var datetime = document.getElementById("datetime");
    var oldindex = datetime.selectedIndex;
    var offset = getTimeZoneOffset();
    if(offset_in != undefined) {
	offset = offset_in;
    }
    var offsethours = Math.round(offset/60);
    var offsetmins = offset % 60;
    if(model_is_hrdps()) {
	hourfuture = 48;
    } else {
	hourfuture = 99;
    }
    //console.log("date time being processed for UTC offset of " + offset);

            var offsethourstring = "Timezone: UTC"+(offsethours<0?"":"+")+offsethours;
            document.getElementById("timezone").innerHTML = offsethourstring;
            document.getElementById("timezone").style.display = "block";
    if(oldindex == -1) { // We need to do an initial population of the options.
	//console.log("Resetting datetime options")
	while (datetime.options.length) {
	    datetime.options.remove(0);
	}
	for (var i = hourpast ; i < hourfuture ; i++ ) {
	    var newDate = new Date(now + msperhour*i);
	    var hourutc = newDate.getUTCHours();
	    var dayutc = padwithzero(newDate.getUTCDate());
	    var monthutc = padwithzero(newDate.getUTCMonth() + 1);
	    var yearutc = newDate.getUTCFullYear();
	    var localDate = new Date(now + msperhour*i + msperminute*offset);
	    var hourlocal = localDate.getUTCHours();
	    var daylocal = padwithzero(localDate.getUTCDate());
	    var monthlocal = padwithzero(localDate.getUTCMonth() + 1);
	    var yearlocal = localDate.getUTCFullYear();
	    if(model() != "gdps" || ((hourutc % 3) == 0)) { // GDPS is every 3 hours
		var localtime = yearlocal+"-"+monthlocal+"-"+daylocal+" "+padwithzero(hourlocal)+":"+padwithzero(offsetmins); //+ " UTC"+(offsethours<0?"":"+")+offsethours;
		var utctime = yearutc+"-"+monthutc+"-"+dayutc+" "+padwithzero(hourutc)+"00";
		var option = new Option(localtime,utctime);
		option.date = newDate;
		datetime.options.add(option,j);
		datetime.options[j].date = newDate;
		//console.log("Created option " + j + " with localtime " + localtime + " UTC time " + utctime + " ms " + datetime.options[j].date);
		if(!selected) {
		    if(i==0) { datetime.options[j].selected = true; selected = true;};
		    if(i>0) { datetime.options[j].selected = true; selected = true; };
		}
		j = j + 1;
	    }
	}
    } else {
	//console.log("updating " + datetime.options.length + " time strings")
	for(j = 0 ; j < datetime.options.length; j++ ) {
	    var ms = datetime.options[j].date.getTime();
	    var localDate = new Date(ms + msperminute*offset);
	    var hourlocal = localDate.getUTCHours();
	    var daylocal = padwithzero(localDate.getUTCDate());
	    var monthlocal = padwithzero(localDate.getUTCMonth() + 1);
	    var yearlocal = localDate.getUTCFullYear();
	    //console.log("Processing option " + j + " with old local time " + datetime.options[j].text + " and UTC time " + datetime.options[j].value);
	    datetime.options[j].text = yearlocal+"-"+monthlocal+"-"+daylocal+" "+padwithzero(hourlocal)+":"+padwithzero(offsetmins);
	    //console.log("new local time is " + datetime.options[j].text);
	}
    }
}

function daysInMonth(iMonth, iYear) {
    return 32 - new Date(iYear, iMonth, 32).getDate();
}

function set_number_of_days() {
    var year=document.getElementById("yearpicker").value;
    var month=document.getElementById("monthpicker").value;
    var day=document.getElementById("daypicker");
    var oldIndex=day.options.selectedIndex;
    var num_days=daysInMonth(month-1, year);
    // console.log('oldIndex was ' + oldIndex + ' and num_days is ' + num_days)
    while(day.options.length > 0) {
	day.options.remove(0);
    }
    for (j = 1 ; j <= num_days ; j++) {
	d = document.createElement("OPTION");
	d.label = j;
	d.text = padwithzero(j);
	day.options.add(d);
	// console.log('Added ' + d)
    }
    // console.log('day.options.length is ' + day.options.length)
    // day.options[Math.min(oldIndex, num_days-1)].selected = 'selected';
    day.options.selectedIndex = Math.min(oldIndex, num_days-1)
    // console.log('tried to set to ' + Math.min(oldIndex, num_days) + ' day.options.selectedIndex is ' + day.options.selectedIndex)
}

function set_archive_hours(offset_in) {
    if(offset_in != undefined) {
	offset = offset_in;
    } else {
	offset = getTimeZoneOffset();  // backup if we can't talk to Google
    }
    var offsethours = Math.round(offset/60);
    var offsetmins = offset % 60;

    var offsethourstring = "YYYY/MM/DD UTC"+(offsethours<0?"":"+")+offsethours;
    document.getElementById("archive_offset_display").innerHTML = offsethourstring;
    document.getElementById("archive_offset_display").style.display = "block";

}

function set_datetime_and_load_images (offset) {
    if(model()=="hrdps archive"){
	document.getElementById("archivetimepicker").style.height = ""
	document.getElementById("archivetimepicker").style.visibility = "visible"
	document.getElementById("normaltimepicker").style.height = "0px"
	document.getElementById("normaltimepicker").style.visibility = "hidden"
	set_archive_hours(offset)
    } else {
	document.getElementById("archivetimepicker").style.height = "0px"
	document.getElementById("normaltimepicker").style.height = ""
	document.getElementById("normaltimepicker").style.visibility = "visible"
	document.getElementById("archivetimepicker").style.visibility = "hidden"
	set_datetime_options(offset);
    }
    doChange();
}

function findIndexOfOptionValue(el, option) {
    for (i = 0 ; i < el.options.length ; i++) {
	if(option == el.options[i].value) {
	    return i;
	}
    }
}

function update_archive_date() {
    set_number_of_days();
    callWithTimeZone((function(offset) { set_archive_hours(offset); doChange(); }))
}

function initIt() {
    document.getElementById("model").onchange = (function () { document.getElementById("datetime").selectedIndex = -1; callWithTimeZone(set_datetime_and_load_images); setupParamset(); });
    document.getElementById("monthpicker").onchange = update_archive_date;
    document.getElementById("yearpicker").onchange = update_archive_date;
    document.getElementById("daypicker").onchange = update_archive_date;
    document.getElementById("hourpicker").onchange = doChange;
    document.body.style.overflow = "auto";
    oldParam = document.getElementById("Param").options.value;
    var T = new Date();      // Instantiate a Date object
    year=document.getElementById("yearpicker")
    month=document.getElementById("monthpicker")
    day=document.getElementById("daypicker")
    hour=document.getElementById("hourpicker")
    year.selectedIndex = findIndexOfOptionValue(year,T.getFullYear())
    month.selectedIndex = findIndexOfOptionValue(month,T.getMonth()+1)
    day.selectedIndex = findIndexOfOptionValue(day, T.getDate())
    hour.selectedIndex = 10

    
    var lat = 49.05
    var lon = -122.18;
    var zoom = 10;

    setupParamset();
    document.getElementById("Param").onchange = doChange;
    document.getElementById("datetime").onchange = doChange;
    windgram_checkbox = document.getElementById('windgram_checkbox');
    
    document.getElementById("Param").options[4].selected  = true;
    whichBrowser();
    /*****************************************
     * Process URL tail and set menu options *
     *****************************************/
    if( location.href.split("?")[1]) { // Any args?
	args=location.href.split("?")[1].split(",");
	for(i = 0; i < args.length; i++){
	    prams = args[i].split("=");
	    if(prams[0] == "domain") { // backwards compatibility
		if(prams[1] == "swBC-hrdps") {
		    lat = 49.734337;
		    lon = -122.51675;
		    zoom = 8;
		} else if(prams[1] == "seBC-hrdps") {
		    lat = 49.7116;
		    lon = -116.5601;
		    zoom = 8;
		} else if(prams[1] == "Lumby-hrdps") {
		    lat = 50.48037;
		    lon = -118.973;
		    zoom = 9;
		} else if(prams[1] == "Pemberton-hrdps") {
		    lat = 50.15255;
		    lon = -122.6544;
		    zoom = 9;		    
		}
	    }
	    if(prams[0] == "opacity") {
		opacity = 1*prams[1];
	    }
	    if(prams[0] == "param"){
		for(j = 0; j < document.getElementById("Param").options.length; j++){
		    if(document.getElementById("Param").options[j].value == prams[1]){
			document.getElementById("Param").options[j].selected = true;
			oldParam = document.getElementById("Param").value
			break;
		    }
		}
	    }
	    if(prams[0] == "lat") {
		lat = 1*prams[1];
	    }
	    if(prams[0] == "lon") {
		lon = 1*prams[1];
	    }
	    if(prams[0] == "zoom") {
		zoom = 1*prams[1];
	    }
	    if(prams[0] == "windgrams") {
		if(prams[1]=="true") {
		    windgram_checkbox.checked = true;
		} else {
		    windgram_checkbox.checked = false;
		}
	    }
	}
    }

    var myOptions = {
	zoom:          zoom,
	center:        new google.maps.LatLng(lat, lon),
	mapTypeId:      google.maps.MapTypeId.TERRAIN,
	draggableCursor:    "crosshair",
	streetViewControl:  false,
	minZoom: 3,
	maxZoom: 15,
	scaleControl: true,
	fullscreenControl: false,
	mapTypeControl: true,
	mapTypeControlOptions: {
	    // style: google.maps.MapTypeControlStyle.DROPDOWN_MENU,
	    position: google.maps.ControlPosition.LEFT_BOTTOM
	}
    };
    window.addEventListener("resize",setSize,false)
    map = new google.maps.Map(document.getElementById("zoomBox"), myOptions);
    setSize();
    // if ( opacity_control == "N" ) {
    // 	createOpacityControl(map, opacity);
    // 	opacity_control = "Y";
    // }

    if(windgram_checkbox.checked) {
	displayWindgrams();
    } else {
	clearWindgrams();
    }
    //doChange();

    var boundsupdate;
    google.maps.event.addListener(map, "bounds_changed",
				  function() {
				      if(boundsupdate) {clearTimeout(boundsupdate)};
				      boundsupdate = window.setTimeout(function ()
								       { //console.log("Bounds changed!")
									   recordpageurl();
									   callWithTimeZone(set_datetime_and_load_images);
								       },300);
				  });
    var moveupdate;
    google.maps.event.addListener(map, 'mousemove',
				  function (e) {
				      if(moveupdate) {
					  clearTimeout(moveupdate);
				      }
				      moveupdate = setTimeout(function () {updateLL(e)},100)
				  });
    
    windgram_checkbox.addEventListener('change', (event) => {
	if (event.target.checked) {
	    displayWindgrams();
	} else {
	    clearWindgrams();
	}
	recordpageurl();
    })
    wind_checkbox.addEventListener('change',(event) => { doChange(); recordpageurl();});
    google.maps.event.addListener(map,'mouseup', function(){ clearTimeout(map.pressButtonTimer);});
    google.maps.event.addListener(map,'mousemove', function(){ clearTimeout(map.pressButtonTimer);});
    google.maps.event.addListener(map,'dragstart', function(){ clearTimeout(map.pressButtonTimer);});
    google.maps.event.addListener(map,'drag', function(){ clearTimeout(map.pressButtonTimer);});

    google.maps.event.addListener(map, "mousedown", function(event) {
	clearTimeout(map.pressButtonTimer);
	map.pressButtonTimer = setTimeout(function() {
            var lat = event.latLng.lat();
            var lng = event.latLng.lng();
	    // because we are about to switch windows we want to tosend a mouseup event
	    var evt = new MouseEvent("mouseup", {
		view: window,
		bubbles: true,
		cancelable: true,
		clientX: 20,
		/* whatever properties you want to give it */
	    });
	    document.dispatchEvent(evt);
	    onDemandWindgram(lat,lng);
	},1000);
    });
}

var ie;

function whichBrowser()
{
  ie = navigator.appName === 'Microsoft Internet Explorer' ;
}

function setOpacityA(opa) {
    opacity = opa;
    //console.log("setOpacityA("+opa+")")
    recordpageurl();
    var opacity_pixelx = OPACITY_MAX_PIXELS * opacity / 100;
    // opacityCtrlKnob.setValueX(opacity_pixelx);
    setOpacity(opacity_pixelx)
}

function clipto0to100 (num) {
    if(num <= 1) { return 1; } else if (num > 100) { return 100; } else {return num;}
}

function opacityplus() {
    setOpacityA(clipto0to100(opacity + 10))
}

function opacityminus() {
    setOpacityA(clipto0to100(opacity - 10))
}

function doChange()
{
    if(document.getElementById("Param").value === "nope1" ) {
	return 0;    // Catch a stupid selection
    }

    loadImages();
}

function getTimeZoneOffset() {
    var d = new Date();
    var n = d.getTimezoneOffset();
    return -1*n; // offset in minutes
}

function getBasedir()
{
    if(model()=="hrdps archive") {
        return("hrdps-map-archive/")
    } else {
        return("map-pngs/" + model() + "/latest/")
    }
}

function recordpageurl() {
    var str;
    var pos = map.getCenter()
    var zoom = map.getZoom()
    str = location.href.split("?")[0]
	+ "?param=" + document.getElementById("Param").value
	+ ",opacity=" + opacity + ",zoom=" + zoom + ",lat=" + pos.lat() + ",lon=" + pos.lng();
    if(windgram_checkbox.checked) {
	str = str + ",windgrams=true";
    } else {
	str = str + ",windgrams=false";
    }
    if (window.history.replaceState) {
	//prevents browser from storing history with each change:
	window.history.replaceState({}, "CanadaRASP", str);
    } else {
	//parent.location.hash = "hello"
	//console.log(window.location)
    }
}

function Cache(maxsize, generator) {
    this.maxsize = maxsize;
    this.activeelements = new Array(maxsize).fill(null);
    this.dict = {};
    this.index = 0;
    this.generator = generator;
}

Cache.prototype.get = function(key) {
    var result = this.dict[key];
    if(result) {
	//console.log("cache.get(" + key + ") -> " + result);
	return result;
    } else {
	return this.set(key, this.generator(key))
    }
}

Cache.prototype.purge = function () {
    // remove the element at the front of the array
    var key = this.activeelements.shift();
    if(key) {
	delete this.dict[key];
    }
}

Cache.prototype.set = function(key, value) {
    var oldvalue = this.dict[key];
    if(oldvalue) {
	this.dict[key] = value;
    } else {
	this.purge();
	this.dict[key] = value;
	this.activeelements.push(key);
    }
    //console.log("cache.set(" + key + "," + value + ") -> " + value);
    return value;
}

var myimagecache = new Cache(200,function(src) { var img = new Image(); img.src = src; return img;});

function getImage(filename) {
    return myimagecache.get(filename);
}

function lat_in_bounds (lat) {
    if(model() == "hrdps") {
	if(lat > 29 && lat < 70) {
	    return true;
	} else {
	    return false;
	}
    } else if(model() == "hrdps archive") {
        if(lat >= 48 && lat < 52) {
            return true;
        } else {
            return false;
        }
    } else {
	return true;
    }
}

function lng_in_bounds (lng) {
    if(model() == "hrdps") {
	if(lng>= -153 && lng < -43) {
	    return true;
	} else {
	    return false;
	}
    } else if(model() == "hrdps archive") {
        if(lng>=-124 && lng < -118) {
            return true;
        } else {
            return false;
        }
    } else { return true; }
}

function getTilesIn(bounds) {
    var ne = bounds.getNorthEast();
    var sw = bounds.getSouthWest();
    var result = [];
    var lngstart = sw.lng();
    var lngmid = ne.lng();
    //console.log("ne is " + ne + " sw is " + sw);
    if(ne.lng() < sw.lng()) { // wrap around the globe
	lngmid = 180;
    } else {
	lngmid = ne.lng();
    }
    var mystep = step();
    for (lat = mystep*Math.floor(sw.lat()/mystep) ; lat < mystep*Math.ceil(ne.lat()/mystep) ; lat += mystep) {
	if(lat_in_bounds(lat)) {
	    for (lng = mystep*Math.floor(lngstart/mystep) ; lng < mystep*Math.ceil(lngmid/mystep) ; lng += mystep) {
		if(lng_in_bounds(lng)) {
		    result.push({ lat: lat, lng: lng });
		}
	    }
	    if(ne.lng() < sw.lng()) {
		for(lng = -180 ; lng < ne.lng() ; lng += mystep) {
		    if(lng_in_bounds(lng)) {
			result.push({ lat: lat, lng: lng });
		    }
		}
	    }
	}
    }
    //console.log(result);
    return result;
}

var overlays = [];

function clearOverlaysnotIn (tiles) {
    var clearme = [];
    var zoom = zoomLevel();
    overlays.forEach(function(overlay) {
	//console.log("Looking for overlay " + overlay + " in tiles " + tiles);
	if(overlay.zoom == zoom && tiles.find(function(el) {return el == overlay.tile;})) {
	} else {
	    clearme.push(overlay);
	}
    });
        //console.log("Deleting " + clearme.length + " overlays");
    clearme.forEach(function(overlay) {
	overlay.overlay.setMap(null);
	overlays.splice( overlays.indexOf(overlay), 1);
    });
}

function model_parent() {
   var model;
   if(model_is_hrdps()) {
       model = "hrdps"
   } else {
       model = "gdps"
   }
   return model;
}

function realtilelat(lat) {
 return realtileinfo[model_parent()].lats[lat];
}
function realtilelng(lng) {
 return realtileinfo[model_parent()].lons[lng];
}

function addOverlay(tile, image, map, zoom, step) {
    var bounds = new google.maps.LatLngBounds(
        new google.maps.LatLng(  realtilelat(tile.lat), realtilelng(tile.lng) ),
        new google.maps.LatLng(  realtilelat(tile.lat+step) , realtilelng(tile.lng + step) )
    );
    // console.log("step is " + step + "Adding overlay at tile " + tile + " which is really " + bounds);
    overlays.push({tile: tile, zoom: zoom, overlay: new RASPoverlay(bounds, image, map)});
}

function addCanvasOverlay(tile, image, map, zoom, step) {
    var bounds = new google.maps.LatLngBounds(
        new google.maps.LatLng(  realtilelat(tile.lat), realtilelng(tile.lng) ),
        new google.maps.LatLng(  realtilelat(tile.lat+step) , realtilelng(tile.lng + step) )
    );
    // console.log("Adding canvas overlay to tile " +tile);
    overlays.push({tile: tile, zoom: zoom, overlay: new CanvasOverlay(bounds, image, map)});
}

function tileName(tile,splittime,param,step) {
  var lng2 = tile.lng + step;
  var lat2 = tile.lat + step;
  baseName = getBasedir() +"/" +splittime[0] + "/"+tile.lng + ":" + lng2 + ":"
      + tile.lat + ":" + lat2 + "/" + document.getElementById("Param").value
      + "_" + splittime[0] + "_" + splittime[1];
  return baseName;
}

function getSelectedValue(el) {
    var element = document.getElementById(el)
    var index = element.selectedIndex
    // var real_index = Math.min(index, element.options.length-1)
    // console.log('index was ' + index + ' but options had length ' + element.options.length + ' so we used ' + real_index);
    return element.options[index].value
}

function getSplitTime() {
    // returns ["yyyy-mm-dd" "0800"] in UTC
    if(model()=="hrdps archive") {
	var offset = getTimeZoneOffset();
	var year = getSelectedValue("yearpicker")
	var month = getSelectedValue("monthpicker")
	var day = getSelectedValue("daypicker")
	var hour = getSelectedValue("hourpicker")
	var D = new Date()
	D.setFullYear(year, month, day)
	D.setHours(hour/100 - offset/60)
	var splittime = [D.getFullYear() + "-" + padwithzero(D.getMonth()) + "-" + padwithzero(D.getDate()), padwithzero(D.getHours()) + "00"];
	// console.log(splittime)
	return splittime
    } else {
	var datetimeidx = document.getElementById("datetime").selectedIndex;
	var tValue  = document.getElementById("datetime").options[datetimeidx].value;
	var splittime = tValue.split(" ");
	return splittime;
    }
}

function getParam() {
 var param = document.getElementById("Param").value;
 return param;
}

function displayWind () {
  wind_checkbox = document.getElementById('wind_checkbox').checked;
  if(wind_checkbox) {
    var name = getParam();
    var index = name.indexOf("wind");
    if((index > -1) && (name.indexOf("vwind") == -1)) {
      return true;
    } else { return false; }}
  else { return false; }
}

function loadImages()
{
    var mapBounds = map.getBounds();

    if(!mapBounds) {
	return -1;
    }
    var str;
    var splittime = getSplitTime();
    var param = getParam();
    var tiles = getTilesIn(map.getBounds());
    var mystep = step();
    var zoom = zoomLevel();
    // console.log("map bounds are " + mapBounds);
    // console.log("tiles are " + tiles);
    // console.log("step is " + mystep);
    clearOverlaysnotIn(tiles);
    recordpageurl();
    drawWindVectors(tiles);
    tiles.forEach(function(tile) {
	var fullURL = tileName(tile,splittime,param,mystep)+ ".body.png";
	var image = getImage(fullURL);
 	// console.log("Trying to get image: " + fullURL);
	if(image) {
	    addOverlay(tile,image.src, map, zoom, mystep);
	}
    })
    // the head and foot are the same for all tiles (if I generate them right)
    headerfooterurlbase = getBasedir() + param + "_" + splittime[0];
    // console.log('Looking for header foot in ' + headerfooterurlbase)
    document.getElementById("theTitle").src = headerfooterurlbase + ".head.png" ;
//    console.log("Header should be in " + headerfooterurlbase + ".head.png")
    // document.getElementById("theTitle").style.maxHeight = "48px";
    // document.getElementById("theTitle").style.height = "10%";
    document.getElementById("theScale").src = headerfooterurlbase + ".foot.png" ;
 //       console.log("footer should be in " + headerfooterurlbase + ".foot.png")
    // document.getElementById("theScale").style.maxHeight = "48px";
    // document.getElementById("theScale").style.height = "10%";
}

var Event;

var infoArray = [];

function addInfo(location, txt, xsiz, ysiz)
{
  var infoOpts;

  // Putting the text in a DIV is a rather horrible cludge to get the scrollbars to be visible!
  // var txt1 = '<div style="height:' + ysiz + 'px, width:' + xsiz> + 'px>' + txt + '</div>';

  // If xsiz == ysiz == 0, use defaults
  if(xsiz == 0 && ysiz == 0){
    infoOpts = {
                 position: location,
                 map:      map,
                 content:  txt
    };
  }
  else {
    infoOpts = {
                 position: location,
                 map:      map,
                 content:  txt,
                 size:     new google.maps.Size(xsiz, ysiz)
    };
  }
  var infowindow = new google.maps.InfoWindow( infoOpts );
  infowindow.open(map);
  infoArray.push(infowindow);
}

function deleteInfo()
{
  if (infoArray) {
    for (i in infoArray) {
      infoArray[i].setMap(null);
    }
    infoArray.length = 0;
  }
}


function badImage(thisImage){
	thisImage.src = "noImage.png";
}

/**** Subclass Google Maps OverlayView() to add Opacity ****/

RASPoverlay.prototype = new google.maps.OverlayView();

function RASPoverlay(bounds, image, map)
{
  this.bounds_ = bounds ;
  this.map_ = map ;
    this.image_ = image ;
  //this.div = null;
  this.percentOpacity = opacity;

  this.setMap(map);

  // Is this IE, if so we need to use AlphaImageLoader
  var agent = navigator.userAgent.toLowerCase();
  if ((agent.indexOf("msie") > -1) && (agent.indexOf("opera") < 1)) {
    this.ie = true ;
  }
  else {
   this.ie = false ;
  }
}


RASPoverlay.prototype.onAdd = function()
{
    var div = document.createElement("div") ;
    
    div.style.borderStyle = "none";
    div.style.borderWidth = "0px";
    div.style.position = "absolute" ;
    div.setAttribute('id',this.id) ;
    var box = document.getElementById("zoomBox");

  box.appendChild(div);

  var img = document.createElement("img");
    img.src = this.image_;
    img.id = img.src;
    this.id = img.id;
  img.style.width = "100%";
  img.style.height = "100%";
  div.appendChild(img);

  this.div_ = div ;
  var panes = this.getPanes();
   panes.overlayLayer.appendChild(div);

  if( this.percentOpacity ) {
    this.setOpacity(this.percentOpacity) ;
  }

}

// Remove the main DIV from the map pane
RASPoverlay.prototype.onRemove = function()
{
    if(this.div_) {
	this.div_.parentNode.removeChild(this.div_);
	this.div_ = null;
    }
}


RASPoverlay.prototype.draw = function()
{
  var overlayProjection = this.getProjection();

  var sw = overlayProjection.fromLatLngToDivPixel(this.bounds_.getSouthWest());
  var ne = overlayProjection.fromLatLngToDivPixel(this.bounds_.getNorthEast());

  // Position our DIV using our bounds
  this.div_.style.left   = Math.min(sw.x,  ne.x) + "px";
  this.div_.style.top    = Math.min(ne.y,  sw.y) + "px";
  this.div_.style.width  = Math.abs(sw.x - ne.x) + "px";
  this.div_.style.height = Math.abs(ne.y - sw.y) + "px";

  if (this.ie)
  {
     this.div_.innerHTML = '<img src="' + this.image_ + '" width=' + this.div_.style.width + ' height=' + this.div_.style.height + ' >';
  }
}

/*
 * Opacity utility functions
 */
RASPoverlay.prototype.setOpacity=function(opacity)
{
    var c = opacity/100 ;
    var d = document.getElementById( this.id ) ;
    if(c<0) { c = 0 };
    if(c>1) {c = 1 };
    // console.log("RASP Setting opacity to " + opacity)
    //    console.log("id: " + this.id + " d: " + d + " RASPoverlay " + this + " setOpacity( " + opacity + ")");

    if (d) {
	if (typeof(d.style.filter) =='string') {  //IE
	    d.style.filter = 'alpha(opacity=' + opacity + ');';
	}
	if (typeof(d.style.KHTMLOpacity) == 'string' ) {
	    d.style.KHTMLOpacity = c ;
	}
	if (typeof(d.style.MozOpacity) == 'string') {
	    d.style.MozOpacity = c ;
	}
	if (typeof(d.style.opacity) == 'string') {
	    d.style.opacity = c ;
	}
    }
}

// RASPoverlay.prototype.getOpacity=function()
// {
//   var d = document.getElementById(this.id).parentNode;

//   return(d.style.opacity);
// }

function createOpacityControl(map, opacity) {
  var sliderImageUrl = "opacity-slider3d7.png";
  // Create main div to hold the control.
  var opacityDiv = document.createElement('DIV');
  opacityDiv.setAttribute("style", "margin:5px;overflow-x:hidden;overflow-y:hidden;background:url(opacity-slider3d7.png) no-repeat;width:71px;height:21px;cursor:pointer;");
//  opacityDiv.setAttribute("style", "margin:5px;overflow-x:hidden;overflow-y:hidden;background:url(" + sliderImageUrl + ") no-repeat;width:71px;height:21px;cursor:pointer;");

  // Create knob
//   var opacityKnobDiv = document.createElement('DIV');
//   opacityKnobDiv.id = "opacityKnobDiv";
//   opacityKnobDiv.setAttribute("style", "padding:0;margin:0;overflow-x:hidden;overflow-y:hidden;background:url(opacity-slider3d7.png) no-repeat -71px 0;width:14px;height:21px;");
// //  opacityKnobDiv.setAttribute("style", "padding:0;margin:0;overflow-x:hidden;overflow-y:hidden;background:url(" + sliderImageUrl + ") no-repeat -71px 0;width:14px;height:21px;");
//   opacityDiv.appendChild(opacityKnobDiv);

//   opacityCtrlKnob = new ExtDraggableObject(opacityKnobDiv, {
//     restrictY: true,
//     container: opacityDiv
//   });

//   google.maps.event.addListener(opacityCtrlKnob, "dragend", function () {
//       setOpacity(opacityCtrlKnob.valueX());
//   });

  // google.maps.event.addDomListener(opacityDiv, "click", function (e) {
  //   var left = findPosLeft(this);
  //   opacity = e.pageX - left - 5; // - 5 as we're using a margin of 5px on the div
  //   opacityCtrlKnob.setValueX(opacity);
  //   setOpacity(opacity);
  // });
  // map.controls[google.maps.ControlPosition.TOP_RIGHT].push(opacityDiv);

  // Set initial value
  var initialValue = OPACITY_MAX_PIXELS * (opacity/ 100);
  // opacityCtrlKnob.setValueX(initialValue);
  setOpacity(initialValue);
}

function setOpacity(pixelX) {
    // Range = 0 to OPACITY_MAX_PIXELS
    opacity = (100 / OPACITY_MAX_PIXELS) * pixelX;
    opacity = Math.round(opacity);
    opacity = clipto0to100(opacity);
    recordpageurl()
    overlays.forEach(function (overlay) {
    	overlay.overlay.setOpacity(opacity);
    })
}

function findPosLeft(obj) {
  var curleft = 0;
  if (obj.offsetParent) {
    do {
      curleft += obj.offsetLeft;
    } while (obj = obj.offsetParent);
    return curleft;
  }
  return undefined;
}
function updateLL(e) {
    var latDisplay = document.getElementById("latBox");
    var lonDisplay = document.getElementById("lonBox");
    latDisplay.value = e.latLng.lat().toFixed(6);
    lonDisplay.value = e.latLng.lng().toFixed(6);
  }

CanvasOverlay.prototype = new google.maps.OverlayView();

function CanvasOverlay(bounds, image, map) {
            // Initialize all properties.
            this.bounds_ = bounds;
            this.image_ = image;
            this.map_ = map;
            this.div_ = null;
            this.setMap(map);
        }
CanvasOverlay.prototype.setOpacity=function(opacity)
{
    return;
    /* var c = opacity/100 ;
    if(c<0) { c = 0 };
    if(c>1) {c = 1 };
    var canvas = this.image_;
    var width = canvas.width;
    var height = canvas.height;
    var context = canvas.getContext('2d');
    var image = context.getImageData(0,0,width,height);
    c = 0;
    for(var x = 0 ; x < width ; x++) {
       for(var y = 0 ; y < height ; y++) {
        image[4*(x + y*width)+3] = 255*c;    
       };
    };
    context.putImageData(image,0,0);
    console.log("opacity is " + 255*c); */
}


CanvasOverlay.prototype.onAdd = function() {
            var div = document.createElement("div")
            div.style.borderStyle = 'none';
            div.style.borderWidth = '0px';
            div.style.position = 'absolute';
 
            // Load the inline svg element and attach it to the div.
            var svg = this.image_;
            svg.style.width = '100%';
            svg.style.height = '100%';
 
            div.appendChild(svg);
            this.div_ = div;
            // Add the element to the "overlayLayer" pane.
            var panes = this.getPanes();
            panes.overlayLayer.appendChild(div);
        };
 
CanvasOverlay.prototype.draw = function() {
            // We use the south-west and north-east
            // coordinates of the overlay to peg it to the correct position and size.
            // To do this, we need to retrieve the projection from the overlay.
            var overlayProjection = this.getProjection();
 
            // Retrieve the south-west and north-east coordinates of this overlay
            // in LatLngs and convert them to pixel coordinates.
            // We'll use these coordinates to resize the div.
            var sw = overlayProjection.fromLatLngToDivPixel(this.bounds_.getSouthWest());
            var ne = overlayProjection.fromLatLngToDivPixel(this.bounds_.getNorthEast());
 
            // Resize the image's div to fit the indicated dimensions.
            var div = this.div_;
            div.style.left = sw.x + 'px';
            div.style.top = ne.y + 'px';
            div.style.width = (ne.x - sw.x) + 'px';
            div.style.height = (sw.y - ne.y) + 'px';
        };

CanvasOverlay.prototype.onRemove = function () {
    if(this.div_) {
	this.div_.parentNode.removeChild(this.div_);
	this.div_ = null;
    }
}

function canvas_arrow(canvas, fromx, fromy, length, angle,linewidth){
    var headlen = length / 3;   // length of head in pixels
    var startx = fromx - length/2*Math.cos(angle);
    var starty = fromy - length/2*Math.sin(angle);
    var tox = fromx + length/2*Math.cos(angle);
    var toy = fromy + length/2*Math.sin(angle);
    var context = canvas.getContext('2d');
    context.beginPath();
    context.lineWidth = linewidth;
    context.strokeStyle="rgb(255,0,255)";
    context.moveTo(startx, starty);
    context.lineTo(tox, toy);
    context.lineTo(tox-headlen*Math.cos(angle-Math.PI/6),toy-headlen*Math.sin(angle-Math.PI/6));
    context.moveTo(tox, toy);
    context.lineTo(tox-headlen*Math.cos(angle+Math.PI/6),toy-headlen*Math.sin(angle+Math.PI/6));
    context.stroke();
}

function drawWindCanvas(inputcanvas,outputcanvas, zoom) {
    var width = inputcanvas.width;
    var height = inputcanvas.height;
    var scale;
    var step;
    var linewidth;
    if(model_is_hrdps()) {
      if(zoom == "high") { scale = 10; step = 1; linewidth = 1;}
      else if(zoom == "medium") { scale = 20; step = 4; linewidth = 4;}
      else { scale = 30 ; step = 15 ; linewidth = 7;}
    } else if (model() == "gdps") {
      if(zoom == "high" || zoom == "medium") {
        scale = 10; step = 1; linewidth = 1;
      } else if(zoom=="low") { scale = 30; step = 3; linewidth = 3;}
	else {scale = 50; step = 5; linewidth = 5; }
    }
    //console.log("input canvas is " + width + "x" + height);
    var inputcontext = inputcanvas.getContext('2d');
    var angles = inputcontext.getImageData(0,0,width,height).data;
    outputcanvas.height = height*scale / step;
    outputcanvas.width = width*scale / step;
    var halfscale = scale / 2;
    // console.log("Drawing at scale = " + scale + " step = " + step + " for zoom " + zoom);
    for (var y = 0; y<height ; y+=step){
        for (var x = 0; x<width ;  x+=step){
            var raw = angles[4*(x + y*width)];
            if(raw > 0) {
                var angle = ((raw-1)/37.0)-Math.PI;
                canvas_arrow(outputcanvas, halfscale+x*scale/step, halfscale+y*scale/step, scale,angle, linewidth);
            }
        }
    }
}

function zoomLevel () {
  var zoom = map.getZoom();
    if(zoom > 8) { return "high"; } else if( zoom >= 6) { return "medium"; } else if (zoom > 3) {return "low";} else { return "verylow";}
}

function drawWindVectors(tiles,outputcanvas) {
    if(displayWind()) {
	var param = getParam();
	var zoom = zoomLevel();
	var mystep = step();
	var splittime = getSplitTime();
	tiles.forEach(function(tile) {
	    var basictile = tileName(tile,splittime,param,mystep);
	    var image = new Image();
	    var outputcanvas = document.createElement("canvas");
	    addCanvasOverlay(tile,outputcanvas,map,zoom,mystep);
	    image.onload = function() {
		var inputcanvas = document.createElement("canvas");
		//console.log("Got an image " + this.width + "x" + this.height);
		inputcanvas.width = this.width;
		inputcanvas.height = this.height;
		inputcanvas.getContext('2d').drawImage(this,0,0);
		drawWindCanvas(inputcanvas,  outputcanvas, zoom);
	    };
	    image.src = basictile + ".vector.png";
	    //console.log("Trying to load " + image.src);
	});
    }
}

