var Loaded;  // is this image cached?
var Pics; // cache for overlay images

function getQueryStrings() {
  var assoc  = {};
  var decode = function (s) { return decodeURIComponent(s.replace(/\+/g, " ")); };
  var queryString = location.search.substring(1);
  var keyValues = queryString.split('&');
  for(var i in keyValues) {
    var key = keyValues[i].split('=');
    if (key.length > 1) {
      assoc[decode(key[0])] = decode(key[1]);
    }
  }
  return assoc;
}

function UpdateQueryString(updatedatequery) {
    var region=document.getElementById("region").selectedIndex;
    var loc=document.getElementById("location").selectedIndex;
    var plotType=document.getElementById("plotType").selectedIndex;
    var Year=document.getElementById("Year").value;
    var Month=document.getElementById("Month").value;
    var Day=document.getElementById("Day").value;
    var Model=document.getElementById("Model").value;
    if (history.pushState) {
	if (updatedatequery) {
	    var newurl = window.location.protocol + "//" + window.location.host + window.location.pathname + '?region='+region+'&location='+loc+'&plotType='+plotType+'&Year='+Year+'&Month='+Month+'&Day='+Day+'&Model='+Model;
	    window.history.pushState({path:newurl},'',newurl);
	} else {
	    var newurl = window.location.protocol + "//" + window.location.host + window.location.pathname + '?region='+region+'&location='+loc+'&plotType='+plotType;
	    var newurlwithdate = window.location.protocol + "//" + window.location.host + window.location.pathname + '?region='+region+'&location='+loc+'&plotType='+plotType+'&Year='+Year+'&Month='+Month+'&Day='+Day+'&Model='+Model;
	    var a = document.createElement('a');
	    a.setAttribute('href',newurlwithdate);
	    a.innerHTML = "Link to this windgram";
	    var mycurrentsettings = document.getElementById("currentsettings");
	    if(mycurrentsettings.firstChild) {
		mycurrentsettings.replaceChild(a,mycurrentsettings.childNodes[0]);
	    } else { currentsettings.appendChild(a); }
	    window.history.pushState({path:newurl},'',newurl);
	}
  }
}

function padwithzero(string) {
    string = string+"";
    if (string.length < 2) {
	return("0" + string + "");
    } else {
	return(string);
    }
}

function showWindgram(updatedatequery){
    var wIdx=document.getElementById("location").value
    if (!Loaded[wIdx]) {
	var yIdx = document.getElementById("Year").selectedIndex;
	var year = document.getElementById("Year").options[yIdx].value;
	var mIdx = document.getElementById("Month").selectedIndex;
	var month = padwithzero(document.getElementById("Month").options[mIdx].value);
	var dIdx = document.getElementById("Day").selectedIndex;
	var model = document.getElementById("Model").value;
	if (dIdx == -1) {return;}
	var day = padwithzero(document.getElementById("Day").options[dIdx].value);
	var dateStr = "/" + year + "-" + month + "-" + day;
	var duration=document.getElementById("plotType").value;
	Pics[wIdx].src = "/windgrams-data/"+duration+dateStr+"/"+model+"windgram"+wIdx+".png";
	Loaded[wIdx] = true;
    }
    console.log(Pics[wIdx].src);
    document.getElementById('wframe').src = Pics[wIdx].src;
    UpdateQueryString(updatedatequery);
}

function clearLoadedCache () {
  for(i = 0; i < locations.length; i++){
      Loaded[i] = false;
      Pics[i] = new Image();
  }
}

function updateDuration () {
    clearLoadedCache();
    showWindgram(false);
}

function updateDate (dayid) {
    var targetYear = document.getElementById("Year").options[document.getElementById("Year").selectedIndex].value;
    var targetMonth = document.getElementById("Month").options[document.getElementById("Month").selectedIndex].value;
    var thisDay = new Date().getDate();
    var Day = document.getElementById("Day");
    var dayName   = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
    var dayofmonth = new Date();
    dayofmonth.setFullYear(targetYear);
    dayofmonth.setDate(1);
    dayofmonth.setMonth(targetMonth-1);
    var i = 0;
    while(dayofmonth.getMonth()==targetMonth-1) {
      Day.options[i++] = new Option(dayofmonth.getDate() + ' ' + dayName[dayofmonth.getDay()] , dayofmonth.getDate());
      dayofmonth.setDate(dayofmonth.getDate()+1);
	if((dayid && dayid==i) || (!dayid && i==thisDay)) {
	    Day.options[i-1].selected = true;
	}
    }
    clearLoadedCache();
}

function updateRegion(){
  var region=document.getElementById("region").value;
  while(document.getElementById("location").options.length>0){
    document.getElementById("location").options[0] = null;
  }
  var j = 0;
  for (i=0;i<locations.length;i++){
    if(locations[i][0]==region){
      document.getElementById("location").options[j++] = new Option(locations[i][1],i)
    }
  }
  document.getElementById("location").index = 0;
  document.getElementById("location").options[0].selected=true;
  showWindgram(false);
}

function badImage(thisImage)
{
    //console.log(thisImage);
	//thisImage.onerror="";
	thisImage.src = "imageMissing.jpg";
}

function init() {
    // Disable Scrolling
    document.body.style.overflow = "auto";

    Loaded = new Array();
    Pics = new Array();

    // populate the windgram select box
    var year = document.getElementById("Year");
    var month = document.getElementById("Month");

    var monthName = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
    var thisYear = new Date().getFullYear();
    var thisMonth = new Date().getMonth();
    //var thisDay = new Date().getDate();
    year.options[0] = new Option(thisYear-1 + '',thisYear-1);
    year.options[1] = new Option(thisYear + '',thisYear);
    year.options[2] = new Option(thisYear+1 + '',thisYear+1);
    year.options[1].selected = true;
    for (var i=0;i<12;i++) {
	month.options[i] = new Option(monthName[i],i+1);
	if(i==thisMonth) {
	    month.options[i].selected=true;
	}
    }
    clearLoadedCache();
    
    var qs = getQueryStrings();
    var query = qs["region"];
    if (query){
	document.getElementById("region").selectedIndex=query;
    }
    updateRegion();
    query = qs["location"];
    if (query){
	document.getElementById("location").selectedIndex=query;
    }
    query = qs["plotType"];
    if (query){
	document.getElementById("plotType").selectedIndex=query;
    }
    query = qs["Year"];
    if (query){
	document.getElementById("Year").value = query;
    }
    query = qs["Month"];
    if (query){
	document.getElementById("Month").value = query;
    }
    query = qs["Model"];
    if (query){
	document.getElementById("Model").value = query;
    }
    query = qs["Day"];
    if (query){
	updateDate(query);
	showWindgram(false);
    } else {
	updateDate(false);
	showWindgram(false);
    }
}
