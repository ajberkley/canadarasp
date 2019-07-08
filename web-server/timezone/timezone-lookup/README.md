# TimeZoneLookup

A high performance timezone lookup library taking as parameters a latitude and a longitude and returning a timezone as string (e.g. Europe/Paris). 

The goal of the library is to retrieve as fast as possible a timezone into a dataset by giving a latitude and a longitude. This library gives a timezone from a latitude and a longitude in about 10 ns. Most timezone lookup libraries that we benchmarked were at least two order of magnitude slower (>1ms).


# How it works

## Dataset 

The key resource of the library is the dataset. Let's see how it was generated (notice that the library uses the generated dataset directly):  

```
 ____________________________      _______________      ________________      _____________
|                            |    |               |    |                |    |             |
| lat-long-points-generator  | -> | apply-tzwhere | -> | kdtree-islands | -> | reduce file |
|____________________________|    |_______________|    |________________|    |_____________|
```

### [lat-long-points-generator](https://github.com/databerries/lat-long-points-generator)
Generates a csv with lat/long points that cover the entire earth at a regular step in degrees. Sample of data (latitude, longitude, index): 

```
51.15,2.2,1
51.15,2.25,2
51.15,2.3,3
51.15,2.35,4
51.15,2.4,5
51.15,2.45,6
51.15,2.5,7
51.2,2.35,8
51.2,2.4,9
51.2,2.45,10
```
The dataset generated at this stage contains 25'920'000 points with an accuracy of 0.005 degree.

### [apply-tzwhere](https://github.com/databerries/apply-tzwhere)

Associates each lat/long point with the corresponding timezone using the library [java-tzwhere](https://github.com/sensoranalytics/java-tzwhere/).

This process step generates a new csv. Here is a sample of data (latitude, longitude, string timezone, index):

```
51.15,2.2,Europe/Paris,1
51.15,2.25,Europe/Paris,2
51.15,2.3,Europe/Paris,3
51.15,2.35,Europe/Paris,4
51.15,2.4,Europe/Paris,5
51.15,2.45,Europe/Paris,6
51.15,2.5,Europe/Paris,7
51.2,2.35,Europe/Paris,8
51.2,2.4,Europe/Paris,9
51.2,2.45,Europe/Paris,10
```

### [kdtree-islands](https://github.com/databerries/neareast-tz)

At this step there are timezones only for points on land. We decided to compute timezones for the points in the seas near the coasts by 20km. To do so, we used an implementation of a [KdTree](https://github.com/phishman3579/java-algorithms-implementation/blob/master/src/com/jwetherell/algorithms/data_structures/KdTree.java).

### reduce file

At this step the dataset is already usable but quite heavy. To reduce it we applied transformations on it:
* use timezone ids instead of timezone strings. Keep a mapping file to do the conversion. 
* remove the index (the index was useful to process steps in parallel and reorder the result)
* remove latitude and the longitude, because only the position of the points in the file matters (see later the hash function).

Here a sample:
```
328
328
328
328
```

## The hash function 
The dataset is loaded into an array in the order it is read in the csv. Each index of the array correspond to a specific hash calculted from a latitude and a longitude. The array contains timezone ids.

```java
  private int hash(double latitude, double longitude) {
    int iLat = (int) Math.floor((latitude + 90) / STEP);
    int iLng = (int) Math.floor((longitude + 180) / STEP);
    return iLng + (iLat * NB_POINTS_LONGITUDE);
  }
```

# How to use it 

```
compile 'com.databerries.timezone:timezone-lookup:1.0.0'
```

```java
// instanciating the library takes ~1.5sec because it loads the dataset.
TimeZoneLookup tz = new TimeZoneLookup();
double latitude = 48.3904;
double longitude = -4.4861;
// fast lookup in the dataset, takes few nanosecondes.
ZoneId result = tz.getZoneId(latitude, longitude);
System.out.println(result);
```

* result

```
Europe/Paris
```

# TODO
- Include international seas timezones
