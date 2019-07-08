package com.databerries.timezone;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.time.ZoneId;

/**
 * Get a time zone (e.g Europe/Paris) from a latitude  and longitude
 */
public class TimeZoneLookup {
  // the step is the level of granularity in degrees of the dataset
  private static final double STEP = 0.05;
  // nb of longitude points in the dataset. This value is used in the hash function.
  private static final int NB_POINTS_LONGITUDE = 7200;
  // total number of points in the dataset (depends of the step)
  private static final int TOTAl_NB_POINTS = 25920000;
  // array populated from the dataset:
  // value = a short represented a time zone id (it's an internal id).
  // index = computed from a hash function taking a latitude and a longitude.
  private final short[] indexToTimeZoneId = new short[TOTAl_NB_POINTS];
  // mapping between the internal time zone id and a string time zone
  private final String[] timeZoneIdToString = new String[398];

  // dataset file and time zone mapping file
  private static final String TIMEZONE_MAPPING_FILE = "/timezone_id.csv";
  private static final String TIMEZONE_DATASET = "/dataset.csv";

  /**
   * TimeZoneLookup constructor:
   * Load the internal dataset and the time zone mapping from the config files.
   * @throws UncheckedIOException if there is an issue reading the files
   */
  public TimeZoneLookup() {
    initTimeZoneIdToValue();
    loadData();
  }

  /**
   * Get java.time.ZoneId (time zone) from the given latitude and longitude
   * @param latitude between -90 (included) and 90 (excluded)
   * @param longitude between -180 (included) and 180 (excluded)
   * @return java.time.ZoneId
   * @throws IllegalArgumentException if the latitude or the longitude is out of range
   */
  public ZoneId getZoneId(double latitude, double longitude) {

    if(latitude >= 90 || latitude < -90 || longitude >= 180 || longitude < -180) {
      throw  new IllegalArgumentException("Latitude must be between -90 (included) and 90 (excluded) " +
              "and longitude must be between -180 (included) and 180 (excluded)");
    }

    int index = hash(latitude, longitude);
    short timezoneId = indexToTimeZoneId[index];

    if (timezoneId == -1) {
      return null;
    }
    return ZoneId.of(timeZoneIdToString[timezoneId]);
  }

  /**
   * Hash method used to get the index in the dataset array from the latitude and the longitude.
   * @param latitude
   * @param longitude
   * @return index in the dataset
   */
  private int hash(double latitude, double longitude) {
    int iLat = (int) Math.floor((latitude + 90) / STEP);
    int iLng = (int) Math.floor((longitude + 180) / STEP);
    return iLng + (iLat * NB_POINTS_LONGITUDE);
  }

  /**
   * Load dataset from csv file
   * @throws UncheckedIOException if there is an issue reading the file
   */
  private void loadData() {
    try (InputStream datasetStream = TimeZoneLookup.class.getResourceAsStream(TIMEZONE_DATASET);
         BufferedReader br = new BufferedReader(new InputStreamReader(datasetStream))) {

      final int[] index = {0};
      br.lines().forEach((line)-> indexToTimeZoneId[index[0]++] = Short.valueOf(line));
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * Load mapping csv file: time zone string / time zone id
   * @throws UncheckedIOException if there is an issue reading the file
   */
  private void initTimeZoneIdToValue() {
    try (InputStream mappingTimeZonesStream = TimeZoneLookup.class.getResourceAsStream(TIMEZONE_MAPPING_FILE);
         BufferedReader br = new BufferedReader(new InputStreamReader(mappingTimeZonesStream)))
    {
      br.lines().forEach((line)-> {
        String[] lineSplit = line.split(",");
        String timeZoneString = lineSplit[0];
        int timeZoneId = Integer.valueOf(lineSplit[1]);
        timeZoneIdToString[timeZoneId] = timeZoneString;
      });
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
