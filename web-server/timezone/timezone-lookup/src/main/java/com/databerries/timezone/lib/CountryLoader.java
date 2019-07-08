package com.databerries.timezone;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class CountryLoader {

    private static final int TIMEZONE_COLUMN_NUMBER = 2;
    private static final int COUNTRY_CODE_COLUMN_NUMBER = 0;
    private static final int ADMINISTRATION_CODE_COLUMN_NUMBER = 1;
    private static final int CONTINENT_COLUMN_NUMBER = 3;

    private static final String CSV_SEPARATOR = ",";
    private static final int SKIP_CSV_HEADER = 1;

    public static CountryLookup createWithDefaultDataset() {
        List<String> datasource = readFromResources("/country_lookup_dataset.csv");

        Map<String, Country> timezoneToCountryCode = datasource.stream().skip(SKIP_CSV_HEADER)
                .map(line -> line.split(CSV_SEPARATOR))
                .collect(Collectors.toMap(
                        element -> element[TIMEZONE_COLUMN_NUMBER],
                        element -> new Country(element[COUNTRY_CODE_COLUMN_NUMBER], element[ADMINISTRATION_CODE_COLUMN_NUMBER])
                        )
                );

        Set<String> countriesInEurope = datasource.stream().skip(SKIP_CSV_HEADER)
                .map(line -> line.split(CSV_SEPARATOR, 5))
                .filter(elements -> isEuropeanCountry(elements[CONTINENT_COLUMN_NUMBER]))
                .map(element -> element[COUNTRY_CODE_COLUMN_NUMBER])
                .collect(Collectors.toSet());
        return new CountryLookup(countriesInEurope, timezoneToCountryCode);
    }

    private static List<String> readFromResources(String filePath) {
        try (InputStream stream = CountryLoader.class.getResourceAsStream(filePath);
             BufferedReader br = new BufferedReader(new InputStreamReader(stream, Charset.defaultCharset()))) {
            return br.lines().collect(Collectors.toList());
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static boolean isEuropeanCountry(String element) {
        return "EU".equalsIgnoreCase(element);
    }
}
