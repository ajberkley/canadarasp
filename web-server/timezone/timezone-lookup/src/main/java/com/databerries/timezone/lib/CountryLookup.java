package com.databerries.timezone;

import java.util.Map;
import java.util.Set;

public class CountryLookup {
    private final Set<String> countriesInEurope;
    private final Map<String, Country> timezoneToCountry;

    CountryLookup(Set<String> countriesInEurope, Map<String, Country> timezoneToCountry) {
        this.countriesInEurope = countriesInEurope;
        this.timezoneToCountry = timezoneToCountry;
    }

    public Country getCountry(String timezone) {
        if (!timezoneToCountry.containsKey(timezone)) {
            return null;
        }
        return timezoneToCountry.get(timezone);
    }

    /**
     * @param timezone
     * @return administration code otherwise country code
     */
    public String getAdministrationCode(String timezone) {
        if (!timezoneToCountry.containsKey(timezone)) {
            return null;
        }
        Country country = timezoneToCountry.get(timezone);
        if (country.administrationCode() == null || country.administrationCode().isEmpty()) {
            return country.countryCode();
        }
        return country.administrationCode();
    }

    public boolean isTimezoneInEU(String timezone) {
        Country countryCode = getCountry(timezone);
        return countryCode != null && countriesInEurope.contains(countryCode.countryCode());
    }
}
