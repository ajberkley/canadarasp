package com.databerries.timezone;

public class Country {
    private final String countryCode;
    private final String administrationCode;

    Country(String countryCode, String administrationCode) {
        this.countryCode = countryCode;
        this.administrationCode = administrationCode;
    }

    public String countryCode() {
        return countryCode;
    }

    public String administrationCode() {
        return administrationCode;
    }

    @Override
    public String toString() {
        return "Country{" +
                "countryCode='" + countryCode + '\'' +
                ", administrationCode='" + administrationCode + '\'' +
                '}';
    }
}
