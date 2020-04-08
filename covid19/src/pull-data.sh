#!/bin/sh

covid_url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
dead_url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
recover_url="https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
google_mobility_url="https://github.com/nacnudus/google-location-coronavirus/raw/master/2020-03-29.tsv"

echo "* Downloading time_series_covid19_confirmed_global.csv"
wget ${covid_url} -q -O ../data/time_series_covid19_confirmed_global.csv
echo "* Done"

echo "* Downloading time_series_covid19_deaths_global.csv"
wget ${dead_url} -q -O ../data/time_series_covid19_deaths_global.csv
echo "* Done"

echo "* Downloading time_series_covid19_recovered_global.csv"
wget ${recover_url} -q -O ../data/time_series_covid19_recovered_global.csv
echo "* Done"

echo "* Downloading google_mobility.tsv"
wget ${google_mobility_url} -q -O ../data/google_mobility.tsv
echo "* Done"
