# HDRU_dashboard

## Description

This is an R package for internal use at MLW to run the High Dependency Research Unit data dashboard.



## Installation

### From GitHub (development version)

``` r
# install.packages("devtools")
devtools::install_github("mlw-stats/HDRU")
```
### Note

This is currently a private repository, so for the `install_github()` command to work, you will need to generate an access token first (you can do this by going [here](https://github.com/settings/tokens)).

## Example

You will need to

* Load the admission data from the data portal (argument admissionData).
* Load the daily data from the data portal (argument dailyData).
* Set the weeks for which you want summaries (argument curWeek).
* Specify a file name (with .html extension) that will contain the dashboard report.

The example below assumes you've got the 2 input data files sitting in your working directory.

``` r
library(HDRU)

admissionData<-read.csv("hdru_admission_raw.csv")
dailyData<-read.csv("hdru_daily_raw.csv")
curPeriod<-dmy("06/07/2020","13/07/2020","20/07/2020","27/07/2020")

HDRUdashboard(admissionData=admissionData,dailyData=dailyData,curPeriod=curPeriod,unit="week",file.name="HDRUdashboard.html")
```

Once you have run the `HDRUdashboard` function, open the specified output html file.

