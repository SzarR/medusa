
# medusa <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

<!-- badges: end -->

Medusa was developed to automate the data cleaning workflow of
membership analytic reports. Traditionally, the process of data cleaning
was performed manually in Excel, leading to an inefficient and
cumbersome process. The end result of this effort was a semi-annual
membership report with visualizations for key demographic variables that
describes the SIOP membership population. The purpose of the medusa
package was to streamline the ingestion, data cleaning and visualization
processes associated with this project. In the current iteration of
medusa, the data cleaning and table generating steps have been
automated.

## Installation

Medusa is currently available as a developmental package from this
repository. The instructions below will walk you through installing it
on your local machine. As a side note, it relies heavily on the
**tidyverse** package ecosystem to clean and tidy data. You will be
prompted to install all required packages if they are not detected in
your current R environment.

``` r
# install.packages("devtools")
#devtools::install_github("SzarR/medusa")
```

## Current Counts

Current counts after data cleaning for each of the years equate to:

-   2017: 9,905
-   2018: 10,079
-   2019: 9,842
-   2020: 9,660
-   2021: 9,100

## Example

There are two types of functions within *medusa*. The first, are the
individual step functions. Each such step function cleans a specific
part of the demographic dataset. An array of step functions are
associated within the second type of function, the wrapper function.
There are three wrapper functions:

-   make\_demo\_data( ) = Runs a series of 14 functions to clean raw
    demographics data
-   make\_dues( ) = Runs a series of 2 functions to clean raw dues data
-   make\_final\_data( ) = Takes the output of the above to create the
    final data table

These wrapper functions are the primary means of interacting with the R
package. A typical workflow would be:

First, we read in the xlsx datasets that house the data we’d like to
clean.

``` r
#library(medusa)
#library(readxl)
#library(tidyverse)

#demo_raw <- read_xls(
#  path = "~/R-lang/MAS_EB_Reporting/data/Demographics 7-6-20.xls")

#dues_0620 <- read_xlsx(
#  path = "~/R-lang/MAS_EB_Reporting/data/Dues June 2020.xlsx",
#  sheet = "Sheet1")

#dues_0520 <- read_xlsx(
#  path = "~/R-lang/MAS_EB_Reporting/data/May 2020 Dues.xlsx",
#  sheet = "Sheet1")
```

Once the raw data has been loaded into your local R environment, we
begin to clean it. The line below takes the original demographic tibble
and cleans all relevant columns, producing a cleaned tibble as its
output:

``` r
#demo <- make_demo_data(df = demo_raw)
```

The next wrapper function takes any number of tibbles as an argument,
along with a *year* argument to specify which SIOP year analytics are
being performed on. Afterward, standard data cleaning is then performed,
resulting in a cleaned dues tibble.

``` r
#dues <- make_dues(dues_0520, dues_0620, ..., year = 2020)
```

Finally, once the demo and dues files have been cleaned accordingly, we
can run the merge to obtain the resultant tibble of active SIOP members
for the year specified.

``` r
#data_final <- make_final_data(demo = demo, dues = dues, staff=staff)
```

This final dataset then serves as the baseline for all data analytics.
