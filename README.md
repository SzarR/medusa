
# medusa

<!-- badges: start -->
<!-- badges: end -->

Medusa was developed for the express purpose of automating the workflow
surrounding SIOP executive board membership analytic reports.
Traditionally, the data involved in producing these reports was manually
recoded, compiled and aggregated into a semi-annual report. The workflow
present in the functions outlined within this package seek to automate
the cleaning of the files required for analytics. The purpose of the
package is to provide a single and clean data table that contains the
appropriate member for a given SIOP year that the r user specifies. At a
point in the future, the visualizations will be automated, too. This
remains a future vision, at the present time.

## Installation

Medusa is only available as a developmental version from my personal
github repository. There are no plans to upload medusa to CRAN, simply
because the package users are only a select few of people who sit on the
committe as a volunteer.

``` r
# install.packages("devtools")
devtools::install_github("SzarR/medusa")
```

## Example

There are two seperately ‘tiered’ functions within medusa. All functions
with the prefix **step\_** are not meant to be executed directly by the
user. Instead, three wrapper functions help automate the workflow
entirely. A typical workflow would look something like this:

First, we read in the xlsx datasets that house the data we are looking
to clean.

``` r
library(medusa)

#demo_raw <- read_xls(
#  path = "~/R-lang/MAS_EB_Reporting/data/Demographics 7-6-20.xls")

#dues_0620 <- read_xlsx(
#  path = "~/R-lang/MAS_EB_Reporting/data/Dues June 2020.xlsx",
#  sheet = "Sheet1")

#dues_0520 <- read_xlsx(
#  path = "~/R-lang/MAS_EB_Reporting/data/May 2020 Dues.xlsx",
#  sheet = "Sheet1")
```

Once the raw data has been read into your local R environment, it is
time to clean and parse the demographic data. The line below creates a
demographic file that is ready to be merged with dues information:

``` r
#demo <- make_demo_data(df = demo_raw)
```

Next, we combine multiple dues files into a single file to then merge
with the demographics data, this happens in the code below:

``` r
#dues <- make_dues_data(df = demo_raw, year = 2020)
```
