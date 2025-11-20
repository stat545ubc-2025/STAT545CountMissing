
<!-- README.md is generated from README.Rmd. Please edit that file -->

# STAT545CountMissing package

<!-- badges: start -->

<!-- badges: end -->

The goal of STAT545CountMissing is to count the number of missing values
by group. This package contains one function named
`count_all_missing_by_group`, where user can count the number of missing
values with the option of displaying by group.

## Installation

You can install the development version of STAT545CountMissing from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("stat545ubc-2025/STAT545CountMissing")
```

## Example

### Example 1

This is a basic example which shows you how to solve a common problem.
We use the `airquality` dataset and count the number of missing values
group by `Month`.

``` r
library(STAT545CountMissing)
library(palmerpenguins)
#> 
#> Attaching package: 'palmerpenguins'
#> The following objects are masked from 'package:datasets':
#> 
#>     penguins, penguins_raw
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.1     ✔ stringr   1.5.2
#> ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.1.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
count_all_missing_by_group(airquality, Month)
#> # A tibble: 5 × 6
#>   Month Ozone Solar.R  Wind  Temp   Day
#>   <int> <int>   <int> <int> <int> <int>
#> 1     5     5       4     0     0     0
#> 2     6    21       0     0     0     0
#> 3     7     5       0     0     0     0
#> 4     8     5       3     0     0     0
#> 5     9     1       0     0     0     0
```

### Example 2

The following example shows how pipe works with this function. All
settings are the same as above.

``` r
airquality |> count_all_missing_by_group(Month)
#> # A tibble: 5 × 6
#>   Month Ozone Solar.R  Wind  Temp   Day
#>   <int> <int>   <int> <int> <int> <int>
#> 1     5     5       4     0     0     0
#> 2     6    21       0     0     0     0
#> 3     7     5       0     0     0     0
#> 4     8     5       3     0     0     0
#> 5     9     1       0     0     0     0
```

### Example 3

The final example shows the role of `.groups`, which let user to keep
the output grouped by the grouping columns.

``` r
count_all_missing_by_group(airquality, Month, .groups = "keep")
#> # A tibble: 5 × 6
#> # Groups:   Month [5]
#>   Month Ozone Solar.R  Wind  Temp   Day
#>   <int> <int>   <int> <int> <int> <int>
#> 1     5     5       4     0     0     0
#> 2     6    21       0     0     0     0
#> 3     7     5       0     0     0     0
#> 4     8     5       3     0     0     0
#> 5     9     1       0     0     0     0
```
