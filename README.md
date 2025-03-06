
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loco

<!-- badges: start -->

<!-- badges: end -->

This package implements the calculation of local correlation (LoCo)
similarity scores for a pair of time series, proposed by Papadimitriou,
S., Sun, J., and Yu, P.S., in the paper [“Local Correlation Tracking in
Time Series”](https://doi.org/10.1109/ICDM.2006.99).

## Installation

You can install this package using:

``` r
# install.packages("remotes")
remotes::install_github("adamoshen/loco")
```

## Basic usage

``` r
library(tidyverse)
library(patchwork)
library(loco)
```

Suppose we are interested in the local correlations of average daily
temperature readings at DTW and LGA airports. A preview of the data:

``` r
daily_temperature %>%
  group_by(origin) %>%
  slice_head(n = 6) %>%
  group_split()
#> <list_of<
#>   tbl_df<
#>     origin: character
#>     date  : date
#>     temp  : double
#>   >
#> >[2]>
#> [[1]]
#> # A tibble: 6 × 3
#>   origin date        temp
#>   <chr>  <date>     <dbl>
#> 1 DTW    2013-01-01  26.4
#> 2 DTW    2013-01-02  17.1
#> 3 DTW    2013-01-03  21.2
#> 4 DTW    2013-01-04  27.6
#> 5 DTW    2013-01-05  23.3
#> 6 DTW    2013-01-06  34.1
#> 
#> [[2]]
#> # A tibble: 6 × 3
#>   origin date        temp
#>   <chr>  <date>     <dbl>
#> 1 LGA    2013-01-01  39.4
#> 2 LGA    2013-01-02  28.7
#> 3 LGA    2013-01-03  29.7
#> 4 LGA    2013-01-04  35.2
#> 5 LGA    2013-01-05  37.8
#> 6 LGA    2013-01-06  39.7
```

First, we require a data frame containing the two time series’ data as
columns.

``` r
daily_temperature_wide <- daily_temperature %>%
  pivot_wider(id_cols=date, names_from=origin, values_from=temp)

daily_temperature_wide
#> # A tibble: 364 × 3
#>    date         DTW   LGA
#>    <date>     <dbl> <dbl>
#>  1 2013-01-01  26.4  39.4
#>  2 2013-01-02  17.1  28.7
#>  3 2013-01-03  21.2  29.7
#>  4 2013-01-04  27.6  35.2
#>  5 2013-01-05  23.3  37.8
#>  6 2013-01-06  34.1  39.7
#>  7 2013-01-07  32.6  42.2
#>  8 2013-01-08  29.0  41.5
#>  9 2013-01-09  37.4  43.8
#> 10 2013-01-10  34.3  45.8
#> # ℹ 354 more rows
```

We should also double-check that our two time series are aligned,
i.e. that there are no missing values in either series. Treatment of
missing values is left to the user.

``` r
daily_temperature_wide %>%
  summarise(
    missing_dtw = any(is.na(DTW)),
    missing_lga = any(is.na(LGA))
  )
#> # A tibble: 1 × 2
#>   missing_dtw missing_lga
#>   <lgl>       <lgl>      
#> 1 FALSE       FALSE
```

The LoCo scores can be obtained by passing `daily_temperature_wide` to
`loco::loco()`. We set the window size to 5 and the number of principal
eigenvectors used to 3. We can optionally supply the `date` column to
the `timestamps` argument for ease of subsequent visualisation.

``` r
loco_scores <- daily_temperature_wide %>%
  loco(DTW, LGA, timestamps=date, window_size=5, k=3)

loco_scores
#> # A tibble: 356 × 2
#>    timestamps scores
#>    <date>      <dbl>
#>  1 2013-01-05  1.00 
#>  2 2013-01-06  1.00 
#>  3 2013-01-07  1.00 
#>  4 2013-01-08  1.00 
#>  5 2013-01-09  1.00 
#>  6 2013-01-10  0.999
#>  7 2013-01-11  1.00 
#>  8 2013-01-12  1.00 
#>  9 2013-01-13  1.00 
#> 10 2013-01-14  1.00 
#> # ℹ 346 more rows
```

We can now plot the LoCo scores and identify time points of particular
interest.

``` r
p1 <- ggplot(daily_temperature_wide) +
  geom_line(aes(x=date, y=DTW), colour="#009E73", alpha=0.6) +
  geom_line(aes(x=date, y=LGA), colour="#D55E00", alpha=0.6) +
  scale_x_date(date_breaks="2 months", date_labels="%b") +
  labs(x="", y="Average daily temperature (fahrenheit)") +
  theme_bw()

p2 <- ggplot(loco_scores) +
  geom_line(aes(x=timestamps, y=scores), colour="black") +
  scale_x_date(date_breaks="2 months", date_labels="%b") +
  labs(x="", y="LoCo score") +
  theme_bw()

p1 / p2
```

<img src="man/figures/README-unnamed-chunk-7-1.svg" width="100%" />
