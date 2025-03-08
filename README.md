
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
temperature readings at JFK and LAX airports. A quick preview of the
data:

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
#> 1 JFK    2013-01-01  38.9
#> 2 JFK    2013-01-02  28.5
#> 3 JFK    2013-01-03  29.8
#> 4 JFK    2013-01-04  34.0
#> 5 JFK    2013-01-05  36.9
#> 6 JFK    2013-01-06  37.4
#> 
#> [[2]]
#> # A tibble: 6 × 3
#>   origin date        temp
#>   <chr>  <date>     <dbl>
#> 1 LAX    2013-01-01  51.8
#> 2 LAX    2013-01-02  54.0
#> 3 LAX    2013-01-03  55.0
#> 4 LAX    2013-01-04  56.9
#> 5 LAX    2013-01-05  52.9
#> 6 LAX    2013-01-06  52.7
```

First, we require a data frame containing the two time series’ data as
columns.

``` r
daily_temperature_wide <- daily_temperature %>%
  pivot_wider(id_cols=date, names_from=origin, values_from=temp)

daily_temperature_wide
#> # A tibble: 364 × 3
#>    date         JFK   LAX
#>    <date>     <dbl> <dbl>
#>  1 2013-01-01  38.9  51.8
#>  2 2013-01-02  28.5  54.0
#>  3 2013-01-03  29.8  55.0
#>  4 2013-01-04  34.0  56.9
#>  5 2013-01-05  36.9  52.9
#>  6 2013-01-06  37.4  52.7
#>  7 2013-01-07  41.9  55.2
#>  8 2013-01-08  38.7  55.3
#>  9 2013-01-09  40.8  56.1
#> 10 2013-01-10  45.0  55.0
#> # ℹ 354 more rows
```

We should also double-check that our two time series are aligned,
i.e. that there are no missing values in either series. Treatment of
missing values is left to the user.

``` r
daily_temperature_wide %>%
  summarise(
    missing_jfk = any(is.na(JFK)),
    missing_lax = any(is.na(LAX))
  )
#> # A tibble: 1 × 2
#>   missing_jfk missing_lax
#>   <lgl>       <lgl>      
#> 1 FALSE       FALSE
```

The LoCo scores can be obtained by passing `daily_temperature_wide` to
`loco::loco()`. We set the window size to 5 and the number of principal
eigenvectors used to 3. We can optionally supply the `date` column to
the `timestamps` argument for ease of subsequent visualisation.

``` r
loco_scores <- daily_temperature_wide %>%
  loco(JFK, LAX, timestamps=date, window_size=10, k=3)

loco_scores
#> # A tibble: 346 × 2
#>    timestamps scores
#>    <date>      <dbl>
#>  1 2013-01-10  0.984
#>  2 2013-01-11  0.989
#>  3 2013-01-12  0.991
#>  4 2013-01-13  0.993
#>  5 2013-01-14  0.994
#>  6 2013-01-15  0.995
#>  7 2013-01-16  0.995
#>  8 2013-01-17  0.996
#>  9 2013-01-18  0.998
#> 10 2013-01-19  0.999
#> # ℹ 336 more rows
```

We can now plot the LoCo scores and identify time points of particular
interest.

``` r
p1 <- ggplot(daily_temperature) +
  geom_line(aes(x=date, y=temp, colour=origin), alpha=0.6) +
  scale_colour_manual(values = c("#009E73", "#D55E00")) +
  scale_x_date(date_breaks="2 months", date_labels="%b") +
  labs(x="", y="Average daily temperature (°F)", colour="") +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom")

p2 <- ggplot(loco_scores) +
  geom_line(aes(x=timestamps, y=scores), colour="black") +
  scale_x_date(date_breaks="2 months", date_labels="%b") +
  labs(x="", y="LoCo score") +
  theme_bw(base_size = 9)

p1 / p2
```

<img src="man/figures/README-unnamed-chunk-7-1.svg" alt="Plot 1: Daily average temperatures in fahrenheit in 2013 for JFK and LAX airports. Plot 2: LoCo scores." width="100%" />
