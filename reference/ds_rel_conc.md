# Compute Relative Concentration

Compute Relative Concentration

## Usage

``` r
ds_rel_conc(.data, .cols, .name)

rel_conc(..., .data = dplyr::across(everything()))
```

## Arguments

- .data:

  [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  with sf geometry

- .cols:

  [`tidy-select`](https://tidyselect.r-lib.org/reference/language.html)
  Columns to compute the measure with. Must be at least 2 columns. If
  more than 2, treats first column as first group and sum of other
  columns as second.

- .name:

  name for column with relative concentration. Leave missing to return a
  vector.

- ...:

  arguments to forward to ds_rel_conc from rel_conc

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
or numeric vector if .name missing

## Examples

``` r
data('de_county')
ds_rel_conc(de_county, c(pop_black, starts_with('pop_')))
#> [1] 0.1680907 0.1680907 0.1680907
ds_rel_conc(de_county, c(pop_black, starts_with('pop_')), 'rel_conc')
#> Simple feature collection with 3 features and 21 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -75.78866 ymin: 38.45101 xmax: -75.04894 ymax: 39.83901
#> Geodetic CRS:  NAD83
#> # A tibble: 3 × 22
#>   GEOID NAME        pop pop_white pop_black pop_hisp pop_aian pop_asian pop_nhpi
#>   <chr> <chr>     <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>    <dbl>
#> 1 10001 Kent Co… 162310    105891     37812     9346      916      3266       74
#> 2 10003 New Cas… 538479    331836    124426    46921      984     23132      102
#> 3 10005 Sussex … 197145    149025     24544    16954      924      1910       62
#> # ℹ 13 more variables: pop_other <dbl>, pop_two <dbl>, vap <dbl>,
#> #   vap_white <dbl>, vap_black <dbl>, vap_hisp <dbl>, vap_aian <dbl>,
#> #   vap_asian <dbl>, vap_nhpi <dbl>, vap_other <dbl>, vap_two <dbl>,
#> #   rel_conc <dbl>, geometry <MULTIPOLYGON [°]>
```
