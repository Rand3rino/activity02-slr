Activity 2 - Day 1
================

``` r
hfi <- readr::read_csv("https://www.openintro.org/data/csv/hfi.csv")
```

    ## Rows: 1458 Columns: 123
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (3): ISO_code, countries, region
    ## dbl (120): year, pf_rol_procedural, pf_rol_civil, pf_rol_criminal, pf_rol, p...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
hfi_2016 <- filter(hfi, year == 2016)
```

``` r
# plot(hfi_2016$pf_expression_control, hfi_2016$pf_score)
statsr::plot_ss(x = pf_expression_control, y = pf_score, data = hfi_2016, showSquares = TRUE)
```

![](activity02-day01_files/figure-gfm/plot-1.png)<!-- -->

    ## Click two points to make a line.                                
    ## Call:
    ## lm(formula = y ~ x, data = pts)
    ## 
    ## Coefficients:
    ## (Intercept)            x  
    ##      4.2838       0.5418  
    ## 
    ## Sum of Squares:  102.213

``` r
m1 <- lm(pf_score ~ pf_expression_control, data = hfi_2016)
tidy(m1)
```

    ## # A tibble: 2 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              4.28     0.149       28.8 4.23e-65
    ## 2 pf_expression_control    0.542    0.0271      20.0 2.31e-45
