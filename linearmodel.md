linearmodel
================
Yuchen Hua
2022-11-25

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(borough != "Staten Island") %>% 
  select(price, stars, borough, neighborhood, room_type)
```

## fit a model

``` r
nyc_airbnb %>%
  ggplot(aes(x = stars, y= price, color = borough))+
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

![](linearmodel_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Let’s fit the model

Let’s look at the result better …(tidy out result)

``` r
broom::glance(fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.…¹ sigma stati…²   p.value    df  logLik    AIC    BIC devia…³
    ##       <dbl>    <dbl> <dbl>   <dbl>     <dbl> <dbl>   <dbl>  <dbl>  <dbl>   <dbl>
    ## 1    0.0342   0.0341  182.    271. 6.73e-229     4 -2.02e5 4.04e5 4.04e5  1.01e9
    ## # … with 2 more variables: df.residual <int>, nobs <int>, and abbreviated
    ## #   variable names ¹​adj.r.squared, ²​statistic, ³​deviance

``` r
broom::tidy(fit) %>%
  select(-std.error, -statistic) %>%
  mutate(
    term = str_replace(term, "borough", "Borough:")
  ) %>%
  knitr::kable(digits = 3)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |  -70.414 |   0.000 |
| stars             |   31.990 |   0.000 |
| Borough:Brooklyn  |   40.500 |   0.000 |
| Borough:Manhattan |   90.254 |   0.000 |
| Borough:Queens    |   13.206 |   0.145 |

## Be in control of factors

``` r
nyc_airbnb = 
  nyc_airbnb %>%
  mutate(
    borough = fct_infreq(borough),
    room_type = fct_infreq(room_type)
  )
```

Look at that plot again

``` r
nyc_airbnb %>%
  ggplot(aes(x = stars, y= price, color = borough))+
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

![](linearmodel_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
fit = lm(price ~stars + borough, data = nyc_airbnb)

broom::tidy(fit)
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

``` r
broom::glance(fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.…¹ sigma stati…²   p.value    df  logLik    AIC    BIC devia…³
    ##       <dbl>    <dbl> <dbl>   <dbl>     <dbl> <dbl>   <dbl>  <dbl>  <dbl>   <dbl>
    ## 1    0.0342   0.0341  182.    271. 6.73e-229     4 -2.02e5 4.04e5 4.04e5  1.01e9
    ## # … with 2 more variables: df.residual <int>, nobs <int>, and abbreviated
    ## #   variable names ¹​adj.r.squared, ²​statistic, ³​deviance

## Diagnostics

``` r
modelr::add_residuals(nyc_airbnb, fit) %>%
  ggplot(aes(x = borough, y = resid)) +
  geom_violin() +
  ylim(-500, 1500)
```

    ## Warning: Removed 9993 rows containing non-finite values (`stat_ydensity()`).

![](linearmodel_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

or can be written like

``` r
nyc_airbnb %>%
modelr::add_residuals( fit) %>%
  ggplot(aes(x = borough, y = resid)) +
  geom_violin() +
  ylim(-500, 1500)
```

    ## Warning: Removed 9993 rows containing non-finite values (`stat_ydensity()`).

![](linearmodel_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
nyc_airbnb %>%
  modelr::add_residuals( fit) %>%
  ggplot(aes(x = stars, y = resid)) +
  geom_point() +
  facet_wrap(. ~ borough)
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

![](linearmodel_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

## Hypothesis test

This does t-test by default

``` r
fit %>%
  broom::tidy()
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

What about the significantce of ‘borough’

``` r
fit_null = lm(price ~stars, data = nyc_airbnb)
fit_alt = lm(price ~ stars + borough, data = nyc_airbnb)

anova(fit_null, fit_alt) %>%
  broom::tidy()
```

    ## Warning: Unknown or uninitialised column: `term`.

    ## # A tibble: 2 × 7
    ##   term                    df.residual       rss    df   sumsq stati…¹    p.value
    ##   <chr>                         <dbl>     <dbl> <dbl>   <dbl>   <dbl>      <dbl>
    ## 1 price ~ stars                 30528    1.03e9    NA NA          NA  NA        
    ## 2 price ~ stars + borough       30525    1.01e9     3  2.53e7    256.  7.84e-164
    ## # … with abbreviated variable name ¹​statistic

## Nest data, fit models

This is pretty formal and also complex

``` r
fit = lm(price ~ stars*borough + room_type*borough, data= nyc_airbnb)

broom::tidy(fit)
```

    ## # A tibble: 16 × 5
    ##    term                                  estimate std.error statistic  p.value
    ##    <chr>                                    <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                              95.7      19.2     4.99   6.13e- 7
    ##  2 stars                                    27.1       3.96    6.84   8.20e-12
    ##  3 boroughBrooklyn                         -26.1      25.1    -1.04   2.99e- 1
    ##  4 boroughQueens                            -4.12     40.7    -0.101  9.19e- 1
    ##  5 boroughBronx                             -5.63     77.8    -0.0723 9.42e- 1
    ##  6 room_typePrivate room                  -124.        3.00  -41.5    0       
    ##  7 room_typeShared room                   -154.        8.69  -17.7    1.42e-69
    ##  8 stars:boroughBrooklyn                    -6.14      5.24   -1.17   2.41e- 1
    ##  9 stars:boroughQueens                     -17.5       8.54   -2.04   4.09e- 2
    ## 10 stars:boroughBronx                      -22.7      17.1    -1.33   1.85e- 1
    ## 11 boroughBrooklyn:room_typePrivate room    32.0       4.33    7.39   1.55e-13
    ## 12 boroughQueens:room_typePrivate room      54.9       7.46    7.37   1.81e-13
    ## 13 boroughBronx:room_typePrivate room       71.3      18.0     3.96   7.54e- 5
    ## 14 boroughBrooklyn:room_typeShared room     47.8      13.9     3.44   5.83e- 4
    ## 15 boroughQueens:room_typeShared room       58.7      17.9     3.28   1.05e- 3
    ## 16 boroughBronx:room_typeShared room        83.1      42.5     1.96   5.03e- 2

This is more exploratary but maybe easier to understand

``` r
nyc_airbnb %>%
  nest(data = -borough) %>% 
  mutate(
    models = map(data, ~lm(price ~stars, data =.x)),
    results = map(models, broom::tidy)
  ) %>%
  select(-data, -models) %>%
  unnest(results) %>%
  filter(term == "stars")
```

    ## # A tibble: 4 × 6
    ##   borough   term  estimate std.error statistic  p.value
    ##   <fct>     <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 Bronx     stars     4.91      4.10      1.20 2.31e- 1
    ## 2 Queens    stars    15.8       5.63      2.81 5.06e- 3
    ## 3 Brooklyn  stars    28.0       3.10      9.02 2.13e-19
    ## 4 Manhattan stars    43.3       4.78      9.07 1.39e-19

or can be written as

``` r
nyc_airbnb %>%
  nest(data = -borough) %>% 
  mutate(
    models = map(data, ~lm(price ~stars, data =.x)),
    results = map(models, broom::tidy)
  ) %>%
  select(-data, -models) %>%
  unnest(results) %>%
  filter(term != "(Intercept)")
```

    ## # A tibble: 4 × 6
    ##   borough   term  estimate std.error statistic  p.value
    ##   <fct>     <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 Bronx     stars     4.91      4.10      1.20 2.31e- 1
    ## 2 Queens    stars    15.8       5.63      2.81 5.06e- 3
    ## 3 Brooklyn  stars    28.0       3.10      9.02 2.13e-19
    ## 4 Manhattan stars    43.3       4.78      9.07 1.39e-19

``` r
nyc_airbnb %>%
  nest(data = -borough) %>% 
  mutate(
    models = map(data, ~lm(price ~stars + room_type, data =.x)),
    results = map(models, broom::tidy)
  ) %>%
  select(-data, -models) %>%
  unnest(results) %>%
  filter(term != "(Intercept)") %>%
  select(borough, term, estimate) %>%
  pivot_wider(
    names_from = borough,
    values_from = estimate
  )
```

    ## # A tibble: 3 × 5
    ##   term                   Bronx Queens Brooklyn Manhattan
    ##   <chr>                  <dbl>  <dbl>    <dbl>     <dbl>
    ## 1 stars                   4.45   9.65     21.0      27.1
    ## 2 room_typePrivate room -52.9  -69.3     -92.2    -124. 
    ## 3 room_typeShared room  -70.5  -95.0    -106.     -154.

Let’s nest even more…

``` r
nyc_airbnb %>%
  filter(borough =="Manhattan") %>%
  nest(data = -neighborhood) %>%
  mutate(
    models = map(.x = data, ~lm(price ~stars +room_type, data=.x)),
    results = map(models, broom::tidy)
  ) %>%
  select(-data, -models) %>%
  unnest(results) %>%
  filter(str_detect(term, "room_type")) %>%
  ggplot(aes(x= neighborhood, y =estimate)) +
  geom_point() +
  facet_wrap(. ~term) +
  theme(axis.text.x = element_text(angle = 80, hjust = 1))
```

![](linearmodel_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
