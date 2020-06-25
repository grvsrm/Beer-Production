Robust Estimation of Beer Production Data using Bootstrap Resampling
================
Gaurav Sharma
24/06/2020

# Prerequisites

# Lets load the data

``` r
brewing_materials_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   data_type = col_character(),
    ##   material_type = col_character(),
    ##   year = col_double(),
    ##   month = col_double(),
    ##   type = col_character(),
    ##   month_current = col_double(),
    ##   month_prior_year = col_double(),
    ##   ytd_current = col_double(),
    ##   ytd_prior_year = col_double()
    ## )

``` r
brewing_materials_raw
```

    ## # A tibble: 1,440 x 9
    ##    data_type material_type  year month type  month_current month_prior_year
    ##    <chr>     <chr>         <dbl> <dbl> <chr>         <dbl>            <dbl>
    ##  1 Pounds o~ Grain Produc~  2008     1 Malt~     374165152        365300134
    ##  2 Pounds o~ Grain Produc~  2008     1 Corn~      57563519         41647092
    ##  3 Pounds o~ Grain Produc~  2008     1 Rice~      72402143         81050102
    ##  4 Pounds o~ Grain Produc~  2008     1 Barl~       3800844          2362162
    ##  5 Pounds o~ Grain Produc~  2008     1 Whea~       1177186          1195381
    ##  6 Pounds o~ Total Grain ~  2008     1 Tota~     509108844        491554871
    ##  7 Pounds o~ Non-Grain Pr~  2008     1 Suga~      78358212         83664091
    ##  8 Pounds o~ Non-Grain Pr~  2008     1 Hops~       4506546          2037754
    ##  9 Pounds o~ Non-Grain Pr~  2008     1 Hops~        621912           411166
    ## 10 Pounds o~ Non-Grain Pr~  2008     1 Other       1291615           766735
    ## # ... with 1,430 more rows, and 2 more variables: ytd_current <dbl>,
    ## #   ytd_prior_year <dbl>

# Lets explore the data a bit to understand

``` r
brewing_materials_raw %>% group_by(type) %>% summarise(n = sum(month_current)) %>% arrange(desc(n))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 12 x 2
    ##    type                                 n
    ##    <chr>                            <dbl>
    ##  1 Total Used                 53559516695
    ##  2 Total Grain products       44734903124
    ##  3 Malt and malt products     32697313882
    ##  4 Total Non-Grain products    8824613571
    ##  5 Sugar and syrups            6653104081
    ##  6 Rice and rice products      5685742541
    ##  7 Corn and corn products      5207759409
    ##  8 Hops (dry)                  1138840132
    ##  9 Other                        998968470
    ## 10 Barley and barley products   941444745
    ## 11 Wheat and wheat products     202642547
    ## 12 Hops (used as extracts)       33700888

``` r
brewing_materials_raw %>% count(type, wt = month_current,sort = T) # Shorter way of doing the same thing
```

    ## # A tibble: 12 x 2
    ##    type                                 n
    ##    <chr>                            <dbl>
    ##  1 Total Used                 53559516695
    ##  2 Total Grain products       44734903124
    ##  3 Malt and malt products     32697313882
    ##  4 Total Non-Grain products    8824613571
    ##  5 Sugar and syrups            6653104081
    ##  6 Rice and rice products      5685742541
    ##  7 Corn and corn products      5207759409
    ##  8 Hops (dry)                  1138840132
    ##  9 Other                        998968470
    ## 10 Barley and barley products   941444745
    ## 11 Wheat and wheat products     202642547
    ## 12 Hops (used as extracts)       33700888

# More Exploration

``` r
brewing_filtered <- brewing_materials_raw %>%
    filter(
        type %in% c("Malt and malt products",
                    "Sugar and syrups",
                    "Hops (dry)"),
        year < 2016,
        month != 12
    ) %>%
    mutate(date = lubridate::ymd(paste(year, "-", month, "-01")))

brewing_filtered %>%
    ggplot(aes(x = date, y = month_current, color = type)) +
    geom_point(size = 2)
```

![](BeerProd_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
brewing_materials <- brewing_filtered %>% 
    select(date, type, month_current) %>% 
    pivot_wider(names_from = type,
                values_from = month_current) %>% 
    janitor::clean_names()

brewing_materials %>% 
    ggplot(aes(malt_and_malt_products, sugar_and_syrups)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    theme_light()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](BeerProd_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

# Lets fit a simpler linear model assuming that if there is no malt then there will be no requirement of sugar

``` r
beer_fit <- lm(sugar_and_syrups ~ 0 + malt_and_malt_products, data = brewing_materials)

summary(beer_fit)
```

    ## 
    ## Call:
    ## lm(formula = sugar_and_syrups ~ 0 + malt_and_malt_products, data = brewing_materials)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -30678583  -6462561    412298   8460012  22807661 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## malt_and_malt_products 0.207764   0.003432   60.54   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11150000 on 87 degrees of freedom
    ## Multiple R-squared:  0.9768, Adjusted R-squared:  0.9765 
    ## F-statistic:  3665 on 1 and 87 DF,  p-value: < 2.2e-16

``` r
tidy(beer_fit)
```

    ## # A tibble: 1 x 5
    ##   term                   estimate std.error statistic  p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 malt_and_malt_products    0.208   0.00343      60.5 6.64e-73

# Lets create a bootstrap sample

``` r
set.seed(123)
beer_boot <- bootstraps(data = brewing_materials,
                        times = 1e3,
                        apparent = TRUE)
```

# Lets map some functions and check results

``` r
beer_models <-
    beer_boot %>%
    mutate(model = map(
        splits,
        ~ lm(sugar_and_syrups ~ 0 + malt_and_malt_products,
             data = .)
    ))

beer_coefs <- beer_models %>% 
    mutate(coef_info = map(model, tidy)) %>% 
    unnest(coef_info)
```

# Lets evaluatet the results

``` r
beer_coefs %>%
    ggplot(aes(estimate)) +
    geom_histogram() +
    geom_vline(
        xintercept = mean(beer_coefs$estimate),
        color = 'tomato',
        size = 2
    ) +
    annotate(
        geom = "text",
        x = 0.2155,
        y = 77,
        label = "Mean of resampled estimates",
        size = 5,
        color = 'tomato'
    ) +
    annotate(
        geom = "curve",
        x = 0.208,
        xend = 0.212,
        y = 80,
        yend = 77,
        curvature = -0.3,
        size = 1,
        color = 'tomato'
    )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](BeerProd_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Lets see another summary for these models

``` r
beer_models %>% 
    mutate(glance = map(model, glance)) %>% 
    unnest(glance)
```

    ## # A tibble: 1,001 x 14
    ##    splits id    model r.squared adj.r.squared  sigma statistic  p.value    df
    ##    <list> <chr> <lis>     <dbl>         <dbl>  <dbl>     <dbl>    <dbl> <int>
    ##  1 <spli~ Boot~ <lm>      0.980         0.979 1.06e7     4177. 2.56e-75     1
    ##  2 <spli~ Boot~ <lm>      0.979         0.979 1.04e7     4102. 5.52e-75     1
    ##  3 <spli~ Boot~ <lm>      0.979         0.979 1.04e7     4123. 4.46e-75     1
    ##  4 <spli~ Boot~ <lm>      0.976         0.976 1.12e7     3581. 1.79e-72     1
    ##  5 <spli~ Boot~ <lm>      0.973         0.973 1.21e7     3157. 3.77e-70     1
    ##  6 <spli~ Boot~ <lm>      0.972         0.972 1.22e7     3049. 1.65e-69     1
    ##  7 <spli~ Boot~ <lm>      0.980         0.980 1.03e7     4342. 4.89e-76     1
    ##  8 <spli~ Boot~ <lm>      0.975         0.975 1.16e7     3408. 1.46e-71     1
    ##  9 <spli~ Boot~ <lm>      0.973         0.973 1.17e7     3167. 3.26e-70     1
    ## 10 <spli~ Boot~ <lm>      0.979         0.979 1.02e7     4129. 4.17e-75     1
    ## # ... with 991 more rows, and 5 more variables: logLik <dbl>, AIC <dbl>,
    ## #   BIC <dbl>, deviance <dbl>, df.residual <int>

# Lets check the fitted values

``` r
beer_models %>% 
    sample_n(200) %>% 
    mutate(augment = map(model, augment)) %>% 
    unnest(augment) %>% 
    ggplot(aes(malt_and_malt_products, sugar_and_syrups)) +
        geom_point(show.legend = F) +
    geom_line(aes(y = .fitted, group = id, color = 'pink', alpha = 0.1),
              show.legend = F)
```

![](BeerProd_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Thats how we can use tidymodels workflow to see how predictions can be seen visually using resampled data (Bootstrapped)
