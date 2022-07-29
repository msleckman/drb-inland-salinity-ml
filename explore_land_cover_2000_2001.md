review lulc doc
================
Margaux
2022-07-29

``` r
suppressPackageStartupMessages(
  {library(tidyverse)
  library(rmarkdown)}
  )

targets::tar_load(p2_all_lulc_data_cat)
```

Splitting comid total values to a seq that is more easily plotted

``` r
sequence <- seq(27,459,17)
```

Function to plot diff in 2000 and 2001 by comid id.

Simply generating a bunch of plots locally here

``` r
lulc_00_01_plot <- function(df, increment, years_filter = c('2000','2001')){
  
  ## tidy
  lulc_00_01 <- df %>%
    filter(Year %in% years_filter) %>%
    arrange(PRMS_segid)
  
  ## subset every 20 
  for(i in increment){    
    j = 27
    lulc_00_01_subset <- lulc_00_01[(i-j):i,]
    
  ##  Example plot #1
  plt <- ggplot2::ggplot(lulc_00_01_subset,
                  aes(x = PRMS_segid, y = CAT_prop_lcClass_1, color = Year))+
    geom_point()+coord_flip()+ theme_bw()
  plt
  
  ## Export
  filename <- glue::glue('subset_lulc_{i}.png')
  #print(filename)
  ggsave(here::here(paste0('3_visualize/',filename)),width = 7, height = 6)
  }
  print(lulc_00_01_subset %>% head())
  return(plt)
}

## generate plot - sift through then in finder
lulc_00_01_plot(p2_all_lulc_data_cat, increment = sequence)
```

    ## # A tibble: 6 × 12
    ##   PRMS_segid PRMS_area_km2 Year  CAT_prop_lcClass_1 CAT_prop_lcClass_2
    ##   <chr>              <dbl> <chr>              <dbl>              <dbl>
    ## 1 2771_1              79.9 2000              0.720              0.0118
    ## 2 2771_1              75.3 2001              0.794              0.015 
    ## 3 2772_1              78.2 2000              0.118              0.229 
    ## 4 2772_1              76.4 2001              0.126              0.299 
    ## 5 278_1              442.  2000              0.0207             0.0279
    ## 6 278_1              443.  2001              0.0162             0.0413
    ## # … with 7 more variables: CAT_prop_lcClass_3 <dbl>, CAT_prop_lcClass_4 <dbl>,
    ## #   CAT_prop_lcClass_5 <dbl>, CAT_prop_lcClass_6 <dbl>,
    ## #   CAT_prop_lcClass_7 <dbl>, CAT_prop_lcClass_8 <dbl>,
    ## #   CAT_prop_lcClass_9 <dbl>

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Reproduce for all lc class 2. Review proportion by lc diff Tidy

``` r
# lulc_00_01 <- p2_all_lulc_data_cat %>%
#   filter(Year %in% c('2000','2001')) %>%
#   arrange(PRMS_segid) 
```
