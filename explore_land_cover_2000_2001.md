Review lulc doc
================
Margaux
2022-08-02

``` r
suppressPackageStartupMessages(
  {library(tidyverse)
  library(rmarkdown)
  library(patchwork)
  library(viridis)
  }
  )
targets::tar_load(p2_all_lulc_data_cat)
targets::tar_load(p2_all_lulc_data_tot)
```

### Initial point plot of diff per PRMS_segid

Splitting comid total values to a seq that is more easily plotted

``` r
sequence <- seq(27,459,17)
```

Function to plot diff in 2000 and 2001 by comid id. Generating a bunch
of plots locally here

``` r
lulc_00_01_plot <- function(df, increment, years_filter, lc_category, output_path){

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
                  aes(x = PRMS_segid, y = .data[[lc_category]], color = Year))+
    geom_point()+
    coord_flip()+
    theme_bw()

  ## Export
  filename <- glue::glue('subset_lulc_{i}.png')
  #print(filename)
  ggsave(file.path(output_path,filename), width = 7, height = 6)
  }
  #print(lulc_00_01_subset %>% head())
  return(plt)
}
```

Plot - 2000 and 2001

``` r
## generate plot - sifting through outputs  in finder
path2000_2001 <- '3_visualize/land_cover_review/yr2000_2001'
dir.create(path2000_2001,showWarnings = FALSE)

for(i in 1:9){
  spec_path <- file.path(path2000_2001, glue::glue('CAT_prop_lcClass_{i}'))
  dir.create(spec_path,showWarnings = FALSE)
  lulc_00_01_plot(p2_all_lulc_data_cat,
                  increment = sequence,
                  output_path = spec_path,
                  c('2000','2001'),
                  lc_category = glue::glue('CAT_prop_lcClass_{i}'))
}
```

Plot - 1990 to 2008

``` r
## generate plot - sifting through img outputs in finder
path_1990_2008 <- '3_visualize/land_cover_review/yr1990_2008'
dir.create(path_1990_2008,showWarnings = FALSE)

for(i in 1:9){
  spec_path <- file.path(path_1990_2008, glue::glue('CAT_prop_lcClass_{i}'))
  dir.create(spec_path, showWarnings = FALSE)

  lulc_00_01_plot(p2_all_lulc_data_cat,
                  increment = sequence,
                  output_path = spec_path,
                  c('1990','2000','2001','2008'),
                  lc_category = glue::glue('CAT_prop_lcClass_{i}'))
  }
```

### 2. Review proportion by lc diff - bot plot

pre-defined obj

``` r
year_filter <- c('2000','2001')
all_lulc_data <- p2_all_lulc_data_cat
pref <- 'CAT'
```

Tidy

``` r
lulc_00_01_subset <- all_lulc_data %>%
  filter(Year %in% year_filter) %>%
  arrange(PRMS_segid) %>% 
  pivot_longer(cols = starts_with(pref),
               names_to = 'LC_category',
               values_to = 'proportion_LC')
```

1.  initial plot

``` r
plt1 <- ggplot2::ggplot(lulc_00_01_subset,
                       # Only plotting 1 class for now. MUST plot across all lc classes!!!! 
                       aes(x = Year, y = proportion_LC, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+theme_classic()

plt1
```

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
plt1_class_subset <- ggplot2::ggplot(lulc_00_01_subset %>%
                                 filter(grepl('5|7|8|9|1|4',LC_category)),
                               aes(x = Year, y = proportion_LC, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+
  theme_classic()

plt1_class_subset
```

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
plt1_class_subset_2 <- ggplot2::ggplot(lulc_00_01_subset %>%
                                       filter(grepl('5|7|8',LC_category)),
                                     aes(x = Year, y = proportion_LC, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+
  theme_classic()

plt1_class_subset_2
```

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
# Some outliers causing difficulty in graphing
```

2.  Removing outliers

Note: a number of NAs are removed due to filtering out of outliers

``` r
lulc_00_01_subset_outlier_filter <- lulc_00_01_subset %>%
  group_by(LC_category,Year) %>% 
  mutate(proportion_LC = as.numeric(proportion_LC),
         proportion_LC_filt = case_when(proportion_LC - quantile(proportion_LC)[4] > 1.5*IQR(proportion_LC) ~ NA_real_,
                                        quantile(proportion_LC)[2] - proportion_LC > 1.5*IQR(proportion_LC) ~ NA_real_,
                                        TRUE ~ proportion_LC))

plt2 <- ggplot2::ggplot(lulc_00_01_subset_outlier_filter,
                        # Only plotting 1 class for now. MUST plot across all lc classes!!!! 
                        aes(x = Year, y = proportion_LC_filt, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+theme_classic()

plt2
```

    ## Warning: Removed 681 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 681 rows containing missing values (geom_point).

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plt2_class_subset <- ggplot2::ggplot(lulc_00_01_subset_outlier_filter %>%
                                 filter(grepl('5|7|8|9|1|4',LC_category)),
                               aes(x = Year, y = proportion_LC_filt, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  #geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+
  theme_classic()

plt2_class_subset
```

    ## Warning: Removed 633 rows containing non-finite values (stat_boxplot).

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
plt2_class_subset_2 <- ggplot2::ggplot(lulc_00_01_subset_outlier_filter %>%
                                       filter(grepl('5|7|8',LC_category)),
                                     aes(x = Year, y = proportion_LC_filt, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  #geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+
  theme_classic()

plt2_class_subset_2
```

    ## Warning: Removed 280 rows containing non-finite values (stat_boxplot).

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

3.  Adding more years - outliers NOT removed

``` r
year_filter <- c('1990','2000','2001','2008')
```

Tidy

``` r
lulc_00_01_subset <- all_lulc_data %>%
  filter(Year %in% year_filter) %>%
  arrange(PRMS_segid) %>% 
  pivot_longer(cols = starts_with('CAT'),
               names_to = 'LC_category',
               values_to = 'proportion_LC')

plt3 <- ggplot2::ggplot(lulc_00_01_subset,
                        # Only plotting 1 class for now. MUST plot across all lc classes!!!! 
                        aes(x = Year, y = proportion_LC, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  #geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+theme_classic()

plt3
```

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
plt3_class_subset <- ggplot2::ggplot(lulc_00_01_subset %>%
                                       filter(grepl('5|7|8|9|1|4',LC_category)),
                                     aes(x = Year, y = proportion_LC, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  #geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+
  theme_classic()

plt3_class_subset
```

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
plt3_class_subset_2 <- ggplot2::ggplot(lulc_00_01_subset %>%
                                       filter(grepl('5|7|8',LC_category)),
                                     aes(x = Year, y = proportion_LC, fill = Year))+
  geom_boxplot(outlier.shape = 1, 
               outlier.size = 0.5, outlier.color = 'black',
               outlier.fill = 'grey', outlier.stroke = 0.2)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="C")+
  #geom_jitter(color="grey", size=0.4, alpha=0.5)+
  facet_grid(.~LC_category)+
  theme_classic()

plt3_class_subset_2
```

![](explore_land_cover_2000_2001_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->
