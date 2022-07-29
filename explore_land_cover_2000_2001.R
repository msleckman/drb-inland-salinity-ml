#' ---
#' title: "review lulc doc" 
#' author: Margaux
#' output: github_document
#' ---

suppressPackageStartupMessages(
  {library(tidyverse)
  library(rmarkdown)}
  )

targets::tar_load(p2_all_lulc_data_cat)

#' Splitting comid total values to a seq that is more easily plotted
sequence <- seq(27,459,17)

#' Function to plot diff in 2000 and 2001 by comid id.
#'
#' Simply generating a bunch of plots locally here

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

#' Reproduce for all lc class 

#' 2. Review proportion by lc diff

#' Tidy
# lulc_00_01 <- p2_all_lulc_data_cat %>%
#   filter(Year %in% c('2000','2001')) %>%
#   arrange(PRMS_segid) 
