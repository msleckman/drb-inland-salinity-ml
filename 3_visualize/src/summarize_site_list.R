summarize_site_list <- function(site_list_path,nwis_daily_data,nwis_inst_data,fileout){
  #' 
  #' @description Function to summarize data availability info and save log file 
  #'
  #' @param site_list_path file path and name of site list file, including the file extension
  #' @param nwis_daily_data data frame containing daily data for all NWIS daily sites
  #' @param nwis_inst_data data frame containing instantaneous data for all NWIS instantaneous sites
  #' @param fileout file path and name for output data, including the file extension
  #'
  #' @value A data frame containing the total number of observation days (discrete observations + NWIS days), the number of unique lat/lon locations, and the number of sites broken down by data source
  #' @example summarize_site_list(site_list_path = "2_process/out/site_list.csv",nwis_daily_data = daily_df,nwis_inst_data = inst_df)
  #' 

  # Read in list of sites  
  site_list <- read_csv(site_list_path,show_col_types = FALSE)

  # Summarize data availability
  site_summary <- site_list %>%
    summarize(
      # tally number of unique site id's across data sources
      n_unique_siteids = length(unique(site_id)),
      # tally number of unique lat/lon locations across data sources
      n_unique_latlon = as.numeric(tally(distinct(.,lat,lon))),
      # tally total number of WQP sites that contribute discrete data
      n_all_WQP_sites = as.numeric(tally(filter(.,count_days_discrete > 0))),
      # tally total number of NWIS-daily sites that contribute continuous data
      n_all_nwis_daily_sites = length(unique(nwis_daily_data$site_no)),
      # tally total number of NWIS-inst sites that contribute continuous data
      n_all_nwis_inst_sites = length(unique(nwis_inst_data$site_no)),
      # tally number of unique NWIS sites that contribute continuous data (daily or inst)
      n_unique_nwis_sites = as.numeric(tally(filter(.,grepl("NWIS",.$data_src_combined,ignore.case=TRUE)))),
      # tally number of site id's that have observations from both NWIS and WQP
      n_sites_intersect_WQPNWIS = as.numeric(tally(filter(.,.$count_days_discrete>0 & .$data_src_combined %in% c("NWIS_daily/Harmonized_WQP_data","NWIS_instantaneous/Harmonized_WQP_data")))),
      # tally total number of observation-days across data sources
      n_obsdays_total = sum(count_days_total,na.rm=TRUE),
      # tally number of observation-days from NWIS
      n_obsdays_nwis = sum(count_days_nwis,na.rm=TRUE),
      # tally number of discrete sample observation-days
      n_obsdays_discrete = sum(count_days_discrete,na.rm=TRUE))
  
  # Save data availability log file
  write_csv(site_summary, file = fileout)
  
  return(fileout)
  
}
