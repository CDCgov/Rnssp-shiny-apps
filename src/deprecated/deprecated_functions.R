# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# DEPRECATED FUNCTIONS, MIGHT NEED THEM OR A VERSION OF THEM IN
# FUTURE DEVELOPMENT OF APP
# 
# # function does week to end date for a single week string
# week_to_end_date <- function(week_str) {
#   parts <- strsplit(week_str, "-")[[1]]
#   year <- as.numeric(parts[1])
#   week <- as.numeric(parts[2])
#   
#   # get Sunday (1) of the specified MMWR week
#   as.character(MMWRweek::MMWRweek2Date(year, week, 1))
# }

# # Function to complete and extend the dataset
# expand_dataset <- function(df, num_future_steps) {
#   
#   # Ensure date column is of Date class
#   df$date <- as.IDate(df$date)
#   
#   # Determine the increment (daily or weekly)
#   date_increment <- unique(df$date) |> sort() |> diff() |> unique()
#   if (length(date_increment) !=1) {
#     cli::cli_abort("Inconsistent date increments detected.")
#   }
#   
#   # Create full sequence of dates
#   min_date <- min(df$date)
#   max_date <- max(df$date)
#   all_dates <- seq(min_date, max_date + num_future_steps * date_increment, by = date_increment)
#   
#   # Create all combinations of region and date
#   regions <- unique(df$region)
#   
#   #full_grid <- expand.grid(date = all_dates, region = regions)
#   full_grid <- data.table::CJ(date= all_dates, region=regions)
#   df_full <- df[full_grid, on=.(date, region)]
#   df_full[is.na(overall), overall:=1]
#   df_full
#   
#   # # Merge with original data
#   # df_full <- full_grid |>
#   #   left_join(df, by = c("date", "region")) |>
#   #   mutate(overall = ifelse(is.na(overall), 1, overall)) # add ones for denominators
#   # 
#   # return(df_full)
# }

# add_expected <-function(df,nforecasts){
#   
#   # Calculate total value for each date
#   df_date <- df |>
#     group_by(date) |>
#     summarise(target_tot = sum(target, na.rm = TRUE)) |>
#     arrange(date)
#   nrows= nrow(df_date)
#   forecast_inds = (nrows-nforecasts+1):nrows
#   df_date[forecast_inds,"target_tot"]<-df_date[nrows-nforecasts,"target_tot"]
#   # Calculate total population proportion for each date
#   df_reg <- df |>
#     group_by(region) |>
#     summarise(reg_sum = sum(target, na.rm = TRUE)) 
#   df_reg$reg_prop<-df_reg$reg_sum/sum(df$target,na.rm=TRUE)
#   
#   
#   # Calculate adjusted total value
#   df <- inner_join(df,df_date,by="date")
#   df <- inner_join(df,df_reg,by="region")
#   df$expected_target<-df$reg_prop*df$target_tot
#   return(df)
# }

# fit_model_deprecated<-function(data,formula,family){
#   print("Formula:")
#   print(formula)
#   start <- Sys.time()
#   data<-data.frame(data)
#   data$log_offset <- log(data$expected_target)
#   if (family == "poisson"){
#     inla_model <- inla(
#       formula, 
#       family = "poisson", 
#       data = data, 
#       offset=log_offset,
#       #E=expected_target,
#       control.compute=list(waic=TRUE,return.marginals.predictor = TRUE),
#       control.predictor = list(compute = TRUE, link = 1)
#     )
#   }else if (family=="binomial"){
#     inla_model <- inla(
#       formula, 
#       family = "binomial", 
#       data = data, 
#       Ntrials = overall,
#       #offset=log_offset,
#       E=expected_target,
#       control.predictor = list(compute = TRUE, link = 1),
#       control.compute=list(waic=TRUE,return.marginals.predictor = TRUE),
#       control.inla=list(int.strategy="eb")
#     )
#   }else if (family=="nbinomial"){
#     data$log_offset <- log(data$expected_target)
#     inla_model <- inla(
#       formula, 
#       family = "nbinomial", 
#       data = data, 
#       offset=log_offset,
#       control.predictor = list(compute = TRUE, link = 1),
#       control.compute=list(waic=TRUE,return.marginals.predictor = TRUE),
#     )
#   } else {
#     stop("Family not implemented!")
#   }
#   print( Sys.time() - start )
#   return(inla_model)
#   
# }

# get_adjacency<-function(data=NULL, region_col){
#   adj_mat<-read.csv("data/mobility_adj_mat.csv",colClasses=c("X"="character"))
#   rownames(adj_mat)<-adj_mat$X 
#   colnames(adj_mat)<-gsub("^X","",colnames(adj_mat))
#   adj_mat<-adj_mat[,colnames(adj_mat)!=""]
#   if (is.null(data)){
#     return(adj_mat)
#   } else {
#     region_id_col=paste0(region_col,"_id")
#     id_mapping<-data[,c("countyfips", region_id_col)] |> distinct() |> arrange(region_id_col)
#     fips_order<-id_mapping$countyfips
#     adj_mat_filt<-adj_mat[fips_order,fips_order]
#     adj_mat_inla <-inla.read.graph(adj_mat_filt)
#     return(adj_mat_inla)
#   }
#   
# }

# get_adjacency_dt<-function(data=NULL, region_col=NULL){
#   adj_mat = read_mobility_adj_mat()
#   if (is.null(data)){
#     return(adj_mat)
#   } else {
#     region_id_col=paste0(region_col,"_id")
#     id_mapping <- data[, .SD, .SDcols = c("countyfips", region_id_col)] |> 
#       unique() |> 
#       _[order(x), env=list(x=region_id_col)]
#     
#     #id_mapping<-distinct(data[,c("countyfips", region_id_col)]) |> arrange(.data[[region_id_col]])
#     fips_order<-id_mapping$countyfips
#     adj_mat_filt<-adj_mat[fips_order,fips_order]
#     adj_mat_inla <-inla.read.graph(adj_mat_filt)
#     return(adj_mat_inla)
#   }
#   
# }


# get_physical_adjacency<-function(data=NULL, region_col = NULL){
#   adj_mat<-read.csv("data/us_county_adjacency.csv",colClasses=c("X"="character"))
#   rownames(adj_mat)<-str_pad(adj_mat$X,5,pad="0") 
#   colnames(adj_mat)<-str_pad(gsub("^X","",colnames(adj_mat)),5,pad="0")
#   adj_mat<-adj_mat[,colnames(adj_mat)!="00000"]
#   if (is.null(data)){
#     return(adj_mat)
#   } else {
#     region_id_col=paste0(region_col,"_id")
#     id_mapping<-distinct(data[,c("countyfips", region_id_col)]) |> arrange(.data[[region_id_col]])
#     fips_order<-id_mapping$countyfips
#     adj_mat_filt<-adj_mat[fips_order,fips_order]
#     return(adj_mat_filt)
#   }
# }

# get_physical_adjacency_dt<-function(data=NULL, region_col=NULL){
#   adj_mat = read_physical_adj_mat()
#   if (is.null(data)){
#     return(adj_mat)
#   } else {
#     region_id_col=paste0(region_col,"_id")
#     id_mapping <- data[, .SD, .SDcols = c("countyfips", region_id_col)] |> 
#       unique() |> 
#       _[order(x), env=list(x=region_id_col)]
#     
#     fips_order<-id_mapping$countyfips
#     adj_mat_filt<-adj_mat[fips_order,fips_order]
#     adj_mat_inla <-inla.read.graph(adj_mat_filt)
#     return(adj_mat_inla)
#   }
#   
# }

# get_posteriors<-function(res_data,inla_model,date_col,family,suffix=NULL){
#   
# 
#   res_data[[date_col]]<-as.Date(res_data[[date_col]])
#   median_name = "predicted_median"
#   lower_name = "predicted_lower"
#   upper_name = "predicted_upper"
#   if (!is.null(suffix)){
#     median_name<-paste0(median_name,"_",suffix)
#     lower_name<-paste0(lower_name,"_",suffix)
#     upper_name<-paste0(upper_name,"_",suffix)
#   }
#   if (family %in% c("poisson","nbinomial")){
#     res_data[[median_name]]<-inla_model$summary.fitted.values[['0.5quant']]
#     res_data[[lower_name]]<-inla_model$summary.fitted.values[['0.025quant']]
#     res_data[[upper_name]]<-inla_model$summary.fitted.values[['0.975quant']]
#   } else if (family %in% c("binomial","betabinomial")){
#     res_data[[median_name]]<-inla_model$summary.fitted.values[['0.5quant']]*res_data$overall
#     res_data[[lower_name]]<-inla_model$summary.fitted.values[['0.025quant']]*res_data$overall
#     res_data[[upper_name]]<-inla_model$summary.fitted.values[['0.975quant']]*res_data$overall
#     
#   }
#   
#   return(res_data)
# }
# 
# process_region <- function(df, formula, family) {
#   model <- fit_model(df, formula = formula, family = family)  # Train the model with formula and family
#   df_with_results <- get_posteriors(df, model,suffix="temporal")  # Extract results
#   return(df_with_results)
# }
# 
# fit_temporal_model<-function(res_data,formula,family){
#   # Apply the function to each subset and combine the results
#   res_data <- res_data |>
#     split(.$region) |> 
#     lapply(process_region, formula = formula, family = family) |>
#     bind_rows()
#   return(res_data)
# }


# library(tigris)
# options(tigris_use_cache = TRUE)
# draw_map<-function(df,col){
#   counties_sf <- counties(cb = TRUE, resolution = "5m",year=2020)
#   counties_sf$GEOID <- as.character(counties_sf$GEOID)
#   merged_df <- counties_sf |> inner_join(df,by=c("GEOID" = "countyfips"))
#   map1 <- ggplot(data = merged_df) +
#     geom_sf(aes(fill = .data[[col]]), lwd = 0.01) +
#     scale_fill_viridis_c(name = NULL, option = "rocket", na.value = "grey90", direction = -1) +
#     labs(fill = "", title = "") +
#     theme_void()
# }

# count_missing_values <- function(df) {
#   sapply(df, function(col) {
#     sum(
#       is.na(col) | 
#         is.null(col) | 
#         (is.character(col) & col == "")
#     )
#   })
# }

# get_weather_ts_data<-function(start_date,end_date,time_resolution,geo_resolution,state_filter=NULL){
#   base_url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?"
#   start_date<-format(as.Date(start_date), "%d%b%Y")
#   end_date<-format(as.Date(end_date), "%d%b%Y")
#   url<-paste0(base_url,"endDate=",end_date,"&startDate=",start_date)
#   url<-paste0(url,"&detector=nodetectordetector")
#   url<-paste0(url,"&percentParam=noPercent")
#   url<-paste0(url,"&userId=",userid)
#   url<-paste0(url,"&multiStratVal=stationID")
#   url<-paste0(url,"&removeZeroSeries=False")
#   url<-paste0(url,"&weatherFactor=maxtemp")
#   url<-paste0(url,"&datasource=va_weather_aggr")
#   url<-paste0(url,"&stationAggregateFunc=max")
#   url<-paste0(url,"&timeAggregateFunc=max")
#   url<-paste0(url,"&aqtTarget=TimeSeries")
#   if (time_resolution=="daily"){
#     url<-paste0(url,"&timeResolution=daily")
#   } else if (time_resolution == "weekly"){
#     url<-paste0(url,"&timeResolution=weekly")
#   } else if (time_resolution == "monthly"){
#     url<-paste0(url,"&timeResolution=monthly")
#   } else if (time_resolution == "yearly"){
#     url<-paste0(url,"&timeResolution=yearly")
#   } else {
#     stop(paste0("invalid time resoution:",time_resolution))
#   }
#   url<-paste0(url,"&rowFields=timeResolution")
#   if (geo_resolution =="county"){
#     df = data.table::fread("data/weather_station_weight_mapping.csv")
#     if (!is.null(state_filter)){
#       df<-df[df$state %in% state_filter,]
#     }
#     stations = unique(df$stationid)
#     print(paste0("Filtering to ", paste0(stations,collapse=", ")))
#     
#     url <- paste0(url,paste0("&stationID=", gsub(" ","%20",tolower(stations)), collapse = ""))
#   } else if (geo_resolution =="state"){
#     stop("weather data is not available for states.")
#   }
#   
#   weather_df<-Rnssp::get_api_data(url)$timeSeriesData[,c("date","stationID_id","count")]
#   weather_df <- weather_df |> 
#     rename(stationid=stationID_id,temp=count)
#   df_res<-inner_join(df[,c("county_fips","stationid","weight")],weather_df,by="stationid")
#   df_res <- df_res |>
#     group_by(county_fips,date) |>
#     summarise(weighted_temp = sum(temp * weight) / sum(weight), .groups = "drop")
#   return(df_res |> select(county_fips,date,weighted_temp))
#   
#   
# }



# get_time_dependent_covariates<-function(start_date,end_date,time_resolution,geo_resolution,state_filter){
#   
#   weather_df<-get_weather_ts_data(start_date=start_date,
#                                   end_date=end_date,
#                                   time_resolution=time_resolution,
#                                   geo_resolution=geo_resolution,
#                                   state_filter=state_filter)
#   aq_df<-get_air_quality_data(start_date=start_date,
#                               end_date=end_date,
#                               time_resolution=time_resolution,
#                               geo_resolution=geo_resolution,
#                               state_filter=state_filter)
#   return(full_join(weather_df,aq_df,by=c("date","county_fips")))
# }

# get_air_quality_data<-function(start_date,end_date,time_resolution,geo_resolution,state_filter=NULL){
#   base_url <- "https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?"
#   start_date<-format(as.Date(start_date), "%d%b%Y")
#   end_date<-format(as.Date(end_date), "%d%b%Y")
#   url<-paste0(base_url,"endDate=",end_date,"&startDate=",start_date)
#   url<-paste0(url,"&detector=nodetectordetector")
#   url<-paste0(url,"&percentParam=noPercent")
#   url<-paste0(url,"&userId=",userid)
#   url<-paste0(url,"airQualityParameterName=pm2.5-24hr")
#   url<-paste0(url,"&multiStratVal=County")
#   url<-paste0(url,"&stationAggregateFunc=avg")
#   url<-paste0(url,"&datasource=airquality")
#   url<-paste0(url,"&timeAggregateFunc=avg")
#   url<-paste0(url,"&aqtTarget=TimeSeries")
#   if (time_resolution=="daily"){
#     url<-paste0(url,"&timeResolution=daily")
#   } else if (time_resolution == "weekly"){
#     url<-paste0(url,"&timeResolution=weekly")
#   } else if (time_resolution == "monthly"){
#     url<-paste0(url,"&timeResolution=monthly")
#   } else if (time_resolution == "yearly"){
#     url<-paste0(url,"&timeResolution=yearly")
#   } else {
#     stop(paste0("invalid time resoution:",time_resolution))
#   }
#   url<-paste0(url,"&rowFields=timeResolution")
#   if (geo_resolution =="county"){
#     df = data.table::fread("data/air_quality_county_weight_mapping.csv")
#     if (!is.null(state_filter)){
#       df<-df[df$state %in% state_filter,]
#     }
#     stations = unique(df$station_county_name)
#     print(paste0("Filtering to ", paste0(stations,collapse=", ")))
#     
#     url <- paste0(url,paste0("&County=", gsub(" ","%20",tolower(stations)), collapse = ""))
#   } else if (geo_resolution =="state"){
#     stop("Air quality data is not available for states.")
#   }
#   aq_df<-Rnssp::get_api_data(url)$timeSeriesData[,c("date","County_id","count")]
#   aq_df <- aq_df |> 
#     rename(station_county_name=County_id,air_quality=count)
#   
#   df_res<-inner_join(df[,c("county_fips","station_county_name","weight")],aq_df,by="station_county_name")
#   df_res <- df_res |>
#     group_by(county_fips,date) |>
#     summarise(weighted_air_quality = sum(air_quality * weight) / sum(weight), .groups = "drop")
#   
#   return(df_res |> select(county_fips,date,weighted_air_quality))
#   
#   
# }


# add_baselines <- function(df, n) {
#   
#   # Calculate rolling average per region
#   df <- df |>
#     arrange(region, date) |>
#     group_by(region) |>
#     mutate(rolling_avg_target = rollapply(target, width = n, FUN = mean, fill = NA, align = "right")) |>
#     ungroup()
#   daily_total <- df |>
#     group_by(date) |>
#     summarize(total_count = sum(target, na.rm = TRUE), .groups = "drop")
#   # Compute sum of count for each region over all dates
#   region_total <- df |>
#     group_by(region) |>
#     summarize(region_sum = sum(target, na.rm = TRUE), .groups = "drop")
#   
#   # Compute overall sum of count across all dates and regions
#   overall_sum <- sum(df$target, na.rm = TRUE)
#   
#   # Merge total count per date
#   df <- df |>
#     left_join(daily_total, by = "date") |>
#     left_join(region_total, by = "region") |>
#     mutate(rescaled_aggregate_trend = total_count * (region_sum / overall_sum)) |>
#     select(-total_count, -region_sum) # Remove intermediate columns
#   return(df)
# }

# add_day_of_week<-function(data,date_col){
#   data$dayofweek<-wday(data[[date_col]])
#   return(data)
# }
# 
# 
# 
# 
# aggregate_by_week<-function(data,cat_cols){
#   last_date <- max(data[[date_col]])
#   df_weekly <- data |>
#     mutate(
#       Week_Ending = ceiling_date(.data[[date_col]] - days(wday(last_date) %% 7), unit = "week") + days(wday(last_date) %% 7 - 1)
#     )
#   
#   # Keep only complete weeks (exactly 7 days per week)
#   complete_weeks <- df_weekly |>
#     group_by(Week_Ending) |>
#     filter(n_distinct(.data[[date_col]]) == 7) |>
#     ungroup()
#   
#   # Aggregate counts by Region and Week_Ending
#   weekly_summary <- complete_weeks |>
#     group_by(.data[[region_col]], countyfips,Week_Ending) |>
#     summarise(count = sum(count), .groups = "drop") |>
#     arrange(.data[[region_col]], Week_Ending) |>
#     rename(!!date_col:=Week_Ending) |>
#     ungroup()
#   data2<-weekly_summary
#   for (col in cat_cols){
#     data2[[paste0(col,"_id")]]=as.numeric(factor(data2[[col]]))
#     data2[[paste0(col,"_id2")]]=as.numeric(factor(data2[[col]]))
#   }
#   return(data2)
#   
# }


# make_timeseries_plots<-function(res_data,date_col = "date", use_prop=FALSE,add_temporal=TRUE,add_rolling=TRUE,add_rescaled=TRUE){
#   
#   groups <-res_data |> group_split(countyfips)
#   
#   plots = list()
#   
#   for (i in seq_along(groups)) {
#     group <- groups[[i]]
#     if (use_prop){
#       group$target=group$target/group$overall
#       group$predicted_median = group$predicted_median/group$overall
#       group$predicted_lower = group$predicted_lower/group$overall
#       group$predicted_upper = group$predicted_upper/group$overall
#       if (add_temporal){
#         group$predicted_median_temporal = group$predicted_median_temporal/group$overall
#         group$predicted_lower_temporal = group$predicted_lower_temporal/group$overall
#         group$predicted_upper_temporal = group$predicted_upper_temporal/group$overall
#       }
#       if (add_rolling){
#         group$rolling_avg_target = group$rolling_avg_target/group$overall
#       }
#       if (add_rescaled){
#         group$rescaled_aggregate_trend = group$rescaled_aggregate_trend/group$overall
#       }
#     }
#     #group_name <- paste0("location_",unique(group$countyfips))
#     group_name <- as.character(unique(na.omit(group$region)))
#     plt<-ggplot(group, aes(x = .data[[date_col]], y = cases)) +
#       geom_point(size=0.5) +
#       geom_line(aes(y=predicted_median,color='spatio-temporal'),linewidth=0.1) +
#       geom_ribbon(aes(ymin=predicted_lower,ymax=predicted_upper, fill='spatio-temporal'),alpha=0.3) + 
#       labs(title = group_name, x = "Date", y = ifelse(use_prop==TRUE, "Proportion of Visits", "Count"), fill="", color="") +
#       theme_minimal() + 
#       theme(legend.position = "bottom")
#     
#     
#     if (add_temporal){
#       plt<-plt+
#         geom_line(aes(y=predicted_median_temporal,color='temporal'),linewidth=0.1) +
#         geom_ribbon(aes(ymin=predicted_lower_temporal,ymax=predicted_upper_temporal, fill='temporal'),alpha=0.3)
#     }
#     if (add_rolling){
#       plt<-plt+
#         geom_line(aes(y=rolling_avg_target,color="rolling"),linewidth=0.1)
#     }
#     if (add_rescaled){
#       plt<-plt+
#         geom_line(aes(y=rescaled_aggregate_trend,color="aggregate"),linewidth=0.1)
#     }
#     plt<-plt+scale_color_manual(values = c("spatio-temporal" = "red", "temporal" = "blue","rolling"="green","aggregate"="black"))
#     plt<-plt+scale_fill_manual(values = c("spatio-temporal" = "red", "temporal" = "blue"))
#     # folder=paste0("figures/figs_",gsub("%20","_",target),"_",gsub("-","",sd),"_to_",gsub("-","",ed),"_",family)
#     # if (!dir.exists(folder)) {
#     #   dir.create(folder, recursive = TRUE)
#     # }
#     #ggsave(paste0(folder,"/plot_", group_name, ".png"), plt, width = 8, height = 3)
#     plots[[group_name]] <- plt
#   }
#   
#   return(plots)
#   
#   
# }




