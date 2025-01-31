# © 2024 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under 
# contracts no. 75D30120C07643, 75D30122C15442, 75D30124C19958

#----------------------------------------------------
# Scenario Detection App - Phase 1
# Authors:
#   Joshua Kimrey
#   Catherine Schwartz
#   Roseric Azondekon
#   Michael Sheppard
#----------------------------------------------------

#--------------Helper functions to process diagnostic fields------------------

# Process 'Category' fields
process_and_overwrite_category_column <- function(df, column_name) {
  # Extract the symptoms vector from the data frame
  symptoms_vector <- df[[column_name]]
  # Separate 'none' and non-'none' indices
  non_none_indices <- which(symptoms_vector != "none")
  non_none_values <- symptoms_vector[non_none_indices]
  # Process non_none_values
  non_none_values = gsub("^;|;$", "", non_none_values)
  processed_symptoms = strsplit(non_none_values, ";")
  # Reinsert processed codes back into the original vector
  symptoms_vector[non_none_indices] <- processed_symptoms
  # Overwrite the column in the data frame with processed codes
  df[[column_name]] <- symptoms_vector
  return(df)
}

# Helper function (for use in remove_older_CCDD_versions() and interactive 
# diagnostic filtering) to drop non-matching strings
drop_non_matching <- function(vector, subset_list) {
  intersect(vector, subset_list)
}

# Helper function to remove older versions of CCDD Categories
remove_older_CCDD_versions <- function(CCDD_column) {
  # Store inital column structure
  CCDD_column_orig <- CCDD_column
  # Unnest and keep unique sub-elements
  CCDD_column_orig_unique_flat <- unique(unlist(CCDD_column))
  # Convert each string to lowercase
  CCDD_column <- tolower(CCDD_column_orig_unique_flat)
  # Remove spaces from each string
  CCDD_column <- gsub(" ", "", CCDD_column)
  # Append "v1" to strings lacking "v<integer>"
  for (i in seq_along(CCDD_column)) {
    if (!grepl("v[0-9]", CCDD_column[i])) {
      CCDD_column[i] <- paste0(CCDD_column[i], "v1")
    }
  }
  # Drop characters appearing after "v<integer>"
  CCDD_column <- gsub("v[0-9]\\K.*", "", CCDD_column, perl = TRUE)
  # Create a dictionary associating original strings to their processed counterparts
  CCDD_dictionary = sort(setNames(CCDD_column_orig_unique_flat, CCDD_column))
  # Step 5: Extract and keep the highest version for each entity
  filtered_CCDD_dictionary <- list()
  for (name in names(CCDD_dictionary)) {
    entity <- gsub("v[0-9]", "", name)
    version <- as.numeric(gsub(".*v", "", name))
    if (entity %in% gsub("v[0-9]","", names(filtered_CCDD_dictionary))) {
      filtered_CCDD_dictionary[paste0(entity,"v",version-1)] = NULL
      filtered_CCDD_dictionary[name] = CCDD_dictionary[[name]]
    } else {
      filtered_CCDD_dictionary[name] = CCDD_dictionary[[name]]
    }
  }
  # Apply the function to each vector in the list
  CCDD_column <- lapply(CCDD_column_orig, drop_non_matching, unlist(unname(filtered_CCDD_dictionary)))
  return(CCDD_column)
}

# Process 'C_DiagnosisCode_ICD10_Flat' field
process_and_overwrite_codes_column <- function(df, column_name) {
  # Extract the codes vector from the data frame
  codes_vector <- df[[column_name]]
  # Identify NA values and those already set as "none"
  na_indices <- is.na(codes_vector) | codes_vector == "none"
  non_na_indices <- !na_indices
  # Extract non-NA values
  non_na_values <- codes_vector[non_na_indices]
  # Extract all unique ICD Diagnosis elements from text
  processed_codes <- str_extract_all(non_na_values, "[A-Z][0-9]{2}")
  processed_codes <- map(processed_codes, ~unique(.x))
  # Replace any NA values in processed_codes with "none"
  processed_codes <- map_if(processed_codes, is.null, ~"none")
  # Reinsert processed codes back into the original vector
  codes_vector[non_na_indices] <- processed_codes
  codes_vector[na_indices] <- "none"
  # Overwrite the column in the data frame with processed codes
  df[[column_name]] <- codes_vector
  return(df)
}

# Helper to load ICD code to Diagnosis description mapping
read_in_codes_description <- function(input) {
  # Retrieve DD mapping
  dds <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/icdDiagnosis" %>%
    get_api_data(profile = myProfile) %>%
    pluck("values") %>%
    pull("display") %>%
    try(silent = TRUE)
  
  # Split the vector into code and description
  split_items <- strsplit(dds, " - ")
  
  # Extract code and description separately
  dd_codes <- substr(sapply(split_items, "[[", 1), 1, 3)  # Assumes code is always 3 characters long
  dd_descriptions <- sapply(split_items, "[[", 2)
  
  # Create a named list
  icd_list <- setNames(dd_descriptions, dd_codes)
  icd_list['none'] = 'none'
  
  # Retrieve CCSR mapping
  ccsr_cats <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/icdCCSR" %>%
    get_api_data(profile = myProfile) %>%
    pluck("values")
  
  # Extract code and description separately
  ccsr_codes <- ccsr_cats$value
  ccsr_descriptions <- ccsr_cats$display
  
  # Create a named list
  ccsr_list <- setNames(ccsr_descriptions, ccsr_codes)
  ccsr_list['none'] = 'none'
  
  return(list(icd_list, ccsr_list))
}

# helper reactive to pull fips associated to selected state
fips_for_url <- function(state) {
  fips <- county_sf %>%
    rename(state_fips = STATEFP) %>%
    left_join(state_helper, by = "state_fips") %>%
    filter(state_name == state) %>%
    pull(GEOID) %>%
    as.character() %>%
    paste0(., collapse = "&facilityfips=")
  return(fips)
}

# Read in data details from API and process/parse for assignment to master df
readin_and_process_master_df <- function(state, date) {
  url <-paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/",
               "dataDetails/csv?geography=",
               as.character(state_helper[state_helper$state_name == state,]$state_abbr),
               "&datasource=va_hosp&medicalGroupingSystem=essencesyndromes",
               "&userId=1&percentParam=noPercent&aqtTarget=DataDetails",
               "&geographySystem=hospitalstate&detector=probrepswitch",
               "&timeResolution=daily&startDate=",
               format(date %m-% days(38), "%d%b%Y"), "&endDate=",
               format(date, "%d%b%Y"), "&facilityfips=", fips_for_url(state),
               "&hasBeenE=1&field=EssenceID&field=Date&field=HospitalRegion",
               "&field=AgeGroup&field=Sex&field=SubCategory_flat",
               "&field=CCDDCategory_flat&field=C_DiagnosisCode_ICD10_Flat",
               "&field=FacilityCountyFIPS&field=ICD_CCSR_flat",
               "&field=ChiefComplaintParsed")
  withProgress(message = "Reading in and processing data:", value = 0, {
    incProgress(0.0, detail = "Reading in data (this may take a few seconds)...")
    master <- myProfile$get_api_data(url, fromCSV =TRUE)
    master$Date = as.Date(master$Date, format = "%m/%d/%Y")
    incProgress(0.14, detail = "Parsing SubCategory...")
    master = process_and_overwrite_category_column(master, "SubCategory_flat")
    incProgress(0.14, detail = "Parsing CCDD Category...")
    master = process_and_overwrite_category_column(master, "CCDDCategory_flat")
    incProgress(0.14, detail = "Removing lower CCDD Category versions...")
    master[["CCDDCategory_flat"]] = remove_older_CCDD_versions(master[["CCDDCategory_flat"]])
    incProgress(0.14, detail = "Parsing ICD Diagnosis ...")
    master = process_and_overwrite_codes_column(master, "C_DiagnosisCode_ICD10_Flat")
    incProgress(0.14, detail = "Parsing CCSR ...")
    master = process_and_overwrite_category_column(master, "ICD_CCSR_flat")
    incProgress(0.16, detail = "Completed!")
  })
  return(master)
}

#----------p-value computation preparation and execution pipeline-------------

# Helper function to produce a crosstab dataframe of <feature>-by-Date 
create_crosstab_dataframe <- function(df, column_name, all_dates) {
  # Create a temporary dataframe with only 'Date' and the specified column
  temp_df <- df %>% select(Date, !!sym(column_name))
  # Check if the specified column contains lists and unnest if necessary
  if (is.list(temp_df[[column_name]])) {
    temp_df <- temp_df %>% unnest(c(!!sym(column_name)))
  }
  # Create a dataframe with all unique (Date, <column name>) permutations to 
  # ensure inclusion in the final crosstab
  permutations_df <- expand.grid(all_dates, unique(temp_df[[column_name]]))
  # Assign column names
  colnames(permutations_df) <- c("Date", column_name)
  # Compute the crosstab of Date by the specified column's values
  result_df <- temp_df %>%
    group_by(Date, !!sym(column_name)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ungroup() %>%
    right_join(permutations_df, by = c("Date", column_name)) %>%
    replace_na(list(count = 0)) %>%
    pivot_wider(names_from = !!sym(column_name), values_from = count, values_fill = list(count = 0))
  # Convert tibble to dataframe
  result_df <- as.data.frame(result_df)
  # Reorder rows by ascending Date
  result_df <- result_df[order(match(result_df$Date, all_dates)),]
  # Set 'Date' as row names
  row.names(result_df) <- result_df$Date
  result_df$Date <- NULL  # Remove the 'Date' column after setting it as row names
  return(as.data.frame(t(result_df)))
}

# Helper function to drop guardband dates from feature-wise dataframes 
drop_guardband_dates <- function(df, test_date) {
  # Find the index of the given date
  date_index <- which(colnames(df) == test_date)
  # Calculate the indexes of the three preceding dates
  preceding_indexes <- (date_index - 2):(date_index - 1)
  # Check if there are at least three preceding dates
  if (min(preceding_indexes) < 1) {
    stop("There are not enough preceding dates to drop.")
  }
  # Drop the columns at the preceding indexes
  df <- df[,-preceding_indexes]
  
  # Recalculate the index of the given date after dropping preceding columns
  date_index <- which(colnames(df) == test_date)
  
  # Drop the columns following the test_date column
  df <- df[,1:date_index]
  return(df)
}

# Apply minimum baseline count filtering
filter_feature_date_df_baseline <- function(df, min_records_baseline) {
  df <- df[rowSums(df[, -ncol(df)])>min_records_baseline-1,]
}

# Apply minimum test_date count filtering
filter_feature_date_df_test_date <- function(df, min_records_testdate) {
  df <- df[df[,ncol(df)]>min_records_testdate-1,]
  return(df)
}

# Helper function to normalize counts by day (=percent)
normalize_df <- function(filtered_df, feature_date_df, min_records_baseline, min_records_testdate, filter=FALSE) {
  # Generate the frequency table
  date_table <- table(filtered_df$Date)
  # Check and match the columns of feature_date_df with date_table
  matching_dates <- colnames(feature_date_df) %in% names(date_table)
  # Create a vector of date counts that aligns with the columns of feature_date_df
  aligned_date_counts <- date_table[match(colnames(feature_date_df), names(date_table))]
  # Replace NA in aligned_date_counts with 1 to avoid division by NA
  aligned_date_counts[is.na(aligned_date_counts)] <- 1
  
  if (filter==TRUE) {
    # Apply minimum count baseline filter
    feature_date_df <- filter_feature_date_df_baseline(feature_date_df, min_records_baseline)
    # Apply minimum count test_date filter
    feature_date_df <- filter_feature_date_df_test_date(feature_date_df, min_records_testdate)
  }
  # Perform the division only on matching columns
  feature_date_df[, matching_dates] <- sweep(feature_date_df[, matching_dates], 2, aligned_date_counts[matching_dates], "/")
  # Replace NAs resulting from the division with 0s
  feature_date_df[is.na(feature_date_df)] <- 0
  feature_date_df = feature_date_df*100.
  return(feature_date_df)
}

# Helper function to compute p-values
compute_p_values <- function(df, test_date, column_name, normalize) {
  if (nrow(df) > 0) {
    # Slice out baseline dates
    df_samples <- df[, -ncol(df)]
    # slice test date
    df_test <- df[, ncol(df)]
    # Compute vector mean
    means <- rowMeans(df_samples)
    # Compute vector standard deviation
    stds <- apply(df_samples, 1, sd)
    # Compute Z-scores for each feature
    zs_all <- (df_test - means)/stds
    # Convert Z to one-sided p
    p_values <- round(pt(zs_all, df=ncol(df_samples)-1, lower.tail = FALSE),3)
    # Set NaN p-values to 1
    p_values[is.nan(p_values)] <- 1
    # Set N = 0 or Percent = 0 to have p-value = 1.
    p_values[df_test == 0] <- 1
    if (normalize == "count") {
      p_df <- data.frame(column_name = names(p_values), p = round(p_values, 3), N = df_test)
      p_df <- setNames(p_df, c(column_name, "p", "N"))
    } else {
      p_df <- data.frame(column_name = names(p_values), p = p_values, Percent = signif(df_test,2))
      p_df <- setNames(p_df, c(column_name, "p", "Percent"))
    }
  } else {
    p_df = NULL
  }
  return(p_df)
}

# nonparametric
compute_percentiles <- function(df, test_date, column_name, normalize) {
  if (nrow(df) > 0) {
    # Slice baseline dates
    df_samples <- df[, -ncol(df)]
    # Assign filtered, last column as test date data
    df_test <- df[, ncol(df)]
    # Compute percentiles for each feature
    percentiles <- sapply(1:nrow(df_samples), function(i) {
      if (all(is.na(as.numeric(df_samples[i, ])))) {
        # If all values are NA, set to NA
        NA
      } else if (all(as.numeric(df[i,]) == df[i,1])) {
        # If all values are equal, set to 1.0
        1.0
      } else {
        # else, run the empirical cumulative distribution function routine
        round(1.0 - (ecdf(as.numeric(df_samples[i, ]))(as.numeric(df_test[i]))),3)
      }
    })
    
    # Set N = 0 or Percent = 0 to have percentile = 1.0.
    percentiles[df_test == 0] <- 1.0
    
    percentiles <- setNames(percentiles, rownames(df_samples))
    
    if (normalize == "count") {
      percentile_df <- data.frame(column_name = names(percentiles), percentile = percentiles, N = df_test)
      percentile_df <- setNames(percentile_df, c(column_name, "p", "N"))
    } else {
      percentile_df <- data.frame(column_name = names(percentiles), percentile = percentiles, Percent = signif(df_test, 2))
      percentile_df <- setNames(percentile_df, c(column_name, "p", "Percent"))
    }
  } else {
    percentile_df <- NULL
  }
  return(percentile_df)
}

# p-value computation which calls sequence of processing/computation helpers
p_value_pipeline <- function(df, all_dates, column_name, test_date, normalize, 
                             method, min_records_baseline, min_records_testdate, filter=FALSE) {
  # Crosstab
  feature_date_df <- create_crosstab_dataframe(df, column_name, all_dates)
  # Drop guardband dates
  feature_date_df <- drop_guardband_dates(feature_date_df, test_date)
  # Normalize
  if (normalize == "percent") {
    last_count_col = feature_date_df[, ncol(feature_date_df)]
    names(last_count_col) <- rownames(feature_date_df)
    feature_date_df <- normalize_df(df, feature_date_df, min_records_baseline, min_records_testdate, filter)
    last_count_col <- last_count_col[rownames(feature_date_df)]
  } else if (filter == TRUE) {
    # Apply minimum count baseline filter
    feature_date_df <- filter_feature_date_df_baseline(feature_date_df, min_records_baseline)
    # Apply minimum count test_date filter
    feature_date_df <- filter_feature_date_df_test_date(feature_date_df, min_records_testdate)
  }
  
  if (method == "percentile") {
    # Compute percentiles
    p_df = compute_percentiles(feature_date_df, test_date, column_name, normalize)
  } else {
    p_df = compute_p_values(feature_date_df, test_date, column_name, normalize)
  }
  if (normalize == "percent") {
    p_df <- cbind(p_df, N = as.numeric(last_count_col))
  }
  if (!is.null(p_df)) {
    p_df = p_df[order(p_df$p),]
  }
  if (column_name == 'Sex') {
    p_df$Sex <- factor(p_df$Sex, levels = sex_labels[1:3])
    # Sort the dataframe by the 'Sex' column
    p_df <- p_df %>%
      arrange(Sex)
  }
  if (column_name == 'AgeGroup') {
    p_df <- p_df[order(match(p_df[["AgeGroup"]], age_labels)), ]
  }
  return(p_df)
}

df_pipeline <- function(df, all_dates, column_name, normalize, method, min_records_baseline, min_records_testdate, filter=FALSE) {
  
  # Crosstab
  feature_date_df <- create_crosstab_dataframe(df, column_name, all_dates)
  
  # Normalize
  if (normalize == "percent") {
    last_count_col = feature_date_df[, ncol(feature_date_df)]
    names(last_count_col) <- rownames(feature_date_df)
    feature_date_df <- normalize_df(df, feature_date_df, min_records_baseline, min_records_testdate, filter)
    last_count_col <- last_count_col[rownames(feature_date_df)]
  } else if (filter == TRUE) {
    # Apply minimum count baseline filter
    feature_date_df <- filter_feature_date_df_baseline(feature_date_df, min_records_baseline)
    # Apply minimum count test_date filter
    feature_date_df <- filter_feature_date_df_test_date(feature_date_df, min_records_testdate)
  }
  
  concat_df = data.frame()
  for (date in tail(all_dates, 7)) {
    temp_df = drop_guardband_dates(feature_date_df, as.Date(date))
    if (method == 'gauss') {
      temp_df = compute_p_values(temp_df, date, column_name, normalize)
    } else {
      temp_df = compute_percentiles(temp_df, date, column_name, normalize)
    }
    
    if (!is.null(temp_df)) {
      concat_df = rbind(concat_df, temp_df)
    }
  }
  
  if (nrow(concat_df)==0) {
    return(list(NULL, NULL, NULL))
  } else {
    if (column_name %in% c('SubCategory_flat', 'CCDDCategory_flat', 'C_DiagnosisCode_ICD10_Flat')) {
      concat_df = concat_df[concat_df[[column_name]] != "none",]
      temp_df = temp_df[temp_df[[column_name]] != "none",]
    }
    if (column_name == 'ICD_CCSR_flat') {
      concat_df = concat_df[concat_df[[column_name]] != "Unmapped",]
      temp_df = temp_df[temp_df[[column_name]] != "Unmapped",]
    }
    
    if (normalize == 'count') {
      Ns = data.table(temp_df)$N
    } else {
      Percents <- paste0(data.table(temp_df)$Percent, "%")
    }
    if (method == 'gauss') {
      ps = data.table(temp_df)$p
      concat_df <- concat_df %>%
        mutate(color = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c("red", "yellow", "blue")))
    } else {
      Percentiles = data.table(temp_df)$Percentile
      concat_df <- concat_df %>%
        mutate(color =  cut(Percentile, breaks = c(-Inf, 95, 99, Inf), labels = c("blue", "yellow", "red")))
    }
    
    # Return the correct set of values as a list
    if (normalize == 'count') {
      if (method == 'gauss') {
        return(list(as.data.table(concat_df), Ns, ps))
      } else {
        return(list(as.data.table(concat_df), Ns, Percentiles))
      }
    } else {
      if (method == 'gauss') {
        return(list(as.data.table(concat_df), Percents, ps))
      } else {
        return(list(as.data.table(concat_df), Percents, Percentiles))
      }
    }
  }
}

p_loop_over_all_features <- function(p_dfs, df, all_dates, normalize, method, min_records_baseline, min_records_testdate) {
  withProgress(message = ifelse(method=='gauss', "Computing p-values:", "Computing percentiles:"), value = 0, {
    incProgress(0, detail = 'Hospital Region')
    p_dfs$region <- p_value_pipeline(df, all_dates, 'HospitalRegion', all_dates[length(all_dates)], normalize, method, min_records_baseline, min_records_testdate)
    incProgress(0.14, detail = 'Age Group')
    p_dfs$age <- df_pipeline(df, all_dates, 'AgeGroup', normalize, method, min_records_baseline, min_records_testdate)
    incProgress(0.14, detail = 'Sex')
    p_dfs$sex <- df_pipeline(df, all_dates, 'Sex', normalize, method, min_records_baseline, min_records_testdate)
    incProgress(0.14, detail = 'SubCategory')
    p_dfs$subc <- df_pipeline(df, all_dates, 'SubCategory_flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.14, detail = 'CCDD Category')
    p_dfs$ccdd <- df_pipeline(df, all_dates, 'CCDDCategory_flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.14, detail = 'ICD Diagnosis')
    p_dfs$dd <- df_pipeline(df, all_dates, 'C_DiagnosisCode_ICD10_Flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.15, detail = 'CCSR Category')
    p_dfs$ccsr <- df_pipeline(df, all_dates, 'ICD_CCSR_flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.15, detail = "Completed!")
  })
}

p_loop_over_diagnostic_features <- function(p_dfs, df, all_dates, normalize, method, min_records_baseline, min_records_testdate) {
  withProgress(message = ifelse(method=='gauss', "Computing p-values:", "Computing percentiles:"), value = 0, {
    incProgress(0.14, detail = 'SubCategory')
    p_dfs$subc <- df_pipeline(df, all_dates, 'SubCategory_flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.14, detail = 'CCDD Category')
    p_dfs$ccdd <- df_pipeline(df, all_dates, 'CCDDCategory_flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.14, detail = 'ICD Diagnosis')
    p_dfs$dd <- df_pipeline(df, all_dates, 'C_DiagnosisCode_ICD10_Flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.15, detail = 'CCSR Category')
    p_dfs$ccsr <- df_pipeline(df, all_dates, 'ICD_CCSR_flat', normalize, method, min_records_baseline, min_records_testdate, filter=TRUE)
    incProgress(0.15, detail = "Completed!")
  })
}

# function for filtering master df in response to user-selections
filtered <- function (master, filters, p_dfs, selected_state, selected, record_number=FALSE) {
  withProgress(message = "Applying data filters...", value = 0, {
    filtered_df <- master$df
    total_steps <- 7  # Total number of conditions to filter on
    step <- 0
    
    if (!is.null(filters$region)) {
      setProgress(value = step / total_steps, detail = "Filtering by Hospital Region...")
      filtered_df <- filtered_df[filtered_df$HospitalRegion == filters$region, ]
      step <- step + 1
    }
    if (!is.null(filters$age)) {
      setProgress(value = step / total_steps, detail = "Filtering by Age Group...")
      filtered_df <- filtered_df[filtered_df$AgeGroup == filters$age, ]
      step <- step + 1
    }
    if (!is.null(filters$subc)) {
      setProgress(value = step / total_steps, detail = "Filtering by Subcategory...")
      temp_col <- lapply(filtered_df$SubCategory_flat, drop_non_matching, filters$subc)
      filtered_df <- filtered_df[temp_col == filters$subc, ]
      step <- step + 1
    }
    if (!is.null(filters$ccdd)) {
      setProgress(value = step / total_steps, detail = "Filtering by CCDD Category...")
      temp_col <- lapply(filtered_df$CCDDCategory_flat, drop_non_matching, filters$ccdd)
      filtered_df <- filtered_df[temp_col == filters$ccdd, ]
      step <- step + 1
    }
    if (!is.null(filters$dd)) {
      setProgress(value = step / total_steps, detail = "Filtering by Diagnosis Code...")
      temp_col <- lapply(filtered_df$C_DiagnosisCode_ICD10_Flat, drop_non_matching, filters$dd)
      filtered_df <- filtered_df[temp_col == filters$dd, ]
      step <- step + 1
    }
    if (!is.null(filters$ccsr)) {
      setProgress(value = step / total_steps, detail = "Filtering by ICD CCSR...")
      temp_col <- lapply(filtered_df$ICD_CCSR_flat, drop_non_matching, filters$ccsr)
      filtered_df <- filtered_df[temp_col == filters$ccsr, ]
      step <- step + 1
    }
    if (!is.null(filters$sex)) {
      setProgress(value = step / total_steps, detail = "Filtering by Sex...")
      temp_col <- lapply(filtered_df$Sex, drop_non_matching, filters$sex)
      filtered_df <- filtered_df[temp_col == filters$sex, ]
      step <- step + 1
    }
    
    # Final progress update
    setProgress(value = 1, detail = "Filtering complete!")
  })
  
  master$filtered_df = filtered_df
  
  if (record_number==FALSE) {
    p_loop_over_all_features(p_dfs, master$filtered_df, master$all_dates, selected$normalize, selected$method, filters$min_records_baseline, filters$min_records_testdate)
  } else {
    p_loop_over_diagnostic_features(p_dfs, master$filtered_df, master$all_dates, selected$normalize, selected$method, filters$min_records_baseline, filters$min_records_testdate)
  }
  
  if (!is.null(p_dfs$region)) {
    selected_state$df_sf = st_as_sf(right_join(right_join(p_dfs$region,
                                                          master$region_fips, by="HospitalRegion"),
                                               selected_state$county_sf[,c("NAME","GEOID","geometry")],
                                               by = c("FacilityCountyFIPS" = "GEOID")))
  } else {
    region_fips_copy = master$region_fips
    region_fips_copy$p = rep(NA, nrow(region_fips_copy))
    selected_state$df_sf = st_as_sf(right_join(region_fips_copy,
                                               selected_state$county_sf[,c("NAME","GEOID","geometry")],
                                               by = c("FacilityCountyFIPS" = "GEOID")))
  }
}

appendSelectionHistory <- function(selection_history, filters) {
  selection_history$history <- c(selection_history$history, 
                                 list(list(MinRecordsBaseline = filters$min_records_baseline,
                                           MinRecordsTestDate = filters$min_records_testdate,
                                           HospitalRegion = filters$region, 
                                           AgeGroup = filters$age,
                                           Sex = filters$sex,
                                           SubCategory_flat = filters$subc,
                                           CCDDCategory_flat = filters$ccdd,
                                           C_DiagnosisCode_ICD10_Flat = filters$dd,
                                           ICD_CCSR_flat = filters$ccsr)))
}
