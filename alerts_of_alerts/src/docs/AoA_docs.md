---
title: "Alerts of Alerts Shiny Application Documentation"
output: html_document
---

<!-- 
© 2024 The Johns Hopkins University Applied Physics Laboratory LLC
Development of this software was sponsored by the U.S. Government under contracts no. 75D30120C07643, 75D30122C15442 
-->

# **Alerts of Alerts Shiny Application Documentation**

## **Overview**

The **Alerts of Alerts** app is a web-based interactive tool developed to test for and visualize both temporal and spatial alerts for selected U.S. states and regions based on selected syndromes or health conditions. It provides visualizations of statistical alerts for syndrome surveillance using a combination of regression and Exponentially Weighted Moving Average (EWMA) alert detection methods. The app also incorporates spatial autocorrelation testing to detect spatial clusters of alerts.

The app is designed for epidemiologists, public health officials, and analysts working with data from the National Syndromic Surveillance Program (NSSP) ESSENCE platform to monitor and analyze health trends.

Many of the methods and visualization concepts included within the app were originally developed by Michael Sheppard.

## **Features**

- **Statewide Temporal Diagnostics**: Visualize three state-level temporal diagnostics for syndrome severity, including:
  - Percent of emergency department (ED) visits.
  - Number of alerting regions (e.g., counties).
  - Number of regions estimated to have increasing case counts based on Generative Additive Modeling (GAM).
  
- **Spatial Visualization**: The app allows users to visualize region-level (e.g., county) data on choropleth maps for the selected date, displaying:
  - P-values for temporal alerts.
  - Spatial distribution of alerts and increasing trends.
  - Results of statistical tests for spatial clustering using Global Moran’s I and Join Count tests.

- **Interactive Analysis**: Select specific states, categories, and dates to filter and analyze the data. The app provides the ability to download reports summarizing the results.

## **User Interface**

### Main Components:

1. **Navigation Panel**: 
   - **Selectable Options**: Allows users to choose a U.S. state, a CC & DD (Chief Complaint and Diagnosis Discharge) category, and a date range for the analysis. Users can also download a report summarizing the results.
   
2. **App Summary**: A brief description of the app's purpose and functionality.
   
3. **Temporal Alerts Time Series**: A time series plot that shows three state-level diagnostics of syndrome burden over time, with statistical tests for alerts based on EWMA and regression methods.

4. **Spatial Distribution Maps**:
   - **P-value Map**: Displays the spatial distribution of p-values for temporal alerts.
   - **Alerts Map**: Shows the spatial distribution of alerts across the selected regions.
   - **Trends Map**: Displays regions with increasing, stable, or decreasing trends in syndrome counts based on GAM analysis.

### Instructions:

1. **Select a State and Category**:
   - Choose a U.S. state from the dropdown list.
   - Select a CC & DD category for the analysis, such as COVID-19-specific syndromes.
   
2. **Select a Date Range**:
   - Choose the start and end dates for the analysis. The date range determines the time period over which the app will retrieve and analyze data.

3. **Load Data**:
   - Click the "Load Data" button to retrieve data from the NSSP ESSENCE platform and generate the visualizations.

4. **Interactive Features**:
   - The temporal time series plot is interactive, allowing users to click on specific dates to update the spatial maps.
   - Clicking on counties in the spatial maps brings up a detailed time series of alerts and trends for that county.
   
5. **Download Report**:
   - After selecting your state, category, and date range, you can download a comprehensive report in HTML format by clicking the "Download Report" button.

# **Advanced Features**

### Temporal and Spatial Alert Detection

- **Temporal Alert Detection**: Uses a combination of regression and EWMA methods to detect state-level temporal alerts. Alerts are classified into:
  - **Red (Alert)**: Statistically significant increase in syndrome severity.
  - **Yellow (Warning)**: Moderate increase in syndrome severity.
  - **Blue (None)**: No significant increase.

- **Spatial Autocorrelation Testing**:
  - **Global Moran’s I**: A statistical test for detecting clustering of regions with similar alert levels (either high or low p-values).
  - **Join Count Test**: A statistical test for detecting spatial clustering of binary regions (e.g., regions with or without alerts).

### GAM Trend Estimation

The app uses **Generative Additive Modeling (GAM)** to estimate whether case counts in individual regions are increasing, stable, or decreasing over time. The GAM model treats the count data as a binomial process, with the fitted values representing the probability of an increase in case counts. The app considers only regions where more than 20% of the selected dates contain non-zero case counts for trend estimation, and regions with sparse data are excluded from the analysis.

The model computes derivative estimates of the trend (increasing, stable, or decreasing), and the results are displayed in the **Spatial Distribution of Trends** map, color-coded by these trend classifications.

## **Notes and Limitations**

- **Simultaneous process-induced errors**: The application is likely to crash due to uninformative errors if it is interacted with while it is completing the processing required of a previous user-interaction.
- **Performance**: Processing large datasets, especially for regions with many counties, may take some time, particularly for the GAM-based trend analysis.

## **Conclusion**

The **Alerts of Alerts** app provides a comprehensive tool for monitoring and analyzing syndromic surveillance data with a focus on detecting temporal and spatial health alerts. Through interactive time series and spatial visualizations, users can identify potential public health concerns and detect spatial clustering of alerts across regions.
