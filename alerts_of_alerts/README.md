# Alerts of Alerts Shiny Application

## Overview

The **Alerts of Alerts** app is an interactive web-based tool for testing and visualizing both temporal and spatial alerts related to selected U.S. states and regions, based on various syndromes or health conditions. The app uses regression and Exponentially Weighted Moving Average (EWMA) methods for alert detection in syndrome surveillance and incorporates spatial autocorrelation testing to identify spatial clusters of alerts.

The target users are epidemiologists, public health officials, and analysts working with data from the National Syndromic Surveillance Program (NSSP) ESSENCE platform to monitor and analyze health trends.

This app includes methods and visualization concepts originally developed by Michael Sheppard.

## Features

- **Statewide Temporal Diagnostics**:
  - Visualizes three state-level metrics for syndrome severity:
    - Percent of emergency department (ED) visits.
    - Number of alerting regions (e.g., counties).
    - Number of regions showing increasing case counts based on Generative Additive Modeling (GAM).

- **Spatial Visualization**:
  - Choropleth maps show region-level (e.g., county) data for the selected date, including:
    - P-values for temporal alerts.
    - Spatial distribution of alerts and trends.
    - Results of statistical tests for spatial clustering, using Global Moran’s I and Join Count tests.

- **Interactive Analysis**:
  - Users can filter and analyze data by selecting specific states, categories, and dates.
  - The app provides the option to download reports summarizing the results.

## User Interface

### Main Components:

1. **Navigation Panel**:
   - Select options for U.S. state, CC & DD (Chief Complaint and Diagnosis Discharge) category, and a date range for analysis.
   - Download a report summarizing the results.

2. **App Summary**:
   - A brief description of the app's purpose and functionality.

3. **Temporal Alerts Time Series**:
   - Displays a time series plot showing three state-level diagnostics of syndrome burden over time. Includes statistical tests for alerts using EWMA and regression methods.

4. **Spatial Distribution Maps**:
   - **P-value Map**: Shows spatial distribution of p-values for temporal alerts.
   - **Alerts Map**: Displays spatial distribution of alerts across regions.
   - **Trends Map**: Highlights regions with increasing, stable, or decreasing trends in syndrome counts based on GAM analysis.

### Instructions:

1. **Select a State and Category**:
   - Choose a U.S. state and a CC & DD category, such as COVID-19-specific syndromes.

2. **Select a Date Range**:
   - Set the start and end dates for analysis.

3. **Load Data**:
   - Click the "Load Data" button to retrieve data from the NSSP ESSENCE platform and generate visualizations.

4. **Interactive Features**:
   - Click on specific dates in the temporal plot to update the spatial maps.
   - Click on counties in the maps to view detailed time series of alerts for that county.

5. **Download Report**:
   - After selecting your inputs, click "Download Report" to generate a detailed report in HTML format.

## Advanced Features

### Temporal and Spatial Alert Detection

- **Temporal Alert Detection**:
  - Uses regression and EWMA methods to detect state-level alerts, classified into:
    - **Red (Alert)**: Statistically significant increase in syndrome severity.
    - **Yellow (Warning)**: Moderate increase.
    - **Blue (None)**: No significant increase.

- **Spatial Autocorrelation Testing**:
  - **Global Moran’s I**: Detects clustering of regions with similar alert levels.
  - **Join Count Test**: Identifies clustering of binary regions (e.g., regions with/without alerts).

### GAM Trend Estimation

- The app uses **Generative Additive Modeling (GAM)** to estimate case count trends (increasing, stable, or decreasing). The analysis includes only regions where more than 20% of the selected dates have non-zero case counts. Sparse regions are excluded. The trend results are displayed on the **Spatial Distribution of Trends** map.

## Notes and Limitations

- **Simultaneous Process-Induced Errors**: The app may crash if interacted with while processing previous inputs.
- **Performance**: Processing large datasets, especially for regions with many counties, may be slow, particularly during GAM-based trend analysis.
  
