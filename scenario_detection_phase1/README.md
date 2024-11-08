# Scenario Detection Shiny Application Documentation

## Overview

This app is designed for exploring state-level health data alerts. Users can visualize alerts based on demographic and geographic information, filtering and analyzing data through interactive maps and tables. The alerts are computed using either parametric (Gaussian) or non-parametric (percentile) detection methods, helping identify significant deviations in emergency department visit counts across regions and demographics.

## User Interface

The interface consists of several panels for selecting filters, displaying interactive widgets (maps and tables), and generating stateful reports. Below is a breakdown of key components.

### Filter Controls

- **State Selection**: Select the state to analyze.
- **Date**: Choose the end date for analysis. The baseline period is the 3-day guard-banded 28 days prior to this date.
- **Normalization Option**: Choose between raw counts ("Count") or percentages ("Percent") for visualization.
- **Detection Method**: Choose between parametric (Gaussian) or non-parametric (percentile) methods for alert detection.
- **Baseline/Test Date Record Minimum**: Set thresholds for minimum records in the baseline or test date to filter low-count data.

### Help Popups

Popover-style help tools are places next to each interactive widget in order to provide a summary of its purpose and intended interpretation. The help can be accessed by clicking on the encircled "?" icons and their respective corresponding text.

## Data Processing Pipeline

The app processes data in several steps before visualizations are displayed:

1. **Data Retrieval**: The app retrieves emergency department visit records from the NSSP-ESSENCE API.
2. **Data Transformation**: Categories such as age groups, regions, and diagnostic codes are processed and transformed for consistency and analysis.
3. **p-value/Percentile Computation**: Alerts are computed for each region and demographic based on statistical analysis:
   - **Parametric**: Z-scores and p-values are calculated assuming a Gaussian distribution.
   - **Non-parametric**: Percentiles are computed to assess the relative count of events.

## Methods

### Gaussian (Parametric) Method

The Gaussian method assumes a normal distribution for the data. For each feature (such as region or age group), the app calculates **Z-scores** by comparing the test date's count to the mean and standard deviation of the baseline period. The resulting **p-value** indicates the likelihood that the observed count is part of the baseline distribution. Lower p-values (e.g., <0.01) signify anomalies or alerts.

**Key Interpretation**:
- **p-value < 0.01**: Alert (significant deviation).
- **p-value < 0.05**: Warning (moderate deviation).
- **p-value ≥ 0.05**: Normal (no significant deviation).

### Non-parametric (Percentile) Method

The non-parametric method does not assume any specific distribution. Instead, it calculates the **percentile** of the test date's count relative to the baseline counts. This method is useful for data distributions that are unknown or skewed. Computed percentiles are presented as p-values for consistency with the Gaussian method's interpretation.

**Key Interpretation**:
- **Percentile ≥ 0.01**: Alert (high deviation).
- **Percentile ≥ 0.05**: Warning (moderate deviation).
- **Percentile < 0.05**: Normal (no significant deviation).

## Visualization Components

### Overview

Each map and table in the app can be considered **semi-independently** and represents the application of a Naive Bayes statistical independence assumption over the analyzed fields. Alerts are computed for each feature (e.g., region, age group, sex) with respect to all records that match the current filters applied. If no filters are selected, each widget (map or table) performs an **independent analysis** of the observed-versus-expected rates of each feature element without considering other feature categories.

### 1. Region Map (Leaflet)

Displays alerts geographically by state and county, with each county color-coded based on alert levels:

- **Red**: Alert
- **Yellow**: Warning
- **Blue**: Normal
- **White**: No records available

**Interpreting the Map**:
- Hover over a county to view specific p-values or percentiles, along with event counts or percentages.
- Clicking a county filters the data for that region.

### 2. Age Group Table

This table displays alerts stratified by age group.

**Interpreting the Table**:
- Rows represent age groups.
- p-values or percentiles are color-coded to indicate alert severity.

### 3. Sex Table

Similar to the age group table but stratified by sex.

### 4. Syndromic Category Table

Displays alerts based on diagnostic categories, including **Sub-syndrome**, **CCDD Category**, **ICD Diagnosis**, and **CCSR Category**.

**Interpreting the Table**:
- Alerts are sorted by the chosen method (Gaussian or percentile).

### 5. Line-Level Table

Displays individual patient records for further examination.

**Interpreting the Table**:
- Shows diagnostic codes, age groups, sex, and regions for each record under the current filter set from the "Alerts Explorer Tab."

## Alerts

The alerting system helps users quickly identify anomalies in their emergency department visit data. Based on p-values or percentiles, the app flags alerts (red), warnings (yellow), and normal conditions (blue), enabling users to drill down into regions, demographics, and syndromic categories and quickly backtrack in order to rapidly investigate the contents of their data.

---
