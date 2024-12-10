---
title: "ScenarioDetection_docs"
author: "Josh KImrey"
date: "`r Sys.Date()`"
output: html_document
---

<!-- 
© 2024 The Johns Hopkins University Applied Physics Laboratory LLC
Development of this software was sponsored by the U.S. Government under 
contracts no. 75D30120C07643, 75D30122C15442, 75D30124C19958
-->

<!--
#----------------------------------------------------
# Scenario Detection App - Phase 1
# Authors:
#   Joshua Kimrey
#   Catherine Schwartz
#   Roseric Azondekon
#   Michael Sheppard
#----------------------------------------------------
-->

# Scenario Detection Shiny Application Documentation

---

## Overview

This app is designed for exploring state-level health data alerts. Users can visualize alerts based on demographic and geographic information, filtering and analyzing data through interactive maps and tables. The alerts are computed using either parametric (Gaussian) or non-parametric (percentile) detection methods, helping identify significant deviations in emergency department visit counts across regions and demographics.

---

## User Interface

The interface consists of several panels for selecting filters, displaying interactive widgets (maps and tables), and generating stateful reports. Below is a breakdown of key components.

### Filter Controls

- **State Selection**: Select the state to analyze.
- **Date**: Choose the end date for analysis. The baseline period is the 3-day guard-banded 28 days prior to this date.
- **Normalization Option**: Choose between raw counts ("Count") or percentages ("Percent") for visualization.
- **Detection Method**: Choose between parametric (Gaussian) or non-parametric (percentile) methods for alert detection.
- **Baseline/Test Date Record Minimum**: Set thresholds for minimum records in the baseline or test date to filter low-count data.

### Help Popup

A popover-style help tool provides a detailed summary of the app’s purpose and how data is processed. The help can be accessed by clicking on the "App Summary" text.

---

## Data Processing Pipeline

The app processes data in several steps before visualizations are displayed:

1. **Data Retrieval**: The app retrieves emergency department visit records from the NSSP-ESSENCE API.
2. **Data Transformation**: Categories such as age groups, regions, and diagnostic codes are processed and transformed for consistency and analysis.
3. **p-value/Percentile Computation**: Alerts are computed for each region and demographic based on statistical analysis:
   - **Parametric**: p-values are calculated assuming a Gaussian distribution.
   - **Non-parametric**: Percentiles are computed to assess the relative count/percent of events.

---

## Methods

### Gaussian (Parametric) Method

The Gaussian method assumes that the data follows a normal distribution. For each feature (such as region or age group), the app calculates **p-values** by comparing the test date's count/percent to the mean and standard deviation of the 28-day baseline period. The resulting **p-value** indicates how likely it is that the observed count/percent is part of the baseline distribution. Lower p-values (e.g., <0.01) signify anomalies or alerts.

**Key Interpretation**:
- **p-value < 0.01**: Alert (significant deviation).
- **p-value < 0.05**: Warning (moderate deviation).
- **p-value ≥ 0.05**: Normal (no significant deviation).

### Non-parametric (Percentile) Method

The non-parametric method does not assume any specific distribution. Instead, it calculates the **percentile** of the test date's count/percent relative to the 28-day baseline counts/percents. This method is useful when the data distribution is unknown or skewed (e.g., data which is sparse or is likely to contain anomalies within the baseline period). To remain consistent with the Gaussian Method representation, the computed percentiles are presented as p-values. That is, a count or percentage that is computed to be in the 0.95 percentile will be depicted as having a p-value of 0.05.

**Key Interpretation**:
- **Percentile ≥ 0.01**: Alert (high deviation).
- **Percentile ≥ 0.05**: Warning (moderate deviation).
- **Percentile < 0.05**: Normal (no significant deviation).

---

## Visualization Components

### Overview

Each map and table in the app is designed to be considered **semi-independently**. Alerts are computed for each feature (e.g., region, age group, sex) with respect to all records that match the current filters applied. If no filters are selected, each widget (map or table) performs an **independent analysis** of the observed-versus-expected rates of each feature element, without considering other feature categories.

For example:
- If no filters are applied, the p-values or percentiles displayed in the "Region p-values" map are computed using all records for each region, irrespective of other features like age group or sex.
- Similarly, in the "Age p-values" table, each age group is analyzed independently based on the total number of records for that age group, without considering regional or sex-specific information.

As filters are applied (e.g., selecting a particular region or age group), the app dynamically updates the analysis for each feature, recalculating the observed-versus-expected rates for the filtered subset of records. This allows for more granular exploration of the data.

### 1. Region Map (Leaflet)

Displays alerts geographically by state and county. Each county is color-coded based on the computed alert levels:

- **Red**: Alert
- **Yellow**: Warning
- **Blue**: Normal
- **White**: No records available

**Interpreting the Map**:
- Hover over a county to see the specific p-value or percentile, along with event counts or percentages.
- Clicking on a county filters the data for that region.
- The "No records available" category identifies both regions that are not reporting or that have no records satisfying the current filters within the examined time period.

### 2. Age Group Table

This table displays alerts stratified by age group.

**Interpreting the Table**:
- Rows represent age groups.
- p-values or percentiles are color-coded to indicate alert severity (red, yellow, blue).
- You can select a row to filter the data by age group.

### 3. Sex Table

Similar to the age group table, but stratified by sex.

### 4. Syndromic Category Table

Displays alerts based on diagnostic categories, including:

- **Sub-syndrome**: Categories derived from pre-defined weights over terms found in the Chief Complaint field.
- **CCDD Category**: Community-defined and refined Syndromic categories for broader analysis.
- **ICD Diagnosis**: 3-digit coded medical diagnoses.
- **CCSR Category**: Clinical classification software categories for medical procedures.

**Interpreting the Table**:
- Alerts are sorted by the chosen method (Gaussian or percentile).
- Clicking a category filters the data further.

### 5. Line-Level Table

Displays individual patient records for further examination.

**Interpreting the Table**:
- Patient records are displayed with diagnostic codes, age groups, sex, and regions.
- The current filter set from the "Alerts Explorer Tab" is applied.
- Useful for detailed, case-level analysis.

---

## Alerts

The alerting system helps users quickly identify anomalies in health data. Based on p-values or percentiles, the app flags alerts (red), warnings (yellow), and normal conditions (blue). Users can drill down into regions, demographics, and syndromic categories to investigate potential health issues.
