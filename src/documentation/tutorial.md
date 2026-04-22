<!--
# (c) 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
-->

<a id="top"></a>

This tutorial walks through a complete example workflow: creating a query, optionally adding covariates, fitting a model, and exploring the results.

### Tutorial Navigation

- [Part 1. Create a Query](#tutorial-create-a-query)
- [Part 2. Fit a Model](#tutorial-fit-a-model)
- [Part 3. Explore the Results](#tutorial-explore-results)

### Before You Begin

Before starting the tutorial, make sure you have:

- valid ESSENCE credentials
- the app running from the project root
- the sample covariate file `sample_data/static_covariates.csv`
- time to run a small weekly example query and model fit

This tutorial assumes a weekly query.

<h3 id="tutorial-create-a-query">Part 1. Create a Query</h3>

#### 1A. Set the Geographic Scope

This step defines the counties that will be included in the analysis.

1. Click the state selector and type `DE`, `DC`, and `VA`.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step1a_add_states.PNG" alt="Selecting multiple states in the Data Loader" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T1.</strong> State selector used to add `DE`, `DC`, and `VA`.</figcaption>
</figure>

2. Click `Customize Counties` to open the county selector.
3. Select a county on the map. Selected counties turn orange.

You can also search for a county by typing its name into the selected-counties box and choosing it from the dropdown list.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step1a_select_county.PNG" alt="Selecting an initial county in the county selector" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T2.</strong> County selector used to choose an initial county.</figcaption>
</figure>

The county selector supports several selection methods:

- click counties individually to select or deselect them
- use `Select All` or `Deselect All` to change the whole current view
- use the map drawing tools to draw a polygon or circle and select all enclosed counties
- use `Add Neighbors` to expand the current selection to bordering counties
- edit the text list of selected counties directly

4. After selecting an initial county, click `Add Neighbors` to bring in surrounding counties and build a contiguous region.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step1a_add_neighbors.PNG" alt="Using Add Neighbors in the county selector" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T3.</strong> County selector used to expand the region with `Add Neighbors`.</figcaption>
</figure>

5. Continue until you have a contiguous region of roughly 10 to 50 counties.
6. Click `Done` at the bottom of the county selector to return to the main app.

Note: The app can run with more counties, but both querying and model fitting become slower as the geographic set grows. Very large county sets can also increase the chance of convergence issues.

#### 1B. Set the Other Query Parameters

This step defines the time window and syndrome outcome for the example query.

1. Adjust the `Date Range`.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step1b_select_dates.PNG" alt="Selecting the weekly date range for the tutorial query" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T4.</strong> Date-range controls used for the tutorial query.</figcaption>
</figure>

2. Leave the `Time Resolution` at weekly for this example.
3. Open the `Target Outcome` dropdown, delete the default text, type `ILI`, and select `ILI CCDD v1`.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step1b_select_ccdd.PNG" alt="Selecting ILI CCDD v1 as the target outcome" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T5.</strong> Target-outcome selector used to choose `ILI CCDD v1`.</figcaption>
</figure>

For weekly queries, it is best to choose a Sunday start date and a Saturday end date so that all weeks are complete.

Practical guidance:

- for default weekly models, include at least 8 weeks of data
- for seasonal weekly models, 2 or more years of data are often more appropriate
- daily resolution is available, but daily models are more complex because they include day-of-week structure

The default target type is `Chief Complaint and Discharge Diagnosis Category`, but you can also switch to syndrome or sub-syndrome using the radio buttons.

4. Click `Query ESSENCE`.

Expected result: you should now see a populated data table in the `Data Loader` tab, similar to Figure T6.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step1b_load_data.PNG" alt="Loaded tutorial data in the Data Loader table" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T6.</strong> `Data Loader` table after the tutorial query has been loaded.</figcaption>
</figure>

#### 1C. Optionally Add Covariates

This step adds a few external county-level features that can be used in modeling and visualization. For more detail, see the `Adding Covariates` section in the main documentation window.

This step is optional.

1. Click `Add Covariates`.
2. Browse to the file `sample_data/static_covariates.csv`.
3. In the region-column selector, choose `fips_code` so the covariates can be joined using 5-digit county FIPS codes.

If you are using time-dependent covariates, also select the date column from the uploaded dataset.

4. Click `Unselect all`.
5. Add back only the fields `population`, `st_name_abbrev`, and `medn_hhi_acs_22`.
6. Click `Filter` so the covariate table is restricted to counties present in the main query.

If orange warnings appear, the selected features contain missing values. If you want to use those features in the model, click `Impute Missing`, choose an imputation method, and click `Run`.

If you only want the covariates for visualization and do not intend to use them in the model, imputation may not be necessary.

7. Click `Add` to join the covariates to the main dataset.

Expected result: the displayed table should now include the added covariate columns.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step1c_add_covariates.PNG" alt="Adding tutorial covariates from the sample data file" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T7.</strong> Covariate-upload workflow using `sample_data/static_covariates.csv`.</figcaption>
</figure>

#### 1D. Save or Reload the Query

1. To save the current query and data, click `Save Query`.
2. Choose a save location and confirm.

You can also export only the table data by clicking `Download to CSV`.

A saved query uses the `.bsm_query` suffix and is a zip archive containing:

- an `.rds` file with the data
- a `.json` file with the query settings

3. To reload the query later, open the `Load Saved Query` tab and select the saved file.

At this point, the full query setup should be restored. Next, move to the `INLA Model` tab.

Expected result: you should now have either a reusable saved query file or a fully configured query ready for modeling.

<h3 id="tutorial-fit-a-model">Part 2. Fit a Model</h3>

#### 2A. Set the Model Parameters

This step configures a simple model fit with a short forecast horizon.

1. Set `Number of forecasts` to `2`.
2. Set the `Distributional Family` to `Binomial`.

For most routine short-term analyses, forecasts are most useful over the next 2 to 3 weeks, though some seasonal settings may support longer horizons.

Family guidance:

- use `Poisson` or `Negative Binomial` for count models
- use `Binomial` or `Beta-Binomial` for proportion models

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step2a_inla_model.PNG" alt="INLA Model tab configured for the tutorial example" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T8.</strong> `INLA Model` tab configured for the tutorial example.</figcaption>
</figure>

#### 2B. Define the Model Formula

This is an intentionally advanced example. Most new users should start with the default model and only move to a custom formula when they need more control. For more detail, see the `Advanced Model Specification` section in the main documentation window.

1. Expand `Show Generic Formula` so you can see the formula currently implied by the app settings.
2. Copy that formula.
3. Switch `Model Specification` to `Custom Model Formula`.
4. After switching modes, you should see a blank custom-formula view like Figure T9.

If you see red validation text before pasting, that is expected while the formula box is empty or invalid.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step2a_inla_model_custom_before.PNG" alt="Custom model formula view before pasting a formula" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T9.</strong> `Custom Model Formula` view after switching modes but before pasting and editing the formula.</figcaption>
</figure>

5. Paste the copied formula into the formula box.

6. Add the covariate `medn_hhi_acs_22` to the end of the formula using:

```r
+ medn_hhi_acs_22
```

After adding the covariate, the formula should look similar to Figure T10.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step2a_inla_model_custom_after.PNG" alt="Custom model formula with an added covariate" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T10.</strong> `Custom Model Formula` view after adding `medn_hhi_acs_22`.</figcaption>
</figure>

7. If the model includes a spatiotemporal term, choose an adjacency matrix.
8. For this example, select `Proximity`.

`Mobility` uses a movement-based adjacency definition, while `Proximity` uses geographic adjacency.

9. Click `Run Model`.

When fitting completes, the model summary appears on the right.

Note: Small problems may fit in seconds, while larger combinations of counties and dates can take minutes.

Expected result: you should now see a fitted model summary in the `INLA Model` tab, similar to Figure T11.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step2a_inla_model_custom_fitted.PNG" alt="Fitted custom model summary for the tutorial example" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T11.</strong> Fitted model summary after the tutorial model run completes.</figcaption>
</figure>

#### 2C. Save or Reload the Model

1. To save the fitted model, click `Save Model`.
2. Choose a save location and confirm.

A saved model uses the `.bsm_model` suffix and is a zip archive containing:

- an `.rds` file with the fitted model object and processed data
- a `.json` file with the settings needed to restore the modeling session

3. To reopen a saved model later, use the `Load Saved Model` tab and select the saved file.

Now move to the `Visualization` tab.

Expected result: you should now have a fitted model that can be reopened later without rerunning the fit.

<h3 id="tutorial-explore-results">Part 3. Explore the Results</h3>

The examples in Part 3 use synthetic data for illustration, but they are designed to closely resemble the structure and behavior of the fitted results you already created in Parts 1 and 2. No changes are needed before following these visualization steps.

#### 3A. Region-Wide Map

This step uses the map to compare values across counties for a selected date.

1. Open `Region-Wide Map`.
2. In the available-feature list, select the covariate `Median HHI Acs 22`.

The map should update to color counties by median household income.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step3a_map.PNG" alt="Region-Wide Map displaying a covariate in the tutorial" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T12.</strong> `Region-Wide Map` used to display the selected covariate.</figcaption>
</figure>

3. Hover over counties to view the county name and displayed value.
4. Open the feature filters, select `Posterior Quantile`, and then choose `Quantile q=0.5 (proportion)`.

This displays the posterior median proportion for the selected outcome at the current date.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step3a_map_median.PNG" alt="Region-Wide Map displaying the posterior median proportion" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T13.</strong> `Region-Wide Map` after switching to the posterior median proportion feature.</figcaption>
</figure>

5. Use the sparkline in the upper-right area to examine change over time.
6. Drag the red dashed line to change the displayed date.
7. Hover over counties to inspect values.
8. Click a county to open the inset time-series view for that county.
9. Expand `Advanced map settings` if you want to adjust the colorbar range, legend placement, or color palette.

#### 3B. Prediction Time Series Plots

This step uses the dedicated prediction view to compare model output across selected counties.

1. Open `Prediction Time Series Plots`.
2. Use the left panel to choose:

- the scale
- the credible interval width
- one or more regions

3. Change the interval from 95% to 50% and observe that the intervals become narrower.
4. Add a few more counties and note that additional panels appear automatically.
5. Turn on the shared y-axis toggle to compare counties more easily.
6. Hover over the plot to inspect observed values, posterior summaries, and interval bounds.
7. To save the plot, hover in the upper-right corner of the plot area and click the camera icon provided by Plotly.

Figure T14 shows an example prediction-plot layout for this step.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step3b_prediction_ts.PNG" alt="Prediction time series plots for the tutorial example" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T14.</strong> `Prediction Time Series Plots` used to compare selected counties.</figcaption>
</figure>

#### 3C. Other Time Series Plots

This step uses the more flexible time-series view for comparing stored features over time.

1. Open `Other Time Series Plots`.
2. Use the feature selector to choose one or more stored features.
3. Use the feature filters to narrow the available list by scale or type if needed.

For example, filtering to posterior quantiles on the count scale makes it easier to compare precomputed quantile curves on a shared plot.

4. Add one or more regions.
5. Use the separate-feature-panels option if you want to split features into separate panels instead of overlaying them.
6. Use the y-axis toggle as needed.

This tab is more flexible than the prediction tab, but mixed scales can be harder to interpret if they are plotted together.

Figure T15 shows an example `Other Time Series Plots` view with selected stored features.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step3c_other_ts.PNG" alt="Other Time Series Plots for the tutorial example" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T15.</strong> `Other Time Series Plots` used to compare selected stored features.</figcaption>
</figure>

#### 3D. Posterior Data

This step uses the table view for filtering and exporting fitted results.

1. Open `Posterior Data`.
2. Use the feature selector in the left panel to add `St Name Abbrev`.

You can use the `Other` and `Covariate` filters to locate it more quickly.

3. Use the per-column filters in the table:

- numeric and date columns allow min and max filtering
- categorical columns allow value filtering using checkboxes

For example, filtering the state-abbreviation column will restrict the table to rows from that state.

4. Click `Download CSV` to export the displayed table.

The exported CSV should reflect the current visible table, including selected features and active filters. Figure T16 shows the table view used in this step.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step3d_posterior_table.PNG" alt="Posterior Data table for the tutorial example" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T16.</strong> `Posterior Data` table used with selected features and filters.</figcaption>
</figure>

#### 3E. Add a New Output Feature

This step creates a new stored posterior summary that can be reused across visualization tabs. For more detail, see the `Posterior Summaries and Added Features` section in the main documentation window.

1. Open `Add Feature`.
2. In the feature-type menu, choose `Exceedance Probability`.
3. Choose the `proportion` scale.
4. Set a threshold such as `0.05`.

An exceedance probability is the posterior probability that the estimated value is greater than the chosen threshold.

5. Review or edit the automatically generated name and description.
6. Click `Add Feature`.

The new feature should appear in the table and become available on the other visualization tabs.

7. To confirm this, return to `Region-Wide Map` and select the new feature.
8. If you want to remove a feature later, return to `Add Feature` and use `Open delete feature window`.

Figure T17 shows the `Add Feature` workflow for this step.

At this point, you have completed a full workflow: building a query, optionally adding covariates, fitting a model, and exploring multiple types of output.

Expected result: you should now be able to inspect maps, plots, tables, and newly created output features across the app.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-tutorial-screenshots/step3e_add_feature.PNG" alt="Add Feature tab used to create an exceedance probability" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure T17.</strong> `Add Feature` tab used to create an exceedance probability feature.</figcaption>
</figure>

### Next Steps

You can now repeat the same workflow with a different syndrome, stay with the default model for a simpler first analysis, or create additional posterior summaries for reporting and monitoring. If you expect to revisit the analysis, save the query and model so you can return without rebuilding the workflow from scratch.
