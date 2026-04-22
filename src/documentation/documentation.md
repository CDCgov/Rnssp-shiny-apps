<!--
# (c) 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
-->

## Bayesian Spatiotemporal Modeling Documentation

<a id="top"></a>

<h2 id="basic-user-guide">1. Basic User Guide</h2>

### What the App Does

The application supports a typical workflow for Bayesian spatiotemporal surveillance analysis:

1. Load data from ESSENCE or from a saved query file.
2. Fit a default model or load a saved model.
3. Review results in maps, time-series plots, and tables.
4. Export data products for reporting or follow-up analysis.

### Main Areas of the Interface

The user interface (UI) contains three tabs to allow users to complete the modeling workflow:

- `Data Loader`
- `INLA Model`
- `Visualization`
Note that the visualization tab is disabled until a model has been fitted.

There is also a menu accessed using the gear icon with documentation and toggles to customize the appearance of the UI.

### Tooltips and Display Options

Most labels with an information icon have a tooltip. By hovering over the tooltip you can display additional information about the purpose of each UI element. Tooltips are enabled by default.

- Use `Hide Tooltips` in the gear menu if you want a cleaner interface while working.
- Use `Show Tooltips` in the gear menu to bring the help text back.
- Use the light and dark mode toggle in the gear menu to switch the display theme.

### Typical Basic Workflow

For a first analysis, the recommended path is:

1. Open `Data Loader`.
2. Build a query or load a saved query.
3. Review the loaded data table.
4. Open `INLA Model`.
5. Fit the default model.
6. Open `Visualization`.
7. Review the map, prediction plots, and posterior data table.
8. Export tables or save the model if you want to return later.

### Recommended Defaults

If you are new to the app, these defaults are a good place to start:

- use `Poisson` when modeling counts
- use `Binomial` when modeling proportions
- start with `Default Model`
- begin with a modest forecast horizon so results appear quickly and are easy to inspect

<h3 id="loading-data">Loading Data</h3>

Use the `Data Loader` tab to prepare the source dataset.

#### Query ESSENCE

The `Query ESSENCE` panel lets you choose:

- geographic resolution
- states and counties
- date range
- time resolution
- diagnostic grouping

The current interface is configured for county-level querying.  The empty `Data Loader` state is shown in Figure 1.

When data are loaded, the table includes a small set of core fields used throughout the app. `date` is the observation date, `region` is the region label used in displays, `countyfips` is the county FIPS identifier, `target` is the number of ED visits matching the selected syndrome or diagnosis, and `overall` is the total number of ED visits used as the reference count for proportion-based modeling.

For weekly data:

- choose a Sunday start date
- choose a Saturday end date

That helps avoid partial weeks in the downloaded series.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/data_loader_no_data.PNG" alt="Data Loader tab before data are loaded" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 1.</strong> The <code>Data Loader</code> tab before a query is run.</figcaption>
</figure>

#### Choosing States and Counties

You can begin by selecting one or more states, then use `Customize Counties` to refine the county set within those states. The county selector used for this step is shown in Figure 2.

The county selector supports several ways to refine the geography. You can click individual counties, draw a boundary to select a group, add neighboring counties to an existing selection, edit the text list of selected counties directly, or use the available buttons to select all or deselect all counties in the current view.

The app gives feedback on county selection:

- it warns if too many counties are selected
- it warns if the selected counties are not connected

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/county_selector.PNG" alt="County selector modal" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 2.</strong> County selector modal used to refine the county set after choosing one or more states.</figcaption>
</figure>

#### Running the Query

After you click `Query ESSENCE`, the app retrieves the data from ESSENCE using the user-provided credentials. This generates a table with columns for date, region, county FIPS, target, and overall representing the emergency department visit counts for the requested syndrome or diagnosis and across all syndromes or diagnoses. When loading is complete, the data table appears in the main panel, as shown in Figure 3.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/data_loader_with_data.PNG" alt="Data Loader tab with loaded data table" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 3.</strong> The <code>Data Loader</code> tab after a successful ESSENCE query has completed.</figcaption>
</figure>

#### Loading a Saved Query

If you already have a saved query, use `Load Saved Query` instead of submitting a new ESSENCE request. This restores the previously saved dataset and the key query settings.  

#### Basic Data Actions

After data are loaded, you can:

- inspect the table in the app
- add covariates for later modeling and visualization; see [Adding Covariates](#adding-covariates)
- download the retrieved data as CSV
- save the query for later reuse

<h3 id="fitting-a-default-model">Fitting a Default Model</h3>

Once the data has been loaded, you can use the `INLA Model` tab for model fitting.

For basic use, the easiest workflow is:

1. leave `Model Specification` on `Default Model`
2. choose the distribution family
3. set the forecast horizon
4. click `Run Model`

For a first analysis, start with `Poisson` when modeling counts and `Binomial` when modeling proportions. The forecast horizon controls how many future time steps the model should estimate beyond the observed data, so for weekly data it is the number of future weeks and for daily data it is the number of future days.

The default model is intended to be the starting point for most users, and Figure 4 shows the typical default-model setup.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/inla_model_default.PNG" alt="INLA Model tab configured for a default model" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 4.</strong> Default-model workflow in the <code>INLA Model</code> tab.</figcaption>
</figure>

### Loading a Saved Model

If a model has already been fit, you can use `Load Saved Model` instead of fitting again. This is useful when you want to reopen a previously analyzed dataset and go directly to results.

<h3 id="reviewing-results">Reviewing Results</h3>

Once a model is available, the `Visualization` area becomes active.

<h3 id="choosing-a-visualization">Choosing a Visualization</h3>

Use the visualization tabs for different purposes:

| View | Best use |
| --- | --- |
| `Region-Wide Map` | Scan spatial patterns for one selected date |
| `Prediction Time Series Plots` | Compare forecasts across selected regions |
| `Other Time Series Plots` | Compare additional stored features over time |
| `Posterior Data` | Search, filter, and export fitted values in table form |

#### Region-Wide Map

Use this tab to look at one feature across all regions for a selected date.

- the sparkline above the map controls the selected date. You can drag the red dashed line to select a different date.
- clicking a region opens a popup time series for that same feature

This is a good first place to look for geographic patterns. A typical map view is shown in Figure 5.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/viz_map.PNG" alt="Visualization tab region-wide map" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 5.</strong> Region-wide map for a selected feature and date.</figcaption>
</figure>

#### Prediction Time Series Plots

Use this tab to compare predicted trajectories across selected regions.

- choose count (number of visits) or proportion (proportion of visits with the selected diagnosis) scale
- choose a credible interval
- select one or more regions

An example of the prediction-plot view is shown in Figure 6.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/viz_pred_ts.PNG" alt="Prediction time series plots" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 6.</strong> Prediction time-series plots for selected regions.</figcaption>
</figure>

#### Other Time Series Plots

Use this tab when you want to compare additional stored features over time, especially after you have added covariates or other custom model outputs.

If you want to create new stored summaries first, see [Posterior Summaries and Added Features](#posterior-summaries-and-added-features).

#### Posterior Data

Use this tab when you want the fitted values in table form.

You can:

- filter rows and columns
- search the table
- download the displayed data

Figure 7 shows the posterior table view.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/viz_post_tab.PNG" alt="Posterior data table" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 7.</strong> Posterior data table with filters and selected features.</figcaption>
</figure>

### Saving and Exporting

Common basic exports are:

- downloaded data from `Data Loader`
- downloaded processed model data from `INLA Model`
- downloaded posterior tables from `Visualization`
- saved query files
- saved model files
- figures

[Back to top](#top)

<h2 id="advanced-user-guide">2. Advanced User Guide</h2>

### When to Use Advanced Features

Advanced users may want to perform additional customization for the model and/or outputs. The application also allows users to:

- add external covariates
- move beyond the default model specification
- generate and inspect more specialized posterior summaries such as posterior means, quantiles, confidence intervals, and exceedance probabilities
- filter available columns and feature types when customizing what appears in tables, maps, and plots
- fine-tune plots and displayed features
- save work in reusable formats for later sessions


<h3 id="adding-covariates">Adding Covariates</h3>

After loading base query data, you can click `Add Covariates`.

Supported upload formats include:

- `.csv`
- `.xlsx`
- `.xls`
- `.parquet`

When importing covariates, you can specify:

- the region column
- the date column
- which feature columns to keep

You can join covariates:

- by region only
- by date only
- by both region and date

The covariate loader also supports:

- filtering the uploaded data to the dates and or regions present in the query data
- optional missing-data imputation
- previewing the imported table before adding it

#### Imputation Options

If the uploaded covariate data contain missing values, the app offers several imputation choices, including:

- mean or median across the full dataset
- mean or median within each date
- mean or median across neighboring regions
- mean or median across neighboring regions within each date

Neighbor-based methods use physical adjacency.

<h3 id="advanced-model-specification">Advanced Model Specification</h3>

The `INLA Model` tab provides three model specification modes:

1. `Default Model`
2. `Customize Components`
3. `Custom Model Formula`

The advanced user options are in `Customize Components` and `Custom Model Formula`. If you only need a starting point, see [Fitting a Default Model](#fitting-a-default-model).

### Customize Components

This mode lets you turn major model terms on or off and adjust selected hyperparameters without writing a formula manually.

The available components are:

- region random effect
- spatiotemporal component
- seasonal and temporal component

#### What the Main Model Terms Do

The following descriptions are intended to help you choose options, not to describe implementation details.

##### Intercept

The intercept is the baseline level of the model before additional structure is applied.

##### Region Random Effect

The region random effect allows each region to have its own baseline deviation from the overall level. This is useful when some counties are consistently higher or lower than others even after accounting for broader spatial and temporal structure.

##### Spatiotemporal Component

The spatiotemporal component allows nearby regions and adjacent time points to share information. This is often useful when neighboring counties move together over time.

You can choose:

- the neighborhood basis
  - physical adjacency
  - mobility adjacency
- the spatial model form
  - `besagproper`
  - `besag`
  - `bym`
- the temporal dependence used within the grouped spatial effect

In practice:

- physical adjacency is useful when geography is the main basis for local similarity
- mobility adjacency is useful when travel or mixing patterns are more relevant than direct borders

##### Seasonal and Temporal Component

The temporal component captures recurring or evolving patterns over time.

Depending on the time resolution, this can represent:

- weekly seasonal structure
- daily cyclical structure
- smoother temporal drift
- autoregressive dependence

This is useful when visits show regular weekly patterns, gradual changes, or serial dependence over time. 

##### Precision Hyperparameters

The advanced controls for precision affect how strongly the model is regularized because they define prior settings for model components. In general:

- stronger regularization tends to produce smoother, more conservative effects
- weaker regularization allows more flexibility but may fit noise more easily

If you are unsure, start with the defaults.

### Custom Model Formula

Choose `Custom Model Formula` when you want full control over the model structure.

This mode is useful when you want to:

- include specific covariates directly
- remove default terms
- prototype alternative random-effect structures
- reference graph-based spatial terms explicitly

For additional details on INLA formula syntax and latent model terms, see the official [R-INLA documentation](https://www.r-inla.org/documentation). The app shows the available numeric features that can be used in the formula. If the formula references `graph`, the interface also asks which adjacency matrix should be used. Figure 8 shows an example of a customized model setup.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/inla_model_custom.PNG" alt="Customized INLA model settings" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 8.</strong> Example of a customized model specification in the <code>INLA Model</code> tab.</figcaption>
</figure>

### Choosing a Distribution Family

The available families are:

- `poisson`
- `nbinomial`
- `binomial`
- `betabinomial`

As a general guide:

- Poisson and negative binomial are natural choices for counts.
- Binomial and beta-binomial are natural choices for proportions.
- Negative binomial and beta-binomial are useful when data are more variable than the simpler family can comfortably capture.

In many routine cases, Poisson and Binomial models also fit faster than Negative Binomial and Beta-Binomial models, which is another reason to start with the simpler family unless you need the additional flexibility.

<h3 id="advanced-visualization">Advanced Visualization</h3>

#### Region-Wide Map

The map includes advanced display settings under `Advanced map settings`.

These include:

- shared colorbar range across dates
- legend location
- color palette

Use a shared colorbar range when you want map colors to be comparable over time. Turn it off when you want each selected date to use its own local range.

#### Feature Filters

Several visualization tabs include feature filters. These let you narrow the available feature list by:

- scale
- feature type

Feature-type filtering can help you focus on:

- means
- posterior quantiles
- confidence intervals
- exceedance probabilities
- covariates
- identifier fields
- other stored features

#### Prediction and Other Time Series Plots

Advanced time-series use cases include:

- comparing multiple regions
- overlaying several features in one panel
- separating selected features into different panels
- fixing the y-axis across panels for easier comparison

Many advanced displays work from stored feature columns rather than recalculating summaries each time. In practice, this means that once a feature has been generated and stored, it can usually be reused across tables, maps, and time-series views without rerunning the model.

<h3 id="posterior-summaries-and-added-features">Posterior Summaries and Added Features</h3>

The `Add Feature` tab lets you create additional stored summaries without refitting the model.
Supported derived features include posterior means, quantiles, credible intervals, exceedance probabilities, and change probabilities over a user-defined lookback window.

Supported feature types include:

- posterior mean
- posterior quantile
- confidence interval
- exceedance probability

Figure 9 shows the `Add Feature` workflow for defining and storing a derived posterior summary.

<figure style="margin: 0.75rem 0 1rem 0;">
  <img src="/documentation-screenshots/viz_add_feat.PNG" alt="Add Feature tab with derived posterior feature options" style="display:block; width:60%; height:auto; border:1px solid #ddd;" />
  <figcaption><strong>Figure 9.</strong> The <code>Add Feature</code> tab used to create a derived posterior feature such as a quantile, interval, or exceedance probability.</figcaption>
</figure>

#### Exceedance Probability

Exceedance probability answers a question of the form:

"What is the posterior probability that the value is greater than a chosen threshold?"

This can be useful when you want to monitor whether counts or proportions are likely to exceed an operational benchmark.

<h3 id="saved-and-exported-files">Saved and Exported Files</h3>

#### Saved Query Files

Saved queries use the `.bsm_query` extension.

They are intended to preserve:

- the retrieved data
- the key query settings needed to restore the query interface

Use a saved query when you want to reproduce a data pull without rebuilding the setup manually.

Internally, a saved query is a zip archive containing an `.rds` file with the retrieved data and a `.json` file with the settings used to restore the query interface.

#### Saved Model Files

Saved models use the `.bsm_model` extension.

They are intended to preserve:

- the fitted model
- the processed data used with that model
- the key settings needed to restore the modeling session

Use a saved model when:

- fitting is expensive
- you want to share a prepared result
- you want to reopen a completed analysis and go directly to visualization

Internally, a saved model is a zip archive containing an `.rds` file with the fitted model object and processed data plus a `.json` file with the settings needed to restore the modeling session.

#### CSV Exports

CSV exports are useful for:

- external review
- reporting
- follow-up analysis outside the app

Be aware that different export buttons correspond to different stages of the workflow, so exported tables may not all contain the same columns.

[Back to top](#top)

<h2 id="tutorial">Tutorial</h2>

The step-by-step tutorial is now available in a separate window so it can stay visible while you work in the app.

Use the `Tutorial` button in the gear menu to open it, or use this link:

<p><a href="?view=tutorial" target="_blank" rel="noopener noreferrer">Open Tutorial in a Separate Window</a></p>

Once the tutorial window is open, use the `Download PDF` button at the top of the page to save a PDF copy through your browser's print-to-PDF dialog.

[Back to top](#top)

<h2 id="troubleshooting">Troubleshooting</h2>

Common issues and suggested checks are listed below.

### Documentation modal opens but images do not display

- make sure the app is running from the project root so the documentation screenshot resources can be found
- reopen the modal after restarting the app if documentation assets were added during the session

### Visualization tab is disabled

- the `Visualization` tab remains disabled until a model has been fitted or a saved model has been loaded
- confirm that data were loaded first and that model fitting completed successfully

### No data appear after submitting a query

- check that your ESSENCE credentials are valid
- confirm that the selected states, counties, date range, and syndrome options are not too restrictive
- for weekly data, make sure the selected dates align to full weeks

### Model fitting fails

- start with the default model before moving to customized components or a custom formula
- try `Poisson` for counts or `Binomial` for proportions if you are currently using a more complex family
- reduce the forecast horizon and rerun to see whether a smaller problem fits more reliably
- if a custom formula remains invalid, return to `Default Model` or copy the generic formula first and then make small edits

### Covariates do not join as expected

- check that the selected region column uses 5-digit county FIPS codes when joining by region
- check that the selected date column is in a valid date format when joining by date
- use the preview and filter steps in the covariate loader before adding the data

### Posterior summaries or added features do not appear in a view

- confirm that the feature was created successfully in `Add Feature`
- check the feature filters in the relevant visualization tab
- some views only display certain feature types or scales

[Back to top](#top)

<h2 id="developer-guide">3. Developer Guide</h2>

<h3 id="high-level-app-structure">High-Level App Structure</h3>

The application is a modular Shiny app.

Key files and folders include:

- `app.R`
- `src/00_setup.R`
- `src/helpers/`
- `src/modules/`
- `src/modules/viz/`
- `src/documentation/documentation.md`

Startup logic and dependency loading are handled through `app.R` and `src/00_setup.R`. Helper and module files are sourced recursively from `src/00_setup.R`.

### Main Shared Reactive Objects

The server in `app.R` uses several shared reactive containers:

- `dc`
  - data-loading configuration and shared data-loader state
- `cache_transitions`
  - values passed across modules when loading saved queries or saved models
- `im`
  - fitted model objects, processed data class, posterior data, forecast horizon, and feature metadata
- `results`
  - shared outputs such as the loaded source data

These are the main cross-module state objects to inspect before adding new behavior.

<h3 id="data-flow">Data Flow</h3>

The standard data flow is:

1. `Data Loader` retrieves ESSENCE data or loads a saved query.
2. Optional covariates are merged into the base dataset.
3. `INLA Model` preprocesses the data.
4. A model formula is generated or validated.
5. The fit is run through the `epistemic` package.
6. Stored posterior summary columns are added to the processed dataset.
7. Visualization modules consume the stored data and feature metadata.

The current design prefers storing derived columns once and reusing them across views instead of recalculating them in each tab.

### Important Modules

Core modules include:

- `src/modules/data_loader.R`
- `src/modules/covariate_selector.R`
- `src/modules/inla_model.R`
- `src/modules/documentation.R`
- `src/modules/extras_module.R`
- `src/modules/tooltip.R`

Visualization modules include:

- `src/modules/viz/viz.R`
- `src/modules/viz/regional_map_module.R`
- `src/modules/viz/time_series_module.R`
- `src/modules/viz/other_time_series_module.R`
- `src/modules/viz/posterior_info_module.R`
- `src/modules/viz/add_feature.R`
- `src/modules/viz/feature_selector_panel.R`

Location-selection logic lives under:

- `src/modules/location_selector/`

### Data Structures Used by Modeling

Model preprocessing produces an `epistemic` data-class object that the app then augments with metadata describing:

- region identifiers
- date identifiers
- core modeling columns
- covariate columns
- other supporting columns

Several downstream features depend on these classifications, especially:

- feature registration
- table labeling
- visualization eligibility
- saved model reload behavior

If you change preprocessing, review code that reads:

- `data_cls$id_columns`
- `data_cls$core_columns`
- `data_cls$covariate_columns`
- `data_cls$other_columns`

### Formula Construction and Model Terms

Formula assembly is centered in `src/modules/inla_model.R`.

Key functions include:

- `get_formula()`
- `build_region_random_effect()`
- `build_temporal_component()`
- `build_spatial_component()`
- `pretty_formula()`

Default parameter values are controlled through `MODEL_COMPONENT_DEFAULTS`.

If you change a default model option, check both:

- the UI defaults shown to users
- the programmatic defaults used when formula components are auto-generated

<h3 id="feature-metadata-and-stored-columns">Feature Metadata and Stored Columns</h3>

The app uses a shared feature catalog so that tables and plots can work from metadata rather than hard-coded column lists.

This feature store tracks information such as:

- feature ID
- label
- description
- feature type
- feature scale
- output column names
- grouping metadata for composite features

This is especially important for:

- built-in posterior summaries
- user-added features such as custom intervals and exceedance probabilities
- filtering the set of plottable or displayable fields in each visualization tab

### Saved Objects and Backward Compatibility

Saved query and model helpers live in `src/helpers/01_auxiliary_functions.R`.

The app currently relies on archive-based saved objects:

- `.bsm_query`
- `.bsm_model`

If these structures change, prefer adding normalization or compatibility helpers rather than assuming all saved files were created by the newest version of the app.

### UI Support Features

Some cross-cutting UI features worth knowing about are:

- the documentation modal
- the tooltip toggle
- the light and dark mode switch
- shared feature-selector panels

These are implemented separately from the core modeling modules, so documentation or UI improvements can often be made without touching the modeling code.

### Developer Cautions

- The current user workflow is county-focused.
- Visualization modules usually expect stored output columns to exist before plotting.
- Confidence intervals are represented as paired lower and upper columns.
- Saved objects may need normalization when internal field names evolve.
- The Visualization area is intentionally disabled until a model is available.

[Back to top](#top)

<h2 id="glossary">Glossary</h2>

### Data and Workflow Terms

- <span id="glossary-countyfips"></span>`countyfips`: The 5-digit county FIPS identifier used for county-level joins and mapping.
- <span id="glossary-covariate"></span>`covariate`: An additional feature column added to the modeling dataset, often from an external file.
- `ESSENCE query`: The set of user-defined inputs used to request syndrome surveillance data from ESSENCE.
- <span id="glossary-forecast-horizon"></span>`forecast horizon`: The number of future time steps the model estimates beyond the observed data.
- `overall`: The total number of ED visits used as the reference count for proportion-based modeling.
- `saved model`: A reusable model file that stores the fitted model, processed data, and session settings.
- `saved query`: A reusable query file that stores retrieved data together with the query settings.
- `target`: The number of ED visits matching the selected syndrome or diagnosis.

### Model Terms

- `adjacency matrix`: A data structure that encodes which regions are treated as neighbors for spatial modeling.
- `custom model formula`: A model specification mode where the full INLA formula is entered directly by the user.
- `customized components`: A model specification mode where built-in terms can be turned on, off, or tuned without writing the full formula manually.
- `default model`: The guided starting model provided by the app with a standard set of model terms and settings.
- `distribution family`: The probability model used for the response, such as Poisson, Negative Binomial, Binomial, or Beta-Binomial.
- `intercept`: The baseline level of the model before additional effects are added.
- `mobility adjacency`: A neighborhood definition based on movement or connectivity patterns rather than direct borders.
- `physical adjacency`: A neighborhood definition based on shared geographic borders.
- `precision hyperparameter`: A prior-setting control that affects how strongly a model component is regularized.
- `region random effect`: A model term that allows each region to have its own baseline deviation from the overall level.
- `seasonal or temporal component`: A model term used to capture repeating or evolving time patterns such as weekly cycles or smoother trends.
- `spatiotemporal component`: A model term that lets nearby regions and adjacent time points share information.

### Posterior and Feature Terms

- `confidence interval feature`: In this app, a stored lower and upper posterior interval generated for display and export.
- `count scale`: A representation of results as estimated visit counts rather than proportions.
- `credible interval`: A posterior interval summarizing uncertainty around a fitted quantity.
- <span id="glossary-exceedance-probability"></span>`exceedance probability`: The posterior probability that a value is greater than a chosen threshold.
- `feature`: A stored data column or derived summary that can be displayed in tables, maps, or plots.
- `feature scale`: The scale associated with a feature, typically counts, proportion, or other.
- `feature type`: The category assigned to a stored feature, such as mean, quantile, confidence interval, exceedance probability, covariate, or ID.
- `posterior mean`: The average of the posterior distribution for a fitted quantity.
- `posterior quantile`: A selected percentile of the posterior distribution, such as the median or a tail quantile.
- `proportion scale`: A representation of results as the proportion of visits matching the selected syndrome or diagnosis.

### Filtering and Display Terms

- `available features`: Features that can still be added to the selected set after the current filters are applied.
- `feature filter`: A control that restricts which features are shown in the available list for a visualization tab.
- `filter scale`: A feature-filter setting used to limit the available feature list by scale.
- `filter type`: A feature-filter setting used to limit the available feature list by feature category.
- `selected features`: The features currently active in a given visualization or table view.

[Back to top](#top)
