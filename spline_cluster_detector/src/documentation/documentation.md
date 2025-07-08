::: {style="margin: auto; width:100%"}
# Online Tool for *Ad Hoc* On-Demand Spatio-temporal Cluster Detection

## Description

An R Shiny app for implementing a spline-based classification/detection approach for determining clusters of patient health records whose grouping in space and time is anomalous and possibly worthy of investigation.

## Rationale

-   The app code approximates the scan statistics methodology implemented in the open source SaTScan software to enable rapid cluster detection by users of ESSENCE biosurveillance systems for arbitrary as well as standard syndromic record groupings. The app enables prompt iterative experimentation and modification of these groupings and of baseline and test intervals and other cluster parameters.

-   The target audience are ESSENCE system users wishing to work within the web application, and also non-users who provide files of record dates, locations, and counts.

## App Organization

The ribbon at the top of the app has menu selections 'Data Loader', 'Clustering', and 'Documentation', and a button for report generation. The clustering and report options are available after data are ingested.

### Data Specification and Ingestion

There are two basic options for ingesting input data. First option: build or paste a URL address followed by an API call to pull ESSENCE data with this URL. The second option does not require ESSENCE access and allows the user to provide the data directly via uploading a csv or cas file.

IN THE APP:

User selects either the 'Local File' button or the 'NSSP API Call' button. For the local file selection, the user navigates to and selects an input file stored on the local machine. For the API call selection, user forms the URL using control panel options. Acquiring the URL for the API call (not needed for direct .csv data read)

The user may either paste in a custom URL from an ESSENCE query, or build the URL with one of the standard record classification methods: syndrome, sub-syndrome, or CCDD category.

IN THE APP:

User selects the button for either 'ad hoc URL' or 'URL builder' for the ad hoc selection, the app presents a text box, and the user pastes in the URL directly. For the ad hoc selection, the user may use controls below to change the date range but cannot later change categories or locations, which may be complex. The user may then select the 'Ingest Data' button and proceed to inspect the data. The time needed to ingest data with the ad hoc URL depends on the URL complexity.

For the URL builder selection, the user proceeds to 'Data Specifications' controls below.

### Building the URL for data ingestion:

-   **Data time interval:** The user must specify desired data date range. After ingestion, data may be used for multiple cluster detection runs for varying end dates and other parameters.

    -   *IN THE APP: Under 'Location and Time', enter start and end data dates. The default is 120 days. The recommended date range allows cluster intervals of \>= 7 days preceded by \>= 90 baseline days.*

-   **Geography:** The user must specify both the geographic region and the spatial resolution of the requested data. Options for the region are single states or all US states, including the District of Columbia. Options for the resolution are by zip code or by county, though data records do not specify a patient county and are assigned a county (called the ESSENCE 'region') from a zip-to-region mapping.

    -   *Select the geography from the dropdown box for 'State' and the data resolution by clicking either the 'zip' or 'county' radio button.*

-   **Outcome:** The user must specify which data records are to be ingested for cluster investigation, representing a concept of epidemiological concern. Under the URL-building option, the user selects a standard ESSENCE grouping and then one classification within that grouping.

    -   *Select one of the radio buttons for syndrome (13 options), subsyndrome (134 options), or CCDD category (137 options as of April 2025), and then choose an option under 'Select Type'. These instructions are for typical expected data selection. The Advanced Options button allows additional settings:*

### Advanced Options for Data Selection

Both options refer to ESSENCE data sources and are not relevant to the flat file input option.

-   **Data Source:** The user may select the patient level (the default) or facility level ESSENCE data source. The patient level option will ingest records whose patient resident treatment facility is located. The facility level option pulls records only from facilities in the chosen state and may include patient residence zip codes from other states.

-   **Data Type:** The options are to use the default ESSENCE Table Builder option to pull a table of counts of syndromic records for each location and date, or else to pull individual patient records with the data details option. This latter option pulls a subset of about 15 ESSENCE data fields, not full records. While pulling individual records is slower, this option allows de-duplication and filtering using the selected fields. The data details option is available only if the chosen geography is a single state, not the entire US. If the facility level option is chosen, the app cannot currently pull data at zip code resolution unless the data details type is specified.

### Acquiring and examining input data

Once data selections are complete, press the 'Ingest Data' button. The data upload time is a function of the volume to be ingested, typically requiring under one minute for a single state with the default settings. Completion of ingestion enables the clustering menu and report generation options in the top ribbon, and displays the uploaded data in the panel at right. Data listing Input data are displayed in three columns: location, date, and count. The count is the number of records uploaded in the chosen category with the date and location of its row.

The display may be sorted in ascending or descending order by clicking at the top of each column, and a search box at the top allows filtering by a text string in any of the columns. A button at the bottom of the display enables download of the data columns to a file on the local machine. A second button displays the URL used to pull the data, unless the data were uploaded from a file.

### Cluster Determination and Inspection

After the three columns of data are loaded, the user may select the 'Clustering' menu option from the top ribbon. This selection replaces the control panel at left with cluster-related options and replaces the right panel for data and cluster inspection. In top menu at right, a 'Data Explorer' tab is activated, and 'Cluster Summary' and 'Cluster Map' tabs are activated after clusters are computed.

### Data Explorer

The Data Explorer provides three modes of inspection, 'Data Summary', 'Heatmap', and 'Time Series', selectable at the top of the panel, to help show the baseline and test data distributions over time and space before before the clustering computation to help understand the output clusters.

-   **Data Summary:** The short table allows prompt comparison of the amount and richness of the ingested data in the baseline interval and in the interval to be tested for clustering. The number of days, amount of data, average record count per day, number of locations with records present and absent, and numbers of days with data counts (e.g. 2-3 per day) are shown for both intervals.

-   **Heatmap:** The heatmap shows the ingested data in space and time, color-coded for the data count by day. The user can see how many locations contribute records in both baseline and test interval. A vertical red line separates the interval to be tested from the baseline, if the user changes the end date and width of the test interval in the control panel, the plot reflects the change. For richer time series with many counts per day, the color-coding may be unclear, so the user may use the buttons beneath the plot to change to a logarithmic or square-root scale for better visual comparison. By default, the heatmap omits locations with no data at all, and the user may use the button to show those locations. Interactivity: For most states and user selections, the heatmap cannot show all of the rows with legible zip code or county names at left, so a it shows only subset of the location rows. The user may change this subset interactively by positioning the cursor inside the plot area and selecting a set of desired rows. Other interactive controls above the plot enable zooming, panning, and other adjustments.

-   **Time Series:** The time series option gives a single plot of the time series of counts of all ingested records summed for each requested day. Peaks in this plot can help interpret cluster results. A significant cluster may result from an early spatial distribution during a regionwide surge or may occur because of a local outbreak that this plot may mask.

### Options for clustering

The control panel replaced after data ingestion shows default setting for cluster computation that the user may change.

-   **Radius for cluster computation:** this is the radius (in miles) for cluster size. Note that the default for county-level clustering is 50 miles, while the default for zip codes is 15 miles

-   **End date:** This is the final date of the test interval, or the last date of consideration of cluster detection

-   **Max cluster duration:** This is the test-interval length (default = 7 days), and is equivalent to specifying the maximum time period for cluster determination

-   **Baseline length:** This is the number of days of baseline data to include when

The advanced options button provides additional settings:

-   **Spline-based Classifier**: Cluster detection may be set at four levels of sensitivity using splines derived from results of thousands of runs of the open-source SaTScan software applied to ESSENCE data. These levels correspond to SaTScan p-values of 0.05, 0.01 (the default), 0.005, and 0.001. Differences between sets of clusters using these levels are most noticeable for clusters of less than 50 records.

-   **Minimum cluster count:** The default value for the smallest number of records that a reported cluster may contain is two. The user may this minimum to 5, 10, or higher, they are not interested in smaller case clusters of the chosen category, or simply to reduce the burden of alert investigation.

-   **Maximum cluster count:** There is no explicit default value for the largest number of records in a cluster, but the app is currently limited to clusters of at most 300 records. A user interested only in smaller clusters suitable for public health intervention can limit computation to clusters below a chosen value.

-   **Baseline adjustment method:** This option controls handling of cluster determination when records for the chosen category occur in the test interval but not in the much longer baseline, which would lead to divide-by-zero problems in the estimation of observed/expected. All the adjustment methods deal with this issue in a slightly different way. In the default option *"add one as needed"*, if a record is found for a location in the test interval but not the baseline, a baseline count of one is added for that location. In the option *"add test interval to baseline"*, the divide-by-zero problem is eliminated by adding the test interval data counts to the baseline for all locations, as in the SaTScan space-time permutation option. A third option of *"no adjustment"* allows zero baseline counts, and clusters with expected counts of zero are automatically reported without computation. A user who selects this option may want to raise the minimum cluster count to avoid multiple nuisance clusters from individual locations, especially for sparse data categories.

### Cluster Computation and Inspection

After data ingestion and following the inspection options chosen above, press the 'Compute Clusters' Option. Computation will be completed in a few seconds for many requested configurations, but could require close to a minute for nationwide clusters of records from a broad syndromic category. Once the computation is complete, the right display panel shows a summary table of any significant clusters found, one row per cluster. Significant case clusters are not common, so for many runs the chart will show only the text string "No Clusters". The user may change the end date, maximum cluster radius, maximum number of cluster days, and baseline length, and press the compute button again for rapid recalculation, as long as the required data correspond to the category and time interval and geography previously specified--a modified baseline or end date may require another data pull. If clusters are found and appear in the cluster table, the user may view an interactive map by selecting the option 'Cluster Map' above the table. They may then go back to the table and may change the cluster options for iterative cluster re-computation and visualization.

### Cluster Table

Each row of the cluster table summarizes key information for one cluster. The columns are:

-   **Center:** The zip code or county at the center of the cluster

-   **Cluster Date:** The end date of the cluster. At least one of the cluster records will be from this date.

-   **Duration:** The time extent of the cluster in days, no larger than the maximum cluster length specified using the control panel slider.

-   **Observed:** The total number of data records in the cluster.

-   **Expected:** The number of records expected from the cluster locations and time interval, according to the baseline data.

-   **Obs/Exp:** The ratio of the Observed and Expected columns. The logarithm of this ratio is used to determine cluster significance given the Observed count.

-   **p-value:** A measure interpolated from the spline curves used to determine significance, shown as \<0.001 for clusters significant for all of these curves. Significance is determined by the ratio of the total number of records within the cluster to the total expected.

-   **Max distance from center:** The distance from the centroid of the center location to the centroid of the most distant location in the cluster. It may be interpreted as the cluster radius in miles. If a cluster is within a single location, the distance is zero.

-   **Number of locations:** The number of locations within the cluster radius. Some of these locations, including the center, may have no records;

-   **Cluster locations:** A complete list of locations within the cluster; for clusters with many locations, only first 25 characters are shown, but hovering over the list/ellipsis will reveal the full list

-   **Zip Codes affected:** For county or zip code resolution, clicking on the arrow in the last column shows the zip codes or counties, respectively inside the cluster, whether or not any cluster records were present for each.

### Cluster Map

The cluster map shows outlines of all of the zip code or county locations in the region selected by the user. Locations in significant clusters are shaded with separate colors distinguishing each cluster. The map is interactive, allowing the user to zoom or pan and to get details by hovering over individual cluster locations. The details provided include the location, the cluster center location, the number of cluster locations, the total observed and expected record counts as in the table, the observed record count in the individual location, and the time extent of the cluster in days

## Generating Report

Once data are loaded in the app, a button to generate a report appears in the ribbon at the top of the right. Pressing the button generates a report whose contents depend on how much analysis has been done. if clusters have not been computed yet, the report contains the data summary table, the heatmap, and the overall time series. If clusters have been computed, the report also contains the cluster summary table. If the map has been created, the report contains the map as well. After a brief delay for report creation, the app presents a text box with a default title of the form `spatiotemporalclustering_report_YYYY-mm-dd.html` that the user may replace with a title of choice and then select the location for storage of the html report.

### Contacts:

-   Howard Burkom [Howard.Burkom\@jhuapl.edu](mailto:Howard.Burkom@jhuapl.edu)
-   Luke Mullany [Luke.Mullany\@jhuapl.edu](mailto:Luke.Mullany@jhuapl.edu)

### Copyright

© 2024 The Johns Hopkins University Applied Physics Laboratory LLC. Development of this software was sponsored by the U.S. Government under contract no. 75D30124C19958
:::
