# Limited English Proficiency (LEP) & Immigrant Community Mapping
This analysis uses spatial statistical techniques to derive "language overlays" for critical languages in Portland, OR. Critical languages are those that have 1,000 or more speakers with limited English proficiency. The overlays are intended to delineate soft boundaries for where interpretation and translation services should be provided. Because tract-level American Community Survey (ACS) data are not available for certain critical languages, this analysis is supplemented with ACS data on countries of birthplace for the foreign-born population. This is an imprecise measurement, but it is a better proxy to identify locations of immigrant and LEP communities.

## Shiny web application
A link to the shiny web application where you can map language data is available at:
[https://portlandbps.shinyapps.io/LEP-immigrant-communities](https://portlandbps.shinyapps.io/LEP-immigrant-communities).

## Methodology
This analysis identifies census tracts where the number of individuals that speak a specific language or have a specific national origin exceeds a threshold of population density, population count or population percentage when compared to other tracts in the sample. A nearest neighbor analysis was used to group the population densities, population counts and population percentages for specific languages or national origins of clusters of census tracts. If the any of these metrics were found to be significantly greater for the cluster than the average for the entire sample, each census tract in the cluster was determined to have significant populations for that language or national origin.

Here are the steps taken to produce this analysis.

### Step 1: Narrow down list of critical languages
Using a mixed-methods appraoch involving quantitative and qualitative data sources, the Office of Equity and Human Rights (OEHR) [posted guidance](https://www.portlandoregon.gov/oehr/80870) on select critical languages to consider in language justice work. This short list was used, along with two other languages: Eritrean and Ethiopean. In other communities, this step can be done by a [Title VI Factor 1 analysis](https://www.fhwa.dot.gov/civilrights/programs/title_vi/lep_fourfactor.cfm), which involves analyzing ACS data, such as [pre-published tables](https://data.census.gov/cedsci/table?q=ACSDT1Y2019.B16001&g=310M500US38900&tid=ACSDT1Y2019.B16001&hidePreview=true) or PUMS data, as well as collecting administrative data from school districts.

### Step 2: Spatial analysis
This analysis uses two spatial statistical methods on three metric types for flagging a tract as a significant contributor to the region's share of critical language speakers.

1. **Spatial methods**: *k-nearest neighbor* (5 closest neighbors) and *distance-based* (1-mile radius from tract centroid). This approach outputs a "[local G](https://walker-data.com/2016/07/spatial-neighbors-in-r---an-interactive-illustration/)" statistic for the given tract, which is a standardized Z score for how correlated it is with its neighbors. 
2. **3 metrics of measuring concentration**: Raw or absolute estimate, percentage of population, and population density. A tract is flagged if the raw number of people, percentage or density is higher than the 50th percentile of the sample (filtering out tracts with zero observations of such speakers/immigrants).

Below is the decision tree to flag a tract-language combination as a significant contributor:

1. If looking at raw or absolute estimates:
    1. Estimate is not equal to zero; **AND** either of the following:
    2. Local G > 1.96 (two standard deviations above mean); OR
    3. Tract estimate is higher than the median **AND** density is higher than **85** people per square mile; 
2. If percentage of the percentage:
    1. Estimate is not equal to zero; **AND** either of the following:
    2. Local G > 1.96 (two standard deviations above mean); OR
    3. Tract percentage is higher than the median **AND** density is higher than **40** people per square mile
3. If looking at population density:
    1. Estimate is not equal to zero; **AND** either of the following:
    2. Local G > 1.96 (two standard deviations above mean); OR
    3. Tract density is higher than the median **AND** density is higher than **85** people per square mile

### Step 3: Shiny app
To transform the data into a more user-friendly format, staff wrote a [shiny app](https://portlandbps.shinyapps.io/LEP-immigrant-communities) to allow users to map specific languages or countries of origin. 

![Image of shiny app](https://github.com/BPSTechServices/lep-mapping/images/lep_shiny_app_screenshot.PNG)

## Next steps
The Bureau of Planning & Sustainability (BPS) and the Office of Equity and Human Rights (OEHR) are discussing how to best frame language justice within the City and which technical services such as this app we should provide to City staff and community partners who do community outreach in their work.