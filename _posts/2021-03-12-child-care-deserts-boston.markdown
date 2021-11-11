---
layout: post
title:  "Locating Child Care Deserts in Boston"
date:   2021-03-12 09:00:26 -0500
permalink: child-care-deserts-boston
categories: posts
---

## Motivation

To accompany the release of Michelle Wu’s [ambitious child care
plan](https://www.michelleforboston.com/plans/early-education) I set out
to create a visualization to accompany the document that would make
clear the issues that the plan was created to address. Firstmost among
these is the overall lack of child care programs throughout the city of
Boston.

## Data Sources

To accomplish this I needed to know what the potential demand for child
care might be in any given area of Boston. For that I turned to the
[2019 ACS 5-year estimates](https://www.census.gov/programs-surveys/acs)
for the population aged 0-5 in each census tract. I also needed to know
how many programs and how child care seats there were in each census
tract. This information was not contained in a neat table. Instead, I
had to download lists of child care providers and there capacities from
a Massachusetts government web portal and then use the `tidy_geocoder`
package and [Geocodio](https://www.geocod.io/) to get the latitude and
longitude for each provider. From here I was able to begin my analysis.

### Project Code

To start I read in the geocoded provider locations from the Google Sheet
I stored them in. With that I needed shape files for all of the census
tracts in Boston, which I was able to find on [Boston’s Open Data
site](https://data.boston.gov/). Finally I used the `tidycensus` package
to pull in the 0-5 population for each census tract and joined that
information to their respective polygons.

``` r
library(tidyverse) # for manipulation
library(sf) # for spatial data
library(leaflet) # for interactive maps
library(googlesheets4) # for interaction with the google sheet
library(tidycensus) # for ACS data
library(htmltools) # for extra styling

# Read in the childcare site locations for overview plot
sheet_url <- Sys.getenv("CHILDCARE_PROVIDERS")
provider_locations <- range_read(ss = sheet_url, sheet = "Childcare Provider Locations")

# Change to sf for plotting
provider_locations_sf <- provider_locations %>% st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(4326)

# Get tract shapefile from boston open data
boston_tracts <- st_read("https://bostonopendata-boston.opendata.arcgis.com/datasets/4a8eb4fb3be44ed5a1eec28551b9f3b2_0.geojson?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D",
                         stringsAsFactors = FALSE)
# set my Census key
census_api_key(Sys.getenv("CENSUS_API_KEY"))

acs_data <- get_acs(geography = 'tract',
                    variables = c("B01001_003", # Male population 0-5
                                  "B01001_027"), # Female population 0-5
                    county = "Suffolk",
                    state = "MA",
                    output = 'wide')
boston_ct_data <- boston_tracts %>%
  left_join(acs_data, by = c("GEOID10" = "GEOID"))
boston_ct_data <- boston_ct_data %>% 
  transmute(Tract = NAMELSAD10,
            Pop0_5 = B01001_003E + B01001_027E,
            geometry)
```

At this point I needed to group the provider locations (point data) by
the tracts they lied within (polygons). To do this I used the package
`sf`. One of the providers was mistakenly included in my search despite
lying outside Boston. Similarly I removed a Census tract polygon that
was only covering water in Boston Harbor.

``` r
# Get tract that provider is in
tracts <- st_within(provider_locations_sf, boston_ct_data)
tracts <- unlist(lapply(tracts, function(item){
  boston_ct_data$Tract[item]
}))

# Drop the one provider that is located outside of boston add Tracts columns
provider_locations_sf <- provider_locations_sf %>%
  filter(Town != "Methuen") %>% mutate(Tract = tracts)
# Group by Census tract and compute capacity for each tract
tract_capacity <- provider_locations_sf %>% group_by(Tract) %>% 
  summarise(Capacity = sum(Capacity))

# Join tract data to the full data set
boston_capacity <- boston_ct_data %>%
  st_join(tract_capacity, by = "Tract") %>%
  mutate(Pop0_5 = ifelse(Pop0_5 == 0, NA, Pop0_5)) %>% 
  mutate(CapPerCap = Capacity/Pop0_5) %>% 
  mutate(CapPerCap = ifelse(is.na(CapPerCap) & is.na(Pop0_5), NA,
                            ifelse(is.na(CapPerCap) & !is.na(Pop0_5),
                                   0, CapPerCap))) %>% 
  filter(Tract.x != "Census Tract 9901.01")
```

Then, I tried several different ways of breaking the data to best show
the disparities in child care availability per child. I settled on
factoring the data into three groups: tracts where there are not enough
seats for all of the children, tracts where there are at least as many seats
as there were children and finally tracts where there are an excess of
seats per child (\>2 seats per
child).

``` r
boston_capacity$CapFactor <- ordered(ifelse(boston_capacity$CapPerCap < 1,
                                            "Under 1 seat per child",
                                            ifelse(boston_capacity$CapPerCap >= 1 &
                                                     boston_capacity$CapPerCap < 2,
                                                   "1-2 seats per child",
                                                   "Over 2 seats per child")),
                                     levels = c("Under 1 seat per child",
                                                "1-2 seats per child",
                                                "Over 2 seats per child"))

factor_pal <- colorFactor(c("#FF9B7B", "#3E2C70"),
                          domain = boston_capacity$CapFactor)

tractlabs <- sprintf("In this census tract there are <strong>%.2f</strong> EEC seats for every child aged 0-5 <br>
                     Children Aged 0-5: %d <br>
                     EEC Seats: %d",
                     round(boston_capacity$CapPerCap, 2),
                     boston_capacity$Pop0_5,
                     boston_capacity$Capacity) %>% lapply(htmltools::HTML)
```

For extra context I added the outline of Boston’s neighborhoods and
Logan International Airport (BOS) to the map.

``` r
# Add dashed line around airport boundary
airports <- st_read("~/Documents/ma-boston-mayoral-2021/data/raw/Airport_SHP/")
logan <- airports[1,]

pds <- st_read("https://bostonopendata-boston.opendata.arcgis.com/datasets/a6488cfd737b4955bf55b0342c74575b_0.geojson?outSR=%7B%22latestWkid%22%3A2249%2C%22wkid%22%3A102686%7D",
               stringsAsFactors=FALSE)
pds$PD[pds$PD == "Central"] <- "Central Boston"
dorchester <- pds %>% 
  filter(grepl("Dorchester", PD)) %>% st_union()
pds <- pds %>% filter(!grepl("Harbor Islands|Dorchester", PD)) %>% 
  select(PD, geometry) %>% add_row(PD = "Dorchester", geometry = st_geometry(dorchester))
```

All that was left to do was create my Leaflet
map.

``` r
leaf <- leaflet(boston_capacity) %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  addPolygons(color = "grey", weight = 1, opacity = .5,
              fillColor = ~factor_pal(CapFactor),
              fillOpacity = 1,
              highlightOptions = highlightOptions(color = "#2EFFF0",
                                                  weight = 2,
                                                  bringToFront = TRUE,
                                                  sendToBack = TRUE),
              label = tractlabs,
              labelOptions = labelOptions(style = list("color" = "white",
                                                       "font-family" = "fellix-regular",
                                                       "background-color" = "#139675"))) %>%
  addPolygons(data = pds, fill = FALSE, label = ~PD,
              labelOptions = labelOptions(style = list("color" = "white",
                                                       "font-family" = "fellix-bold"),
                                          permanent = TRUE,
                                          textOnly = TRUE,
                                          direction = 'top'),
               color = "#139675",
               opacity = .8,
               weight = 2) %>% 
  addPolygons(data = logan, color = "white", opacity = 1, fill = FALSE,
              weight = 2,
              dashArray = "5",
              label = "Boston Logan International Airport",
              labelOptions = labelOptions(style = list("color" = "white",
                                                       "font-family" = "fellix-bold"),
                                          permanent = TRUE,
                                          textOnly = TRUE,
                                          direction = 'center')) %>% 
  addLegend(position = "bottomright",
            colors = c(factor_pal("Under 1 seat per child"),
                       factor_pal("1-2 seats per child"),
                       factor_pal("Over 2 seats per child")),
            labels = c("Under 1 seat per child",
                       "1-2 seats per child",
                       "Over 2 seats per child"),
            opacity = 1,
            title = "Early Education and Child Care Seats Per Child (Age 0-5)")
browsable(
  tagList(
    list(
      tags$head(
        tags$style(
          " {
          height: 100%;}",
          ".leaflet .legend {
                 font-family: fellix-regular;
                 text-align: center;
                 color: #FFFFFF;
                 background-color: #139675;}",
          ".leaflet .legend i{
          float: left;}"
        )
      ),
      leaf)))
```
The output was a styled interactive map (thanks Leaflet!) showing child care seats per child in every census tract in Boston:
<div style="display: flex; box-sizing: border-box">
    <iframe src="/assets/ChildcareAvailability.html" style="width:1000px; height:400px; padding: 20px;" frameborder="0" title = "Child Care Availability in Boston, MA by Census Tract"></iframe>
</div>
As you can see pretty clearly from the large swaths of orange, there is a lack of local child care options for a huge number of Boston families, particularly those that live further from the city center.

### Questions left to answer
1. How do these child care deserts intersect with otherwise disadvantaged areas, such as areas that lack access to public transit?
2. Are low-income communities being disproportionately harmed by a lack of care in their area? What is the relationship between care availability and care cost?
3. Are families that work off-hours jobs (service, security, healthcare providers) suffering from a lack of care options in their neighborhoods? 

All of these questions are ones that I hope to study in the near future leveraging the data gathered from this initial search. If you've made it this far, thank you so much for reading and if there's anything you think I missed or interesting questions that you think this raises don't hesistate to let me know!

### Acknowledgements
A huge thanks to MaryRose, Anne and Paulina from Michelle Wu's staff for giving me the opportunity to work on this project and for including my visualization on the plan website!


