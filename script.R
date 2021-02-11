getwd()


options(scipen = 999) # turn off scientific notation

# rm(list = ls())
# devtools::install_github("ropensci/USAboundariesData") - in case USAboundariesData error message is thrown

# load packages -----------------------------------------------------------

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages <-  c('readxl', 'USAboundaries', 'sf', 'leaflet', 'leaflet.extras', 'htmlwidgets', 'tidyverse', 'tidylog')

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# import data -------------------------------------------------------------


# we are pulling data from Shank's github, so we need to direct R to that url:
url <-  "https://raw.githubusercontent.com/FWeco/PAAFS_noStatic/master/"


# then we need to download each file necessary for mapping


# results dataframe - credit SRBC and USFWS NEFC
download.file(paste0(url, 'edna_map_df.rds'),'edna_map_df.rds', method="curl")
edna_map_df <- readRDS("edna_map_df.rds")
edna_map_df



# nhd Flowlines from shapefile
download.file(paste0(url, 'Flowlines.shp'), 'Flowlines.shp', method="curl")
download.file(paste0(url, 'Flowlines.cpg'), 'Flowlines.cpg', method="curl")
download.file(paste0(url, 'Flowlines.dbf'), 'Flowlines.dbf', method="curl")
download.file(paste0(url, 'Flowlines.prj'), 'Flowlines.prj', method="curl")
download.file(paste0(url, 'Flowlines.sbn'), 'Flowlines.sbn', method="curl")
download.file(paste0(url, 'Flowlines.sbx'), 'Flowlines.sbx', method="curl")
download.file(paste0(url, 'Flowlines.shx'), 'Flowlines.shx', method="curl")

nhd <- st_read('Flowlines.shp', stringsAsFactors = F) %>% 
  select(COMID, Resolution, GNIS_ID, GNIS_NAME, LENGTHKM, REACHCODE, FTYPE, StreamOrde, geometry)

st_crs(nhd) # extract coordinate system (CS) information from an sf

nhd <- st_transform(nhd, "+proj=longlat +datum=WGS84") # transform projection to be WGS84 (same as other leaflet layers)

nhd
st_crs(nhd) # now in WGS84
str(nhd)
dim(nhd)
names(nhd)
summary(nhd$StreamOrde)




# dam shapefile 
download.file(paste0(url, 'eDNA_sampling_barriers.shp'), 'eDNA_sampling_barriers.shp', method="curl")
download.file(paste0(url, 'eDNA_sampling_barriers.cpg'), 'eDNA_sampling_barriers.cpg', method="curl")
download.file(paste0(url, 'eDNA_sampling_barriers.dbf'), 'eDNA_sampling_barriers.dbf', method="curl")
download.file(paste0(url, 'eDNA_sampling_barriers.prj'), 'eDNA_sampling_barriers.prj', method="curl")
download.file(paste0(url, 'eDNA_sampling_barriers.sbn'), 'eDNA_sampling_barriers.sbn', method="curl")
download.file(paste0(url, 'eDNA_sampling_barriers.sbx'), 'eDNA_sampling_barriers.sbx', method="curl")
download.file(paste0(url, 'eDNA_sampling_barriers.shx'), 'eDNA_sampling_barriers.shx', method="curl")

barriers <- st_read('eDNA_sampling_barriers.shp', stringsAsFactors = F) 

st_crs(barriers) # extract coordinate system (CS) information from an sf

barriers <- st_transform(barriers, "+proj=longlat +datum=WGS84") %>%  # transform projection to be WGS84 (same as other leaflet layers)
  mutate(Fish_Passa = ifelse(Fish_Passa == 'Y', 'Yes', 'No')) # make passage column more descriptive

barriers
st_crs(barriers) # now in WGS84
str(barriers)
dim(barriers)
names(barriers)








# basic map of presence/absence ----------------------------------------------------
sort(unique(edna_map_df$Presence))

factpal_pres <- colorFactor(c('#91bfdb', '#bdbdbd', '#f03b20'),
                            domain = edna_map_df$Presence, 
                            na.color = 'black')
# create basic map
leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Basemap") %>% # basemap
  
  
  # add flowlines
  addPolylines(data = nhd,
               col = '#2b8cbe', weight = 2, opacity = 1,
               group = 'Impaired',
               label = ~ GNIS_NAME) %>% 
  
  
  # add barriers
  addCircleMarkers(
    data = barriers,
    lng = ~ Longitude, lat = ~ Latitude,
    col = ~ 'darkgrey', opacity = 0.8, 
    label = ~ Barrier) %>% 
  
  
  # add eDNA point results
  addCircleMarkers(
    data = edna_map_df,
    lng = ~ Longitude, lat = ~ Latitude,
    col = ~ factpal_pres(Presence), opacity = 1, 
    label = ~ Station,
    popup = ~ Presence) %>% 
  
  addLegend(pal = factpal_pres, values = edna_map_df$Presence, opacity = 0.8,
            title = 'Invasive Fish Presence')


# Flaws :
# little streams and big rivers all the same size
# can't tell dams apart from eDNA samples
# can't tell what species, or what year?
# Points that weren't sampled are just as prominent as points that were
# Dots/colors hard to see
# Would like to zoom in and see aerial imagery
# how to tell what county i'm in?
# how to orient myself once i'm way zoomed in?
# how to search for a specific town or set of coordinates?
# beginning extent too zoomed out
# have no idea of distance between points/features

# Fixes:
# vary nhd width by stream order variable
# images!
# Make separate layers for species and year
# Convert not sampled to NA, and set na color to transparent
# Fill circles with dark outline
# Add aerial basemap
# add county boundaries
# add a map reset button
# add a search box
# add a bounding box to set extent of map zoom
# add a scale bar




# improved map ------------------------------------------------------------

# change 'Not Sampled' to NA
edna_map_df2 <- 
  edna_map_df %>% 
  mutate(Presence = ifelse(Presence == 'Not Sampled', NA_character_, Presence))
edna_map_df2

sort(unique(edna_map_df2$Presence))



# better legend with transparent NA's
# one for blue cats
factpal_bcat <- colorFactor(c('#91bfdb', '#fc8d59'), # color from HEX format - colorbrewer2.org
                             domain = edna_map_df2$Presence, 
                             na.color = 'transparent')

# one for snakeheads
factpal_nsh <- colorFactor(c('dodgerblue', 'brown4'),
                            domain = edna_map_df2$Presence, 
                            na.color = 'transparent')




# get county boundaries from USAboundaries package

edna_co <- USAboundaries::us_counties(resolution = "high", states = c("PA", "MD")) %>%
  filter(name %in% c('York', 'Lancaster', 'Harford', 'Cecil'))

edna_co
st_crs(edna_co)
plot(edna_co$geometry)





# photo of a dam to use as barrier icon

damIcon <- makeIcon(
  iconUrl = paste0(url, 'dam.jpg'),
  iconWidth = 95, iconHeight = 38,
  iconAnchorX = 45, iconAnchorY = 37) # ,




# map time

edna_map <-
  
  leaflet() %>%
  
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>% # basemap
  
  addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>% # aerial map
  
  addResetMapButton() %>%
  
  addSearchOSM() %>%
  
  
  
  # add flowlines
  addPolylines(data = nhd,
               col = '#2b8cbe', weight = ~ StreamOrde, opacity = 1,
               label = ~ GNIS_NAME,
               popup = ~
                 paste("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:250px'", 
                       '<br>',
                       '<b>', 'COMID: ', '</b>', COMID, "<br>",
                       '<b>', 'GNIS_ID:  ', '</b>', GNIS_ID, "<br>",
                       '<b>', 'GNIS_NAME:  ', '</b>', GNIS_NAME, "<br>",
                       '<b>', 'REACHCODE:  ', '</b>', REACHCODE, "<br>",
                       '<b>', 'Flowpath Type: ', '</b>', FTYPE, "<br>",
                       '<b>', 'Stream Order:  ', '</b>', StreamOrde, "<br>"),
               group = 'Hydrology') %>% 
  
  
  
  # add Co boundary
  addPolygons(data = edna_co,
              color = "#737373",
              fill = F,
              weight = 2) %>%
  
  
  # add barriers with jpg points!
  addMarkers(
    data = barriers,
    lng = ~ Longitude, lat = ~ Latitude,
    icon = damIcon,
    popup = ~
      paste("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:250px'", 
            '<br>',
            '<b>', 'Barrier Name: ', '</b>', Barrier, "<br>",
            '<b>', 'Stream Name:  ', '</b>', Stream, "<br>",
            '<b>', 'Fish Passage?:  ', '</b>', Fish_Passa, "<br>",
            '<br>',
            '<b>', 'Latitude:  ', '</b>', Latitude, "<br>",
            '<b>', 'Longitude: ', '</b>', Longitude, "<br>",
            '<b>', 'Notes:  ', '</b>', Info, "<br>"),
    group = 'Dams') %>% 
  
  
  
  # points for NSH 2019
  addCircleMarkers(
    data = filter(edna_map_df2, Year == '2019' & Taxa == 'NSH'),
    lng = ~ Longitude, lat = ~ Latitude,
    radius = 12, stroke = TRUE, col = '#252525', weight = 1, 
    fill = TRUE, fillColor = ~ factpal_nsh(Presence), fillOpacity = 0.9, 
    label = ~ paste(Station, Year, Taxa, Presence, sep = '; '),
    popup = ~
      paste("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:250px'", 
            '<br>',
            '<b>','<u>', paste(Year, Taxa, sep = ' - '), '</b>','</u>', "<br>",
            '<b>', Presence, '</b>', "<br>",
            '<br>',
            '<b>', 'Stream:  ', '</b>', Stream, "<br>",
            '<b>', 'Station:  ', '</b>', Station, "<br>",
            '<b>', 'Station-Filter:  ', '</b>', Station_Filter, "<br>",
            '<b>', 'Count: ', '</b>', round(Count, 1), "<br>",
            '<b>', 'Replicates:  ', '</b>', Replicates, "<br>", 
            '<b>', 'Volume Filtered:  ', '</b>', Volume, "<br>",
            '<br>',
            '<b>', 'Latitude: ', '</b>', Latitude, "<br>",
            '<b>', 'Longitude:  ', '</b>', Longitude, "<br>", 
            '<br>'),
    group = 'NSH 2019') %>% 
  
  
  # points for NSH 2020
  addCircleMarkers(
    data = filter(edna_map_df2, Year == '2020' & Taxa == 'NSH'),
    lng = ~ Longitude, lat = ~ Latitude,
    radius = 6, stroke = TRUE, col = '#252525', weight = 1, 
    fill = TRUE, fillColor = ~ factpal_nsh(Presence), fillOpacity = 0.9, 
    label = ~ paste(Station, Year, Taxa, Presence, sep = '; '),
    popup = ~
      paste("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:250px'", 
            '<br>',
            '<b>','<u>', paste(Year, Taxa, sep = ' - '), '</b>','</u>', "<br>",
            '<b>', Presence, '</b>', "<br>",
            '<br>',
            '<b>', 'Stream:  ', '</b>', Stream, "<br>",
            '<b>', 'Station:  ', '</b>', Station, "<br>",
            '<b>', 'Station-Filter:  ', '</b>', Station_Filter, "<br>",
            '<b>', 'Count: ', '</b>', round(Count, 1), "<br>",
            '<b>', 'Replicates:  ', '</b>', Replicates, "<br>", 
            '<b>', 'Volume Filtered:  ', '</b>', Volume, "<br>",
            '<br>',
            '<b>', 'Latitude: ', '</b>', Latitude, "<br>",
            '<b>', 'Longitude:  ', '</b>', Longitude, "<br>", 
            '<br>'),
    group = 'NSH 2020') %>% 
  
  
  # points for BCAT 2019
  addCircleMarkers(
    data = filter(edna_map_df2, Year == '2019' & Taxa == 'BCAT'),
    lng = ~ Longitude, lat = ~ Latitude,
    radius = 12, stroke = TRUE, col = '#252525', weight = 1, 
    fill = TRUE, fillColor = ~ factpal_bcat(Presence), fillOpacity = 0.6, 
    label = ~ paste(Station, Year, Taxa, Presence, sep = '; '),
    popup = ~
      paste("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:250px'", 
            '<br>',
            '<b>','<u>', paste(Year, Taxa, sep = ' - '), '</b>','</u>', "<br>",
            '<b>', Presence, '</b>', "<br>",
            '<br>',
            '<b>', 'Stream:  ', '</b>', Stream, "<br>",
            '<b>', 'Station:  ', '</b>', Station, "<br>",
            '<b>', 'Station-Filter:  ', '</b>', Station_Filter, "<br>",
            '<b>', 'Count: ', '</b>', round(Count, 1), "<br>",
            '<b>', 'Replicates:  ', '</b>', Replicates, "<br>", 
            '<b>', 'Volume Filtered:  ', '</b>', Volume, "<br>",
            '<br>',
            '<b>', 'Latitude: ', '</b>', Latitude, "<br>",
            '<b>', 'Longitude:  ', '</b>', Longitude, "<br>", 
            '<br>'),
    group = 'BCAT 2019') %>% 
  
  
  # points for BCAT 2020
  addCircleMarkers(
    data = filter(edna_map_df2, Year == '2020' & Taxa == 'BCAT'),
    lng = ~ Longitude, lat = ~ Latitude,
    radius = 6, stroke = TRUE, col = '#252525', weight = 1, 
    fill = TRUE, fillColor = ~ factpal_bcat(Presence), fillOpacity = 0.6, 
    label = ~ paste(Station, Year, Taxa, Presence, sep = '; '),
    popup = ~
      paste("<div class='leaflet-popup-scrolled' style='max-width:250px;max-height:250px'", 
            '<br>',
            '<b>','<u>', paste(Year, Taxa, sep = ' - '), '</b>','</u>', "<br>",
            '<b>', Presence, '</b>', "<br>",
            '<br>',
            '<b>', 'Stream:  ', '</b>', Stream, "<br>",
            '<b>', 'Station:  ', '</b>', Station, "<br>",
            '<b>', 'Station-Filter:  ', '</b>', Station_Filter, "<br>",
            '<b>', 'Count: ', '</b>', round(Count, 1), "<br>",
            '<b>', 'Replicates:  ', '</b>', Replicates, "<br>", 
            '<b>', 'Volume Filtered:  ', '</b>', Volume, "<br>",
            '<br>',
            '<b>', 'Latitude: ', '</b>', Latitude, "<br>",
            '<b>', 'Longitude:  ', '</b>', Longitude, "<br>", 
            '<br>'),
    group = 'BCAT 2020') %>% 
  
  
  
  # BCAT legend
  addLegend(position = "bottomright", pal = factpal_bcat, values = edna_map_df2$Presence, opacity = 0.9,
            title = 'Blue Catfish', group = c('BCAT 2019', 'BCAT 2020')) %>%

  # NSH legend
  addLegend(position = "bottomright", pal = factpal_nsh, values = edna_map_df2$Presence, opacity = 0.9,
            title = 'Northern Snakehead') %>% 
  
  
  # add scale bar
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200, metric = TRUE)) %>% 
  
  
  # Layers control
  addLayersControl(
    baseGroups = c("Topographic", "Aerial"),
    overlayGroups = c('NSH 2019', 'NSH 2020', 'BCAT 2019', 'BCAT 2020', 'Hydrology', 'Dams'), # define all groups that will overlay
    options = layersControlOptions(collapsed = FALSE)) %>% 
  
  hideGroup(c('NSH 2019', 'NSH 2020', 'BCAT 2019', 'BCAT 2020')) %>%   # hide all groups at first, let user turn them on
  
  
  fitBounds(-76.5, 40.0, -75.9, 39.4) # bounding box so we are zoomed into good extent when map initializes

edna_map

saveWidget(edna_map, file = paste0(Sys.Date(), "_edna_map.html"))


# or publish edna_map to rpubs and have a shareable link available to anyone with internet





























