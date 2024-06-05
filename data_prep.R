##' Title: Data preparation 
##' Description: In this R-script we prepare data for accessibility analysis; roads,barriers to movement and land cover fro Kenya
##' Author: Mercy Chepkorir, (mercychepkorir2368@gmail.com)
##' Date: 05-June-2024
##' University: University of Nairobi
##' Institute: Center fr Epidemiological Modelling and Analysis
##' Data downloaded using inAccessmod tutorial: https://github.com/unige-geohealth/inAccessMod/blob/main/TUTORIAL.md


# libraries----
if (!require ("pacman")) install.packages("pacman")
if (!require("devtools")) install.packages("devtools")
if (!require("remotes")) install.packages("remotes")

remotes::install_github("rspatial/geodata")
devtools::install_github("unige-geohealth/inAccessMod", 
                         build_vignettes = TRUE, 
                         dependencies = TRUE)

pacman::p_load(dplyr,
               terra,
               sf,
               rgdal,
               inAccessMod)

# download data-----
## initiate the project
mainPath <- getwd()
initiate_project(mainPath) #select 88(Kenya)
country <- "Kenya"

## download country boundaries 
download_boundaries(mainPath, 
                    country, 
                    adminLevel = 1, 
                    type = "gbOpen", 
                    alwaysDownload = TRUE)

## Set the projected coordinate reference system
set_projection(mainPath, 
               country, 
               mostRecent = TRUE, 
               alwaysSet = TRUE, 
               bestCRS = TRUE)

## import facilities (from MoH)
inputPath <- "health_facilities_kenya.shp"
copy_input(mainPath, 
           country, 
           inputPath) #select 5

## population raster (from worldpop)
download_population(mainPath, 
                    country, 
                    alwaysDownload = TRUE) #select 5 > 1 > 2 > 1

## land cover (from copernicus)
download_landcover(mainPath, 
                   country, 
                   alwaysDownload = TRUE, 
                   mostRecent = TRUE)

## roads and barriers (from OSM)
download_osm("roads", 
             mainPath, 
             country, 
             alwaysDownload = TRUE, 
             countryName = TRUE, 
             mostRecent = NULL, 
             defaultClasses = TRUE)

download_osm("waterLines", 
             mainPath, country, 
             alwaysDownload = TRUE, 
             countryName = TRUE, 
             mostRecent = NULL, 
             defaultClasses = TRUE)

download_osm("naturalPolygons", 
             mainPath, country, 
             alwaysDownload = TRUE, 
             countryName = TRUE, 
             mostRecent = NULL, 
             defaultClasses = TRUE)

# process the data-----
process_inputs(mainPath, 
               country, 
               selectedInputs = "All", 
               mostRecent = TRUE, 
               alwaysProcess = TRUE, 
               defaultMethods = TRUE, 
               changeRes = TRUE, 
               newRes = 100, 
               popCorrection = TRUE, 
               gridRes = 3000)

#Label land cover classes (processed landcover required)
label_landcover(mainPath, 
                country, 
                mostRecent = TRUE, 
                overwrite = TRUE, 
                defaultLabels = TRUE)

