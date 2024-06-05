##' Title: Friction layer 
##' Description: In this R-script we merge data into one land cover and generate friction surface
##' Author: Mercy Chepkorir, (mercychepkorir2368@gmail.com)
##' Date: 05-June-2024
##' University: University of Nairobi
##' Institute: Center fr Epidemiological Modelling and Analysis

# libraries----
pacman::p_load(dplyr,
               terra,
               sf,
               rgdal,
               raster,
               tidyverse,
               gdalUtils,
               readxl,
               gdistance,
               iterators,
               data.table,
               rmapshaper,
               fasterize)

# admin boundary ---------------------------------------------------------------
admin <- st_read(path)
# projections <- read.csv("/Users/ethelogallo/Library/CloudStorage/Dropbox/Datasets/raw/projections/projections.csv")
# epsg <- make_EPSG() # fetch projection data from EPSG
# projections <- left_join(projections, epsg, by = c("epsg_code"="code"))
# admin <- left_join(admin, projections, by = c("GID_0" = "country_code"))
# countries <- admin$GID_0

# roads ---------------------------------------------------------------
landcov <- rast(path)
  
# resample landcover to 1km resolution
# grid_landcov <- rast(ext(landcov)) 
# res(grid_landcov) <-  0.0083333 #(malariaAtlas)
# crs(grid_landcov) <- crs(landcov)
# landcover_resamp  <- terra::resample(landcov, grid_landcov, "ngb") 

# writeRaster(landcover_resamp, "Accessibility_per_service/Data/Inputs/landcover_resamp.tif", overwrite = TRUE)


roads_new <-  st_read(path) |> 
  mutate(highway = recode(highway, "yes" = "unclassified")) |> 
  mutate(class = recode(highway, 
                        "trunk" ='1001' ,
                        "trunk_link"= '1002' ,
                        "primary" = '1003' ,
                        "primary_link" = '1004' ,
                        "motorway" = '1005' ,
                        "motorway_link" = '1006' ,
                        "secondary" = '1007' ,
                        "secondary_link" = '1008',
                        "tertiary" = '1009' ,
                        "tertiary_link" = '1010' ,
                        "road" = '1011',
                        "raceway" = '1012',
                        "residential" = '1013',
                        "living_street" = '1014',
                        "service" = '1015' ,
                        "track" = '1016' ,
                        "pedestrian" = '1017' ,
                        "path" = '1018' ,
                        "footway" = '1019',
                        "piste" = '1020',
                        "bridleway" = '1021',
                        "cycleway" = '1022',
                        "steps" = '1023',
                        "unclassified" = '1024',
                        "corridor" = '1025' ,
                        "crossing" = '1026',
                        "proposed" =  '1027',
                        "rest_area" = '1028' , 
                        "services" = '1029',
                        "construction" = '1030',
                        "junction" = '1031'))
roads_new$class <- as.integer(roads_new$class)
roads_new <- roads_new |> 
  st_transform(crs(landcov)) 
roads_new <- roads_new %>% 
  as(., "Spatial")
roads_new <- roads_new %>% 
  vect()

roads_rast <- terra::rasterize(roads_new, landcover_resamp, "class", touches = TRUE)
roads_rast <- mask(roads_rast, admin)
plot(roads_rast)

#save
writeRaster(roads_rast, "Accessibility_per_service/Data/Inputs/roads_raster.tif", overwrite = TRUE)

# water ways --------------------------------------------------------------
## waterlines ratser  
water_lines_new <- st_read(path) |> 
  filter(waterway == "river") |>
  mutate(class = 1) 
water_lines_new <- water_lines_new |> 
  st_transform(crs(landcov))
water_lines_new <- water_lines_new %>% 
  as(., "Spatial") 
water_lines_new <- water_lines_new %>% 
  vect()

water_lines_rast <- terra::rasterize(water_lines_new, landcover_resamp, field = "class", touches = T)
values(water_lines_rast)[values(water_lines_rast) == 0] <- NA
water_lines_rast <- mask(water_lines_rast, admin)

#save
# writeRaster(water_lines_rast, "Accessibility_per_service/Data/Inputs/waterlines_raster.tif", overwrite = TRUE)

## water polygons  
water_poly_new <- st_read(path) |> 
  mutate(class = 1)
water_poly_new <- water_poly_new |> 
  st_transform(crs(landcov)) 
water_poly_new <- water_poly_new %>%
  as(., "Spatial") 
water_poly_new <- water_poly_new %>% 
  vect()

water_poly_rast <- terra::rasterize(water_poly_new, landcover_resamp, field = "class", touches = TRUE)
values(water_poly_rast)[values(water_poly_rast) == 0] <- NA
water_poly_rast <- mask(water_poly_rast,admin)

#save
# writeRaster(water_poly_rast, "Accessibility_per_service/Data/Inputs/waterpoly_raster.tif", overwrite = TRUE)

#plot
plot(water_lines_rast, main = "Water Lines")
plot(water_poly_rast, main = "Water Polygons")


# merged landcover --------------------------------------------------------
##merge
landcover_merge <- terra::merge(water_lines_rast, landcover_resamp)
landcover_merge <- terra::merge(water_poly_rast, landcover_merge)
landcover_merge <- terra::merge(roads_rast, landcover_merge)
## rename 1(waterways) to NA effectively removing them as barriers
values(landcover_merge)[values(landcover_merge) == 1] <- NA

plot(landcover_merge)

#save
# writeRaster(landcover_merge, "Accessibility_per_service/Data/Inputs/landcover_merge_KEN.tif", overwrite = TRUE)






# speeds ------------------------------------------------------------------
## speeds
travelspeeds <- read_xlsx("/Users/ethelogallo/Library/CloudStorage/Dropbox/Datasets/raw/data/raw/travel_scenario/travel speeds.xlsx", sheet = "Supplementary table 1", range = "R2C1:R11C6") # speeds coming from Weiss et al (2020)
landcoverspeeds <- read_xlsx("/Users/ethelogallo/Library/CloudStorage/Dropbox/Datasets/raw/data/raw/travel_scenario/Kenya_travel_sce.xlsx", sheet = "Sheet1") # table containing the speeds for the different land cover classes 

# subset the country names, road classes, and land cover classes
admin <- st_read("/Users/ethelogallo/Library/CloudStorage/Dropbox/Datasets/gadm41_KEN_shp/gadm41_KEN_0.shp") 
projections <- read.csv("/Users/ethelogallo/Library/CloudStorage/Dropbox/Datasets/raw/projections/projections.csv")
epsg <- make_EPSG() # fetch projection data from EPSG
projections <- left_join(projections, epsg, by = c("epsg_code"  ="code"))
admin <- left_join(admin, projections, by = c("GID_0" = "country_code")) 
countries1 <- admin$GID_0 

## define road and landcover classes
road_classes <- c(1001:1031)
landcover_classes <- landcoverspeeds$class

countries1 <- admin$GID_0 
countries1 <- rep(countries1, each = length(road_classes))
road_classes <- rep(road_classes, length(unique(countries1)))


data_roads <- data.frame(country = countries1, 
                         road_class = road_classes) 
data_roads <- data_roads |> 
  mutate(osm_class = recode(road_class,  
                            '1001' = "trunk",
                            '1002' = "trunk_link",
                            '1003' = "primary",
                            '1004' = "primary_link",
                            '1005' = "motorway",
                            '1006' = "motorway_link",
                            '1007' = "secondary",
                            '1008' = "secondary_link",
                            '1009' = "tertiary",
                            '1010' = "tertiary_link",
                            '1011' = "road",
                            '1012' = "raceway",
                            '1013' = "residential",
                            '1014' = "living_street",
                            '1015' = "service",
                            '1016' = "track",
                            '1017' = "pedestrian",
                            '1018' = "path",
                            '1019' = "footway",
                            '1020' = "piste",
                            '1021' = "bridleway",
                            '1022' = "cycleway",
                            '1023' = "steps",
                            '1024' = "unclassified",
                            '1025' = "corridor" ,
                            '1026' = "crossing" ,
                            '1027' =  "proposed",
                            '1028' = "rest_area" , 
                            '1029' =  "services",
                            '1030' = "construction",
                            '1031' = "junction")) # reclassify the different road classes
# merge the empty table with the Weiss et al table
data_roads <- data_roads %>% 
  left_join(.,travelspeeds, by = c("country" = "ISO3", "osm_class" = "OSM Class")) 

# if Weiss table does not have a speed for a certain category, assign the class "other" to a new column
data_roads <- data_roads |> 
  mutate(weiss_class = ifelse(osm_class %in% c("construction", "junction"), "other", osm_class)) 

# only keep important columns
data_roads <- data_roads |> 
  dplyr::select(-c(GAUL, Country_name, OSM_tag_name, `OSM_speed (km/h)`)) 

# join travel speeds of weiss again, including the "other" category
data_roads <- data_roads %>%
  left_join(.,travelspeeds, by = c("country" = "ISO3", "weiss_class" = "OSM Class"))

# the input of the accessibility analysis, requires a time cost of traversing one cell per meter.
data_roads <- data_roads |> 
  mutate(conversion = (60/(`OSM_speed (km/h)`*1000))) |>
  dplyr::select(c(country, Country_name, osm_class, weiss_class, road_class, `OSM_speed (km/h)`, conversion)) 

# filter out the NA
data_roads <- data_roads |> 
  filter(!is.na(`OSM_speed (km/h)`))

# create data frame for land cover classes
countries2 <- admin$GID_0
countries2 <- rep(countries2, each = length(landcover_classes))
landcover_classes <- rep(landcover_classes, length(unique(countries2)))


data_landcover <- data.frame(country = countries2, landcover_class = landcover_classes) |> 
  mutate(landcover_label = recode(landcover_class, 
                                  '0'   = "unknown",
                                  '20'  = "shrubs",
                                  '30'  = "herbaceous vegetation",
                                  '40'  = "cultivated/agriculture",
                                  '50'  = "urban/built up",
                                  '60'  = "bare/sparse vegetation",
                                  '70'  = "snow and ice",
                                  '80'  = "permanent waterbodies",
                                  '81'  = "unclassified",
                                  '90'  = "herbaceous wetland",
                                  '100' = "moss and lichen",
                                  '111' = "closed forest, evergreen needle leaf",
                                  '112' = "closed forest, evergreen broad leaf",
                                  '113' = "closed forest, deciduous needle leaf",
                                  '114' = "closed forest, deciduous broad leaf",
                                  '115' = "closed forest, mixed",
                                  '116' = "closed forest, not matching other definitions",
                                  '121' = "open forest, evergreen needle leaf",
                                  '122' = "open forest, evergreen broad leaf",
                                  '123' = "open forest, deciduous needle leaf",
                                  '124' = "open forest, deciduous needle leaf",
                                  '125' = "open forest, mixed",
                                  '126' = "open forest, not matching other definitions",
                                  '200' = "oceans, seas")) 

data_landcover <- data_landcover %>%
  left_join(.,landcoverspeeds, by = c("landcover_class" = "class")) |> 
  mutate(speed = case_when(
    landcover_class == 80 ~ 20,
    TRUE ~ speed
  ))

data_landcover <- data_landcover |> 
  dplyr::select(-c(landcover_label))  |> 
  # for calculation of minutes per kilometer.
  mutate(conversion = (60/(speed*1000)))

## to remove values already in data_roads
data_landcover <- data_landcover %>%
  filter(!landcover_class %in% data_roads$road_class)


##combine
speeds_roads <- data_roads |> 
  dplyr::select(c(country, road_class, conversion)) |> 
  dplyr::rename(class = "road_class")

speeds_landcover <- data_landcover |> 
  dplyr::select(c(country, landcover_class, conversion)) |> 
  dplyr::rename(class = "landcover_class") 

speeds <- rbind(speeds_roads, speeds_landcover) |> 
  dplyr::select(-country) 

## combine
data_roads_ <- data_roads |> 
  dplyr::select(road_class, osm_class, `OSM_speed (km/h)` ) |> 
  dplyr::rename(class = "road_class", label = "osm_class", speed = `OSM_speed (km/h)`) |> 
  mutate(mode = "motorized")

data_landcover_ <- data_landcover |> 
  dplyr::select(landcover_class, label, speed) |> 
  dplyr::rename(class = landcover_class) |> 
  mutate(mode = ifelse(class >= 1002 & class <= 1029, "motorized", "walking"))

speed <- rbind(data_roads_, data_landcover_)
# write.csv(speed, "/Users/ethelogallo/Documents/GitHub/health_accessibility/Accessibility_per_service/Data/Inputs/speeds.csv")

# friction surface --------------------------------------------------------
# create friction surface
landcover_friction <- terra::classify(landcover_merge, speeds)

#plot
plot(landcover_friction)

#save
# writeRaster(landcover_friction,"Accessibility_per_service/Data/Inputs/friction_surface.tif", overwrite = TRUE)

## Convert friction surface to a transition matrix
friction <- raster(landcover_friction)
T2 <- gdistance::transition(own_friction, function(x) 1/mean(x), 8)
T.GC1 <- gdistance::geoCorrection(T2) 
saveRDS(T.GC1, 'TGC1.rds')
