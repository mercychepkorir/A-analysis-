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
admin <- st_read("gadm41_KEN_0.shp") 
projections <- read.csv("projections.csv")
epsg <- make_EPSG() # fetch projection data from EPSG
projections <- left_join(projections, epsg, by = c("epsg_code"="code"))
admin <- left_join(admin, projections, by = c("GID_0" = "country_code"))
countries <- admin$GID_0

# roads ---------------------------------------------------------------
landcov <- rast("Kenya/data/rLandcover/20240605203339/processed/20240605213450/rLandcover_landcover.tif") 
landcov_ <- terra::project(landcov, crs(admin)) # changing to 4326

# resample landcover to 1km resolution
grid_landcov <- rast(ext(landcov_))
res(grid_landcov) <-  0.0083333
crs(grid_landcov) <- crs(landcov)
landcover_resamp  <- terra::resample(landcov_, grid_landcov, "near")

#save
writeRaster(landcover_resamp, "Kenya/data/rLandcover/20240605203339/processed/20240605213450/rLandcover_resamp.tif", overwrite = TRUE)

roads_new <-  st_read("Kenya/data/vRoads/20240326140236/processed/20240605213612/vRoads_roads.shp")
roads_new <- roads_new |> 
  st_transform(crs(landcov_)) 
roads_new <- roads_new %>% 
  as(., "Spatial")
roads_new <- roads_new %>% 
  vect()

roads_rast <- terra::rasterize(roads_new, landcover_resamp, "class", touches = TRUE)
roads_rast <- mask(roads_rast, admin)
plot(roads_rast)

#save
writeRaster(roads_rast, "Kenya/data/vRoads/20240326140236/processed/vRoads_raster.tif", overwrite = TRUE)

# barriers to movement --------------------------------------------------------------
## waterlines ratser  
water_lines_new <- st_read("Kenya/data/vWaterLines/20240326154204/processed/20240605213626/vWaterLines_waterlines.shp") |> 
  mutate(class = 1) 
water_lines_new <- water_lines_new |> 
  st_transform(crs(landcov_))
water_lines_new <- water_lines_new %>% 
  as(., "Spatial") 
water_lines_new <- water_lines_new %>% 
  vect()

water_lines_rast <- terra::rasterize(water_lines_new, landcover_resamp, field = "class", touches = T)
values(water_lines_rast)[values(water_lines_rast) == 0] <- NA
water_lines_rast <- mask(water_lines_rast, admin)

plot(water_lines_rast, col = "blue")

#save
writeRaster(water_lines_rast, "Kenya/data/vWaterLines/20240326154204/processed/vWaterLines_raster.tif", overwrite = TRUE)

## water polygons  
water_poly_new <- st_read("Kenya/data/vNaturalPolygons/20240326193051/processed/20240605213554/vNaturalPolygons_naturalpolygons.shp") |> 
  mutate(class = 1)
water_poly_new <- water_poly_new |> 
  st_transform(crs(landcov_)) 
water_poly_new <- water_poly_new %>%
  as(., "Spatial") 
water_poly_new <- water_poly_new %>% 
  vect()

water_poly_rast <- terra::rasterize(water_poly_new, landcover_resamp, field = "class", touches = TRUE)
values(water_poly_rast)[values(water_poly_rast) == 0] <- NA
water_poly_rast <- mask(water_poly_rast,admin)

plot(water_poly_rast, col="blue")

#save
writeRaster(water_poly_rast, "Kenya/data/vNaturalPolygons/20240326193051/processed/vNaturalPolygons_raster.tif", overwrite = TRUE)


# merged land cover --------------------------------------------------------

##merge
landcover_merge <- terra::merge(water_lines_rast, landcover_resamp)
landcover_merge <- terra::merge(water_poly_rast, landcover_merge)
landcover_merge <- terra::merge(roads_rast, landcover_merge)

## rename 1(waterways) to NA effectively removing them as barriers
values(landcover_merge)[values(landcover_merge) == 1] <- NA

plot(landcover_merge)

#save
writeRaster(landcover_merge, "Kenya/data/rLandcover/20240605203339/processed/rLandcover_merged.tif", overwrite = TRUE)

# speeds ------------------------------------------------------------------
## load speed tables
travelspeeds <- read_xlsx("travel speeds.xlsx", 
                          sheet = "Supplementary table 1", 
                          range = "R2C1:R11C6") # speeds coming from Weiss et al (2020)
landcoverspeeds <- read_xlsx("Kenya_travel_sce.xlsx", 
                             sheet = "Sheet1") # table containing the speeds for the different land cover classes 

## define road and land cover classes
road_classes <- c(1001:1031)
landcover_classes <- landcoverspeeds$class

data_roads <- data.frame(country = countries, 
                         road_class = road_classes) 

# reclassify the different road classes
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
                            '1031' = "junction")) 

# merge the empty table with the Weiss et al table
data_roads <- data_roads %>% 
  left_join(.,travelspeeds, 
            by = c("country" = "ISO3", "osm_class" = "OSM Class")) 

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
data_landcover <- data.frame(landcover_class = landcover_classes) |> 
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
  dplyr::select(c(road_class, conversion)) |> 
  dplyr::rename(class = "road_class")

speeds_landcover <- data_landcover |> 
  dplyr::select(c(landcover_class, conversion)) |> 
  dplyr::rename(class = "landcover_class") 

speeds <- rbind(speeds_roads, speeds_landcover) 

# friction surface --------------------------------------------------------
# create friction surface
landcover_friction <- terra::classify(landcover_merge, speeds)

#plot
plot(landcover_friction)

#save
writeRaster(landcover_friction,"Kenya/data/friction_surface.tif", overwrite = TRUE)

## Convert friction surface to a transition matrix
friction <- raster(landcover_friction)
T1 <- gdistance::transition(friction, function(x) 1/mean(x), 8)
T.GC1 <- gdistance::geoCorrection(T1) 
saveRDS(T.GC1, 'TGC1.rds')
