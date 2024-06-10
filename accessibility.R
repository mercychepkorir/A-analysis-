##' Title: Travel time to facilities with basic maternity services 
##' Description: In this R-script we calculate the travel time to facilities with basic maternity services
##' Author: Mercy Chepkorir, (mercychepkorir2368@gmail.com)
##' Date: 07-June-2024
##' University: University of Nairobi
##' Institute: Center fr Epidemiological Modelling and Analysis

# packages----
pacman::p_load(dplyr,
               plyr,
               terra,
               sf,
               rgdal,
               raster,
               osmextract,
               tidyverse,
               gdalUtils,
               readxl,
               gdistance,
               iterators,
               data.table,
               rmapshaper,
               fasterize,
               exactextractr)
# load data----
#admin shapefiles
adm0 <- st_read("gadm41_KEN_0.shp")
adm1 <- st_read("Kenya/data/vBorders/20240605201406/processed/20240605213317/vBorders_borders.shp") |> 
  st_transform(crs(adm0))

#population of women of reproductive health
pop <- raster("KEN_population_v1_0_agesex_f15_49.tif")

#transition matrix
T.GC <- readRDS('TGC1.rds')

# all facilities
health_facilities <- st_read("health_facilities_kenya.shp")

# facilities with basic maternity services
bms <- st_read("basic_maternity_services.shp")

# travel time calculation-----
# rasterise the facilities
## Point locations
bms_new <- as.data.frame(bms) |>
  dplyr::select(longitude, latitude, facility, county, readiness,keph_level) 

names(bms_new) <- c("X_COORD", "Y_COORD", "facility", "county", "readiness","keph_level")

# Keep only point coordinates within the shapefile bounds
coordinates(bms_new) <- ~ X_COORD + Y_COORD 

# convert to sp from sf
crs(bms_new) <- crs(adm1)
adm1.sp <- as_Spatial(adm1)
overlap <- over(bms_new, adm1.sp)
# bms_new <- bms_new[!is.na(overlap$GID_0),]
points_bms <- as.matrix(bms_new@coords)

# travel time raster
bms_raster <- gdistance::accCost(T.GC, points_bms)
plot(bms_raster)

# save
writeRaster(bms_raster,"bms_access.tif", overwrite = TRUE)

# zonal statistics----
# creating a reclassification matrix
reclass <- c(0, 30, 1, # within 30 minutes
             30, 60, 2, # within 60 minutes
             60, 90, 3, # within 90 minutes
             90, 120, 4, # within 120 minutes
             120, Inf, 5) # greater than 120 minutes
reclass.mat <- matrix(reclass,
                      ncol = 3,
                      byrow = TRUE)

# reclassifying   raster
bms_raster_cat <- reclassify(bms_raster,
                             reclass.mat,
                             include.lowest = TRUE) # this includes a travel time of zero in the lowest group

bms_raster_cat <- rast(bms_raster_cat)
bms_raster_cat_poly_diss <- terra::as.polygons(bms_raster_cat, dissolve=TRUE)
bms_raster_cat_poly_diss <- st_as_sf(bms_raster_cat_poly_diss)
bms_raster_cat_poly_diss <- st_set_crs(bms_raster_cat_poly_diss,crs(adm1))
colnames(bms_raster_cat_poly_diss)[colnames(bms_raster_cat_poly_diss) == "layer"] ="bms_raster"

#zonal statistics
#speeding up st_intersection using a function & zonal statistics
# This function was copied from this thread: https://github.com/r-spatial/sf/issues/801
st_intersection_faster <- function(x,y){
  #faster replacement for st_intersection(x, y,...)
  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}
  st_intersection(x, y_subset)
}

bms_raster_cat_poly_diss.adm1 <- st_intersection_faster(bms_raster_cat_poly_diss, adm1)

bms_raster_cat_poly_diss.adm1 <- bms_raster_cat_poly_diss.adm1 |> 
  mutate(pop =exact_extract(pop, bms_raster_cat_poly_diss.adm1, 
                            'sum'))

#summarise motorised travel time to health facilities in Kenya at the national and county levels 
#at the adm0 level
pop_pct_by_bms_at_adm0 <- as.data.frame(bms_raster_cat_poly_diss.adm1[, c("bms_raster", "pop")]) |> 
  group_by(bms_raster) |> 
  summarise_at(c("pop"), sum,
               na.rm = TRUE) |> 
  ungroup() |> 
  mutate(travel_minutes = case_when(
    bms_raster == 1 ~ "≤30",
    bms_raster == 2 ~ "31-60",
    bms_raster == 3 ~ "61-90",
    bms_raster == 4 ~ "91-120",
    bms_raster == 5 ~ ">120")) |> 
  mutate(pop = round(pop, digits = 0),
         pop_pct0 = round((pop / sum(pop) * 100), digits = 2),
         pop_pct = round((pop / sum(pop) * 100), digits = 1)) |> 
  mutate(transport = "mixed",
         service = "accessibility",
         country = "Kenya")

pop_pct_by_bms_at_adm0 <- pop_pct_by_bms_at_adm0[, c("country", "transport", "service", "travel_minutes", "bms_raster", "pop","pop_pct0","pop_pct")]

#save
# write_csv(pop_pct_by_basic_access_at_adm0, "Accessibility_per_service/App/Data/basic maternity services/proportion_pop_basic.csv")

# at adm1 level
pop_pct_by_bms_n_adm1 <- as.data.frame(bms_raster_cat_poly_diss.adm1[, c("bms_raster", "shapeName", "pop")]) |> 
  dplyr::group_by(shapeName) |> 
  mutate(adm1_pop = round(sum(pop), digits = 0)) |> 
  ungroup() |> 
  mutate(travel_minutes = case_when(
    bms_raster == 1 ~ "≤30",
    bms_raster == 2 ~ "31-60", 
    bms_raster == 3 ~ "61-90",
    bms_raster == 4 ~ "91-120",
    bms_raster == 5 ~ ">120")) |> 
  mutate(pop_1hr = round(pop, digits = 0),
         pop_pct_1hr = round((pop / adm1_pop * 100), digits = 1)) |> 
  group_by(shapeID) |> 
  mutate(pop_2hr = round(sum(pop), digits = 0),
         pop_pct_2hr = round((pop_2hr / adm1_pop * 100), digits = 1),
         rid = row_number(shapeName),
         le_1hour = paste0(pop_1hr, " (", (paste0(sprintf("%1.1f", pop_pct_1hr))),"%)"),
         le_2hour = paste0(pop_2hr, " (", (paste0(sprintf("%1.1f", pop_pct_2hr))),"%)"),
         Transport = "mixed",
         Service = "accessibility",
         Country = "Kenya") |> 
  ungroup()

# proportion per county-----
## generate a table showing column, ≤30 min,31-60 min, 61-90 min,91-120 min, >120 min 
pop_basic_table <- pop_pct_by_bms_n_adm1 |> 
  dplyr::select(shapeName, pop, travel_minutes, bms_raster) |> 
  dplyr::rename(county = shapeName,
                population = pop)

# ensure each county has travel minutes
all_combi <- expand.grid(
  county = unique(pop_basic_table$county),
  bms_raster = unique(pop_basic_table$bms_raster)
)

pop_basic_table_new <- all_combi |> 
  left_join(pop_basic_table, by = c("county", "bms_raster")) |> 
  mutate(travel_minutes = case_when(
    bms_raster == 1 ~ "≤30",
    bms_raster == 2 ~ "31-60",
    bms_raster == 3 ~ "61-90",
    bms_raster == 4 ~ "91-120",
    bms_raster == 5 ~ ">120",
    TRUE ~ travel_minutes
  )) |> 
  replace_na(list(population = 0)) |> 
  dplyr::select(-"bms_raster") 

pop_basic_table_new <- pop_basic_table_new |> 
  group_by(county) |> 
  mutate(total_population = sum(population),
         pct_pop = round((population / total_population) * 100, 2))

basic_county <- pop_basic_table_new |>
  merge(adm1, by.x = "county", by.y = "shapeName") |>
  st_as_sf() |>
  dplyr::select(c("county", "population", "travel_minutes", "total_population", "pct_pop",
                  "geometry"))

basic_county <- basic_county |>
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  group_by(county) |> 
  mutate(cum_pct = cumsum(pct_pop)) |> 
  ungroup() |> 
  mutate(pct_pop_range = case_when(
    cum_pct >= 0 & cum_pct < 21 ~ "0-20",
    cum_pct >= 20 & cum_pct < 41 ~ "21-40",
    cum_pct >= 40 & cum_pct < 61 ~ "41-60",
    cum_pct >= 60 & cum_pct < 81 ~ "61-80",
    cum_pct >= 80 & cum_pct < 101 ~ "81-100",
    TRUE ~ as.character(cum_pct)
  )) |> 
  mutate(pct_pop_range = as.factor(pct_pop_range)) 


# plots----