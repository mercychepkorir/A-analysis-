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
               exactextractr,
               classInt,
               ggspatial,
               cowplot)
# load data----
#admin shapefiles
adm0 <- st_read("gadm41_KEN_0.shp")
adm1 <- st_read("gadm41_KEN_1.shp") 

#population of women of reproductive health
pop <- raster("KEN_population_v1_0_agesex_f15_49.tif")

#transition matrix
T.GC <- readRDS('TGC1.rds')

# all facilities
health_facilities <- st_read("health_facilities_kenya.shp")

## BASIC MATERNITY SERVICES-----
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
pop_pct_by_bms_n_adm1 <- as.data.frame(bms_raster_cat_poly_diss.adm1[, c("bms_raster", "NAME_1", "pop")]) |> 
  dplyr::group_by(NAME_1) |> 
  reframe(adm1_pop = round(sum(pop), digits = 0)) |> 
  ungroup() 

pop_per_county <- pop_pct_by_bms_n_adm1 |> 
  merge(bms_raster_cat_poly_diss.adm1, by = "NAME_1") |> 
  dplyr::select("bms_raster", "NAME_1", "pop", "adm1_pop") |> 
  mutate(travel_minutes = case_when(
    bms_raster == 1 ~ "≤30",
    bms_raster == 2 ~ "31-60", 
    bms_raster == 3 ~ "61-90",
    bms_raster == 4 ~ "91-120",
    bms_raster == 5 ~ ">120")) |> 
  mutate(pop_1hr = round(pop, digits = 0),
         pop_pct_1hr = round((pop / adm1_pop * 100), digits = 1)) |> 
  group_by(NAME_1) |> 
  mutate(pop_2hr = round(sum(pop), digits = 0),
         pop_pct_2hr = round((pop_2hr / adm1_pop * 100), digits = 1),
         rid = row_number(NAME_1),
         le_1hour = paste0(pop_1hr, " (", (paste0(sprintf("%1.1f", pop_pct_1hr))),"%)"),
         le_2hour = paste0(pop_2hr, " (", (paste0(sprintf("%1.1f", pop_pct_2hr))),"%)"),
         Transport = "mixed",
         Service = "accessibility",
         Country = "Kenya") |> 
  ungroup() 

## generate a table showing column, ≤30 min,31-60 min, 61-90 min,91-120 min, >120 min 
pop_basic_table <- pop_per_county |> 
  dplyr::select(NAME_1, pop, adm1_pop, travel_minutes, bms_raster) |> 
  dplyr::rename(county = NAME_1,
                population = pop,
                county_pop = adm1_pop)

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
  mutate(pct_pop = round((population / county_pop) * 100, 2)) 

basic_county <- pop_basic_table_new |>
  merge(adm1, by.x = "county", by.y = "NAME_1") |>
  st_as_sf() |>
  dplyr::select(c("county", "population", "travel_minutes", "county_pop", "pct_pop",
                  "geometry"))

basic_county2 <- basic_county |>
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  group_by(county) |> 
  replace_na(list(county_pop = 0)) |>
  replace_na(list(pct_pop = 0)) |> 
  reframe(cum_pct = cumsum(pct_pop)) |> 
  ungroup()

basic_county3 <-  as.data.frame(basic_county) |> 
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  cbind(basic_county2) |> 
  dplyr::select(-7) |> 
  mutate(pct_pop_range = case_when(
    cum_pct >= 0 & cum_pct < 21 ~ "0-20",
    cum_pct >= 20 & cum_pct < 41 ~ "21-40",
    cum_pct >= 40 & cum_pct < 61 ~ "41-60",
    cum_pct >= 60 & cum_pct < 81 ~ "61-80",
    cum_pct >= 80 & cum_pct < 101 ~ "81-100",
    TRUE ~ as.character(cum_pct)
  )) |> 
  mutate(pct_pop_range = as.factor(pct_pop_range))

# travel time to nearest facility plot-----
bms_df <- raster::as.data.frame(bms_raster, xy=TRUE) 
bms_df <- na.omit(bms_df) |> 
  dplyr::rename("bms_raster" = "layer") 
bms_df <- bms_df[is.finite(bms_df$bms_raster), ]

# create fixed interval
fix.brks <- classIntervals(bms_df$bms_raster, n = 5, style="fixed",
                           fixedBreaks=c(-0.1, 30, 60, 90, 120, Inf), # used '-0.1' to make sure 0 is included 
                           intervalClosure="right")

bms_df <- bms_df |> 
  mutate(pop_cat = cut(bms_raster, fix.brks$brks))

hist_factor = 2
legend_w = 24
vline_w = 0.2


bms_map <- ggplot() + 
  geom_sf(data = adm0, fill = "white", alpha = 1,show.legend = NA, color = NA) +
  geom_raster(data = bms_df, aes(x=x, y=y, fill = factor(pop_cat)),
              interpolate = FALSE, show.legend = NA) +
  scale_fill_manual(guide = "none",values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#969696") +
  # geom_sf(data = adm1, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  geom_sf(data = adm0, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  annotation_scale(location = "bl", width_hint = 0.25,pad_x = unit(0.26, "in"), pad_y = unit(0.4, "in"),
                   height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.57, "in"), pad_y = unit(0.6, "in"),
                         height = unit(0.9, "cm"), width = unit(0.81, "cm"),style = north_arrow_fancy_orienteering(text_size = 10)) +
  geom_sf(data = bms, aes(geometry = geometry, col = "kmfl"),size = 0.5) +
  scale_colour_manual(name = " ", values = "black",labels =c("health facility"),guide = guide_legend())+
  theme(legend.position = c(1.11, 0.75),legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'),legend.key.width = unit(0.4, 'cm'), 
        legend.text = element_text(size=10.0),legend.spacing.y = unit(0, "pt")) +
  theme(plot.margin = margin(t = 0,r = 20,b = 0,l = 0),panel.background = element_blank(),
        axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        # plot.background = element_rect(fill='transparent', color=NA),legend.background = element_rect(fill='transparent'),
        # legend.box.background = element_rect(fill='transparent', color=NA),legend.key=element_rect(fill="transparent"), 
        # legend.margin =  margin(0,0,0,0,unit="pt")) +
  ggtitle("Accessibility to facilities with basic maternity services")


## bar chart legend using ggplot
bms_bar <- pop_pct_by_bms_at_adm0 |> 
  mutate(pop_pct1 = pop_pct0 + legend_w,alpha = 1,label = paste0(sprintf("%1.1f", pop_pct), "%"))

##bar chart legend 
bms_barchart <- ggplot(bms_bar, aes(x = reorder(travel_minutes,-bms_raster), y = pop_pct1/hist_factor,
                                                  fill = as.factor(bms_raster), alpha=factor(alpha))) +
  coord_flip() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#d9d9d9") +
  scale_alpha_manual(values = c("1" = 1),guide = "none") +
  geom_bar(stat = 'identity',  width = 0.8, color="#d9d9d9", linewidth = 0.05, na.rm = TRUE) + 
  # geom_hline(aes(yintercept=legend_w/hist_factor),col = "white", linewidth = 0.55) +
  geom_text(aes(label=label,y = 15),size = 2.8, position=position_dodge(width=0.9), vjust= 0.5,hjust= 0) + 
  scale_colour_manual(values=c("#000000", "#de77ae")) +
  theme_bw() +
  labs(title="Travel minutes to nearest facility given population",x = "", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size=9.5, colour="#000000", hjust = 0, vjust = 0),
        axis.text.y = element_text(size=8.5, colour="#000000"),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.ticks=element_blank(),axis.line = element_blank(),
        axis.text.x = element_blank(),legend.position = "none",
        plot.margin = margin(t = 5, r = 0,b = 0,l = 10)) +
  theme(panel.background = element_rect(fill='transparent', color=NA),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        aspect.ratio = 1/1.2) 

##combined plot 
combined_plots <- cowplot::ggdraw() +
  draw_plot(bms_map, height = 1, width = 0.8) +
  expand_limits(x = 0.5, y = 0) +
  draw_plot(bms_barchart, x = 0.59, y = 0.39, hjust = 0, vjust = 0.1,
            width = 0.32, height = 0.32) +
  theme(plot.margin = margin(t=0.1, l= -0.1, b=0.1, r=0.6, "cm"),
        plot.background = element_rect(fill='transparent', color=NA)) 


#save plot
dev.off()
ggsave(
  "output/bms_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

# cumulative proportion per county plot----
basic_county3$travel_minutes <- factor(basic_county3$travel_minutes, 
                                       levels = c("≤30", "31-60", "61-90", "91-120", ">120"))

basic_county3$cum_pct <- ifelse(basic_county3$cum_pct > 100, 100, basic_county3$cum_pct)
basic_county3$pct_pop_range2 <- cut(basic_county3$cum_pct, 
                                   breaks = c(-Inf, 20, 40, 60, 80, 100), 
                                   labels = c("0-20", "21-40", "41-60", "61-80", "81-100"),
                                   right = TRUE, include.lowest = TRUE)

bms_county_map <- basic_county3 |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = pct_pop_range2)) +
  theme_void() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                    limits = c("0-20","21-40", "41-60","61-80", "81-100"),
                    drop = FALSE) +  # Ensure all levels are shown in the legend
  labs(fill = "Population (%)") +
  ggtitle("Cumulative proportion of population with access \nto facilities with basic maternity services") +
  theme(plot.title = element_text(colour = 'black', hjust = .5, face = 'bold'),
        legend.text = element_text(colour = 'black', size = 12, face = 'bold'),
        legend.title = element_text(colour = 'black', size = 12, face = 'bold')) +
  facet_wrap(~travel_minutes)
  

#save plot
dev.off()
ggsave(
  "output/bms_county_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

#final data table
bms_table <- pop_basic_table_new |> 
  mutate(population = round(population),
         population = paste0(population," (", (paste0(sprintf("%1.1f", pct_pop))),"%)" )) |> 
  dplyr::select(-c(county_pop, pct_pop)) |> 
  pivot_wider(names_from = travel_minutes, values_from = population) |> 
  mutate(across(where(is.numeric), ~format(., big.mark   = ","))) |> 
  as.data.frame() |> 
  merge(basic_county, by = "county") |> 
  dplyr::select(c("county","county_pop", "≤30", "31-60", "61-90", "91-120", ">120")) |> 
  filter(!is.na(county_pop)) |> 
  # mutate(county_pop = round(county_pop)) |> 
  distinct(county, .keep_all = T)
names(bms_table) <- c("County", "Total Population","≤30 min", "31-60 min", "61-90 min", "91-120 min", ">120 min")

#save 
write_csv(bms_table, "output/bms_county_table.csv")



# readiness------
# facilities with basic maternity services
bmr <- st_read("basic_maternity_services.shp") |> 
  filter(readiness >= 75)

# travel time calculation-----
# rasterise the facilities
## Point locations
bmr_new <- as.data.frame(bmr) |>
  dplyr::select(longitude, latitude, facility, county, readiness,keph_level) 

names(bmr_new) <- c("X_COORD", "Y_COORD", "facility", "county", "readiness","keph_level")

# Keep only point coordinates within the shapefile bounds
coordinates(bmr_new) <- ~ X_COORD + Y_COORD 

# convert to sp from sf
crs(bmr_new) <- crs(adm1)
adm1.sp <- as_Spatial(adm1)
overlap <- over(bmr_new, adm1.sp)
bmr_new <- bmr_new[!is.na(overlap$GID_0),]
points_bmr <- as.matrix(bmr_new@coords)

# travel time raster
bmr_raster <- gdistance::accCost(T.GC, points_bmr)
plot(bmr_raster)

# save
writeRaster(bmr_raster,"bmr_access.tif", overwrite = TRUE)

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
bmr_raster_cat <- reclassify(bmr_raster,
                             reclass.mat,
                             include.lowest = TRUE) # this includes a travel time of zero in the lowest group

bmr_raster_cat <- rast(bmr_raster_cat)
bmr_raster_cat_poly_diss <- terra::as.polygons(bmr_raster_cat, dissolve=TRUE)
bmr_raster_cat_poly_diss <- st_as_sf(bmr_raster_cat_poly_diss)
bmr_raster_cat_poly_diss <- st_set_crs(bmr_raster_cat_poly_diss,crs(adm1))
colnames(bmr_raster_cat_poly_diss)[colnames(bmr_raster_cat_poly_diss) == "layer"] ="bmr_raster"

#zonal statistics
#speeding up st_intersection using a function & zonal statistics
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

bmr_raster_cat_poly_diss.adm1 <- st_intersection_faster(bmr_raster_cat_poly_diss, adm1)

bmr_raster_cat_poly_diss.adm1 <- bmr_raster_cat_poly_diss.adm1 |> 
  mutate(pop =exact_extract(pop, bmr_raster_cat_poly_diss.adm1, 
                            'sum'))

#summarise motorised travel time to health facilities in Kenya at the national and county levels 
#at the adm0 level
pop_pct_by_bmr_at_adm0 <- as.data.frame(bmr_raster_cat_poly_diss.adm1[, c("bmr_raster", "pop")]) |> 
  group_by(bmr_raster) |> 
  summarise_at(c("pop"), sum,
               na.rm = TRUE) |> 
  ungroup() |> 
  mutate(travel_minutes = case_when(
    bmr_raster == 1 ~ "≤30",
    bmr_raster == 2 ~ "31-60",
    bmr_raster == 3 ~ "61-90",
    bmr_raster == 4 ~ "91-120",
    bmr_raster == 5 ~ ">120")) |> 
  mutate(pop = round(pop, digits = 0),
         pop_pct0 = round((pop / sum(pop) * 100), digits = 2),
         pop_pct = round((pop / sum(pop) * 100), digits = 1)) |> 
  mutate(transport = "mixed",
         service = "accessibility",
         country = "Kenya")

pop_pct_by_bmr_at_adm0 <- pop_pct_by_bmr_at_adm0[, c("country", "transport", "service", "travel_minutes", "bmr_raster", "pop","pop_pct0","pop_pct")]

#save
# write_csv(pop_pct_by_basic_access_at_adm0, "Accessibility_per_service/App/Data/basic maternity services/proportion_pop_basic.csv")

# at adm1 level
pop_pct_by_bmr_n_adm1 <- as.data.frame(bmr_raster_cat_poly_diss.adm1[, c("bmr_raster", "NAME_1", "pop")]) |> 
  dplyr::group_by(NAME_1) |> 
  reframe(adm1_pop = round(sum(pop), digits = 0)) |> 
  ungroup() 

pop_per_county <- pop_pct_by_bmr_n_adm1 |> 
  merge(bmr_raster_cat_poly_diss.adm1, by = "NAME_1") |> 
  dplyr::select("bmr_raster", "NAME_1", "pop", "adm1_pop") |> 
  mutate(travel_minutes = case_when(
    bmr_raster == 1 ~ "≤30",
    bmr_raster == 2 ~ "31-60", 
    bmr_raster == 3 ~ "61-90",
    bmr_raster == 4 ~ "91-120",
    bmr_raster == 5 ~ ">120")) |> 
  mutate(pop_1hr = round(pop, digits = 0),
         pop_pct_1hr = round((pop / adm1_pop * 100), digits = 1)) |> 
  group_by(NAME_1) |> 
  mutate(pop_2hr = round(sum(pop), digits = 0),
         pop_pct_2hr = round((pop_2hr / adm1_pop * 100), digits = 1),
         rid = row_number(NAME_1),
         le_1hour = paste0(pop_1hr, " (", (paste0(sprintf("%1.1f", pop_pct_1hr))),"%)"),
         le_2hour = paste0(pop_2hr, " (", (paste0(sprintf("%1.1f", pop_pct_2hr))),"%)"),
         Transport = "mixed",
         Service = "accessibility",
         Country = "Kenya") |> 
  ungroup() 

## generate a table showing column, ≤30 min,31-60 min, 61-90 min,91-120 min, >120 min 
pop_basic_table <- pop_per_county |> 
  dplyr::select(NAME_1, pop, adm1_pop, travel_minutes, bmr_raster) |> 
  dplyr::rename(county = NAME_1,
                population = pop,
                county_pop = adm1_pop)

# ensure each county has travel minutes
all_combi <- expand.grid(
  county = unique(pop_basic_table$county),
  bmr_raster = unique(pop_basic_table$bmr_raster)
)

pop_basic_table_new <- all_combi |> 
  left_join(pop_basic_table, by = c("county", "bmr_raster")) |> 
  mutate(travel_minutes = case_when(
    bmr_raster == 1 ~ "≤30",
    bmr_raster == 2 ~ "31-60",
    bmr_raster == 3 ~ "61-90",
    bmr_raster == 4 ~ "91-120",
    bmr_raster == 5 ~ ">120",
    TRUE ~ travel_minutes
  )) |> 
  replace_na(list(population = 0)) |> 
  dplyr::select(-"bmr_raster") 

pop_basic_table_new <- pop_basic_table_new |> 
  group_by(county) |> 
  mutate(pct_pop = round((population / county_pop) * 100, 2)) 

basic_county <- pop_basic_table_new |>
  merge(adm1, by.x = "county", by.y = "NAME_1") |>
  st_as_sf() |>
  dplyr::select(c("county", "population", "travel_minutes", "county_pop", "pct_pop",
                  "geometry"))

basic_county2 <- basic_county |>
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  group_by(county) |> 
  replace_na(list(county_pop = 0)) |>
  replace_na(list(pct_pop = 0)) |> 
  reframe(cum_pct = cumsum(pct_pop)) |> 
  ungroup()

basic_county3 <-  as.data.frame(basic_county) |> 
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  cbind(basic_county2) |> 
  dplyr::select(-7) |> 
  mutate(pct_pop_range = case_when(
    cum_pct >= 0 & cum_pct < 21 ~ "0-20",
    cum_pct >= 20 & cum_pct < 41 ~ "21-40",
    cum_pct >= 40 & cum_pct < 61 ~ "41-60",
    cum_pct >= 60 & cum_pct < 81 ~ "61-80",
    cum_pct >= 80 & cum_pct < 101 ~ "81-100",
    TRUE ~ as.character(cum_pct)
  )) |> 
  mutate(pct_pop_range = as.factor(pct_pop_range))

# travel time to nearest facility plot-----
bmr_df <- raster::as.data.frame(bmr_raster, xy=TRUE) 
bmr_df <- na.omit(bmr_df) |> 
  dplyr::rename("bmr_raster" = "layer") 
bmr_df <- bmr_df[is.finite(bmr_df$bmr_raster), ]

# create fixed interval
fix.brks <- classIntervals(bmr_df$bmr_raster, n = 5, style="fixed",
                           fixedBreaks=c(-0.1, 30, 60, 90, 120, Inf), # used '-0.1' to make sure 0 is included 
                           intervalClosure="right")

bmr_df <- bmr_df |> 
  mutate(pop_cat = cut(bmr_raster, fix.brks$brks))

hist_factor = 2
legend_w = 24
vline_w = 0.2


bmr_map <- ggplot() + 
  geom_sf(data = adm0, fill = "white", alpha = 1,show.legend = NA, color = NA) +
  geom_raster(data = bmr_df, aes(x=x, y=y, fill = factor(pop_cat)),
              interpolate = FALSE, show.legend = NA) +
  scale_fill_manual(guide = "none",values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#969696") +
  # geom_sf(data = adm1, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  geom_sf(data = adm0, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  annotation_scale(location = "bl", width_hint = 0.25,pad_x = unit(0.26, "in"), pad_y = unit(0.4, "in"),
                   height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.57, "in"), pad_y = unit(0.6, "in"),
                         height = unit(0.9, "cm"), width = unit(0.81, "cm"),style = north_arrow_fancy_orienteering(text_size = 10)) +
  geom_sf(data = bmr, aes(geometry = geometry, col = "kmfl"),size = 0.5) +
  scale_colour_manual(name = " ", values = "black",labels =c("health facility"),guide = guide_legend())+
  theme(legend.position = c(1.11, 0.75),legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'),legend.key.width = unit(0.4, 'cm'), 
        legend.text = element_text(size=10.0),legend.spacing.y = unit(0, "pt")) +
  theme(plot.margin = margin(t = 0,r = 20,b = 0,l = 0),panel.background = element_blank(),
        axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # plot.background = element_rect(fill='transparent', color=NA),legend.background = element_rect(fill='transparent'),
  # legend.box.background = element_rect(fill='transparent', color=NA),legend.key=element_rect(fill="transparent"), 
  # legend.margin =  margin(0,0,0,0,unit="pt")) +
  ggtitle("Accessibility to facilities with 75% readiness \nfor basic maternity services")


## bar chart legend using ggplot
bmr_bar <- pop_pct_by_bmr_at_adm0 |> 
  mutate(pop_pct1 = pop_pct0 + legend_w,alpha = 1,label = paste0(sprintf("%1.1f", pop_pct), "%"))

##bar chart legend 
bmr_barchart <- ggplot(bmr_bar, aes(x = reorder(travel_minutes,-bmr_raster), y = pop_pct1/hist_factor,
                                    fill = as.factor(bmr_raster), alpha=factor(alpha))) +
  coord_flip() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#d9d9d9") +
  scale_alpha_manual(values = c("1" = 1),guide = "none") +
  geom_bar(stat = 'identity',  width = 0.8, color="#d9d9d9", linewidth = 0.05, na.rm = TRUE) + 
  # geom_hline(aes(yintercept=legend_w/hist_factor),col = "white", linewidth = 0.55) +
  geom_text(aes(label=label,y = 15),size = 2.8, position=position_dodge(width=0.9), vjust= 0.5,hjust= 0) + 
  scale_colour_manual(values=c("#000000", "#de77ae")) +
  theme_bw() +
  labs(title="Travel minutes to nearest facility given population",x = "", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size=9.5, colour="#000000", hjust = 0, vjust = 0),
        axis.text.y = element_text(size=8.5, colour="#000000"),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.ticks=element_blank(),axis.line = element_blank(),
        axis.text.x = element_blank(),legend.position = "none",
        plot.margin = margin(t = 5, r = 0,b = 0,l = 10)) +
  theme(panel.background = element_rect(fill='transparent', color=NA),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        aspect.ratio = 1/1.2) 

##combined plot 
combined_plots <- cowplot::ggdraw() +
  draw_plot(bmr_map, height = 1, width = 0.8) +
  expand_limits(x = 0.5, y = 0) +
  draw_plot(bmr_barchart, x = 0.59, y = 0.39, hjust = 0, vjust = 0.1,
            width = 0.32, height = 0.32) +
  theme(plot.margin = margin(t=0.1, l= -0.1, b=0.1, r=0.6, "cm"),
        plot.background = element_rect(fill='transparent', color=NA)) 


#save plot
dev.off()
ggsave(
  "output/bmr_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

# cumulative proportion per county plot----
basic_county3$travel_minutes <- factor(basic_county3$travel_minutes, 
                                       levels = c("≤30", "31-60", "61-90", "91-120", ">120"))

basic_county3$cum_pct <- ifelse(basic_county3$cum_pct > 100, 100, basic_county3$cum_pct)
basic_county3$pct_pop_range2 <- cut(basic_county3$cum_pct, 
                                    breaks = c(-Inf, 20, 40, 60, 80, 100), 
                                    labels = c("0-20", "21-40", "41-60", "61-80", "81-100"),
                                    right = TRUE, include.lowest = TRUE)

bmr_county_map <- basic_county3 |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = pct_pop_range2)) +
  theme_void() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                    limits = c("0-20","21-40", "41-60","61-80", "81-100"),
                    drop = FALSE) +  # Ensure all levels are shown in the legend
  labs(fill = "Population (%)") +
  ggtitle("Cumulative proportion of population with \naccess to facilities with 75% readiness for \nbasic maternity services") +
  theme(plot.title = element_text(colour = 'black', hjust = .5, face = 'bold'),
        legend.text = element_text(colour = 'black', size = 12, face = 'bold'),
        legend.title = element_text(colour = 'black', size = 12, face = 'bold')) +
  facet_wrap(~travel_minutes)


#save plot
dev.off()
ggsave(
  "output/bmr_county_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

#final data table
bmr_table <- pop_basic_table_new |> 
  mutate(population = round(population),
         population = paste0(population," (", (paste0(sprintf("%1.1f", pct_pop))),"%)" )) |> 
  dplyr::select(-c(county_pop, pct_pop)) |> 
  pivot_wider(names_from = travel_minutes, values_from = population) |> 
  mutate(across(where(is.numeric), ~format(., big.mark   = ","))) |> 
  as.data.frame() |> 
  merge(basic_county, by = "county") |> 
  dplyr::select(c("county","county_pop", "≤30", "31-60", "61-90", "91-120", ">120")) |> 
  filter(!is.na(county_pop)) |> 
  # mutate(county_pop = round(county_pop)) |> 
  distinct(county, .keep_all = T)
names(bmr_table) <- c("County", "Total Population","≤30 min", "31-60 min", "61-90 min", "91-120 min", ">120 min")

#save 
write_csv(bmr_table, "output/bmr_county_table.csv")



## COMPREHENSIVE MATERNITY SERVICES-----
# facilities with basic maternity services
cms <- st_read("comprehensive_maternity_services.shp")

# travel time calculation-----
# rasterise the facilities
## Point locations
cms_new <- as.data.frame(cms) |>
  dplyr::select(longitude, latitude, facility, county, readiness,keph_level) 

names(cms_new) <- c("X_COORD", "Y_COORD", "facility", "county", "readiness","keph_level")

# Keep only point coordinates within the shapefile bounds
coordinates(cms_new) <- ~ X_COORD + Y_COORD 

# convert to sp from sf
crs(cms_new) <- crs(adm1)
adm1.sp <- as_Spatial(adm1)
overlap1 <- over(cms_new, adm1.sp)
cms_new <- cms_new[!is.na(overlap1$GID_0),]
points_cms <- as.matrix(cms_new@coords)

# travel time raster
cms_raster <- gdistance::accCost(T.GC, points_cms)
plot(cms_raster)

# save
writeRaster(cms_raster,"cms_access.tif", overwrite = TRUE)

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
cms_raster_cat <- reclassify(cms_raster,
                             reclass.mat,
                             include.lowest = TRUE) # this includes a travel time of zero in the lowest group

cms_raster_cat <- rast(cms_raster_cat)
cms_raster_cat_poly_diss <- terra::as.polygons(cms_raster_cat, dissolve=TRUE)
cms_raster_cat_poly_diss <- st_as_sf(cms_raster_cat_poly_diss)
cms_raster_cat_poly_diss <- st_set_crs(cms_raster_cat_poly_diss,crs(adm1))
colnames(cms_raster_cat_poly_diss)[colnames(cms_raster_cat_poly_diss) == "layer"] ="cms_raster"

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

cms_raster_cat_poly_diss.adm1 <- st_intersection_faster(cms_raster_cat_poly_diss, adm1)

cms_raster_cat_poly_diss.adm1 <- cms_raster_cat_poly_diss.adm1 |> 
  mutate(pop =exact_extract(pop, cms_raster_cat_poly_diss.adm1, 
                            'sum'))

#summarise motorised travel time to health facilities in Kenya at the national and county levels 
#at the adm0 level
pop_pct_by_cms_at_adm0 <- as.data.frame(cms_raster_cat_poly_diss.adm1[, c("cms_raster", "pop")]) |> 
  group_by(cms_raster) |> 
  summarise_at(c("pop"), sum,
               na.rm = TRUE) |> 
  ungroup() |> 
  mutate(travel_minutes = case_when(
    cms_raster == 1 ~ "≤30",
    cms_raster == 2 ~ "31-60",
    cms_raster == 3 ~ "61-90",
    cms_raster == 4 ~ "91-120",
    cms_raster == 5 ~ ">120")) |> 
  mutate(pop = round(pop, digits = 0),
         pop_pct0 = round((pop / sum(pop) * 100), digits = 2),
         pop_pct = round((pop / sum(pop) * 100), digits = 1)) |> 
  mutate(transport = "mixed",
         service = "accessibility",
         country = "Kenya")

pop_pct_by_cms_at_adm0 <- pop_pct_by_cms_at_adm0[, c("country", "transport", "service", "travel_minutes", "cms_raster", "pop","pop_pct0","pop_pct")]

#save
# write_csv(pop_pct_by_basic_access_at_adm0, "Accessibility_per_service/App/Data/basic maternity services/proportion_pop_basic.csv")

# at adm1 level
pop_pct_by_cms_n_adm1 <- as.data.frame(cms_raster_cat_poly_diss.adm1[, c("cms_raster", "NAME_1", "pop")]) |> 
  dplyr::group_by(NAME_1) |> 
  reframe(adm1_pop = round(sum(pop), digits = 0)) |> 
  ungroup() 

pop_per_county <- pop_pct_by_cms_n_adm1 |> 
  merge(cms_raster_cat_poly_diss.adm1, by = "NAME_1") |> 
  dplyr::select("cms_raster", "NAME_1", "pop", "adm1_pop") |> 
  mutate(travel_minutes = case_when(
    cms_raster == 1 ~ "≤30",
    cms_raster == 2 ~ "31-60", 
    cms_raster == 3 ~ "61-90",
    cms_raster == 4 ~ "91-120",
    cms_raster == 5 ~ ">120")) |> 
  mutate(pop_1hr = round(pop, digits = 0),
         pop_pct_1hr = round((pop / adm1_pop * 100), digits = 1)) |> 
  group_by(NAME_1) |> 
  mutate(pop_2hr = round(sum(pop), digits = 0),
         pop_pct_2hr = round((pop_2hr / adm1_pop * 100), digits = 1),
         rid = row_number(NAME_1),
         le_1hour = paste0(pop_1hr, " (", (paste0(sprintf("%1.1f", pop_pct_1hr))),"%)"),
         le_2hour = paste0(pop_2hr, " (", (paste0(sprintf("%1.1f", pop_pct_2hr))),"%)"),
         Transport = "mixed",
         Service = "accessibility",
         Country = "Kenya") |> 
  ungroup() 

## generate a table showing column, ≤30 min,31-60 min, 61-90 min,91-120 min, >120 min 
pop_comprehensive_table <- pop_per_county |> 
  dplyr::select(NAME_1, pop, adm1_pop, travel_minutes, cms_raster) |> 
  dplyr::rename(county = NAME_1,
                population = pop,
                county_pop = adm1_pop)

# ensure each county has travel minutes
all_combi <- expand.grid(
  county = unique(pop_comprehensive_table$county),
  cms_raster = unique(pop_comprehensive_table$cms_raster)
)

pop_comprehensive_table_new <- all_combi |> 
  left_join(pop_comprehensive_table, by = c("county", "cms_raster")) |> 
  mutate(travel_minutes = case_when(
    cms_raster == 1 ~ "≤30",
    cms_raster == 2 ~ "31-60",
    cms_raster == 3 ~ "61-90",
    cms_raster == 4 ~ "91-120",
    cms_raster == 5 ~ ">120",
    TRUE ~ travel_minutes
  )) |> 
  replace_na(list(population = 0)) |> 
  dplyr::select(-"cms_raster") 

pop_comprehensive_table_new <- pop_comprehensive_table_new |> 
  group_by(county) |> 
  mutate(pct_pop = round((population / county_pop) * 100, 2)) 

comprehensive_county <- pop_comprehensive_table_new |>
  merge(adm1, by.x = "county", by.y = "NAME_1") |>
  st_as_sf() |>
  dplyr::select(c("county", "population", "travel_minutes", "county_pop", "pct_pop",
                  "geometry"))

comprehensive_county2 <- comprehensive_county |>
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  group_by(county) |> 
  replace_na(list(county_pop = 0)) |>
  replace_na(list(pct_pop = 0)) |> 
  reframe(cum_pct = cumsum(pct_pop)) |> 
  ungroup()

comprehensive_county3 <-  as.data.frame(comprehensive_county) |> 
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  cbind(comprehensive_county2) |> 
  dplyr::select(-7) |> 
  mutate(pct_pop_range = case_when(
    cum_pct >= 0 & cum_pct < 21 ~ "0-20",
    cum_pct >= 20 & cum_pct < 41 ~ "21-40",
    cum_pct >= 40 & cum_pct < 61 ~ "41-60",
    cum_pct >= 60 & cum_pct < 81 ~ "61-80",
    cum_pct >= 80 & cum_pct < 101 ~ "81-100",
    TRUE ~ as.character(cum_pct)
  )) |> 
  mutate(pct_pop_range = as.factor(pct_pop_range))

# travel time to nearest facility plot-----
cms_df <- raster::as.data.frame(cms_raster, xy=TRUE) 
cms_df <- na.omit(cms_df) |> 
  dplyr::rename("cms_raster" = "layer") 
cms_df <- cms_df[is.finite(cms_df$cms_raster), ]

# create fixed interval
fix.brks <- classIntervals(cms_df$cms_raster, n = 5, style="fixed",
                           fixedBreaks=c(-0.1, 30, 60, 90, 120, Inf), # used '-0.1' to make sure 0 is included 
                           intervalClosure="right")

cms_df <- cms_df |> 
  mutate(pop_cat = cut(cms_raster, fix.brks$brks))

hist_factor = 2
legend_w = 24
vline_w = 0.2


cms_map <- ggplot() + 
  geom_sf(data = adm0, fill = "white", alpha = 1,show.legend = NA, color = NA) +
  geom_raster(data = cms_df, aes(x=x, y=y, fill = factor(pop_cat)),
              interpolate = FALSE, show.legend = NA) +
  scale_fill_manual(guide = "none",values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#969696") +
  # geom_sf(data = adm1, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  geom_sf(data = adm0, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  annotation_scale(location = "bl", width_hint = 0.25,pad_x = unit(0.26, "in"), pad_y = unit(0.4, "in"),
                   height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.57, "in"), pad_y = unit(0.6, "in"),
                         height = unit(0.9, "cm"), width = unit(0.81, "cm"),style = north_arrow_fancy_orienteering(text_size = 10)) +
  geom_sf(data = cms, aes(geometry = geometry, col = "kmfl"),size = 0.5) +
  scale_colour_manual(name = " ", values = "black",labels =c("health facility"),guide = guide_legend())+
  theme(legend.position = c(1.11, 0.75),legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'),legend.key.width = unit(0.4, 'cm'), 
        legend.text = element_text(size=10.0),legend.spacing.y = unit(0, "pt")) +
  theme(plot.margin = margin(t = 0,r = 20,b = 0,l = 0),panel.background = element_blank(),
        axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # plot.background = element_rect(fill='transparent', color=NA),legend.background = element_rect(fill='transparent'),
  # legend.box.background = element_rect(fill='transparent', color=NA),legend.key=element_rect(fill="transparent"), 
  # legend.margin =  margin(0,0,0,0,unit="pt")) +
  ggtitle("Accessibility to facilities with comprehensive maternity services")


## bar chart legend using ggplot
cms_bar <- pop_pct_by_cms_at_adm0 |> 
  mutate(pop_pct1 = pop_pct0 + legend_w,alpha = 1,label = paste0(sprintf("%1.1f", pop_pct), "%"))

##bar chart legend 
cms_barchart <- ggplot(cms_bar, aes(x = reorder(travel_minutes,-cms_raster), y = pop_pct1/hist_factor,
                                    fill = as.factor(cms_raster), alpha=factor(alpha))) +
  coord_flip() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#d9d9d9") +
  scale_alpha_manual(values = c("1" = 1),guide = "none") +
  geom_bar(stat = 'identity',  width = 0.8, color="#d9d9d9", linewidth = 0.05, na.rm = TRUE) + 
  # geom_hline(aes(yintercept=legend_w/hist_factor),col = "white", linewidth = 0.55) +
  geom_text(aes(label=label,y = 15),size = 2.8, position=position_dodge(width=0.9), vjust= 0.5,hjust= 0) + 
  scale_colour_manual(values=c("#000000", "#de77ae")) +
  theme_bw() +
  labs(title="Travel minutes to nearest facility given population",x = "", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size=9.5, colour="#000000", hjust = 0, vjust = 0),
        axis.text.y = element_text(size=8.5, colour="#000000"),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.ticks=element_blank(),axis.line = element_blank(),
        axis.text.x = element_blank(),legend.position = "none",
        plot.margin = margin(t = 5, r = 0,b = 0,l = 10)) +
  theme(panel.background = element_rect(fill='transparent', color=NA),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        aspect.ratio = 1/1.2) 

##combined plot 
combined_plots <- cowplot::ggdraw() +
  draw_plot(cms_map, height = 1, width = 0.8) +
  expand_limits(x = 0.5, y = 0) +
  draw_plot(cms_barchart, x = 0.59, y = 0.39, hjust = 0, vjust = 0.1,
            width = 0.32, height = 0.32) +
  theme(plot.margin = margin(t=0.1, l= -0.1, b=0.1, r=0.6, "cm"),
        plot.background = element_rect(fill='transparent', color=NA)) 


#save plot
dev.off()
ggsave(
  "output/cms_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

# cumulative proportion per county plot----
comprehensive_county3$travel_minutes <- factor(comprehensive_county3$travel_minutes, 
                                       levels = c("≤30", "31-60", "61-90", "91-120", ">120"))

comprehensive_county3$cum_pct <- ifelse(comprehensive_county3$cum_pct > 100, 100, comprehensive_county3$cum_pct)
comprehensive_county3$pct_pop_range2 <- cut(comprehensive_county3$cum_pct, 
                                    breaks = c(-Inf, 20, 40, 60, 80, 100), 
                                    labels = c("0-20", "21-40", "41-60", "61-80", "81-100"),
                                    right = TRUE, include.lowest = TRUE)

cms_county_map <- comprehensive_county3 |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = pct_pop_range2)) +
  theme_void() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                    limits = c("0-20","21-40", "41-60","61-80", "81-100"),
                    drop = FALSE) +  # Ensure all levels are shown in the legend
  labs(fill = "Population (%)") +
  ggtitle("Cumulative proportion of population with access \nto facilities with comprehensive maternity services") +
  theme(plot.title = element_text(colour = 'black', hjust = .5, face = 'bold'),
        legend.text = element_text(colour = 'black', size = 12, face = 'bold'),
        legend.title = element_text(colour = 'black', size = 12, face = 'bold')) +
  facet_wrap(~travel_minutes)


#save plot
dev.off()
ggsave(
  "output/cms_county_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

#final data table
cms_table <- pop_comprehensive_table_new |> 
  mutate(population = round(population),
         population = paste0(population," (", (paste0(sprintf("%1.1f", pct_pop))),"%)" )) |> 
  dplyr::select(-c(county_pop, pct_pop)) |> 
  pivot_wider(names_from = travel_minutes, values_from = population) |> 
  mutate(across(where(is.numeric), ~format(., big.mark   = ","))) |> 
  as.data.frame() |> 
  merge(comprehensive_county, by = "county") |> 
  dplyr::select(c("county","county_pop", "≤30", "31-60", "61-90", "91-120", ">120")) |> 
  filter(!is.na(county_pop)) |> 
  # mutate(county_pop = round(county_pop)) |> 
  distinct(county, .keep_all = T)
names(cms_table) <- c("County", "Total Population","≤30 min", "31-60 min", "61-90 min", "91-120 min", ">120 min")

#save 
write_csv(cms_table, "output/cms_county_table.csv")



#  readiness----
# facilities with basic maternity services
cmr <- st_read("comprehensive_maternity_services.shp") |> 
  filter(readiness > 75)

# travel time calculation-----
# rasterise the facilities
## Point locations
cmr_new <- as.data.frame(cmr) |>
  dplyr::select(longitude, latitude, facility, county, readiness,keph_level)

names(cmr_new) <- c("X_COORD", "Y_COORD", "facility", "county", "readiness","keph_level")

# Keep only point coordinates within the shapefile bounds
coordinates(cmr_new) <- ~ X_COORD + Y_COORD 

# convert to sp from sf
crs(cmr_new) <- crs(adm1)
adm1.sp <- as_Spatial(adm1)
overlap1 <- over(cmr_new, adm1.sp)
cmr_new <- cmr_new[!is.na(overlap1$GID_0),]
points_cmr <- as.matrix(cmr_new@coords)

# travel time raster
cmr_raster <- gdistance::accCost(T.GC, points_cmr)
plot(cmr_raster)

# save
writeRaster(cmr_raster,"cmr_access.tif", overwrite = TRUE)

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
cmr_raster_cat <- reclassify(cmr_raster,
                             reclass.mat,
                             include.lowest = TRUE) # this includes a travel time of zero in the lowest group

cmr_raster_cat <- rast(cmr_raster_cat)
cmr_raster_cat_poly_diss <- terra::as.polygons(cmr_raster_cat, dissolve=TRUE)
cmr_raster_cat_poly_diss <- st_as_sf(cmr_raster_cat_poly_diss)
cmr_raster_cat_poly_diss <- st_set_crs(cmr_raster_cat_poly_diss,crs(adm1))
colnames(cmr_raster_cat_poly_diss)[colnames(cmr_raster_cat_poly_diss) == "layer"] ="cmr_raster"

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

cmr_raster_cat_poly_diss.adm1 <- st_intersection_faster(cmr_raster_cat_poly_diss, adm1)

cmr_raster_cat_poly_diss.adm1 <- cmr_raster_cat_poly_diss.adm1 |> 
  mutate(pop =exact_extract(pop, cmr_raster_cat_poly_diss.adm1, 
                            'sum'))

#summarise motorised travel time to health facilities in Kenya at the national and county levels 
#at the adm0 level
pop_pct_by_cmr_at_adm0 <- as.data.frame(cmr_raster_cat_poly_diss.adm1[, c("cmr_raster", "pop")]) |> 
  group_by(cmr_raster) |> 
  summarise_at(c("pop"), sum,
               na.rm = TRUE) |> 
  ungroup() |> 
  mutate(travel_minutes = case_when(
    cmr_raster == 1 ~ "≤30",
    cmr_raster == 2 ~ "31-60",
    cmr_raster == 3 ~ "61-90",
    cmr_raster == 4 ~ "91-120",
    cmr_raster == 5 ~ ">120")) |> 
  mutate(pop = round(pop, digits = 0),
         pop_pct0 = round((pop / sum(pop) * 100), digits = 2),
         pop_pct = round((pop / sum(pop) * 100), digits = 1)) |> 
  mutate(transport = "mixed",
         service = "accessibility",
         country = "Kenya")

pop_pct_by_cmr_at_adm0 <- pop_pct_by_cmr_at_adm0[, c("country", "transport", "service", "travel_minutes", "cmr_raster", "pop","pop_pct0","pop_pct")]

#save
# write_csv(pop_pct_by_basic_access_at_adm0, "Accessibility_per_service/App/Data/basic maternity services/proportion_pop_basic.csv")

# at adm1 level
pop_pct_by_cmr_n_adm1 <- as.data.frame(cmr_raster_cat_poly_diss.adm1[, c("cmr_raster", "NAME_1", "pop")]) |> 
  dplyr::group_by(NAME_1) |> 
  reframe(adm1_pop = round(sum(pop), digits = 0)) |> 
  ungroup() 

pop_per_county <- pop_pct_by_cmr_n_adm1 |> 
  merge(cmr_raster_cat_poly_diss.adm1, by = "NAME_1") |> 
  dplyr::select("cmr_raster", "NAME_1", "pop", "adm1_pop") |> 
  mutate(travel_minutes = case_when(
    cmr_raster == 1 ~ "≤30",
    cmr_raster == 2 ~ "31-60", 
    cmr_raster == 3 ~ "61-90",
    cmr_raster == 4 ~ "91-120",
    cmr_raster == 5 ~ ">120")) |> 
  mutate(pop_1hr = round(pop, digits = 0),
         pop_pct_1hr = round((pop / adm1_pop * 100), digits = 1)) |> 
  group_by(NAME_1) |> 
  mutate(pop_2hr = round(sum(pop), digits = 0),
         pop_pct_2hr = round((pop_2hr / adm1_pop * 100), digits = 1),
         rid = row_number(NAME_1),
         le_1hour = paste0(pop_1hr, " (", (paste0(sprintf("%1.1f", pop_pct_1hr))),"%)"),
         le_2hour = paste0(pop_2hr, " (", (paste0(sprintf("%1.1f", pop_pct_2hr))),"%)"),
         Transport = "mixed",
         Service = "accessibility",
         Country = "Kenya") |> 
  ungroup() 

## generate a table showing column, ≤30 min,31-60 min, 61-90 min,91-120 min, >120 min 
pop_comprehensive_table <- pop_per_county |> 
  dplyr::select(NAME_1, pop, adm1_pop, travel_minutes, cmr_raster) |> 
  dplyr::rename(county = NAME_1,
                population = pop,
                county_pop = adm1_pop)

# ensure each county has travel minutes
all_combi <- expand.grid(
  county = unique(pop_comprehensive_table$county),
  cmr_raster = unique(pop_comprehensive_table$cmr_raster)
)

pop_comprehensive_table_new <- all_combi |> 
  left_join(pop_comprehensive_table, by = c("county", "cmr_raster")) |> 
  mutate(travel_minutes = case_when(
    cmr_raster == 1 ~ "≤30",
    cmr_raster == 2 ~ "31-60",
    cmr_raster == 3 ~ "61-90",
    cmr_raster == 4 ~ "91-120",
    cmr_raster == 5 ~ ">120",
    TRUE ~ travel_minutes
  )) |> 
  replace_na(list(population = 0)) |> 
  dplyr::select(-"cmr_raster") 

pop_comprehensive_table_new <- pop_comprehensive_table_new |> 
  group_by(county) |> 
  mutate(pct_pop = round((population / county_pop) * 100, 2)) 

comprehensive_county <- pop_comprehensive_table_new |>
  merge(adm1, by.x = "county", by.y = "NAME_1") |>
  st_as_sf() |>
  dplyr::select(c("county", "population", "travel_minutes", "county_pop", "pct_pop",
                  "geometry"))

comprehensive_county2 <- comprehensive_county |>
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  group_by(county) |> 
  replace_na(list(county_pop = 0)) |>
  replace_na(list(pct_pop = 0)) |> 
  reframe(cum_pct = cumsum(pct_pop)) |> 
  ungroup()

comprehensive_county3 <-  as.data.frame(comprehensive_county) |> 
  arrange(county, 
          case_when(
            travel_minutes == "≤30" ~ 1,
            travel_minutes == "31-60" ~ 2,
            travel_minutes == "61-90" ~ 3,
            travel_minutes == "91-120" ~ 4,
            travel_minutes == ">120" ~ 5
          )) |> 
  cbind(comprehensive_county2) |> 
  dplyr::select(-7) |> 
  mutate(pct_pop_range = case_when(
    cum_pct >= 0 & cum_pct < 21 ~ "0-20",
    cum_pct >= 20 & cum_pct < 41 ~ "21-40",
    cum_pct >= 40 & cum_pct < 61 ~ "41-60",
    cum_pct >= 60 & cum_pct < 81 ~ "61-80",
    cum_pct >= 80 & cum_pct < 101 ~ "81-100",
    TRUE ~ as.character(cum_pct)
  )) |> 
  mutate(pct_pop_range = as.factor(pct_pop_range))

# travel time to nearest facility plot-----
cmr_df <- raster::as.data.frame(cmr_raster, xy=TRUE) 
cmr_df <- na.omit(cmr_df) |> 
  dplyr::rename("cmr_raster" = "layer") 
cmr_df <- cmr_df[is.finite(cmr_df$cmr_raster), ]

# create fixed interval
fix.brks <- classIntervals(cmr_df$cmr_raster, n = 5, style="fixed",
                           fixedBreaks=c(-0.1, 30, 60, 90, 120, Inf), # used '-0.1' to make sure 0 is included 
                           intervalClosure="right")

cmr_df <- cmr_df |> 
  mutate(pop_cat = cut(cmr_raster, fix.brks$brks))

hist_factor = 2
legend_w = 24
vline_w = 0.2


cmr_map <- ggplot() + 
  geom_sf(data = adm0, fill = "white", alpha = 1,show.legend = NA, color = NA) +
  geom_raster(data = cmr_df, aes(x=x, y=y, fill = factor(pop_cat)),
              interpolate = FALSE, show.legend = NA) +
  scale_fill_manual(guide = "none",values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#969696") +
  # geom_sf(data = adm1, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  geom_sf(data = adm0, fill = NA, lwd = 0.1, colour="#000000", show.legend = NA) + 
  annotation_scale(location = "bl", width_hint = 0.25,pad_x = unit(0.26, "in"), pad_y = unit(0.4, "in"),
                   height = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.57, "in"), pad_y = unit(0.6, "in"),
                         height = unit(0.9, "cm"), width = unit(0.81, "cm"),style = north_arrow_fancy_orienteering(text_size = 10)) +
  geom_sf(data = cmr, aes(geometry = geometry, col = "kmfl"),size = 0.5) +
  scale_colour_manual(name = " ", values = "black",labels =c("health facility"),guide = guide_legend())+
  theme(legend.position = c(1.11, 0.75),legend.key.size = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'),legend.key.width = unit(0.4, 'cm'), 
        legend.text = element_text(size=10.0),legend.spacing.y = unit(0, "pt")) +
  theme(plot.margin = margin(t = 0,r = 20,b = 0,l = 0),panel.background = element_blank(),
        axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # plot.background = element_rect(fill='transparent', color=NA),legend.background = element_rect(fill='transparent'),
  # legend.box.background = element_rect(fill='transparent', color=NA),legend.key=element_rect(fill="transparent"), 
  # legend.margin =  margin(0,0,0,0,unit="pt")) +
  ggtitle("Accessibility to facilities with 75% \nreadiness for comprehensive maternity services")


## bar chart legend using ggplot
cmr_bar <- pop_pct_by_cmr_at_adm0 |> 
  mutate(pop_pct1 = pop_pct0 + legend_w,alpha = 1,label = paste0(sprintf("%1.1f", pop_pct), "%"))

##bar chart legend 
cmr_barchart <- ggplot(cmr_bar, aes(x = reorder(travel_minutes,-cmr_raster), y = pop_pct1/hist_factor,
                                    fill = as.factor(cmr_raster), alpha=factor(alpha))) +
  coord_flip() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),na.value = "#d9d9d9") +
  scale_alpha_manual(values = c("1" = 1),guide = "none") +
  geom_bar(stat = 'identity',  width = 0.8, color="#d9d9d9", linewidth = 0.05, na.rm = TRUE) + 
  # geom_hline(aes(yintercept=legend_w/hist_factor),col = "white", linewidth = 0.55) +
  geom_text(aes(label=label,y = 15),size = 2.8, position=position_dodge(width=0.9), vjust= 0.5,hjust= 0) + 
  scale_colour_manual(values=c("#000000", "#de77ae")) +
  theme_bw() +
  labs(title="Travel minutes to nearest facility given population",x = "", y = "") +
  theme_classic() +
  theme(plot.title = element_text(size=9.5, colour="#000000", hjust = 0, vjust = 0),
        axis.text.y = element_text(size=8.5, colour="#000000"),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.ticks=element_blank(),axis.line = element_blank(),
        axis.text.x = element_blank(),legend.position = "none",
        plot.margin = margin(t = 5, r = 0,b = 0,l = 10)) +
  theme(panel.background = element_rect(fill='transparent', color=NA),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        aspect.ratio = 1/1.2) 

##combined plot 
combined_plots <- cowplot::ggdraw() +
  draw_plot(cmr_map, height = 1, width = 0.8) +
  expand_limits(x = 0.5, y = 0) +
  draw_plot(cmr_barchart, x = 0.59, y = 0.39, hjust = 0, vjust = 0.1,
            width = 0.32, height = 0.32) +
  theme(plot.margin = margin(t=0.1, l= -0.1, b=0.1, r=0.6, "cm"),
        plot.background = element_rect(fill='transparent', color=NA)) 


#save plot
dev.off()
ggsave(
  "output/cmr_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

# cumulative proportion per county plot----
comprehensive_county3$travel_minutes <- factor(comprehensive_county3$travel_minutes, 
                                               levels = c("≤30", "31-60", "61-90", "91-120", ">120"))

comprehensive_county3$cum_pct <- ifelse(comprehensive_county3$cum_pct > 100, 100, comprehensive_county3$cum_pct)
comprehensive_county3$pct_pop_range2 <- cut(comprehensive_county3$cum_pct, 
                                            breaks = c(-Inf, 20, 40, 60, 80, 100), 
                                            labels = c("0-20", "21-40", "41-60", "61-80", "81-100"),
                                            right = TRUE, include.lowest = TRUE)

cmr_county_map <- comprehensive_county3 |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = pct_pop_range2)) +
  theme_void() +
  scale_fill_manual(values = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                    limits = c("0-20","21-40", "41-60","61-80", "81-100"),
                    drop = FALSE) +  # Ensure all levels are shown in the legend
  labs(fill = "Population (%)") +
  ggtitle("Cumulative proportion of population with \naccess to facilities with 75% readiness \ncomprehensive maternity services") +
  theme(plot.title = element_text(colour = 'black', hjust = .5, face = 'bold'),
        legend.text = element_text(colour = 'black', size = 12, face = 'bold'),
        legend.title = element_text(colour = 'black', size = 12, face = 'bold')) +
  facet_wrap(~travel_minutes)


#save plot
dev.off()
ggsave(
  "output/cmr_county_map.png",
  width = 10,
  height = 6,
  dpi = 1e3,
  bg = NULL
)

#final data table
cmr_table <- pop_comprehensive_table_new |> 
  mutate(population = round(population),
         population = paste0(population," (", (paste0(sprintf("%1.1f", pct_pop))),"%)" )) |> 
  dplyr::select(-c(county_pop, pct_pop)) |> 
  pivot_wider(names_from = travel_minutes, values_from = population) |> 
  mutate(across(where(is.numeric), ~format(., big.mark   = ","))) |> 
  as.data.frame() |> 
  merge(comprehensive_county, by = "county") |> 
  dplyr::select(c("county","county_pop", "≤30", "31-60", "61-90", "91-120", ">120")) |> 
  filter(!is.na(county_pop)) |> 
  # mutate(county_pop = round(county_pop)) |> 
  distinct(county, .keep_all = T)
names(cmr_table) <- c("County", "Total Population","≤30 min", "31-60 min", "61-90 min", "91-120 min", ">120 min")

#save 
write_csv(cmr_table, "output/cmr_county_table.csv")


