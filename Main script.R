libs <- c('viridis', 'sf','stars','tidyverse',
          'ggthemes','raster','terra','ggplot2','osmdata','httr','showtext')


installed_libs <- libs %in% rownames (installed.packages())
  
  if(any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
  }

invisible(
  lapply(libs,library,character.only = T)
)


 #load raster and shapefiles
ras_pop <- terra::rast("raster\\mubi.tif") #select path as it relates to your device
mubi_wards <-sf:: st_read ("nigeria_health_facilities\\Wards_mubi.shp")
health_f <- sf::st_read ("nigeria_health_facilities\\Nigeria_-_Health_Care_Facilities_.shp") 
adamawa_dem <-terra::rast("raster\\n10_e013_1arc_v3.tif")
mubi_lc <- terra::rast("raster\\landcover_mubi.tif")
mubi_rds <- sf::st_read('nigeria_health_facilities\\roads.shp')

mubi_wards <- mubi_wards[mubi_wards$wrd_nm_x !='Uba',] #removing an unwanted row

#set uniform coordinate reference system for all datasets
utm33n_proj4 <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs" 


mubi_wards <- sf::st_transform(mubi_wards,crs = 32633)
health_f <- sf::st_transform(health_f,crs = 32633)
adamawa_dem <- terra::project(adamawa_dem,utm33n_proj4)
ras_pop <- terra::project(ras_pop,utm33n_proj4)
mubi_lc <- terra::project(mubi_lc,utm33n_proj4)
mubi_rds <- sf::st_transform(mubi_rds,crs=32633)

# Extract zonal statistics to get population sum for each ward and add as a  new column to mubi_wards
zonal_stats <- terra::zonal( 
  ras_pop, 
  terra::vect(mubi_wards), # your sf object needs to be converted into a terra Spatvector
  sum,
  na.rm = T # you should also ignore NAs
)

mubi_wards$pop <- round(zonal_stats$mubi) #Add zonal_stats column to mubi_wards

#calculate area and population density of mubi_wards
mubi_wards$area_sqkm <- st_area(mubi_wards)/1e6
mubi_wards$pop_dens <- round(mubi_wards$pop/mubi_wards$area_sqkm)

mubi_wards$pop_dens <- as.numeric(mubi_wards$pop_dens) #convert $pop_dens to a numeric variable


#get a subset of health facilites in mubi wards 

mubi_health <- st_intersection(health_f,mubi_wards)


#Rasterize Population density data
extent <- extent(mubi_wards)
resolution <- c(30, 30)  
attribute <- "pop_dens"

template_raster <- rast(mubi_wards, res = resolution) # Create a raster template

template_raster <- project(template_raster,utm33n_proj4) #transform template raster to a uniform CRS

mubi_wards_rasterized <- rasterize(mubi_wards,template_raster, field = attribute) #rasterize
plot(mubi_wards_rasterized)

#Reclassify mubi_wards_rasterized
pop_breaks <- c(0,10000,2,10000,20000,4,20000,30000,6,30000,40000,8,40000,Inf,10)
pop_matrix <- matrix(pop_breaks,ncol = 3, byrow = TRUE)

mubi_pop_reclassified <- terra::classify(mubi_wards_rasterized,pop_matrix) 

plot(mubi_pop_reclassified)

#Calculate the distance from exixting health facilities
mubi_ecd_health <- terra::distance(template_raster,vect(mubi_health))

#Reclassify the Euclidean distances between health facilities to unitless values
reclasv <- c(0,2500,0,2500,3500,4,3500,5500,10,5500,7500,8,7500,9500,8,9500,11500,4,11500,Inf,0) #create a classification matrix
mat <- matrix(reclasv,ncol = 3, byrow = TRUE) 

mubi_ecd_relass <- terra::classify(mubi_ecd_health,mat) # reclassify values using the matrix object

plot(mubi_ecd_relass)


#Calculate euclidean distances for roads
mubi <- st_union(mubi_wards) #merge all mubi wards to have a single geometry

mubi<- st_transform(mubi, crs=4326) # transform to WGS84  for compatibilty with OSM


road_tags <- c(
  "motorway", "trunk", "primary", "secondary",
  "tertiary", "motorway_link", "trunk_link", 
  "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
  bbox <- sf::st_bbox(mubi)
  roads <- bbox |>
    opq() |>
    add_osm_feature(
      key = "highway",
      value = road_tags
    ) |>
    osmdata::osmdata_sf()
  
  return(roads)
} #create a function to extract roads from Mubi

roads <- get_osm_roads() #Assign to a new object

mubi_roads <- roads$osm_lines |>
  sf::st_set_crs(4326) |>
  sf::st_transform(crs = st_crs(4326)) #Assign CRS

mubi_roads <- st_transform(mubi_roads,crs=32633) #transform CRS to uniform CRS for the project

rds_ecd <- terra::distance(template_raster,vect(mubi_roads)) #Calculate distance from Lines

#Reclassify rds_ecd raster
rds_breaks <- c(0,2000, 10,2000,4000,8,4000,8000,6,8000,10000,4,10000,12000,2,12000,Inf,0) 
rd_break_matrix <- matrix(rds_breaks,ncol = 3,byrow = TRUE)

rds_ecd_reclassified <- terra::classify(rds_ecd,rd_break_matrix) 


#crop adamawa_dem with mubi_wards sf
mubi_dem <- terra::crop(adamawa_dem,mubi_wards)

#calculate slope for mubi_dem
mubi_slope <- terra::terrain(mubi_dem,"slope", unit = "degrees")


#reclassify Slope
mubi_slop_breaks <- c(0,2,10,2,4,8,4,6,6,6,8,4,8,10,2,10,Inf,0)
slop_brks_matrix <- matrix(mubi_slop_breaks,ncol=3,byrow = TRUE)

Mubi_slope_reclassified <- classify(mubi_slope,slop_brks_matrix)
plot(Mubi_slope_reclassified)


#reclassify landcover
breaks_lc <- c(10,11,8,11,12,4,12,13,6,13,14,10,14,Inf,8)
matrix_lc <- matrix(breaks_lc,ncol = 3, byrow = TRUE)

lc_reclassified <- classify(mubi_lc, matrix_lc)
plot(lc_reclassified)

#Check extent for each layer 
ext(mubi_pop_reclassified)
ext(lc_reclassified)
ext(rds_ecd_reclassified)
ext(Mubi_slope_reclassified)
ext(mubi_ecd_relass)


#Resample unaligned raster layer for uniform extents and resolution
lc_reclassified <- resample(lc_reclassified,rds_ecd_reclassified)


#Carry out raster calculation with figures representing weights (priority)
suitable_location <- mubi_pop_reclassified * 0.27 + lc_reclassified * 0.17 + rds_ecd_reclassified *0.19 +
  Mubi_slope_reclassified * 0.17 +  mubi_ecd_relass * 0.22

suitable_location_breaks <- c(0,4,0,4,6,1,6,Inf,2)

sl_matrix <-  matrix(suitable_location_breaks,ncol=3, byrow=TRUE)

suitable_location_reclassified <- classify(suitable_location,sl_matrix)
plot(suitable_location_reclassified)

Suitable_location_df <- terra::as.data.frame(suitable_location,xy=T) #convert to data frame for use in ggplot

names(Suitable_location_df)[3] <- "value" #rename column

# define categorical values
Suitable_location_df$category <- round(Suitable_location_df$value, 0) *10


#Crop road to be used for map, The roads need to fall within the mubi boundary
mubi_roads_cropped <- st_intersection(mubi_roads,mubi_wards)

font_add(family = 'century gothic', regular = 'gothic.ttf') #Add font to be used on Map
showtext_auto()


#Map

m<- ggplot() +
  geom_raster(data=Suitable_location_df, aes(x=x,y=y, fill = category), alpha=1) +
  
  geom_sf(
    data = mubi_roads_cropped,
    color = "grey20",
    size = .1,
    alpha = 1,
    fill = "transparent"
  ) +
  
  scale_fill_gradientn(
    name = "Scale of Suitabilty",
    colours = rev(viridis(6)),
    limits = c(0, 100) ) +
  
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keywidth = unit(10, "mm"),
      keyheight = unit(2.5, "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T)
       ) +
  
  theme_minimal() +
  
  theme(text = element_text(family = 'century gothic'),
        legend.direction = "horizontal",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.height = unit(0.1, 'mm'),
        legend.position = 'top',
        legend.text = element_text(size = 35, color = "#009e73"),
        legend.title = element_text(size = 35, color = "#009e73"),
        plot.title = element_text(
          size = 60, color = "#009e73", hjust = .5, vjust = -3
        ),
        plot.caption = element_text(
          size = 30, color = "#009e73", hjust = .5, vjust = 5
        ),
        legend.spacing.y = unit(0.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#ecfefa", color = NA),
        panel.background = element_rect(fill = "#ecfefa", color = NA),
        legend.background = element_rect(fill = "#ecfefa", color = NA)
  ) +
  labs(
    x = "",
    y = NULL,
    title = "Where in Mubi Do We Build Our New Clinic?",
    subtitle = "",
    caption = "©2023 Victor James (https://spatialnode.net/jamesa47)|Data & Contributions: GRID3, USGS & ©OpenStreetMap contributors, @milos_agathon"
  )

m 


ggsave('Suitability Analysis_2.png',
        width = 6,
        height = 6,
        dpi = 600,
        device = 'png')
   
