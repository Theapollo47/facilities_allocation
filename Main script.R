libs <- c('viridis', 'sf','stars','tidyverse',
          'ggthemes','rvest','ggrepel',
          'plotly','raster','terra','ggplot2')

invisible(
  lapply(libs,library,character.only = T)
)



#load raster and shapefiles
ras_pop <- terra::rast("raster\\mubi.tif")
mubi_wards <-sf:: st_read ("nigeria_health_facilities\\Wards_mubi.shp")
health_f <- sf::st_read ("nigeria_health_facilities\\Nigeria_-_Health_Care_Facilities_.shp") 
adamawa_dem <-terra::rast("raster\\n10_e013_1arc_v3.tif")
mubi_lc <- terra::rast("raster\\landcover_mubi.tif")
mubi_rds <- sf::st_read('nigeria_health_facilities\\roads.shp')
mubi_ecd <- terra::rast('raster\\mubi_ecd.tif')

mubi_wards <- mubi_wards[mubi_wards$wrd_nm_x !='Uba',] #removing an unwanted row

#set uniform coordinate reference system for all datasets
mubi_wards <- sf::st_transform(mubi_wards,crs = st_crs(mubi_ecd))
health_f <- sf::st_transform(health_f,crs = st_crs(mubi_ecd))
adamawa_dem <- terra::project(adamawa_dem,mubi_ecd)
ras_pop <- terra::project(ras_pop,mubi_ecd)
mubi_lc <- terra::project(mubi_lc,mubi_ecd)
mubi_rds <- sf::st_transform(mubi_rds,crs=st_crs(mubi_ecd))

# Extract zonal statistics to get population sum for each ward and add as a  new column to mubi_wards
zonal_stats <- terra::zonal( 
  ras_pop, 
  terra::vect(mubi_wards), # your sf object needs to be converted into a terra Spatvector
  sum,
  na.rm = T # you should also ignore NAs
)

mubi_wards$pop <- round(zonal_stats$mubi) #substitue columns

#calculate area and population density of mubi_wards
mubi_wards$area_sqkm <- st_area(mubi_wards)/1e6
mubi_wards$pop_dens <- round(mubi_wards$pop/mubi_wards$area_sqkm)

mubi_wards$pop_dens <- as.numeric(mubi_wards$pop_dens) #convert $pop_dens to a numeric variable


#get a subset of health facilites in mubi wards 
mubi_health <- sf::st_join( 
  health_f,
  mubi_wards,
  sf::st_within #function did not creat a subset for just mubi_wards and so I used the next line of code
)

mubi_health <- st_intersection(health_f,mubi_wards)


#Rasterize Population data
extent <- extent(mubi_wards)
resolution <- c(30, 30)  
attribute <- "pop"

template_raster <- rast(mubi_wards, res = resolution) # Create a raster template

template_raster <- project(template_raster,mubi_ecd) #transform template raster to a uniform CRS

mubi_wards_rasterized <- rasterize(mubi_wards,template_raster, field = attribute) #rasterize
plot(mubi_wards_rasterized)

#Reclassify mubi_wards_rasterized
pop_breaks <- c(0,10000,2,10000,20000,4,20000,30000,6,30000,40000,8,40000,Inf,10)
pop_matrix <- matrix(pop_breaks,ncol = 3, byrow = TRUE)

mubi_pop_reclassified <- terra::classify(mubi_wards_rasterized,pop_matrix) 

plot(mubi_pop_reclassified) #for unknown reasons this is'nt classifying as specified , I mean it was perfect just last night!

#Calculate the distance from exixting health facilities
mubi_ecd_health <- terra::distance(template_raster,vect(mubi_health))

#Reclassify the Euclidean distances between health facilities to unitless values
reclasv <- c(0,2500,0,2500,3500,4,3500,5500,10,5500,7500,8,7500,9500,8,9500,11500,4,11500,Inf,0) #create a classification matrix
mat <- matrix(reclasv,ncol = 3, byrow = TRUE) 

mubi_ecd_relass <- terra::classify(mubi_ecd_health,mat) # reclassify values using the matrix object

plot(mubi_ecd_relass)


#Reclassify euclidean distances for roads
rds_ecd <- terra::distance(template_raster,vect(mubi_rds))
crs(template_raster)
rds_breaks <- c(0,5000, 10,5000,10000,8,10000,15000,6,15000,20000,4,20000,25000,2,25000,Inf,0) 
rd_break_matrix <- matrix(rds_breaks,ncol = 3,byrow = TRUE)
rds_ecd_reclassified <- classify(rds_ecd,rd_break_matrix) 


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
#Resample unaligned raster for  uniform extents and resolution
rds_ecd_reclassified <- resample(rds_ecd_reclassified,Mubi_slope_reclassified)
mubi_pop_reclassified <- resample(mubi_pop_reclassified,Mubi_slope_reclassified)

#Carry out raster calculation with figures representing weights (priority)
suitable_location <- mubi_pop_reclassified * 0.27 + lc_reclassified * 0.17 + rds_ecd_reclassified *0.19 +
  Mubi_slope_reclassified * 0.17 +  mubi_ecd_relass * 0.22

Suitable_location_df <- terra::as.data.frame(suitable_location,xy=T)

names(Suitable_location_df)[3] <- "value"

m<- ggplot() +
  geom_raster(data=Suitable_location_df, aes(x=x,y=y, fill = value))
