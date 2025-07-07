library(sf)
library(readr)
library(summarytools)
library(dplyr)
library(raster)
library(terra)
library(fasterize)

### Internal Points

internal_points <- data.frame(
  id = zip_ny$ZCTA5CE20,
  lon = zip_ny$INTPTLON20,
  lat = zip_ny$INTPTLAT20)

points_sf <- st_as_sf(internal_points, coords = c("lon", "lat"), crs = crs(nlcd)) %>%
  st_transform(crs(nlcd)) 

points_sf$land_cover <- extract(nlcd, points_sf)


###
internal_points <- zip_ny %>%
  st_drop_geometry() 

internal_points <- st_as_sf(internal_points, coords = c("INTPTLON20", "INTPTLAT20")) %>%
  st_transform(crs(nlcd))
internal_points$land_cover <- extract(nlcd, internal_points)

plot(internal_points)

freq(internal_points$land_cover)



pts <- vect(ny_df, geom = c("INTPTLON20", "INTPTLAT20"), crs = crs(zip_ny))


crs(zip_ny_nlcd_crs)

plot(st_geometry(zip_ny))



zip_10024 <- filter(zip_2024_nlcd_crs, ZCTA5CE20 == "10024")
nlcd_10024 <- crop(nlcd, extent(zip_10024))

plot(nlcd_10024)
plot(st_geometry(zip_10024), add = TRUE)

# freq(nlcd) <- Count frequency of cells. Takes a long time to render
plot(nlcd)
plot(st_geometry(zip_10024))


### Internal Points - End

allstate_ny <- read_csv("./references_private/02_Allstate_Factors.csv",
                        col_names = c("zip",
                                      "fire1", "fire2", "brush_fire", "water_weather",
                                      "water_nonweather", "wind", "hail", "lightning",
                                      "theft", "liability", "hurricane", "other"),
                        skip = 1)

zip_2024  <- read_sf("data_in/geo/tl_2024_us_zcta520/tl_2024_us_zcta520.shp")

zip_allstate_ny <- zip_2024 %>%
  inner_join(allstate_ny, by = c("ZCTA5CE20" = "zip"))


sf_ny <- read_csv("./references_private/StateFarmNY_20200715.csv", col_names = c("GRID_ID", "HURR_FCTR", "NONHURR_FCTR"))  %>%
  mutate(
    y =   0.01 * as.numeric(substr(GRID_ID, 1, 5)),
    x = - 0.01 * as.numeric(substr(GRID_ID, 6, 10)))

nonhurr_ny <- rasterFromXYZ(sf_ny[, c("x", "y", "NONHURR_FCTR")])
plot(nonhurr_ny)

sf_man <- read_csv("./references_private/StateFarmNY_20200715.csv", col_names = c("GRID_ID", "HURR_FCTR", "NONHURR_FCTR"))  %>%
  mutate(
    y =   0.01 * as.numeric(substr(GRID_ID, 1, 5)),
    x = - 0.01 * as.numeric(substr(GRID_ID, 6, 10))) %>%
  filter(x >= -74.05 & x <= -73.90 & y >= 40.65 & y <= 40.90)


r <- rasterFromXYZ(sf_man[, c("x", "y", "HURR_FCTR")])
r <- rasterFromXYZ(sf_man[, c("x", "y", "NONHURR_FCTR")])

plot(r)

coordinates(sf_man) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(sf_man) <- TRUE
# coerce to raster
rasterDF <- raster(sf_man)
values(rasterDF) <- sf_man@data[["HURR_FCTR"]]
plot(rasterDF)

values(rasterDF)
hasValues(rasterDF)
rasterDF <- setValues(rasterDF, y_nyc$NONHURR_FCTR)
plot(rasterDF)


ncell(rasterDF)

-74.05, xmax = -73.90, ymin = 40.65, ymax = 40.90)

rasterFromXYZ(y_nyc)




muaggatt_names <- c(
  "musym",
  "muname",
  "mustatus",
  "slopegraddcp",
  "slopegradwta",
  "brockdepmin",
  "wtdepannmin",
  "wtdepaprjunmin",
  "flodfreqdcd",
  "flodfreqmax",
  "pondfreqprs",
  "aws025wta",
  "aws050wta",
  "aws0100wta",
  "aws0150wta",
  "drclassdcd",
  "drclasswettest",
  "hydgrpdcd",
  "iccdcd",
  "iccdcdpct",
  "niccdcd",
  "niccdcdpct",
  "engdwobdcd",
  "engdwbdcd",
  "engdwbll",
  "engdwbml",
  "engstafdcd",
  "engstafll",
  "engstafml",
  "engsldcd",
  "engsldcp",
  "englrsdcd",
  "engcmssdcd",
  "engcmssmp",
  "urbrecptdcd",
  "urbrecptwta",
  "forpehrtdcp",
  "hydclprs",
  "awmmfpwwta",
  "mukey")

muaggatt_types <- c("cclnnnnnccnnnnnccclnnncccccccccccccncnnc")

ssurgo_manhattan <- read_sf("~/Source/geospatial_rating/data_in/ssurgo/NY061/spatial/soilmu_a_ny061.shp")
x <- muaggatt <- read_delim(
  "~/Source/geospatial_rating/data_in/ssurgo/NY061/tabular/muaggatt.txt",
  delim = "|",
  col_names = muaggatt_names,
  col_types = muaggatt_types)

ssurgo_manhattan %>%
  left_join(x, by = c("MUKEY" = "mukey"))
# plot(st_geometry(ssurgo_manhattan))
leaflet(ssurgo_manhattan) %>% addPolygons(weight = 0.5)

view(dfSummary(x))

ssurgo <- read_sf("data_in/ssurgo/NY061/spatial/soilmu_a_ny061.shp")

zip_2024  <- read_sf("~/Source/geospatial_rating/data_in/geo/tl_2024_us_zcta520/tl_2024_us_zcta520.shp")

nlcd <- rast("~/Source/geospatial_rating/data_in/nlcd/Annual_NLCD_LndCov_2023_CU_C1V0.tif") 

# Transform to raster CRS
ssurgo_manhattan_nlcd <- st_transform(ssurgo_manhattan, crs = crs(nlcd))

nlcd_man <- crop(nlcd, y = extent(ssurgo_manhattan_nlcd))
plot(nlcd_man)

nlcd_man <- mask(nlcd_man, mask = ssurgo_manhattan_nlcd)

plot(nlcd_man)

plot(nlcd)


ct_2024  <- read_sf("data_in/geo/tl_2024_36_tract/tl_2024_36_tract.shp")

new_york_count_2024 <- ct_2024 %>% filter(STATEFP == "36", COUNTYFP == "061")

plot(new_york_count_2024)

new_york_county_tabblock  <- read_sf("data_in/geo/tl_2024_36_tabblock20/tl_2024_36_tabblock20.shp")

pols_r <- as(new_york_county_tabblock, "Spatial")

new_york_county_tracts  <- read_sf("data_in/geo/tl_2024_36_tract/tl_2024_36_tract.shp") %>%
  filter(STATEFP == "36", COUNTYFP == "061") %>%
  mutate(GEOID_NUMERIC = as.numeric(GEOID))

new_york_county_tracts  <- read_sf("data_in/geo/tl_2024_36_tract/tl_2024_36_tract.shp") %>%
  filter(STATEFP == "36", COUNTYFP == "061") %>%
  mutate(GEOID_NUMERIC = as.numeric(GEOID))

tracts_raster <- raster(new_york_county_tracts, res = 1/6000)
tracts_rasterize <- rasterize(new_york_county_tracts, tracts_raster, field = "GEOID_NUMERIC", fun="first")
#tracts_fasterize <- fasterize(new_york_county_tracts, tracts_raster, field = "GEOID_NUMERIC", fun="first")

count(tracts_rasterize)

plot(tracts_rasterize)


#

bbox(tracts_raster)

mgrs  <- read_sf("data_in/geo/MGRS_1km_18T_unprojected/MGRS_1km_18T_unprojected.shp") %>%
  st_crop(xmin = -74.05, xmax = -73.90, ymin = 40.65, ymax = 40.90)

leaflet(mgrs) %>%
  addPolygons(weight = 0.5, opacity = 1.0) %>%
  addTiles()



# Filter to New York county
tract_ny <- zip_2024 %>%
  filter(ZCTA5CE20 %in% c(as.character(seq(from = 10000, to = 10299)),
                          "10115", "10128", "10153", "10154",
                          "10162"))




library(RCurl)

url <- "ftp://yourServer"
userpwd <- "yourUser:yourPass"

dat <- try(getURL(paste(url, filenames[ind], sep=""), userpwd = userpwd))


if(!file.exists(destfile)){
  res <- tryCatch(download.file(fileURL,
                                destfile="./data/samsungData.rda",
                                method="auto"),
                  error=function(e) 1)
  if(dat!=1) load("./data/samsungData.rda") 
}




Code Folding Lua
https://stackoverflow.com/questions/76032786/button-to-hide-fold-code-in-revealjs-quarto
Creating Filter
https://quarto.org/docs/extensions/filters.html
leaflet(ssurgo) %>%
  addPolygons(weight = 0.5)


library(sf)
library(dplyr)

getwd()

a <- st_read("data_in/geo/tl_2009_04_zcta5/tl_2009_04_zcta5.shp")
b <- st_read("data_in/geo/tl_2024_us_zcta520/tl_2024_us_zcta520.shp")

c <- st_intersection(a, b)

d <- 
  filter(a, ZCTA5CE == "85940")

a %>% filter.sf(ZCTA5CE == "85940")

slice <- a[a$ZCTA5CE %in% c("85940","85941","85924"),]
slice2 <- b[b$ZCTA5CE20 %in% c("85940","85941","85924"),]
c <- st_intersection(slice, slice2, dimension = "polygon")



plot(st_geometry(slice))
plot(st_geometry(slice2))
plot(st_geometry(c))

st_area(c)

plot(c)
str(a)

a$ZCTA5CE
plot(a)


st_rasterize

overlay

library(raster)
library(dplyr)
library(leaflet)
library(FedData)


prism_ppt <- raster("data_in/PRISM/prism_ppt_us_30s_2020_avg_30y/prism_ppt_us_30s_2020_avg_30y.tif")

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(prism_ppt),
                    na.color = "transparent")
cols <- 
leaflet() %>% addTiles() %>%
  addRasterImage(prism_ppt, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(prism_ppt),
            title = "Surface temp")

plot(prism_ppt, 
     breaks = c(0, 4,8,12,16,2, 6250), 
     col =  c("#800000", "#2600FF"),
     main="Precipitation", 
     axes = FALSE)

hist(prism_ppt)
cut
# cut(map.df$data1, 
#                    breaks = c(0, 1, 10, 25, 100, 250, 1000, 10000), 
#                    labels = c("0", "1 - 10", "11 - 25", "26 - 100", 
#                             "101 - 250", "251 - 1000", "1001 - 10000"),
#                    include.lowest = TRUE)


plot(prism_ppt)




nlcd <- raster("./data_in/nlcd/Annual_NLCD_LndCov_2023_CU_C1V0.tif")

# There's probably a more direct way of doing this
mgrs  <- read_sf("data_in/geo/MGRS_1km_18T_unprojected/MGRS_1km_18T_unprojected.shp") %>%
  st_crop(xmin = -74.05, xmax = -73.90, ymin = 40.65, ymax = 40.90)

#Transform extent
extent_man <- st_transform(mgrs, crs(nlcd)) %>% extent()
nlcd_man <- crop(nlcd, man_ext)


plot(nlcd)


mgrs_man  <- read_sf("data_in/geo/MGRS_1km_18T_unprojected/MGRS_1km_18T_unprojected.shp") %>%
  st_crop(xmin = -74.05, xmax = -73.90, ymin = 40.65, ymax = 40.90)

#Transform extent
extent_man <- st_transform(mgrs_man, crs(nlcd)) %>% extent()
nlcd_man <- crop(nlcd, man_ext)


plot(nlcd_man)



     
crop(nlcd, st_crop(

LCnames <-c(
  "Water",
  "DevelopedOpen",
  "DevelopedLow",
  "DevelopedMed",
  "DevelopedHigh",
  "Barren",
  "DeciduousForest",
  "EvergreenForest",
  "MixedForest",
  "ShrubScrub",
  "GrassHerbaceous",
  "PastureHay",
  "CultCrops",
  "WoodyWetlands",
  "EmergentHerbWet")


# Raster comparison of 2010 and 2020 blocks

# Read shapefiles
ny_tabblock10 <- read_sf("data_in/geo/tl_2010_36_tabblock10/tl_2010_36_tabblock10.shp") %>%
  filter(STATEFP10 == "36", COUNTYFP10 == "061") %>%
  mutate(GEOID_NUMERIC = as.numeric(GEOID10)) 

ny_tabblock20 <- read_sf("data_in/geo/tl_2024_36_tabblock20/tl_2024_36_tabblock20.shp") %>%
  filter(STATEFP20 == "36", COUNTYFP20 == "061") %>%
  mutate(GEOID_NUMERIC = as.numeric(GEOID20)) 

tabblock_raster <- raster(ny_tabblock20, res = 0.00005) # Roughly 8-9M resolution

tabblock_raster10 <- rasterize(ny_tabblock10, tabblock_raster, field = "GEOID_NUMERIC", fun="first")
tabblock_raster20 <- rasterize(ny_tabblock20, tabblock_raster, field = "GEOID_NUMERIC", fun="first")

tabblock_compare <- tabblock_raster10 == tabblock_raster20

freq(tabblock_compare, useNA = 'no') %>%
  as.data.frame() %>%
  mutate(Percentage = count/sum(count))

plot(tabblock_compare)

pal <- colorNumeric(c("#67a9cf", "#ef8a62"), values(tabblock_compare),
                    na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addRasterImage(tabblock_compare, colors = pal, opacity = 0.5)
