# assign values to different HTAP regions: arguments in.file month 
# Version 0: Jane Coates 8/2/2017

library("ncdf4")
library("dplyr")
library("stringr")
library("raster")
library("lubridate")

args <- commandArgs(trailingOnly = TRUE) # 1: file, 2: Month, 
file.name <- args[1]
month <- args[2]
model <- "C-IFS_v2"

#args <- commandArgs(trailingOnly = TRUE)
#in.file <- args[1]
#out.file <- args[2]
#month <- args[3]
#model <- args[4]
#print(in.file)
#print(out.file)
#print(month)
#print(model)

var.raster <- raster(file.name, varname = "vmro3", lev = 1)
#var.rotated.raster <- rotate(var.raster)
var.df <- tbl_df(as.data.frame(rasterToPoints(var.raster)))
colnames(var.df) <- c("lon", "lat", "Mixing.Ratio")
var.df$Month <- rep(month, length(var.df$lon))
var.df$Model <- rep(model, length(var.df$lon))
var.df$lat <- round(var.df$lat, digits = 6)
print(var.df)

# get HTAP regions
regions <- raster("/work/users/jco/tagging_global/land_mask/Tier2_receptor_regions_1.9x2.5.nc")
#region.rot <- rotate(regions)
region.rot <- (regions)
region.df <- tbl_df(as.data.frame(rasterToPoints(region.rot)))
colnames(region.df) <- c("lon", "lat", "region")
region.df$lat <- round(region.df$lat, digits = 6)
print(region.df)
levels(factor(var.df$lon))
levels(factor(region.df$lon))
levels(factor(var.df$lat))
levels(factor(region.df$lat))
#lat.filtered <- region.df %>%
#    filter(abs(lat - my.lat) == min(abs(lat - my.lat))) %>%
#    filter(abs(lon - my.lon) == min(abs(lon - my.lon))) 

assign_region <- function (longitude, latitude, regions.df) { # assign HTAP regions 
  pinpoint <- regions.df %>%
    filter(lon == longitude, lat == latitude)
  region.code <- pinpoint$region
  if (region.code == 20 ) {
    region <- "Baltic Sea"
  } else if (region.code == 21) {
    region <- "North Atlantic"    
  } else if (region.code == 22) {
    region <- "South Atlantic"    
  } else if (region.code == 23) {
    region <- "North Pacific"    
  } else if (region.code == 24) {
    region <- "South Pacific"    
  } else if (region.code == 25) {
    region <- "Indian Ocean"    
  } else if (region.code == 26) {
    region <- "Hudson Bay"    
  } else if (region.code == 27) {
    region <- "Mediterranean Sea"    
  } else if (region.code == 28) {
    region <- "Black and Caspian Sea"    
  } else if (region.code == 31) {
    region <- "NE US"    
  } else if (region.code == 32) {
    region <- "SE US"    
  } else if (region.code == 33) {
    region <- "NW US"    
  } else if (region.code == 34) {
    region <- "SW US"    
  } else if (region.code == 35) {
    region <- "E Canada"
  } else if (region.code == 36) {
    region <- "W Canada and Alaska"
  } else if (region.code == 41) {
    region <- "NW Europe"
  } else if (region.code == 42) {
    region <- "SW Europe"
  } else if (region.code == 43) {
    region <- "E Europe"
  } else if (region.code == 44) {
    region <- "Greece; Turkey; Cyprus"
  } else if (region.code == 51) {
    region <- "N India; Nepal; Bangladesh; Afghanistan; Pakistan"
  } else if (region.code == 52) {
    region <- "S India; Sri Lanka"
  } else if (region.code == 53) {
    region <- "Indian Himalaya"
  } else if (region.code == 61) {
    region <- "NE China"
  } else if (region.code == 62) {
    region <- "SE China"
  } else if (region.code == 63) {
    region <- "W China; Mongolia"
  } else if (region.code == 64) {
    region <- "N Korea; S Korea"
  } else if (region.code == 65) {
    region <- "Japan"
  } else if (region.code == 66) {
    region <- "China; Tibet Himalaya"
  } else if (region.code == 111) {
    region <- "Lebanon; Israel; Jordan; Syria"
  } else if (region.code == 112) {
    region <- "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain"
  } else if (region.code == 113) {
    region <- "Iran; Iraq"
  } else if (region.code == 141) {
    region <- "W Russia"
  } else if (region.code == 142) {
    region <- "E Russia"
  } else if (region.code == 143) {
    region <- "Belarus; Ukraine"
  } else {
    region <- "Rest"
  }
  return(region)
}

assigned.df <- var.df %>%
  rowwise() %>%
  mutate(Region = assign_region(lon, lat, region.df))
print(assigned.df)
out.file <- paste0(model, "_2010-", month, "_assigned_Tier2.csv")
write.csv(assigned.df, file = out.file, row.names = FALSE, quote = FALSE)
