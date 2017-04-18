# assign MDA8 values to different HTAP regions
# Version 0: Jane Coates 10/1/2017
# Version 1: Jane Coates 31/1/2017 Assigning to Tier 2 regions
# Version 2: Jane Coates 14/2/2017 Making script more flexible with arguments

library("ncdf4")
library("dplyr")
library("stringr")
library("raster")
library("lubridate")

args <- commandArgs(trailingOnly = TRUE) # 1: file 2: Month
file.name <- args[1]
month <- args[2]

nc <- nc_open(file.name)
all.vars <- as.data.frame(names(nc$var))
colnames(all.vars) <- c("Vars")
o3.vars.df <- all.vars %>%
  filter(str_detect(Vars, 'O3'))
o3.vars <- as.vector(o3.vars.df$Vars)
o3.vars

get.monthly.data <- function (month, species, file.name) {
  #file.name <- paste0(file.path, case, ".2010-", sprintf("%02d", month), ".mean.nc")
  
  var.raster <- raster(file.name, varname = species, lev = 56)
  var.rotated.raster <- rotate(var.raster)
  var.raster.ppbv <- var.rotated.raster
  
  var.df <- as.data.frame(rasterToPoints(var.raster.ppbv))
  colnames(var.df) <- c("lon", "lat", "Mixing.Ratio")
  var.df$Species <- rep(species, length(var.df$lon))
  var.df$Month <- rep(month, nrow(var.df))
  return (var.df)
}

all.data.df <- NULL
for (spc in o3.vars) {
  data.df <- get.monthly.data(month = month, species = spc, file.name = file.name)
  all.data.df <- rbind(all.data.df, data.df)
}
#all.data.df$lat <- round(all.data.df$lat, digits = 1)

# get HTAP regions
regions <- raster("/work/users/jco/tagging_global/land_mask/Tier2_receptor_regions_1.9x2.5.nc")
region.rot <- rotate(regions)
region.df <- as.data.frame(rasterToPoints(region.rot))
colnames(region.df) <- c("lon", "lat", "region")
levels(factor(region.df$lon))

#filter(region.df, lon == -177.5)
#filter(region.df, lat == 90)

#print(tbl_df(all.data.df))
#levels(factor(all.data.df$lon))
#levels(factor(region.df$lon))
#levels(factor(all.data.df$lat))
#levels(factor(region.df$lat))
  
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

assigned.df <- all.data.df %>%
  rowwise() %>%
  mutate(Region = assign_region(lon, lat, region.df))
tbl_df(assigned.df)

out.file <- paste0("HTAP_base_", month, "_assigned_to_Tier2_regions.csv")
write.csv(assigned.df, file = out.file, row.names = FALSE, quote = FALSE)
