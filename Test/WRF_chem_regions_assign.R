setwd("~/Documents//Analysis//2016_HTAP//Test")

regions.file <- raster("/work/users/alu/for_jane/WRF_OUT/map.nc", varname = "reg")
regions.df <- tbl_df(as.data.frame(rasterToPoints(regions.file)))
regions.df

data.file <- raster("/work/users/alu/for_jane/WRF_OUT/AVE//aug.nc", varname = "o3", level = 1)
data.df <- tbl_df(as.data.frame(rasterToPoints(data.file)))
data.df

assign_region <- function (longitude, latitude, regions.df) { # assign regions
  pinpoint <- regions.df %>% 
    filter(x == longitude, y == latitude)
  region.code <- pinpoint$reg
  
  if (region.code == 1) {
    region <- "Atlantic"
  } else if (region.code == 2) {
    region <- "Mediterranean + Black Sea"
  } else if (region.code == 3) {
    region <- "Baltic Sea"
  } else if (region.code == 4) {
    region <- "Central E Europe"
  } else if (region.code == 5) {
    region <- "Benelux"
  } else if (region.code == 6) {
    region <- "Greece"
  } else if (region.code == 7) {
    region <- "Germany"
  } else if (region.code == 8) {
    region <- "Iberia"
  } else if (region.code == 9) {
    region <- "Scandinavia"
  } else if (region.code == 10) {
    region <- "France"
  } else if (region.code == 11) {
    region <- "UK; Iceland"
  } else if (region.code == 12) {
    region <- "Italy"
  } else if (region.code == 13) {
    region <- "Poland; NE Europe"
  } else if (region.code == 14) {
    region <- "Russia"
  } else if (region.code == 15) {
    region <- "Turkey"
  } else if (region.code == 0) {
    region <- "TBC"
  } else {
    region <- "No match"
  }
  return (region)
}

assigned.df <- data.df %>%
  rowwise() %>%
  mutate(Region = assign_region(x, y, regions.df))
tbl_df(assigned.df)

out.file <- paste0(month, "_assigned_to_regions.csv")
write.csv(assigned.df, file = out.file, row.names = FALSE, quote = FALSE)      