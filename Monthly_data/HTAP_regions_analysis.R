nc <- nc_open("../HTAP_regions.nc")

var.raster <- raster("../HTAP_regions.nc", varname = "region_codes")
var.rotated.raster <- rotate(var.raster)
var.df <- tbl_df(as.data.frame(rasterToPoints(var.rotated.raster)))

ggplot() + geom_raster(data = var.df, aes(x = x, y = y, fill = region_codes)) + theme_tufte() + scale_fill_distiller(palette = "Spectral")

nam <- var.df %>%
  filter(region_codes == 3)
max(nam$y)
