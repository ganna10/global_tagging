setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

NOx.tagging.raster <- raster("Column//HTAP_NOx_Tagging.cam.h2.2010-01.timeavColumnO3.nc", varname = "O3")
NOx.tagging.df <- tbl_df(as.data.frame(rasterToPoints(rotate(NOx.tagging.raster))))
NOx.tagging.df <- NOx.tagging.df %>%
  mutate(Type = "NOx.Tagging")
NOx.tagging.df

tagging.monthly.mean <- raster::stack("HTAP_tagged_base.2010-01.mean.nc", varname = "O3")

indices <- format(as.Date(names(tagging.monthly.mean), format = "X%Y.%m.%d.%h"), format = "%m")
indices <- as.numeric(indices)
indices
as.Date(names(tagging.monthly.mean), format = "X%Y.%m.%d.%H")

tagging.col.sum <- stackApply(tagging.monthly.mean, indices = , fun = sum)
tagging.col.sum
tagging.col.df <- tbl_df(as.data.frame(rasterToPoints(tagging.col.sum)))
tagging.col.df
