setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

get.data.df <- function (source.type, month) {
  if (source.type == "NCAR") {
    path <- "/data/nosync//modeloutput//CESM//HTAP2_CCMI//cesm111ccmi32_htap_base.cam.h0.2010-"
    ending <- ".nc"
  } else if (source.type == "NOx") {
    path <- "/worknb/users/jco/HTAP//evaluation_with_ncar/HTAP_NOx_Tagging.cam.h2.2010-"
    ending <- ".O3mean.nc"
  } else if (source.type == "VOC") {
    path <- "/worknb/users/jco/HTAP/HTAP_VOC_Tagging/voc_new_htap.cam.h1.2010-"
    ending <- ".meanO3.alltimesteps.nc"
  }
  
  file.month <- sprintf("%02d", month)
  file.name <- paste0(path, file.month, ending)
  raster.data <- raster(file.name, lev = 56, varname = "O3")
  data.df <- tbl_df(as.data.frame(rasterToPoints(rotate(raster.data))))
  data.df <- data.df %>%
    mutate(O3 = O3.concentration * 1e9, Type = source.type, Month = month.abb[month]) %>%
    dplyr::select(lon = x, lat = y, O3, Type, Month)
  
  return(data.df)
}

get.difference.data <- function (month) {
  all.sources.list <- lapply(sources, get.data.df, month = month)
  all.sources.df <- tbl_df(do.call("rbind", all.sources.list))
  return(all.sources.df)
}

months <- seq(1, 12, by = 1)
sources <- c("VOC", "NOx", "NCAR")

list.data <- lapply(months, get.difference.data)
df.data <- tbl_df(do.call("rbind", list.data))
df.data

diff.df <- df.data %>%
  spread(Type, O3) %>%
  mutate(NOx.diff = NOx - NCAR, VOC.diff = VOC - NCAR) %>%
  dplyr::select(-NCAR, -NOx, -VOC) %>%
  gather(Type, Difference, -lon, -lat, -Month)
diff.df

ggplot() + geom_raster(data = diff.df, aes(x = lon, y = lat, fill = Difference)) + geom_path(data = map_data("world"), aes(x= long, y = lat, group = group)) + plot_theme() + scale_fill_distiller(palette = "Spectral") + facet_grid(Type ~ Month)
