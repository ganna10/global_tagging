setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

data.path <- "/work/users/jco/HTAP2_Model_Output/"
data.dirs <- list.dirs(path = data.path)
data.dirs <- data.dirs[-1] # remove data.path directory

get.ensemble.data <- function (month.nr) {
  month <- toupper(month(month.nr, label = TRUE))
  model.name <- str_split_fixed(test.dir, pattern = "Output//", n = 2)[2]
  
  file.name <- paste0(test.dir, "/htap2_", model.name, "_vmro3_ModelLevel_2010-", month, ".nc")
  existance <- file.exists(file.name)
  if (existance == "FALSE") {
    stop(cat(file.name, "doesn't exist\n"))
  }
  
  model <- gsub(pattern = "_BASE", replacement = '', x = model.name)
#   print(model)  
  raster <- raster(file.name, varname = "vmro3", lev = 1, stopIfNotEqualSpaced=FALSE)
  data.df <- tbl_df(as.data.frame(rasterToPoints(raster)))
  print(raster)
  data.df <- data.df %>%
    mutate(O3 = O3.Volume.Mixing.Ratio * 1e9, Model = model, Month = month(month.nr, label = TRUE)) %>%
    dplyr::select(lon = x, lat = y, O3, Model, Month)
}

test.dir <- data.dirs[12]
month.nrs <- seq(1, 12, by = 1)
month.nrs <- 1
list.data <- lapply(month.nrs, get.ensemble.data)
data.df <- tbl_df(do.call("rbind", list.data))
data.df

p <- ggplot()
p <- p + geom_raster(data = data.df, aes(x = lon, y = lat, fill = O3))
p <- p + geom_path(data = map_data("world"), aes( x = long, y = lat, group = group))
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + plot_theme()
p <- p + scale_fill_distiller(palette = "Spectral", name = "O3 (ppbv)")
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.line = element_blank())
p <- p + facet_grid(Model ~ Month)
p
