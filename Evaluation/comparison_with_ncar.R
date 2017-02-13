setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

get.difference.data <- function (month) {
  file.month <- sprintf("%02d", month)
  
  # ncar data
  ncar.file.name <- paste0("/data/nosync//modeloutput//CESM//HTAP2_CCMI//cesm111ccmi32_htap_base.cam.h0.2010-", file.month, ".nc")
  ncar.raster <- raster(ncar.file.name, lev = 56, varname = "O3")
  ncar.df <- tbl_df(as.data.frame(rasterToPoints(rotate(ncar.raster))))
  ncar.df <- ncar.df %>%
    mutate(O3 = O3.concentration * 1e9, Type = "NCAR") %>%
    dplyr::select(lon = x, lat = y, O3, Type)
  
  # NOx tagging data
  nox.file.name <- paste0("/worknb/users/jco/HTAP//evaluation_with_ncar/HTAP_NOx_Tagging.cam.h2.2010-", file.month, ".O3mean.nc")
  nox.raster <- raster(nox.file.name, varname = "O3", lev = 56)
  nox.df <- tbl_df(as.data.frame(rasterToPoints(rotate(nox.raster))))
  nox.df <- nox.df %>%
    mutate(O3 = O3.concentration * 1e9, Type = "NOx") %>%
    dplyr::select(lon = x, lat = y, O3, Type)
  
  all.data <- rbind(ncar.df, nox.df) 
  diff.df <- all.data %>%
    spread(Type, O3) %>%
    mutate(Abs.diff = NOx - NCAR, Rel.diff = (NOx - NCAR) * 100 / NCAR, Month = month.abb[month]) %>%
    dplyr::select(-NCAR, -NOx) %>%
    gather(Type, Difference, -lon, -lat, -Month)
  return(diff.df)
}

months <- seq(1, 12, by = 1)
data.list <- lapply(months, get.difference.data)
data.df <- do.call("rbind", data.list)
data.df

data.df$Month <- factor(data.df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#sort out world map outline
world.map <- map_data("world")
out.grid <- world.map %>%
  filter(long >= 180) %>%
  mutate(long = long - 360)
out.grid$group[out.grid$group == 539] <- 1628 # max(world.map$group) + 1
out.grid$group[out.grid$group == 540] <- max(out.grid$group) + 1
out.grid$group[out.grid$group == 541] <- max(out.grid$group) + 1
out.grid$group[out.grid$group == 1276] <- max(out.grid$group) + 1
out.grid$group[out.grid$group == 1309] <- max(out.grid$group) + 1

world.map.correct <- world.map %>%
  filter(long < 180)
world.map.correct <- rbind(world.map.correct, out.grid)

p <- ggplot()
p <- p + geom_raster(data = data.df %>% filter(Type == "Abs.diff"), aes(x = lon, y = lat, fill = Difference))
p <- p + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group))
p <- p + facet_wrap(~ Month, nrow = 3)
p <- p + scale_fill_gradient2(low = muted("Blue"), mid = "White", high = muted("Red"), name = "NOx.Tagging - NCAR (ppbv)", limits = c(-30, 30), breaks = seq(-30, 30, by = 5))
p <- p + plot_theme()
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.line = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(legend.position = "top")
p <- p + theme(legend.title = element_text(face = "bold", size = 16))
p <- p + theme(legend.text = element_text(size = 12))
p <- p + theme(strip.text = element_text(face = "bold", size = 20))
p <- p + guides(fill = guide_colourbar(barwidth = 30, barheight = 4))

CairoPDF(file = "Absolute_differences_O3_between_NCAR_NOx.Tagging.pdf", width = 20, height = 14)
print(p)
dev.off()
