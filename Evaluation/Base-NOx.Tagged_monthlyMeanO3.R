setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

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

get.monthly.diffs <- function (month) {
  mon <- month.abb[month]
  file.name <- paste0("/worknb/users/jco/HTAP/evaluation_with_ncar/Base-NOx/Base-NOx.Tagging.2010-", mon, ".nc")
  diff.raster <- rotate(raster(file.name, varname = "O3", lev = 56))
  diff.df <- tbl_df(as.data.frame(rasterToPoints(diff.raster)))
  return.df <- diff.df %>%
    mutate(Month = mon, O3 = O3.concentration * 1e9) %>%
    dplyr::select(lon = x, lat = y, Month, O3)
  return(return.df)
}

months <- seq(1, 12, by = 1)
list.data <- lapply(months, get.monthly.diffs)
list.df <- bind_rows(list.data)
list.df$Month <- factor(list.df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

max.diff = sprintf("%.2f", max(list.df$O3))
min.diff = sprintf("%.2f", min(list.df$O3))

p <- ggplot()
p <- p + geom_raster(data = list.df, aes(x = lon, y = lat, fill = O3), hjust = 1, vjust = 1)
p <- p + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group))
p <- p + facet_wrap(~ Month, ncol = 4)
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + plot_theme()
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.line = element_blank())
p <- p + theme(strip.text = element_text(face = "bold"))
p <- p + ggtitle(paste0("Difference betwen Monthly Mean O3 in Base and NOx Tagged Runs\nMaximum Difference: ", max.diff, " ppbv, Minimum Difference: ", min.diff, " ppbv"))
p <- p + scale_fill_gradient2(low = muted("Blue"), mid = "White", high = muted("Red"), name = "Base - NOx Tagged (ppbv)")
p <- p + theme(legend.position = "top")
p <- p + theme(legend.title = element_text(face = "bold", size = 16))
p <- p + theme(legend.text = element_text(size = 12))
p <- p + theme(strip.text = element_text(face = "bold", size = 20))
p <- p + theme(plot.title = element_text(face = "bold", size = 25))
p <- p + guides(fill = guide_colourbar(barwidth = 30, barheight = 4))

CairoPDF(file = "Difference_Base-NOx_Tagged_Monthly_Mean_O3.pdf", width = 20, height = 14)
print(p)
dev.off()
