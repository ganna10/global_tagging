setwd("~/Documents//Analysis//2016_HTAP//Monthly_data")

all.data <- tbl_df(read.csv(file = "seasonal_mda8_average_total_allocated.csv", header = TRUE))
all.data$Source <- factor(all.data$Source, levels = c("Total", "Local", "Transported", "Stratosphere", "Other"))
all.data$Season <- factor(all.data$Season, levels = c("DJF", "MAM", "JJA", "SON"))
all.data <- all.data %>% rename(long = lon)

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
p <- p + geom_raster(data = all.data, aes(x = long, y = lat, fill = Mixing.Ratio), hjust = 1, vjust = 1)
p <- p + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group))
p <- p + plot_theme()  
p <- p + facet_grid(Source ~ Season)
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_fill_distiller(name = "O3 MDA8 (ppbv)", palette = "Spectral", breaks = seq(0, 125, 25), limits = c(0, 125))
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.line = element_blank())
p <- p + ggtitle("Seasonal Average of O3 MDA8 in 2010")
p <- p + theme(legend.position = "top")
p <- p + guides(fill = guide_colourbar(barwidth = 22, barheight = 2.5))
p <- p + theme(strip.text = element_text(face = "bold", size = 20))
p <- p + theme(legend.title = element_text(face = "bold", size = 20))
p <- p + theme(strip.text.y = element_text(hjust = 0))
p <- p + theme(legend.text = element_text(size = 18))
p <- p + theme(plot.title = element_text(size = 22))
p

CairoPDF(file = "Seasonal_MDA8_Global_distribution.pdf", width = 21, height = 15)
print(p)
dev.off()
