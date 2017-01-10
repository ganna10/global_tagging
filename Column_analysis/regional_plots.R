setwd("~/Documents//Analysis//2016_HTAP//Column_analysis")

# world map
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

all.data <- read.csv("Monthly_assigned_column_values_to_regions.csv")
all.data <- tbl_df(all.data)
all.data$Month <- factor(all.data$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

p <- ggplot()
p <- p + geom_raster(data = all.data %>% filter(Species == "O3"), aes(x = lon, y = lat, fill = Mixing.Ratio), hjust = 1, vjust = 1)
p <- p + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group), size = 0.7)
p <- p + facet_wrap( ~ Month, nrow = 2)
p <- p + theme_tufte()
p <- p + labs(fill = "Column O3 (DU)")
p <- p + theme(legend.position = "top")
p <- p + scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30), expand = c(0, 0))
p <- p + scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30), expand = c(0, 0))
p <- p + scale_fill_distiller(palette = "Spectral") # , breaks = seq(0, 125, 25), limits = c(0, 100))
# p <- p + theme(plot.margin = unit(c(1, 5, 1, 1), "mm"))
p <- p + ggtitle("Global Concentrations of Ozone in each Month")
p <- p + theme(plot.title = element_text(face = "bold"))
p <- p + theme(legend.title = element_text(face = "bold"))
p <- p + theme(strip.text = element_text(face = "bold"))
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(panel.spacing = unit(5, "mm"))
p

CairoPDF(file = "Global_column_O3_each_month.pdf", width = 20, height = 14)
print(p)
dev.off()
