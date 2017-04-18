setwd("~/Documents//Analysis//2016_HTAP//WRF")

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

aug.26 <- rotate(raster("/worknb/users/jco/cesm/archive/Post_process_alu_HTAP_NOx_Tagging/NOx_Tagged.cam.h2.2010-08-26.O3surface.nc", varname = "O3_X_STR"))
str.df.26 <- tbl_df(as.data.frame(rasterToPoints(aug.26)))
str.df.26$Date <- rep("Aug.26", nrow(str.df.26))
colnames(str.df.26) <- c("lon", "lat", "Strat", "Date")
str.df.26$Strat <- str.df.26$Strat * 1e9
str.df.26

aug.27 <- rotate(raster("/worknb/users/jco/cesm/archive/Post_process_alu_HTAP_NOx_Tagging/NOx_Tagged.cam.h2.2010-08-27.O3surface.nc", varname = "O3_X_STR"))
str.df.27 <- tbl_df(as.data.frame(rasterToPoints(aug.27)))
str.df.27$Date <- rep("Aug.27", nrow(str.df.27))
colnames(str.df.27) <- c("lon", "lat", "Strat", "Date")
str.df.27$Strat <- str.df.27$Strat * 1e9
str.df.27

all.data <- rbind(str.df.26, str.df.27)

p <- ggplot()
p <- p + geom_raster(data = all.data, aes(x = lon, y = lat, fill = Strat), hjust = 0, vjust = 0)
p <- p + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group))
p <- p + facet_wrap(~ Date)
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_fill_distiller(palette = "Spectral")
p <- p + theme_tufte()
p
