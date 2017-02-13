voc.raster <- rotate(raster("/worknb/users/jco//HTAP//HTAP_VOC_Tagging/voc_new_htap.cam.h1.2010-07.meanO3.alltimesteps.nc", varname = "O3", lev = 56))
voc.df <- tbl_df(as.data.frame(rasterToPoints(voc.raster)))
voc.df <- voc.df %>%
  mutate(O3 = O3.concentration * 1e9, Type = "VOC.Tagging") %>%
  dplyr::select(lon = x, lat = y, O3, Type)
voc.df

nox.raster <- rotate(raster("/worknb/users/jco/HTAP//evaluation_with_ncar/HTAP_NOx_Tagging.cam.h2.2010-07.O3mean.nc", varname = "O3", lev = 56))
nox.df <- tbl_df(as.data.frame(rasterToPoints(nox.raster)))
nox.df <- nox.df %>%
  mutate(O3 = O3.concentration * 1e9, Type = "NOx.Tagging") %>%
  dplyr::select(lon = x, lat = y, O3, Type)
nox.df

all.df <- rbind(voc.df, nox.df)
all.df

ggplot() + geom_raster(data = all.df, aes(x = lon, y = lat, fill = O3)) + geom_path(data = map_data("world"), aes(x= long, y = lat, group = group)) + plot_theme() + scale_fill_distiller(palette = "Spectral") + facet_grid(~ Type)

diff.df <- all.df %>%
  spread(Type, O3) %>%
  mutate(Diff = NOx.Tagging - VOC.Tagging)
diff.df

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

p <- ggplot() + geom_raster(data = diff.df, aes(x = lon, y = lat, fill = Diff), hjust = 1, vjust = 1) + geom_path(data = world.map.correct, aes(x= long, y = lat, group = group)) + plot_theme() + scale_fill_distiller(palette = "Spectral", name = "Difference (ppbv)", limits = c(-40, 70), na.value = "green", breaks = seq(-40, 70, 25))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + theme(axis.line = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + ggtitle(label = "Monthly Mean O3 difference in July (NOx.Tagging - VOC.Tagging)", subtitle = paste0("Max Difference = ", sprintf("%.2f", max(diff.df$Diff)), " ppbv, Min Difference = ", sprintf("%.2f", min(diff.df$Diff)), " ppbv"))
p <- p + theme(plot.title = element_text(size = 13, hjust = 0.5))
p <- p + theme(plot.subtitle = element_text(size = 12, hjust = 0.5))
p

CairoPDF(file = "NOx_tagging-VOC_tagging_differences.pdf", width = 10, height = 7)
print(p)
dev.off()
