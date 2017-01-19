setwd("~/Documents//Analysis//2016_HTAP//Monthly_data")

jul.file <- "HTAP_NOx_Tagging.cam.h2.2010-07.avMDA8.nc"
nc <- nc_open(jul.file)
all.vars <- as.data.frame(names(nc$var))
colnames(all.vars) <- c("Vars")
o3.vars.df <- all.vars %>%
  filter(str_detect(Vars, 'O3'))
o3.vars <- as.vector(o3.vars.df$Vars)
o3.vars

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

get.data <- function (species, file.name) {
  var.raster <- raster(file.name, varname = species, lev = 56)
  var.rotated.raster <- rotate(var.raster)
  var.raster.ppbv <- var.rotated.raster
  
  var.df <- as.data.frame(rasterToPoints(var.raster.ppbv))
  colnames(var.df) <- c("lon", "lat", "Mixing.Ratio")
  var.df$Species <- rep(species, length(var.df$lon))
  return (var.df)
}

data.list <- lapply(o3.vars, FUN = get.data, file.name = jul.file)
data.df <- do.call("rbind", data.list)
data.df <- tbl_df(data.df)

spreaded <- data.df %>%
  spread(Species, Mixing.Ratio)

sums <- spreaded %>%
  mutate(Anthropogenic = O3_X_EASANT + O3_X_EURANT + O3_X_NAMANT + O3_X_MDEANT + O3_X_OCNANT + O3_X_RBUANT + O3_X_RSTANT + O3_X_SASANT, Natural = O3_X_EASSOI + O3_X_EURSOI + O3_X_NAMSOI + O3_X_MDESOI + O3_X_OCNSOI + O3_X_RBUSOI + O3_X_RSTSOI + O3_X_SASSOI, Biomass.Burning = O3_X_EASBMB + O3_X_EURBMB + O3_X_NAMBMB + O3_X_MDEBMB + O3_X_OCNBMB + O3_X_RBUBMB + O3_X_RSTBMB + O3_X_SASBMB, Others = O3_X_INI + O3_X_XTR) %>%
  dplyr::select(-O3_X_EASANT, -O3_X_EURANT, -O3_X_NAMANT, -O3_X_MDEANT, -O3_X_OCNANT, -O3_X_RBUANT, -O3_X_RSTANT, -O3_X_SASANT, -O3_X_EASSOI, -O3_X_EURSOI, -O3_X_NAMSOI, -O3_X_MDESOI, -O3_X_OCNSOI, -O3_X_RBUSOI, -O3_X_RSTSOI, -O3_X_SASSOI, -O3_X_EASBMB, -O3_X_EURBMB, -O3_X_NAMBMB, -O3_X_MDEBMB, -O3_X_OCNBMB, -O3_X_RBUBMB, -O3_X_RSTBMB, -O3_X_SASBMB, -O3_X_INI, -O3_X_XTR, Aircraft = O3_X_AIR, Lightning = O3_X_LGT, Stratosphere = O3_X_STR) %>%
  gather(Source, Mixing.Ratio, -lon, -lat)

sums$Source[sums$Source == "O3"] <- "Total O3"
sums$Source[sums$Source == "Biomass.Burning"] <- "Biomass Burning"
sums$Source <- factor(sums$Source, levels = c("Total O3", "Anthropogenic", "Natural", "Biomass Burning", "Aircraft", "Stratosphere", "Lightning", "Others"))

p <- ggplot()
p <- p + geom_raster(data = sums, aes(x = lon, y = lat, fill = Mixing.Ratio), hjust = 1, vjust = 1)
p <- p + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group))
p <- p + facet_wrap(~ Source, ncol = 2, scales = "free")
p <- p + theme_tufte()
p <- p + theme(strip.text = element_text(size = 18, face = "bold"))
p <- p + theme(legend.position = "bottom")
p <- p + scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30), expand = c(0, 0))
p <- p + scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30), expand = c(0, 0))
p <- p + scale_fill_gradientn(name = "O3 MDA8 (ppbv)", colours = rev(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')), values = rescale(c(1, 2.5, 5, 10, 20, 30, 40, 50, 75, 100, 125)), breaks = seq(0, 125, 25), limits = c(0, 125))
p <- p + theme(legend.title = element_text(size = 18, face = "bold"))
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(legend.text = element_text(size = 16))
p <- p + theme(panel.spacing = unit(5, "mm"))
# p <- p + theme(plot.margin = unit(c(1, 5, 1, 1), "mm"))
p <- p + guides(fill = guide_colourbar(barwidth = 20, barheight = 4))
# p <- p + ggtitle("Total Ozone Allocated to Different Sources of NOx Emissions")
# p <- p + theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
p

CairoPDF(file = "July_allocated.pdf", width = 13, height = 21)
print(p)
dev.off()
