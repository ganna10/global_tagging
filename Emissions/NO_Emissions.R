setwd("~//Documents//Analysis//2016_HTAP//Emissions")

# all.nox.nc <- nc_open(filename = "/data/nosync/modelinput/CESM/inputdata/atm/cam/chem/emis/HTAP2/emissions_htap2sectors-gfed3_2007-2011_NO_mol_1.9x2.5.nc")
all.vars <- c("ENERGY", "INDUSTRY", "TRANSPORT", "RESIDENTIAL", "SHIPS", "bb", "soil")

get.data <- function (variable) {
  raster.data <- brick("/data/nosync/modelinput/CESM/inputdata/atm/cam/chem/emis/HTAP2/emissions_htap2sectors-gfed3_2007-2011_NO_mol_1.9x2.5.nc", varname = variable)
  rotated.raster <- rotate(raster.data)
  data.df <- tbl_df(as.data.frame(rasterToPoints(rotated.raster)))
  data.df.2010 <- data.df %>%
    dplyr::select(lon = x, lat = y, Jan = X2010.01.15, Feb = X2010.02.15, Mar = X2010.03.15, Apr = X2010.04.15, May = X2010.05.15, Jun = X2010.06.15, Jul = X2010.07.15, Aug = X2010.08.15, Sep = X2010.09.15, Oct = X2010.10.15, Nov = X2010.11.15, Dec = X2010.12.15) %>%
    gather(Month, NO.Emissions, -lon, -lat) %>% 
    mutate(Type = variable)
  return (data.df.2010)
}

all.data.list <- lapply(all.vars, get.data)
all.data.df <- do.call("rbind", all.data.list)
all.data.df <- tbl_df(all.data.df)
assigned.emissions <- all.data.df %>%
  spread(Type, NO.Emissions) %>%
  mutate(Anthropogenic = ENERGY + INDUSTRY + RESIDENTIAL + SHIPS + TRANSPORT) %>%
  dplyr::select(lon, lat, Month, Anthropogenic, Biogenic = soil, Biomass.burning = bb) %>%
  gather(Type, NO.Emissions, -lon, -lat, -Month)
assigned.emissions$Month <- factor(assigned.emissions$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
assigned.emissions$Type[assigned.emissions$Type == "Biomass.burning"] <- "Biomass\nBurning"
assigned.emissions$Type <- factor(assigned.emissions$Type, levels = c("Anthropogenic", "Biogenic", "Biomass\nBurning"))

p <- ggplot()
p <- p + geom_raster(data = assigned.emissions, aes(x = lon, y = lat, fill = NO.Emissions))
p <- p + geom_path(data = map_data("world"), aes(x = long, y = lat, group = group))
p <- p + facet_grid(Type ~ Month)
p <- p + scale_fill_distiller(palette = "Spectral", name = "NO Emissions\n(molecules cm-2 s-1)")
p <- p + plot_theme()
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.line = element_blank())
p <- p + ggtitle("Monthly NO Emissions in 2010")
p <- p + theme(legend.position = "top")
p <- p + guides(fill = guide_colourbar(barwidth = 22, barheight = 2.5))
p <- p + theme(strip.text = element_text(face = "bold", size = 20))
p <- p + theme(legend.title = element_text(face = "bold", size = 18))
p <- p + theme(strip.text.y = element_text(hjust = 0))
p <- p + theme(legend.text = element_text(size = 18))
p <- p + theme(plot.title = element_text(size = 22))
p

CairoPDF(file = "Monthly_NO_Emissions.pdf", width = 22, height = 15)
print(p)
dev.off()
