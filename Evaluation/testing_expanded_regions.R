setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

get.data <- function (name, month) {
  mon <- sprintf("%02d", month)
  if (name == "New") {
    file.name <- paste0("/worknb/users/jco/cesm/archive/HTAP_NOx_Tagging_Location_Sources_Spinup/atm/hist/HTAP_NOx_Tagging_Location_Sources_Spinup.2010-", mon, ".timavg.mean.nc")
  } else if (name == "Old") {
    file.name <- paste0("/worknb/users/jco/HTAP/evaluation_with_ncar/HTAP_NOx_Tagging.cam.h2.2010-", mon, ".O3mean.nc")
  }
  
  nc <- nc_open(file.name)
  all.vars <- as.data.frame(names(nc$var))
  colnames(all.vars) <- c("Vars")
  o3.vars.df <- all.vars %>%
    filter(str_detect(Vars, 'O3'))
  o3.vars <- as.vector(o3.vars.df$Vars)
  
  data.df <- NULL
  for (spc in o3.vars) {
    spc.raster <- rotate(raster(file.name, varname = spc, lev = 56))
    spc.df <- tbl_df(as.data.frame(rasterToPoints(spc.raster)))
    colnames(spc.df) <- c("lon", "lat", "Mixing.Ratio")
    spc.df <- spc.df %>%
      mutate(Mixing.Ratio = Mixing.Ratio * 1e9, Species = spc, Month = month.abb[month], Type = name)
    data.df <- rbind(data.df, spc.df)
  }
  
  if (name == "Old") { # combine all emission types into source tag
    data.df <- data.df %>%
      spread(Species, Mixing.Ratio) %>%
      mutate(O3_X_OCN = O3_X_OCNANT + O3_X_OCNBMB + O3_X_OCNSOI,
             O3_X_NAM = O3_X_NAMANT + O3_X_NAMBMB + O3_X_NAMSOI,
             O3_X_EUR = O3_X_EURANT + O3_X_EURBMB + O3_X_EURSOI,
             O3_X_SAS = O3_X_SASANT + O3_X_SASBMB + O3_X_SASSOI,
             O3_X_EAS = O3_X_EASANT + O3_X_EASBMB + O3_X_EASSOI,
             O3_X_MDE = O3_X_MDEANT + O3_X_MDEBMB + O3_X_MDESOI,
             O3_X_RBU = O3_X_RBUANT + O3_X_RBUBMB + O3_X_RBUSOI,
             O3_X_RST = O3_X_RSTANT + O3_X_RSTBMB + O3_X_RSTSOI) %>%
      dplyr::select(lon, lat, Month, Type, O3, O3_X_INI, O3_X_STR, O3_X_XTR, O3_X_LGT, O3_X_AIR, O3_X_OCN, O3_X_NAM, O3_X_EUR, O3_X_SAS, O3_X_EAS, O3_X_MDE, O3_X_RBU, O3_X_RST) %>%
      gather(Species, Mixing.Ratio, -lon, -lat, -Month, -Type)
  }
  return(data.df)
}

months <- seq(1, 8, by = 1)
types <- c("Old", "New")
all.data <- NULL

for (mon in months) {
  data.list <- lapply(types, get.data, month = mon)
  data.df <- bind_rows(data.list)
  all.data <- rbind(all.data, data.df)  
}

##### Difference between Tagged and Real O3
real <- all.data %>%
  filter(Species == "O3")

tagged <- all.data %>%
  filter(Species != "O3") %>%
  group_by(lon, lat, Month, Type) %>%
  summarise(Mixing.Ratio = sum(Mixing.Ratio)) %>%
  mutate(Species = "Total.Tagged") %>%
  dplyr::select(lon, lat, Month, Type, Species, Mixing.Ratio)

diff.df <- bind_rows(real, tagged)
diff.df <- diff.df %>%
  spread(Species, Mixing.Ratio) %>%
  mutate(Difference = O3 - Total.Tagged) %>%
  dplyr::select(-O3, -Total.Tagged)
diff.df$Month <- factor(diff.df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
diff.df$Type <- factor(diff.df$Type, levels = c("Old", "New"))

# plot differences in tagged and real O3
p <- ggplot()
p <- p + geom_raster(data = diff.df, aes(x = lon, y = lat, fill = Difference), hjust = 1, vjust = 1)
p <- p + geom_path(data = map_data("world"), aes(x = long, y = lat, group = group))
p <- p + facet_grid(Month ~ Type)
p <- p + plot_theme()
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + theme(axis.title = element_blank())
p <- p + theme(axis.text = element_blank())
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.line = element_blank())
p <- p + scale_fill_distiller(palette = "Spectral", name = "Real - Tagged.Sum (ppbv)")
p <- p + theme(legend.position = "top")
p <- p + theme(legend.title = element_text(face = "bold", size = 18))
p <- p + guides(fill = guide_colourbar(barwidth = 40, barheight = 3))
p <- p + theme(strip.text = element_text(face = "bold", size = 20))
p <- p + ggtitle("Difference between Real O3 and the Sum of All Tagged O3 Components (ppbv)")
p <- p + theme(legend.text = element_text(size = 16))
p <- p + theme(plot.title = element_text(face = "bold", size = 22))

CairoPDF(file = "Tagged_vs_Real_O3_in_New_and_Old_Tagging_runs.pdf", width = 15, height = 30)
print(p)
dev.off()

##### Spatial differences between Real O3 and common tags in New and Old Tagged runs
o3.diff <- all.data %>%
  filter(!Species %in% c("O3_X_SEA", "O3_X_NAF", "O3_X_MCA", "O3_X_RST")) %>%
  spread(Type, Mixing.Ratio) %>%
  group_by(lon, lat, Month, Species) %>%
  mutate(Difference = New - Old) %>%
  dplyr::select(-New, -Old)
o3.diff$Month <- factor(o3.diff$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
o3.diff

# p1 <- ggplot()
# p1 <- p1 + geom_raster(data = o3.diff, aes(x = lon, y = lat, fill = Difference), hjust = 1, vjust = 1)
# p1 <- p1 + geom_path(data = map_data("world"), aes(x = long, y = lat, group = group))
# p1 <- p1 + facet_grid(Species ~ Month)
# p1 <- p1 + plot_theme()
# p1 <- p1 + scale_y_continuous(expand = c(0, 0))
# p1 <- p1 + scale_x_continuous(expand = c(0, 0))
# p1 <- p1 + theme(axis.title = element_blank())
# p1 <- p1 + theme(axis.text = element_blank())
# p1 <- p1 + theme(axis.ticks = element_blank())
# p1 <- p1 + theme(axis.line = element_blank())
# p1 <- p1 + scale_fill_distiller(palette = "Spectral", name = "New - Old Run (ppbv)")
# p1 <- p1 + theme(legend.position = "top")
# p1 <- p1 + theme(legend.title = element_text(face = "bold", size = 28))
# p1 <- p1 + guides(fill = guide_colourbar(barwidth = 70, barheight = 6))
# p1 <- p1 + theme(strip.text = element_text(face = "bold", size = 28))
# p1 <- p1 + ggtitle("Difference between Real O3 and the Sum of All Tagged O3 Components (ppbv)")
# p1 <- p1 + theme(legend.text = element_text(size = 26))
# p1 <- p1 + theme(plot.title = element_text(face = "bold", size = 32))
# 
# CairoPDF(file = "Differences_in_O3_between_New_and_Old_Tagging_runs.pdf", width = 45, height = 30)
# print(p1)
# dev.off()

##### Global Monthly Mean differences between Real O3 and common tags in New and Old Tagged runs
global.mean <- all.data %>%
  filter(Species == "O3") %>%
  group_by(Month, Type) %>%
  summarise(Global.Mean = mean(Mixing.Ratio))
global.mean$Month <- factor(global.mean$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
global.mean

p2 <- ggplot(data = global.mean, aes(x = Month, y = Global.Mean, colour = Type, group = Type))
p2 <- p2 + geom_point()
p2 <- p2 + geom_line()
p2 <- p2 + plot_theme()
p2 <- p2 + ylab("Global Mean O3 (ppbv)")
p2 <- p2 + theme(axis.title.x = element_blank())
p2 <- p2 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
p2 <- p2 + scale_colour_manual(values = c("New" = "#0352cb", "Old" = "#ef6638"))
p2 <- p2 + scale_y_continuous(limits = c(0, 30))
p2 <- p2 + theme(legend.position = "top")
p2 <- p2 + theme(legend.title = element_blank())

CairoPDF(file = "Monthly_Global_Mean_O3_between_New_Old_Runs.pdf", width = 10, height = 7)
print(p2)
dev.off()

###### percentage contribution of initial conditions to real O3
calc.perct <- all.data %>%
  filter(Type == "New", Species %in% c("O3", "O3_X_INI")) %>%
  spread(Species, Mixing.Ratio) %>%
  mutate(INI.Contribution = O3_X_INI / O3 * 100) %>%
  dplyr::select(lon, lat, Month, INI.Contribution)
calc.perct$Month <- factor(calc.perct$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

p3 <- ggplot()
p3 <- p3 + geom_raster(data = calc.perct, aes(x = lon, y = lat, fill = INI.Contribution), hjust = 1, vjust = 1)
p3 <- p3 + geom_path(data = map_data("world"), aes(x = long, y = lat, group = group))
p3 <- p3 + facet_wrap(~ Month, nrow = 3)
p3 <- p3 + plot_theme()
p3 <- p3 + scale_y_continuous(expand = c(0, 0))
p3 <- p3 + scale_x_continuous(expand = c(0, 0))
p3 <- p3 + theme(axis.title = element_blank())
p3 <- p3 + theme(axis.text = element_blank())
p3 <- p3 + theme(axis.ticks = element_blank())
p3 <- p3 + theme(axis.line = element_blank())
p3 <- p3 + scale_fill_distiller(palette = "Spectral", name = "% Contribution")
p3 <- p3 + theme(legend.position = "top")
p3 <- p3 + theme(legend.title = element_text(face = "bold", size = 18))
p3 <- p3 + guides(fill = guide_colourbar(barwidth = 40, barheight = 3))
p3 <- p3 + theme(strip.text = element_text(face = "bold", size = 20))
p3 <- p3 + ggtitle("Percent Contribution of Initial Conditions to Total O3")
p3 <- p3 + theme(legend.text = element_text(size = 16))
p3 <- p3 + theme(plot.title = element_text(face = "bold", size = 22))

CairoPDF(file = "Contribution_of_initial_conditions.pdf", width = 30, height = 15)
print(p3)
dev.off()
