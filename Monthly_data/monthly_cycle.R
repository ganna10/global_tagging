setwd("~/Documents//Analysis//2016_HTAP//Monthly_data")

# do commented parts on server
# months <- seq(1, 12, 1)
# file <- "HTAP_NOx_Tagging.cam.h2.2010-01.avMDA8.nc"
# nc <- nc_open(file)
# all.vars <- as.data.frame(names(nc$var))
# colnames(all.vars) <- c("Vars")
# o3.vars.df <- all.vars %>%
#   filter(str_detect(Vars, 'O3'))
# o3.vars <- as.vector(o3.vars.df$Vars)
# o3.vars
# 
# get.monthly.data <- function (month, species) {
#   file.name <- paste0("HTAP_NOx_Tagging.cam.h2.2010-", sprintf("%02d", month), ".avMDA8.nc")
#   
#   var.raster <- raster(file.name, varname = species, lev = 56)
#   var.rotated.raster <- rotate(var.raster)
#   var.raster.ppbv <- var.rotated.raster
#   
#   var.df <- as.data.frame(rasterToPoints(var.raster.ppbv))
#   colnames(var.df) <- c("lon", "lat", "Mixing.Ratio")
#   var.df$Species <- rep(species, length(var.df$lon))
#   var.df$Month <- rep(month(month, label = TRUE, abbr = TRUE), length(var.df$lon))
#   return (var.df)
# }
# 
# all.data.df <- NULL
# for (spc in o3.vars) {
#   data.list <- lapply(months, FUN = get.monthly.data, species = spc)
#   data.df <- do.call("rbind", data.list)
#   all.data.df <- rbind(all.data.df, data.df)
# }

# get HTAP regions
# regions <- raster("../HTAP_regions.nc")
# region.rot <- rotate(regions)
# region.df <- as.data.frame(rasterToPoints(region.rot))
# colnames(region.df) <- c("lon", "lat", "region")
# 
# assign_region <- function (longitude, latitude, regions.df) { # assign HTAP regions
#   pinpoint <- regions.df %>%
#     filter(lon == longitude, lat == latitude)
#   region.code <- pinpoint$region
#   if (region.code == 2) {
#     region <- "Ocean"
#   } else if (region.code == 3) {
#     region <- "North America"    
#   } else if (region.code == 4) {
#     region <- "Europe"
#   } else if (region.code == 5) {
#     region <- "South Asia"
#   } else if (region.code == 6) {
#     region <- "East Asia"
#   } else if (region.code == 11) {
#     region <- "Middle East"
#   } else if (region.code == 14) {
#     region <- "Russia"
#   } else {
#     region <- "Rest"
#   }
#   return(region)
# }

#### this step takes AAGGEESS!!!
# assigned.df <- all.data.df %>%
#   rowwise() %>%
#   mutate(Region = assign_region(lon, lat, region.df))
# write.csv(assigned.df, file = "Monthly_assigned_MDA8_to_regions.csv", row.names = FALSE, quote = FALSE)

#sort out world map outline
# world.map <- map_data("world")
# out.grid <- world.map %>%
#   filter(long >= 180) %>%
#   mutate(long = long - 360)
# out.grid$group[out.grid$group == 539] <- 1628 # max(world.map$group) + 1
# out.grid$group[out.grid$group == 540] <- max(out.grid$group) + 1
# out.grid$group[out.grid$group == 541] <- max(out.grid$group) + 1
# out.grid$group[out.grid$group == 1276] <- max(out.grid$group) + 1
# out.grid$group[out.grid$group == 1309] <- max(out.grid$group) + 1
# 
# world.map.correct <- world.map %>%
#   filter(long < 180)
# world.map.correct <- rbind(world.map.correct, out.grid)

assigned.df <- read.csv(file = "Monthly_assigned_MDA8_to_regions.csv", header = TRUE)

zonal.mean <- assigned.df %>%
  group_by(Species, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio))
tbl_df(zonal.mean)

zone.contribution <- function (region, month, total.o3, zonal.o3) { # calculate contributions
  zone.o3 <- zonal.o3 %>%
    filter(Region == region, Month == month)
  cont = total.o3 / zone.o3$Total
  return(cont)
}

all.o3.zonal <- zonal.mean %>%
  filter(Species == "O3") %>%
  group_by(Region, Month) %>%
  summarise(Total = sum(Zonal.Mean))

contribs <- zonal.mean %>%
  filter(Species != "O3") %>%
  group_by(Region, Month, Species) %>%
  summarise(Total = sum(Zonal.Mean)) %>%
  rowwise() %>%
  mutate(Contribution = zone.contribution(region = Region, month = Month, total.o3 = Total, zonal.o3 = all.o3.zonal) * 100)

contribs$Species <- as.character(contribs$Species)
contribs$Species[contribs$Species == "O3_X_AIR"] <- "Aircraft"
contribs$Species[contribs$Species == "O3_X_EASANT"] <- "EAS ANT"
contribs$Species[contribs$Species == "O3_X_EASSOI"] <- "EAS BIO"
contribs$Species[contribs$Species == "O3_X_EASBMB"] <- "EAS BMB"
contribs$Species[contribs$Species == "O3_X_EURANT"] <- "EUR ANT"
contribs$Species[contribs$Species == "O3_X_EURSOI"] <- "EUR BIO"
contribs$Species[contribs$Species == "O3_X_EURBMB"] <- "EUR BMB"
contribs$Species[contribs$Species == "O3_X_INI"] <- "Initial Conditions"
contribs$Species[contribs$Species == "O3_X_LGT"] <- "Lightning"
contribs$Species[contribs$Species == "O3_X_MDEANT"] <- "MDE ANT"
contribs$Species[contribs$Species == "O3_X_MDESOI"] <- "MDE BIO"
contribs$Species[contribs$Species == "O3_X_MDEBMB"] <- "MDE BMB"
contribs$Species[contribs$Species == "O3_X_NAMANT"] <- "NAM ANT"
contribs$Species[contribs$Species == "O3_X_NAMSOI"] <- "NAM BIO"
contribs$Species[contribs$Species == "O3_X_NAMBMB"] <- "NAM BMB"
contribs$Species[contribs$Species == "O3_X_OCNANT"] <- "OCN ANT"
contribs$Species[contribs$Species == "O3_X_OCNSOI"] <- "OCN BIO"
contribs$Species[contribs$Species == "O3_X_OCNBMB"] <- "OCN BMB"
contribs$Species[contribs$Species == "O3_X_RBUANT"] <- "RBU ANT"
contribs$Species[contribs$Species == "O3_X_RBUSOI"] <- "RBU BIO"
contribs$Species[contribs$Species == "O3_X_RBUBMB"] <- "RBU BMB"
contribs$Species[contribs$Species == "O3_X_RSTANT"] <- "RST ANT"
contribs$Species[contribs$Species == "O3_X_RSTSOI"] <- "RST BIO"
contribs$Species[contribs$Species == "O3_X_RSTBMB"] <- "RST BMB"
contribs$Species[contribs$Species == "O3_X_SASANT"] <- "SAS ANT"
contribs$Species[contribs$Species == "O3_X_SASSOI"] <- "SAS BIO"
contribs$Species[contribs$Species == "O3_X_SASBMB"] <- "SAS BMB"
contribs$Species[contribs$Species == "O3_X_STR"] <- "Stratosphere"
contribs$Species[contribs$Species == "O3_X_XTR"] <- "Chemical Source"

contribs$Species <- factor(contribs$Species, levels = c("NAM ANT", "NAM BIO", "NAM BMB", "EUR ANT", "EUR BIO", "EUR BMB", "RBU ANT", "RBU BIO", "RBU BMB", "MDE ANT", "MDE BIO", "MDE BMB", "SAS ANT", "SAS BIO", "SAS BMB", "EAS ANT", "EAS BIO", "EAS BMB", "RST ANT", "RST BIO", "RST BMB", "OCN ANT", "OCN BIO", "OCN BMB", "Stratosphere", "Aircraft", "Lightning", "Initial Conditions", "Chemical Source"))
contribs$Region <- factor(contribs$Region, levels = c("North America", "Europe", "Russia", "Middle East", "South Asia", "East Asia", "Rest", "Ocean"))

discretise.contributions <- function (contribution) {
  value <- as.numeric(sprintf("%.1f", contribution))
  if (value < 0.5) {
    discrete = "<0.5"
  } else if (value >= 0.5 & value < 1) {
    discrete = "0.5 - 1"
  } else if (value >= 1 & value < 2) {
    discrete = "1 - 2"
  } else if (value >= 2 & value < 3) {
    discrete = "2 - 3"
  } else if (value >= 3 & value < 4) {
    discrete = "3 - 4"
  } else if (value >= 4 & value < 5) {
    discrete = "4 - 5"
  } else if (value >= 5 & value < 10) {
    discrete = "5 - 10"
  } else if (value >= 10 & value < 30) {
    discrete = "10 - 30"
  } else if (value >= 30 & value < 50) {
    discrete = "30 - 50"
  } else {
    discrete = ">50"
  }
  return(discrete)
}

discrete.contribs <- contribs %>%
  dplyr::select(-Total, Source = Species) %>%
  rowwise() %>%
  mutate(Discrete = discretise.contributions(Contribution))

discrete.contribs$Discrete <- factor(discrete.contribs$Discrete, levels = c("<0.5", "0.5 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5", "5 - 10", "10 - 30", "30 - 50", ">50"))
discrete.contribs$Month <- factor(discrete.contribs$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

discrete.colours = c("<0.5" = "#6c254f", "0.5 - 1" = "#f9c500", "1 - 2" = "#0e5c28", "2 - 3" = "#ef6638", "3 - 4" = "#0352cb", "4 - 5" = "#b569b3", "5 - 10" = "#2b9eb3", "10 - 30" = "#ba8b01", "30 - 50" = "#ae4901", ">50" = "#000000")

p <- ggplot(discrete.contribs, aes(x = Region, y = Source, fill = Discrete))
p <- p + geom_tile(colour = "white", size = 1)
p <- p + facet_wrap(~ Month, ncol = 4, scales = "free")
p <- p + theme_tufte()
p <- p + theme(strip.text = element_text(face = "bold"))
p <- p + theme(panel.spacing = unit(5, "mm"))
p <- p + scale_x_discrete(expand = c(0, 0))
p <- p + scale_y_discrete(expand = c(0, 0))
p <- p + scale_fill_manual(values = discrete.colours, name = "% Contribution", limits = rev(levels(factor(discrete.contribs$Discrete))))
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.title = element_text(face = "bold"))
p <- p + theme(legend.title = element_text(face = "bold"))
p

CairoPDF(file = "Source_Receptor_Tiles_Monthly.pdf", width = 20, height = 14)
print(p)
dev.off()
