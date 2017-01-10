require('ggplot2', 'raster', 'dplyr', 'ncdf4', 'Cairo')

# get HTAP regions
regions <- raster("../HTAP_regions.nc")
region.rot <- rotate(regions)
region.df <- as.data.frame(rasterToPoints(region.rot))
colnames(region.df) <- c("lon", "lat", "region")

# plot HTAP regions for checking purposes
# count <- length(unique(region.df$region))
# getPalette = colorRampPalette(brewer.pal(9, "Set1"))
# ggplot(region.df, aes(x = lon, y = lat)) + geom_raster(aes(fill = factor(region))) + theme_bw() + scale_fill_manual(values = getPalette(count))

assign_region <- function (longitude, latitude, regions.df) { # assign HTAP regions
  pinpoint <- regions.df %>%
    filter(lon == longitude, lat == latitude)
  region.code <- pinpoint$region
  if (region.code == 2) {
    region <- "Ocean"
  } else if (region.code == 3) {
    region <- "North America"    
  } else if (region.code == 4) {
    region <- "Europe"
  } else if (region.code == 5) {
    region <- "South Asia"
  } else if (region.code == 6) {
    region <- "East Asia"
  } else if (region.code == 11) {
    region <- "Middle East"
  } else if (region.code == 14) {
    region <- "Russia"
  } else {
    region <- "Rest"
  }
  return(region)
}

contribution <- function (longitude, latitude, o3, total.o3) { # calculate contributions
  tot.o3 <- total.o3 %>%
    filter(lon == longitude, lat == latitude)
  cont = o3 / tot.o3$O3 * 100
  return(cont)
}

# get all o3 variables from nc file for looping
file <- "../Test/HTAP_NOx_Tagging.cam.h2.2010-01-01-39600.nc"
nc <- nc_open(file)
all.vars <- as.data.frame(names(nc$var))
colnames(all.vars) <- c("Vars")
o3.vars.df <- all.vars %>%
  filter(str_detect(Vars, 'O3'))
o3.vars <- as.vector(o3.vars.df$Vars)

get.data <- function (variable, file.name) {
  var.raster <- raster(file.name, varname = variable, lev = 56)
  var.rotated.raster <- rotate(var.raster)
  var.raster.ppbv <- var.rotated.raster * 1e9
  
  var.df <- as.data.frame(rasterToPoints(var.raster.ppbv))
  colnames(var.df) <- c("lon", "lat", "O3")
  
  if (variable == "O3") {
    emission.region = "All"
    emission.type = "All"
  } else if (str_detect(variable, '_X_')) {
    tag <- str_sub(variable, start = 6, end = str_length(variable))
    split.tag <- strsplit(tag, split = "")[[1]]
    strings <- paste0(split.tag[c(TRUE, FALSE, FALSE)], split.tag[c(FALSE, TRUE, FALSE)], split.tag[c(FALSE, FALSE, TRUE)])
    emission.region = strings[1]
    emission.type = strings[2]
  } else {
    emission.region = "ERROR"
    emission.type = "ERROR"
  }
  var.df$Emission.Region = rep(emission.region, length(var.df$lon))
  var.df$Emission.Type = rep(emission.type, length(var.df$lon))
  return(var.df)
}

data.list <- lapply(o3.vars, FUN = get.data, file.name = file)
data.df <- tbl_df(do.call("rbind", data.list))

# real - tagged
# non.tagged.o3 <- data.df %>%
#   filter(Emission.Region == "All") %>%
#   select(-Emission.Type)
# tagged.o3 <- data.df %>%
#   filter(Emission.Region != "All") %>%
#   select(-Emission.Type) %>%
#   mutate(Emission.Region = "Tagged") %>% 
#   group_by(lon, lat) %>%
#   mutate(O3 = sum(O3)) %>%
#   distinct(lon, lat, .keep_all = TRUE)

# plot diff between tagged and non-tagged
# all.df <- tbl_df(rbind(as.data.frame(non.tagged.o3), as.data.frame(tagged.o3)))
# diff.df <- all.df %>%
#   spread(Emission.Region, O3) %>%
#   mutate(Diff = All - Tagged)
# ggplot() + geom_raster(data = diff.df, aes(x = lon, y = lat, fill = Diff), hjust = 0, vjust = 0) + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group)) + scale_fill_distiller(palette = "Spectral") + theme_tufte()

# assign to HTAP regions
assigned.df <- data.df %>%
  rowwise() %>%
  mutate(Region = assign_region(lon, lat, region.df))

# calculate zonal sums with contribution to total
zone.contribution <- function (region, total.o3, zonal.o3) { # calculate contributions
  zone.o3 <- zonal.o3 %>%
    filter(Region == region)
  cont = total.o3 / zone.o3$Total
  return(cont)
}

all.o3.zonal <- assigned.df %>%
  filter(Emission.Region == "All") %>%
  group_by(Region) %>%
  summarise(Total = sum(O3))

# sub categories
contribs <- assigned.df %>%
  filter(Emission.Region != "All") %>%
  group_by(Region, Emission.Region, Emission.Type) %>%
  summarise(Total = sum(O3)) %>%
  rowwise() %>%
  mutate(Contribution = zone.contribution(region = Region, total.o3 = Total, zonal.o3 = all.o3.zonal))
contribs$Emission.Type[contribs$Emission.Type == "SOI"] <- "BIO"
contribs <- contribs %>% dplyr::select(-Total)

tiles <- contribs %>%
  rowwise() %>%
  mutate(Type = paste(Emission.Region, Emission.Type), Contribution = 100 * Contribution)
tiles$Emission.Region <- factor(tiles$Emission.Region, levels = c("NAM", "EUR", "RBU", "EAS", "SAS", "MDE", "OCN", "RST", "AIR", "STR", "LGT", "INI", "XTR"))
tiles$Emission.Type <- factor(tiles$Emission.Type, levels = c("ANT", "BIO", "BMB"))
tiles$Type <- factor(tiles$Type, levels = c("EAS ANT", "EAS BIO", "EAS BMB", "EUR ANT", "EUR BIO", "EUR BMB", "MDE ANT", "MDE BIO", "MDE BMB", "NAM ANT", "NAM BIO", "NAM BMB", "OCN ANT", "OCN BIO", "OCN BMB", "RST ANT", "RST BIO", "RST BMB", "RBU ANT", "RBU BIO", "RBU BMB", "SAS ANT", "SAS BIO", "SAS BMB", "AIR NA", "INI NA", "STR NA", "XTR NA", "LGT NA"))

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

new.tiles <- tiles %>%
  rowwise() %>%
  mutate(Discrete = discretise.contributions(Contribution))
new.tiles$Discrete <- factor(new.tiles$Discrete, levels = c("<0.5", "0.5 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5", "5 - 10", "10 - 30", "30 - 50", ">50"))

discrete.colours = c("<0.5" = "#6c254f", "0.5 - 1" = "#f9c500", "1 - 2" = "#0e5c28", "2 - 3" = "#ef6638", "3 - 4" = "#0352cb", "4 - 5" = "#b569b3", "5 - 10" = "#2b9eb3", "10 - 30" = "#ba8b01", "30 - 50" = "#ae4901", ">50" = "#000000")

p <- ggplot()
p <- p + geom_tile(data = new.tiles, aes(x = Region, y = Type, fill = Discrete), colour = "white", size = 1)
p <- p + scale_y_discrete(expand = c(0, 0))
p <- p + scale_x_discrete(expand = c(0, 0))
p <- p + theme_tufte()
p <- p + theme(legend.title = element_text(face = "bold"))
p <- p + theme(axis.ticks = element_blank())
p <- p + theme(axis.title = element_text(face = "bold"))
p <- p + xlab("Receptor Region") + ylab("Source Region and Type") + labs(fill = "% Contribution")
p <- p + scale_fill_manual(values = discrete.colours, limits = rev(levels(factor(new.tiles$Discrete))))

CairoPNG(file = "Tiled_contributions.png", width = 1000, height = 700)
print(p)
dev.off()
