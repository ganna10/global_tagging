setwd("~/Documents//Analysis//2016_HTAP//Test")

# get HTAP regions
regions <- raster("HTAP_regions.nc")
region.rot <- rotate(regions)
region.df <- as.data.frame(rasterToPoints(region.rot))
colnames(region.df) <- c("lon", "lat", "region")

# plot HTAP regions for checking purposes
# count <- length(unique(region.df$region))
# getPalette = colorRampPalette(brewer.pal(9, "Set1"))
# ggplot(region.df, aes(x = lon, y = lat)) + geom_raster(aes(fill = factor(region))) + theme_bw() + scale_fill_manual(values = getPalette(count))

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
file <- "HTAP_NOx_Tagging.cam.h2.2010-01-01-39600.nc"
nc <- nc_open(file)
all.vars <- as.data.frame(names(nc$var))
colnames(all.vars) <- c("Vars")
o3.vars.df <- all.vars %>%
  filter(str_detect(Vars, 'O3'))
o3.vars <- as.vector(o3.vars.df$Vars)
# o3.vars <- c("O3", "O3_X_NAMANT")

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
#     if (str_length(tag) == 6) {
      emission.region = strings[1]
      emission.type = strings[2]
#     } else {
#       emission.region = "NA"
#       emission.type = strings[1]
#     }
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
non.tagged.o3 <- data.df %>%
  filter(Emission.Region == "All") %>%
  select(-Emission.Type)
tagged.o3 <- data.df %>%
  filter(Emission.Region != "All") %>%
  select(-Emission.Type) %>%
  mutate(Emission.Region = "Tagged") %>% 
  group_by(lon, lat) %>%
  mutate(O3 = sum(O3)) %>%
  distinct(lon, lat, .keep_all = TRUE)

# plot diff between tagged and non-tagged
all.df <- tbl_df(rbind(as.data.frame(non.tagged.o3), as.data.frame(tagged.o3)))
diff.df <- all.df %>%
  spread(Emission.Region, O3) %>%
  mutate(Diff = All - Tagged)
ggplot() + geom_raster(data = diff.df, aes(x = lon, y = lat, fill = Diff), hjust = 0, vjust = 0) + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group)) + scale_fill_distiller(palette = "Spectral") + theme_tufte()

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

parts.o3 <- assigned.df %>% 
  filter(Emission.Region != "All") %>%
  group_by(Region, Emission.Region) %>% #, Emission.Type) %>%
  summarise(Total = sum(O3)) %>%
  rowwise() %>%
  mutate(Contribution = zone.contribution(region = Region, total.o3 = Total, zonal.o3 = all.o3.zonal))
parts.o3$Emission.Type[parts.o3$Emission.Type == "SOI"] <- "BIO"

# total tagged O3 of zone
total.tagged.zonal.o3 <- parts.o3 %>% dplyr::select(-Contribution)

ggplot(total.tagged.zonal.o3, aes(x = Region, y = Total, fill = Emission.Region)) + geom_bar(stat = "identity", position = "stack") + facet_wrap( ~ Emission.Type) + theme_tufte()

# contributions of tagged O3 to total in different regions
contributions.tagged.zonal.o3 <- parts.o3 %>% dplyr::select(-Total)  
# contributions.tagged.zonal.o3$Emission.Region <- factor(contributions.tagged.zonal.o3$Emission.Region, levels = c("NAM", "EUR", "RBU", "EAS", "SAS", "MDE", "OCN", "RST", "NA"))
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "AIR"] <- "Aircraft"
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "ANT"] <- "Anthropogenic"
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "BIO"] <- "Biogenic"
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "BMB"] <- "Biomass Burning"
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "INI"] <- "Initial Conditions"
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "LGT"] <- "Lightning"
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "STR"] <- "Stratosphere"
# contributions.tagged.zonal.o3$Emission.Type[contributions.tagged.zonal.o3$Emission.Type == "XTR"] <- "Extra"
# contributions.tagged.zonal.o3$Emission.Type = factor(contributions.tagged.zonal.o3$Emission.Type, levels = c("Anthropogenic", "Biogenic", "Biomass Burning", "Stratosphere", "Aircraft", "Lightning", "Initial Conditions", "Extra"))

my.colours = c("NA" = "#000000", "NAM" = "#6c254f", "EUR" = "#f9c500", "RBU" = "#0e5c28", "EAS" = "#ef6638", "SAS" = "#0352cb", "RST" = "#b569b3", "OCN" = "#2b9eb3", "MDE" = "#ba8b01", "AIR" = "#ae4901", "INI" = "#000000", "LGT" = "#9bb18d", "STR" = "#8c1531", "XTR" = "#86b650")

contributions.tagged.zonal.o3$Emission.Region <- factor(contributions.tagged.zonal.o3$Emission.Region, levels = c("NAM", "EUR", "RBU", "EAS", "SAS", "MDE", "OCN", "RST", "AIR", "STR", "LGT", "INI", "XTR"))

p <- ggplot()
p <- p + geom_bar(data = contributions.tagged.zonal.o3 %>% arrange(Emission.Region), aes(x = Region, y = Contribution, fill = Emission.Region), stat = "identity", position = "stack")
# p <- p + facet_wrap(~ Emission.Type, nrow = 2, scales = "free_x")
p <- p + scale_y_continuous(label = percent, expand = c(0, 0)) #, limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1))
p <- p + scale_x_discrete(expand = c(0, 0))
p <- p + plot_theme()
p <- p + scale_fill_manual(values = my.colours, limits = rev(levels(factor(contributions.tagged.zonal.o3$Emission.Region))), name = "Emission Source\nRegion")
p <- p + theme(panel.margin = unit(5, "mm"))
p <- p + theme(strip.text = element_text(face = "bold"))
p <- p + theme(legend.title = element_text(face = "bold"))
p <- p + xlab("Receptor Region") + ylab("Percent Contribution")
p

# sub categories
contribs <- assigned.df %>%
  filter(Emission.Region != "All") %>%
  group_by(Region, Emission.Region, Emission.Type) %>%
  summarise(Total = sum(O3)) %>%
  rowwise() %>%
  mutate(Contribution = zone.contribution(region = Region, total.o3 = Total, zonal.o3 = all.o3.zonal))
contribs$Emission.Type[contribs$Emission.Type == "SOI"] <- "BIO"
contribs <- contribs %>% dplyr::select(-Total)

test <- contribs %>%
  rowwise() %>%
  mutate(Type = paste(Emission.Region, Emission.Type), Contribution = 100 * Contribution)
test$Emission.Region <- factor(test$Emission.Region, levels = c("NAM", "EUR", "RBU", "EAS", "SAS", "MDE", "OCN", "RST", "AIR", "STR", "LGT", "INI", "XTR"))
test$Emission.Type <- factor(test$Emission.Type, levels = c("ANT", "BIO", "BMB"))
test$Type <- factor(test$Type, levels = c("EAS ANT", "EAS BIO", "EAS BMB", "EUR ANT", "EUR BIO", "EUR BMB", "MDE ANT", "MDE BIO", "MDE BMB", "NAM ANT", "NAM BIO", "NAM BMB", "OCN ANT", "OCN BIO", "OCN BMB", "RST ANT", "RST BIO", "RST BMB", "RBU ANT", "RBU BIO", "RBU BMB", "SAS ANT", "SAS BIO", "SAS BMB", "AIR NA", "INI NA", "STR NA", "XTR NA", "LGT NA"))

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

new.test <- test %>%
  rowwise() %>%
  mutate(Discrete = discretise.contributions(Contribution))
new.test$Discrete <- factor(new.test$Discrete, levels = c("<0.5", "0.5 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5", "5 - 10", "10 - 30", "30 - 50", ">50"))

discrete.colours = c("<0.5" = "#6c254f", "0.5 - 1" = "#f9c500", "1 - 2" = "#0e5c28", "2 - 3" = "#ef6638", "3 - 4" = "#0352cb", "4 - 5" = "#b569b3", "5 - 10" = "#2b9eb3", "10 - 30" = "#ba8b01", "30 - 50" = "#ae4901", ">50" = "#000000")

p <- ggplot()
p <- p + geom_tile(data = new.test, aes(x = Region, y = Type, fill = Discrete), colour = "white", size = 1)
p <- p + scale_y_discrete(expand = c(0, 0))
p <- p + scale_x_discrete(expand = c(0, 0))
p <- p + plot_theme()
p <- p + theme(legend.title = element_text(face = "bold"))
p <- p + xlab("Receptor Region") + ylab("Source Region and Type") + labs(fill = "% Contribution")
p <- p + scale_fill_manual(values = discrete.colours, limits = rev(levels(factor(new.test$Discrete))))
p

# namant.cont <- namant.split %>%
#   mutate(Region = assign_region(lon, lat, region.df))
#   rowwise() %>%
#   mutate(Contribution = contribution(lon, lat, O3, total.o3 = o3.points))

# plot
p <- ggplot()
p <- p + geom_raster(data = data.df, aes(x = lon, y = lat, fill = O3), hjust = 1, vjust = 1)
p <- p + facet_grid(Region ~ Emission)
p <- p + geom_path(data = world.map.correct, aes(x = long, y = lat, group = group))
p <- p + coord_fixed(1:1.5)
p <- p + theme_tufte()
p <- p + scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30), expand = c(0, 0))
p <- p + scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30), expand = c(0, 0))
p <- p + scale_fill_distiller(palette = "Spectral") #, breaks = seq(0, 100, 20), limits = c(0, 100))
p <- p + xlab("Longitude") + ylab("Latitude") + labs(fill = "O3 (ppbv)")
p <- p + theme(legend.title = element_text(face = "bold"))
p <- p + theme(strip.text = element_text(face = "bold"))
p 

# zonal contribution
# o3.zone.sum <- o3.split %>%
#   group_by(Region) %>%
#   summarise(Total = sum(O3))
# 
# zone.cont.namant <- namant.split %>%
#   group_by(Region) %>%
#   summarise(Total = sum(O3)) %>%
#   rowwise() %>%
#   mutate(Contribution = zone.contribution(region = Region, total.o3 = Total, zonal.o3 = o3.zone.sum))
# 
# plot <- ggplot(data = zone.cont.namant, aes(x = Region, y = Contribution))
# plot <- plot + geom_bar(stat = "identity")
# plot <- plot + theme_tufte()
# plot <- plot + scale_y_continuous(label = percent, expand = c(0, 0))
# plot <- plot + scale_x_discrete(expand = c(0, 0))
# plot
# 
# split.chars <- strsplit("namant", split = "")[[1]] # string into separate characters
# strings <- paste0(split.chars[c(TRUE, FALSE, FALSE)], split.chars[c(FALSE, TRUE, FALSE)], split.chars[c(FALSE, FALSE, TRUE)])
# region <- strings[1]
# emission <- strings[2]
