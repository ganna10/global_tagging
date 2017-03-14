setwd("~/Documents//Analysis//2016_HTAP//Test")

get.data <- function (month) {
  month.text <- month.abb[month]
  file.name <- paste0("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/Mean/Data/HTAP_NOx_Tagging_", month.text, "_assigned_to_Tier2_regions.csv")
  data.df <- read.csv(file = file.name, header = TRUE)
  return(data.df)
}

# assign to total, local, transported, natural, other
get.source <- function (species, Region) {
  # convert Region to tag format for matching
  if (str_detect(Region, "Greece") | str_detect(Region, "Europe")) {
    match.Region = "EUR"
  } else if (str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan")) {
    match.Region = "EAS"
  } else if (str_detect(Region, "India") & !str_detect(Region, "Ocean")) {
    match.Region = "SAS"
  } else if (str_detect(Region, "Lebanon") | str_detect(Region, "Yemen") | str_detect(Region, "Iraq")) {
    match.Region = "MDE"
  } else if (str_detect(Region, " US") | str_detect(Region, "Canada")) {
    match.Region = "NAM"
  } else if (str_detect(Region, "Atlantic") | str_detect(Region, " Sea") | str_detect(Region, "Pacific") | str_detect(Region, "Hudson") | str_detect(Region, "Ocean")) {
    match.Region = "OCN"
  } else if (Region == "Rest") {
    match.Region = "RST"
  } else if (str_detect(Region, "Russia") | str_detect(Region, "Ukraine")) {
    match.Region = "RBU"
  }
  
  if (species == "O3") { # total ozone
    source.emiss = "Total"
  } else if (str_detect(species, 'STR')) { # stratosphere
    source.emiss = "Stratosphere"
  } else if (str_detect(species, 'LGT') | str_detect(species, 'INI') | str_detect(species, 'AIR') | str_detect(species, 'XTR')) { # other sources: aircraft, lightning, initial conditions and chemical source
    source.emiss = "Other"
  } else if (str_detect(species, match.Region)) { # check for local or transported emissions
    #source.emiss = "Local"
    source.emiss = get.emission.source(species)
  } else if (!str_detect(species, match.Region)) { # check for local or transported emissions
    source.emiss = get.emission.source(species)
  } else {
    source.emiss = "No Match"
  }
  return (source.emiss)
}

# transport from which regions influence local ozone
get.emission.source <- function (Species) {
  if (str_detect(Species, 'EAS')) {
    Source = "East.Asia" 
  } else if (str_detect(Species, 'EUR')) {
    Source = "Europe"
  } else if (str_detect(Species, 'NAM')) {
    Source = "North.America"
  } else if (str_detect(Species, 'MDE')) {
    Source = "Middle.East"
  } else if (str_detect(Species, 'RBU')) {
    Source = "Russia"
  } else if (str_detect(Species, 'SAS')) {
    Source = "South.Asia"
  } else if (str_detect(Species, 'OCN')) {
    Source = "Ocean"
  } else if (str_detect(Species, 'RST')) {
    Source = "Rest"
  } else if (str_detect(Species, 'MCA')) {
    Source = "Mexico.Central.America"
  } else if (str_detect(Species, 'NAF')) {
    Source = "North.Africa"
  } else if (str_detect(Species, 'SEA')) {
    Source = "South.East.Asia"
  } else {
    Source = "Error"
  }
  return (Source)
}

months <- seq(1, 12, by = 1)
data.list <- lapply(months, get.data)
data.df <- tbl_df(bind_rows(data.list))

# calculate zonal means
zonal.mean <- data.df %>%
  mutate(Mixing.Ratio = Mixing.Ratio * 1e9) %>%
  group_by(Species, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio)) 
tbl_df(zonal.mean)
zonal.mean$Month <- factor(zonal.mean$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

with.sources <- zonal.mean %>%
  filter(!Region %in% c("Indian Ocean", "Rest", "South Pacific", "North Pacific", "South Atlantic", "North Atlantic")) %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
with.sources$Source <- factor(with.sources$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
with.sources$Region <- factor(with.sources$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska", "Hudson Bay", "NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus", "Mediterranean Sea", "Baltic Sea", "N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya", "NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya", "Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq", "W Russia", "E Russia", "Belarus; Ukraine", "Black and Caspian Sea"))

### annual data - absolute
annual.data <- with.sources %>%
  filter(Source != "Total") %>%
  group_by(Region, Source) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean))

p <- ggplot(data = annual.data, aes(x = Source, y = Region, fill = Zonal.Mean))
p <- p + geom_tile(colour = "white", size = 1)
p <- p + ylab("Receptor Region") + xlab("Source Region") + ggtitle("Annual Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
p <- p + scale_x_discrete(expand = c(0, 0), labels = c("North America", "Europe", "South Asia", "East Asia", "Middle East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
p <- p + scale_y_discrete(expand = c(0, 0))
p <- p + geom_hline(yintercept = 7.5, size = 0.5) # NAM
p <- p + geom_hline(yintercept = 13.5, size = 0.5) # EUR
p <- p + geom_hline(yintercept = 17.5, size = 0.5) # SAS
p <- p + geom_hline(yintercept = 23.5, size = 0.5) # EAS
p <- p + geom_hline(yintercept = 26.5, size = 0.5) # MDE
p <- p + geom_hline(yintercept = 30.5, size = 0.5) # RBU
p <- p + geom_vline(xintercept = seq(1.5, 9.5, by = 1), size = 0.5)
p <- p + theme_tufte()
p <- p + theme(panel.border = element_blank())
p <- p + theme(axis.text = element_text(colour = "black"))
p <- p + theme(axis.title = element_text(colour = "black", face = "bold"))
p <- p + theme(plot.title = element_text(face = "bold"))
p <- p + theme(legend.title = element_text(face = "bold"))
p <- p + guides(fill = guide_colourbar(barheight = 25))
p <- p + scale_fill_gradientn(name = "O3 (ppbv)", limits = c(0, 30), colours = rev(c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#3288bd')), values = rescale(c(0, 1, 2, 5, 10, 15, 20, 30)), breaks = c(0, 1, 2, 5, 10, 15, 20, 30))

CairoSVG(file = "Annual_Mean_O3_Tiles.svg", width = 14, height = 10)
print(p)
dev.off()

### annual data - % contribution
annual.contributions <- annual.data %>%
  mutate(Total = sum(Zonal.Mean), Percent.Contribution = Zonal.Mean * 100 / Total) %>%
  dplyr::select(Receptor.Region = Region, Source, Percent.Contribution, -Total)
annual.contributions

p2 <- ggplot(data = annual.contributions, aes(x = Source, y = Receptor.Region, fill = Percent.Contribution))
p2 <- p2 + geom_tile(colour = "white", size = 1)
p2 <- p2 + ylab("Receptor Region") + xlab("Source Region") + ggtitle("Percent Contributions to Annual Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
p2 <- p2 + scale_x_discrete(expand = c(0, 0), labels = c("North America", "Europe", "South Asia", "East Asia", "Middle East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
p2 <- p2 + scale_y_discrete(expand = c(0, 0))
p2 <- p2 + geom_hline(yintercept = 7.5, size = 0.5) # NAM
p2 <- p2 + geom_hline(yintercept = 13.5, size = 0.5) # EUR
p2 <- p2 + geom_hline(yintercept = 17.5, size = 0.5) # SAS
p2 <- p2 + geom_hline(yintercept = 23.5, size = 0.5) # EAS
p2 <- p2 + geom_hline(yintercept = 26.5, size = 0.5) # MDE
p2 <- p2 + geom_hline(yintercept = 30.5, size = 0.5) # RBU
p2 <- p2 + geom_vline(xintercept = seq(1.5, 9.5, by = 1), size = 0.5)
p2 <- p2 + theme_tufte()
p2 <- p2 + theme(panel.border = element_blank())
p2 <- p2 + theme(axis.text = element_text(colour = "black"))
p2 <- p2 + theme(axis.title = element_text(colour = "black", face = "bold"))
p2 <- p2 + theme(plot.title = element_text(face = "bold"))
p2 <- p2 + theme(legend.title = element_text(face = "bold"))
p2 <- p2 + guides(fill = guide_colourbar(barheight = 25))
p2 <- p2 + scale_fill_gradientn(name = "% Contribution", limits = c(0, 70), colours = rev(c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#3288bd')), values = rescale(c(0, 1, 2, 5, 10, 15, 30, 50, 70)), breaks = c(0, 1, 2, 5, 10, 15, 30, 50, 70))

CairoSVG(file = "Annual_Percent_Contributions_Mean_O3_Tiles.svg", width = 14, height = 10)
print(p2)
dev.off()

### seasonal data
get.season <- function (month) {
  if (month %in% c("Dec", "Jan", "Feb")) {
    season = "DJF"
  } else if (month %in% c("Apr", "Mar", "May")) {
    season = "MAM"
  } else if (month %in% c("Jul", "Jun", "Aug")) {
    season = "JJA"
  } else if (month %in% c("Oct", "Sep", "Nov")) {
    season = "SON"
  }
  return (season)
}

seasonal.data <- with.sources %>%
  filter(Source != "Total") %>%
  rowwise() %>%
  mutate(Season = get.season(Month)) %>%
  dplyr::select(-Month) %>%
  group_by(Region, Source, Season) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean))
seasonal.data

seasonal.plot <- function (df, season) {
  plot.df <- df %>% filter(Season == season)
  plot.title <- paste0(season, " Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
  
  p1 <- ggplot(data = plot.df, aes(x = Source, y = Region, fill = Zonal.Mean))
  p1 <- p1 + geom_tile(colour = "white", size = 1)
  p1 <- p1 + ylab("Receptor Region") + xlab("Source Region") + ggtitle(plot.title)
  p1 <- p1 + scale_x_discrete(expand = c(0, 0), labels = c("North America", "Europe", "South Asia", "East Asia", "Middle East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
  p1 <- p1 + scale_y_discrete(expand = c(0, 0))
  p1 <- p1 + geom_hline(yintercept = 7.5, size = 0.5) # NAM
  p1 <- p1 + geom_hline(yintercept = 13.5, size = 0.5) # EUR
  p1 <- p1 + geom_hline(yintercept = 17.5, size = 0.5) # SAS
  p1 <- p1 + geom_hline(yintercept = 23.5, size = 0.5) # EAS
  p1 <- p1 + geom_hline(yintercept = 26.5, size = 0.5) # MDE
  p1 <- p1 + geom_hline(yintercept = 30.5, size = 0.5) # RBU
  p1 <- p1 + geom_vline(xintercept = seq(1.5, 9.5, by = 1), size = 0.5)
  p1 <- p1 + theme_tufte()
  p1 <- p1 + theme(panel.border = element_blank())
  p1 <- p1 + theme(axis.text = element_text(colour = "black"))
  p1 <- p1 + theme(axis.title = element_text(colour = "black", face = "bold"))
  p1 <- p1 + theme(plot.title = element_text(face = "bold"))
  p1 <- p1 + theme(legend.title = element_text(face = "bold"))
  p1 <- p1 + theme(strip.text = element_text(face = "bold"))
  p1 <- p1 + guides(fill = guide_colourbar(barheight = 25))
  p1 <- p1 + scale_fill_gradientn(name = "O3 (ppbv)", limits = c(0, 60), colours = rev(c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#3288bd')), values = rescale(c(0, 1, 2, 5, 10, 20, 40, 60)), breaks = c(0, 1, 2, 5, 10, 20, 40, 60))
  
  file.name = paste0(season, "_mean_O3_Tiles.svg")
  CairoSVG(file = file.name, width = 14, height = 10)
  print(p1)
  dev.off()
}
seasons <- levels(factor(seasonal.data$Season))
lapply(seasons, seasonal.plot, df = seasonal.data)

### seasonal data - % contribution
seasonal.contributions <- seasonal.data %>%
  spread(Source, Zonal.Mean) %>%
  mutate(Total = North.America + Europe + South.Asia + East.Asia + Middle.East + Russia + Rest + Ocean + Stratosphere + Other) %>%
  gather(Source, Zonal.Mean, -Region, -Season, -Total) %>%
  mutate(Percent.Contribution = Zonal.Mean * 100 / Total) %>%
  dplyr::select(Receptor.Region = Region, Source, Percent.Contribution, Season, -Total)

seasonal.contributions$Source <- factor(seasonal.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))

seasonal.contribution.plot <- function (df, season) {
  plot.df <- df %>% filter(Season == season)
  plot.title <- paste0(season, " Pecent Contributions to Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
  
  p1 <- ggplot(data = plot.df, aes(x = Source, y = Receptor.Region, fill = Percent.Contribution))
  p1 <- p1 + geom_tile(colour = "white", size = 1)
  p1 <- p1 + ylab("Receptor Region") + xlab("Source Region") + ggtitle(plot.title)
  p1 <- p1 + scale_x_discrete(expand = c(0, 0), labels = c("North America", "Europe", "South Asia", "East Asia", "Middle East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
  p1 <- p1 + scale_y_discrete(expand = c(0, 0))
  p1 <- p1 + geom_hline(yintercept = 7.5, size = 0.5) # NAM 
  p1 <- p1 + geom_hline(yintercept = 13.5, size = 0.5) # EUR
  p1 <- p1 + geom_hline(yintercept = 17.5, size = 0.5) # SAS
  p1 <- p1 + geom_hline(yintercept = 23.5, size = 0.5) # EAS
  p1 <- p1 + geom_hline(yintercept = 26.5, size = 0.5) # MDE
  p1 <- p1 + geom_hline(yintercept = 30.5, size = 0.5) # RBU
  p1 <- p1 + geom_vline(xintercept = seq(1.5, 9.5, by = 1), size = 0.5)
  p1 <- p1 + theme_tufte()
  p1 <- p1 + theme(panel.border = element_blank())
  p1 <- p1 + theme(axis.text = element_text(colour = "black"))
  p1 <- p1 + theme(axis.title = element_text(colour = "black", face = "bold"))
  p1 <- p1 + theme(plot.title = element_text(face = "bold"))
  p1 <- p1 + theme(legend.title = element_text(face = "bold"))
  p1 <- p1 + theme(strip.text = element_text(face = "bold"))
  p1 <- p1 + guides(fill = guide_colourbar(barheight = 25))
  p1 <- p1 + scale_fill_gradientn(name = "% Contribution", limits = c(0, 85), colours = rev(c('#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#3288bd')), values = rescale(c(0, 1, 2, 5, 10, 15, 30, 60, 85)), breaks = c(0, 1, 2, 5, 10, 15, 30, 60, 85))
  
  file.name = paste0(season, "_Percent_Contribution_mean_O3_Tiles.svg")
  CairoSVG(file = file.name, width = 14, height = 10)
  print(p1)
  dev.off()
}

lapply(seasons, seasonal.contribution.plot, df = seasonal.contributions)
