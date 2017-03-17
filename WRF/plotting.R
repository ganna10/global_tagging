setwd("~/Documents//Analysis//2016_HTAP//WRF")

library("ggplot2")
library("dplyr")
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

get.data <- function (month) {
  month.text <- month.abb[month]
  file.name <- paste0("HTAP_AOTD_", month.text, "_assigned_to_Tier2_regions.csv")
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
    source.emiss = "Local"
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
  #mutate(Mixing.Ratio = Mixing.Ratio * 1e9) %>% 
  group_by(Species, Month, Region) %>% 
  summarise(Zonal.Mean = mean(Mixing.Ratio)) 
tbl_df(zonal.mean)
zonal.mean$Month <- factor(zonal.mean$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

with.sources <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  filter(Source != "Total") %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
with.sources$Source <- factor(with.sources$Source, levels = c("Local", "North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
tbl_df(with.sources)

metric.data <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region), Metric = "AOT40-D") %>%
  filter(Source == "Total")
tbl_df(metric.data)

p <- ggplot(metric.data %>% filter(Region == "NW Europe"), aes(x = Month, y = Zonal.Mean, colour = Source, group = Source))
p <- p + geom_point()
p <- p + geom_line()
p

write.csv(metric.data, file = "AOT40D_O3_data.csv", row.names = FALSE, quote = FALSE)

plotting <- function (region, data.frame) {
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
    plot.title <- "South Asia: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"  
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
    plot.title <- "North America: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
    plot.title <- "Europe: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
    plot.title <- "East Asia: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Middle East: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Russia, Belarus, Ukraine: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    legend.levels <- c("Other", "Stratosphere", "Rest", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Oceans: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Rest of the World: Monthly AOT40-D O3 for Emissions and Meteorology for Year 2010"
  } else {
    stop("No region")
  }

    colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "Other" = "#b569b3", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#fdbf6f", "South.East.Asia" = "#fb9a99", "North.Africa" = "#b15928", "Mexico.Central.America" = "#33a02c")

    y.max <- sum(df$Zonal.Mean)

    p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Month, y = Zonal.Mean, fill = Source))
    p <- p + geom_bar(stat = "identity", width = 0.9)
    p <- p + facet_wrap(~ Region, nrow = 1)
    p <- p + scale_y_continuous(expand = c(0, 0), breaks = seq(0, y.max, by = 500))
    p <- p + scale_x_discrete(expand = c(0, 0), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
    p <- p + ylab("AOT40-D (ppbv.hrs)") + ggtitle(plot.title)
    p <- p + theme_bw()
    p <- p + theme(panel.border = element_blank())
    p <- p + theme(panel.grid = element_blank())
    p <- p + theme(legend.key = element_blank())
    p <- p + theme(axis.title.x = element_blank())
    p <- p + theme(legend.title = element_blank())
    p <- p + theme(strip.background = element_blank())
    p <- p + theme(strip.text = element_text(face = "bold"))
    p <- p + theme(plot.title = element_text(face = "bold"))
    p <- p + theme(axis.title.y = element_text(face = "bold"))
    p <- p + theme(axis.line.x = element_line(colour = "black"))
    p <- p + theme(axis.line.y = element_line(colour = "black"))
    p <- p + scale_fill_manual(values = colours, labels = rev(legend.levels))

    file.name <- paste0(region, "_AOT40D_O3_Contributions.pdf")
    CairoPDF(file = file.name, width = 14, height = 7)
    print(p)
    dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = with.sources)
