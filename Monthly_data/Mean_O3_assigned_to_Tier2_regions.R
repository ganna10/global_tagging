setwd("~/Documents//Analysis//2016_HTAP//Monthly_data")

all.data <- tbl_df(read.csv(file = "NOx_Monthly_assigned_Mean_to_Tier2_regions.csv"))

# calculate zonal means
zonal.mean <- all.data %>%
  mutate(Mixing.Ratio = Mixing.Ratio * 1e9) %>%
  group_by(Species, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio)) 
tbl_df(zonal.mean)

zonal.mean$Month <- factor(zonal.mean$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

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
    source.emiss = "Transported"
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
  } else {
    Source = "Error"
  }
  return (Source)
}

plotting <- function (region, data.frame) {
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    plot.levels <- c("Rest", "Ocean", "East.Asia", "Russia", "Middle.East", "Europe", "North.America")
    legend.levels <- c("Rest", "Ocean", "East Asia", "Russia", "Middle East", "Europe", "North America")
    plot.title <- "South Asia: Monthly Average O3 for Emissions and Meteorology for Year 2010"    
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    plot.levels <- c("Rest", "Ocean", "East.Asia", "South.Asia",  "Russia", "Middle.East", "Europe")
    legend.levels <- c("Rest", "Ocean", "East Asia",  "South Asia", "Russia", "Middle East", "Europe")
    plot.title <- "North America: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    plot.levels <- c("Rest", "Ocean", "East.Asia", "South.Asia",  "Russia", "Middle.East", "North.America")
    legend.levels <- c("Rest", "Ocean", "East Asia",  "South Asia", "Russia", "Middle East", "North America")
    plot.title <- "Europe: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    plot.levels <- c("Rest", "Ocean", "South.Asia", "Russia", "Middle.East", "Europe", "North.America")
    legend.levels <- c("Rest", "Ocean", "South Asia", "Russia", "Middle East", "Europe", "North America")
    plot.title <- "East Asia: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    plot.levels <- c("Rest", "Ocean", "East.Asia", "South.Asia",  "Russia", "Europe", "North.America")
    legend.levels <- c("Rest", "Ocean", "East Asia",  "South Asia", "Russia", "Europe", "North America")
    plot.title <- "Middle East: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    plot.levels <- c("Rest", "Ocean", "East.Asia", "South.Asia",  "Middle.East", "Europe", "North.America")
    legend.levels <- c("Rest", "Ocean", "East Asia",  "South Asia", "Middle East", "Europe", "North America")
    plot.title <- "Russia, Belarus, Ukraine: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    plot.levels <- c("Rest", "East.Asia", "South.Asia",  "Russia", "Middle.East", "Europe", "North.America")
    legend.levels <- c("Rest", "East Asia",  "South Asia", "Russia", "Middle East", "Europe", "North America")
    plot.title <- "Oceans: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    plot.levels <- c("Ocean", "East.Asia", "South.Asia",  "Russia", "Middle.East", "Europe", "North.America")
    legend.levels <- c("Ocean", "East Asia",  "South Asia", "Russia", "Middle East", "Europe", "North America")
    plot.title <- "Rest of the World: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else {
    stop("No region")
  }
  
  with.sources <- df %>%
    rowwise() %>%
    mutate(Source = get.source(Species, Region)) %>%
    group_by(Month, Region, Source) %>%
    summarise(Zonal.Mean = sum(Zonal.Mean))
  with.sources$Source <- factor(with.sources$Source, levels = c("Total", "Local", "Transported", "Stratosphere", "Other"))
  colours <- c("Total" = "#000000", "Local" = "#377eb8", "Stratosphere" = "#4daf4a", "Transported" = "#984ea3", "Other" = "#ff7f00")
  
  p <- ggplot(data = with.sources, aes(x = Month, y = Zonal.Mean, colour = Source, group = Source))
  p <- p + geom_point()
  p <- p + geom_path()
  p <- p + facet_wrap(~ Region, nrow = 1)
  p <- p + plot_theme()
  p <- p + ylab("O3 Mean (ppbv)")
  p <- p + theme(axis.title = element_blank())
  p <- p + ggtitle(plot.title, subtitle = "\nTotal Ozone Mean (ppbv) and Mean from NOx Sources in Tier 2 Receptor Regions")
  p <- p + scale_colour_manual(values = colours, limits = levels(factor(with.sources$Source)))
  p <- p + theme(legend.title = element_blank())
  p <- p + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p <- p + theme(plot.subtitle = element_text(face = "bold", size = 12))
  
  # contributions to total
  contributions <- with.sources %>%
    spread(Source, Zonal.Mean) %>%
    gather(Source, Mixing.Ratio, -Month, -Region, -Total) %>%
    mutate(Contribution = Mixing.Ratio / Total) 
  contributions$Source <- factor(contributions$Source, levels = c("Other", "Stratosphere", "Transported", "Local"))
  
  p1 <- ggplot(data = contributions %>% arrange(Source), aes(x = Month, y = Contribution, fill = Source))
  p1 <- p1 + geom_bar(stat = "identity")
  p1 <- p1 + facet_wrap(~ Region, nrow = 1)
  p1 <- p1 + plot_theme()
  p1 <- p1 + scale_y_continuous(limits = c(0, 1), label = percent, expand = c(0, 0))
  p1 <- p1 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), expand = c(0, 0))
  p1 <- p1 + scale_fill_manual(values = colours, limits = levels(factor(contributions$Source)))
  p1 <- p1 + ylab("Percent Contribution to Total O3")
  p1 <- p1 + theme(axis.title = element_blank())
  p1 <- p1 + ggtitle("Percent Contributions of NOx Sources to Total Ozone in Tier 2 Receptor Regions")
  p1 <- p1 + theme(legend.title = element_blank())
  p1 <- p1 + theme(plot.title = element_text(size = 12))
  
  transport <- df %>%
    rowwise() %>%
    mutate(Source = get.source(Species, Region)) %>%
    filter(Source == "Transported") %>%
    dplyr::select(-Source) %>%
    rowwise() %>%
    mutate(Source = get.emission.source(Species)) %>%
    group_by(Month, Region, Source) %>%
    summarise(Mixing.Ratio = sum(Zonal.Mean)) %>%
    spread(Source, Mixing.Ratio) %>%
    ungroup() %>%
    mutate(Total = rowSums(.[3:9])) %>%
    gather(Emission.Source, Mean, -Month, -Region, -Total) %>%
    mutate(Contribution = Mean / Total) %>%
    dplyr::select(Month, Region, Emission.Source, Contribution, Mean)
  transport$Emission.Source <- factor(transport$Emission.Source, levels = plot.levels)
  
  country.colours = c("Total" = "#000000", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#33a02c")
  
  p2 <- ggplot(data = transport, aes(x = Month, y = Mean, colour = Emission.Source, group = Emission.Source))
  p2 <- p2 + geom_point()
  p2 <- p2 + geom_line()
  p2 <- p2 + facet_wrap(~ Region, nrow = 1)
  p2 <- p2 + plot_theme()
  p2 <- p2 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p2 <- p2 + scale_colour_manual(values = country.colours, labels = legend.levels)
  p2 <- p2 + theme(axis.title = element_blank())
  p2 <- p2 + ggtitle("Monthly Mean O3 of Transported NOx Sources from Source Regions to Tier 2 Receptor Regions")
  p2 <- p2 + theme(legend.title = element_blank())
  p2 <- p2 + theme(plot.title = element_text(size = 12))
  
  p5 <- ggplot(data = transport, aes(x = Month, y = Contribution, fill = Emission.Source))
  p5 <- p5 + geom_bar(stat = "identity")
  p5 <- p5 + facet_wrap( ~ Region, nrow = 1)
  p5 <- p5 + plot_theme()
  p5 <- p5 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), expand = c(0, 0))
  p5 <- p5 + scale_fill_manual(values = country.colours, labels = legend.levels)
  p5 <- p5 + scale_y_continuous(label = percent, expand = c(0, 0))
  p5 <- p5 + theme(axis.title = element_blank())
  p5 <- p5 + ggtitle("Percent Contributions of Transported NOx Sources from Source Regions to Tier 2 Receptor Regions")
  p5 <- p5 + theme(legend.title = element_blank())
  p5 <- p5 + theme(plot.title = element_text(size = 12))
  
  file.name <- paste0(region, "_Mean_O3_Yearly_Cycle_Total_plus_Contributions_plus_Country_Tier2.pdf")
  CairoPDF(file = file.name, width = 14, height = 9)
  print(grid.draw(rbind(ggplotGrob(p), ggplotGrob(p2), ggplotGrob(p5), size = "last")))
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = zonal.mean)
