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
  } else if (str_detect(species, 'BIO')) { # biogenic
    source.emiss = "Biogenic"
  } else if (str_detect(species, 'BMB')) { # biomass burning
    source.emiss = "Biomass Burning"
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
  } else if (str_detect(Species, 'CAS')) {
    Source = "Central.Asia"
  } else if (str_detect(Species, 'BIO')) {
    Source = "Biogenic"
  } else if (str_detect(Species, 'BMB')) {
    Source = "Biomass.Burning"
  } else {
    Source = "Error"
  }
  return (Source)
}

every.nth <- function(x, nth, empty = TRUE, inverse = FALSE, percent = FALSE, decimals = 0) {
  if (percent) {
    x <- paste(x, "%")
  }
  if (decimals != 0) {
    x <- sprintf("%0.1f", x)
  }
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
      } else {
        x[1:nth != 1]
        }
    } else {
      if(empty) {
        x[1:nth != 1] <- ""
        x
        } else {
          x[1:nth == 1]
        }
    }
}

plot.title <- function (region) {
  if (region == "SAS") {
    area <- "South Asia"
  } else if (region == "NAM") {
    area <- "North America"
  } else if (region == "EUR") {
    area <- "Europe"
  } else if (region == "EAS") {
    area <- "East Asia"
  } else if (region == "MDE") {
    area <- "Middle East"
  } else if (region == "RBU") {
    area <- "Russia, Belarus, Ukraine"
  } else if (region == "OCN") {
    area <- "Oceans"
  } else if (region == "Rest") {
    area <- "Rest of the World"
  } else {
    stop("No region")
  }
  return (area)
}

plot.legend.levels <- function (region) {
  if (region == "SAS") {
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
  } else if (region == "NAM") {
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
  } else if (region == "EUR") {
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
  } else if (region == "EAS") {
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
  } else if (region == "MDE") {
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
  } else if (region == "RBU") {
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
  } else if (region == "OCN") {
    legend.levels <- c("Other", "Stratosphere", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
  } else if (region == "Rest") {
    legend.levels <- c("Other", "Stratosphere", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Biomass Burning", "Biogenic", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
  } else {
    stop("No region")
  }
  return (legend.levels)
}

plot.region.levels <- function (region) {
  if (region == "SAS") {
    region.levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya")
  } else if (region == "NAM") {
    region.levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska")
  } else if (region == "EUR") {
    region.levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus")
  } else if (region == "EAS") {
    region.levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya")
  } else if (region == "MDE") {
    region.levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq")
  } else if (region == "RBU") {
    region.levels = c("W Russia", "E Russia", "Belarus; Ukraine")
  } else if (region == "OCN") {
    region.levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea")
  } else if (region == "Rest") {
    region.levels = c("Rest")
  } else {
    stop("No region")
  }
  return (region.levels)
}

plot.source.levels <- function (region) {
  if (region == "SAS") {
    source.levels <- c("Local", "North.America", "Europe", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")
    #source.levels <- c("Local", "North.America", "Europe", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else if (region == "NAM") {
    source.levels <- c("Local", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")
    #source.levels <- c("Local", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else if (region == "EUR") {
    source.levels <- c("Local", "North.America", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")
    #source.levels <- c("Local", "North.America", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else if (region == "EAS") {
    source.levels <- c("Local", "North.America", "Europe", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")
    #source.levels <- c("Local", "North.America", "Europe", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else if (region == "MDE") {
    source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")
    #source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else if (region == "RBU") {
    source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")
    #source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else if (region == "OCN") {
    source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Stratosphere", "Other")
    #source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else if (region == "Rest") {
    source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Ocean", "Stratosphere", "Other")
    #source.levels <- c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Ocean", "Biomass.Burning", "Biogenic", "Stratosphere", "Other")
  } else {
    stop("No region")
  }
  return (source.levels)
}

transported.levels <- function (region) {
  if (region == "SAS") {
    plot.levels <- c("Rest", "Ocean", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "East.Asia", "Europe", "North.America")
    #plot.levels <- c("Rest", "Ocean", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "East.Asia", "Europe", "North.America")
  } else if (region == "NAM") {
    plot.levels <- c("Rest", "Ocean", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "Europe")
    #plot.levels <- c("Rest", "Ocean", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "Europe")
  } else if (region == "EUR") {
    plot.levels <- c("Rest", "Ocean", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "North.America")
    #plot.levels <- c("Rest", "Ocean", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "North.America")
  } else if (region == "EAS") {
    plot.levels <- c("Rest", "Ocean", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "Europe", "North.America")
    #plot.levels <- c("Rest", "Ocean", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "Europe", "North.America")
  } else if (region == "MDE") {
    plot.levels <- c("Rest", "Ocean", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "South.Asia", "East.Asia", "Europe", "North.America")
    #plot.levels <- c("Rest", "Ocean", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "South.Asia", "East.Asia", "Europe", "North.America")
  } else if (region == "RBU") {
    plot.levels <- c("Rest", "Ocean", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Middle.East", "South.Asia", "East.Asia", "Europe", "North.America")
    #plot.levels <- c("Rest", "Ocean", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Middle.East", "South.Asia", "East.Asia", "Europe", "North.America")
  } else if (region == "OCN") {
    plot.levels <- c("Rest", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "Europe", "North.America")
    #plot.levels <- c("Rest", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "Europe", "North.America")
  } else if (region == "Rest") {
    plot.levels <- c("Ocean", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "Europe", "North.America")
    #plot.levels <- c("Ocean", "Central.Asia", "South.East.Asia", "North.Africa", "Mexico.Central.America", "Russia", "Middle.East", "South.Asia", "East.Asia", "Europe", "North.America")
  } else {
    stop("No region")
  }
  return (plot.levels)
}

transported.legend <- function (region) {
  if (region == "SAS") {
    legend.levels <- c("Rest", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America")
    #legend.levels <- c("Rest", "Ocean", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America")
  } else if (region == "NAM") {
    legend.levels <- c("Rest", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe")
    #legend.levels <- c("Rest", "Ocean", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe")
  } else if (region == "EUR") {
    legend.levels <- c("Rest", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America")
    #legend.levels <- c("Rest", "Ocean", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America")
  } else if (region == "EAS") {
    legend.levels <- c("Rest", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America")
    #legend.levels <- c("Rest", "Ocean", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America")
  } else if (region == "MDE") {
    legend.levels <- c("Rest", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America")
    #legend.levels <- c("Rest", "Ocean", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America")
  } else if (region == "RBU") {
    legend.levels <- c("Rest", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America")
    #legend.levels <- c("Rest", "Ocean", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America")
  } else if (region == "OCN") {
    legend.levels <- c("Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America")
    #legend.levels <- c("Rest", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America")
  } else if (region == "Rest") {
    legend.levels <- c("Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America")
    #legend.levels <- c("Ocean", "Central Asia", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America")
  } else {
    stop("No region")
  }
  return (legend.levels)
}

get.local.source <- function (species) {
    if (str_detect(species, "NAM")) {
        local <- "North.America"
    } else if (str_detect(species, "EUR")) {
        local <- "Europe"
    } else if (str_detect(species, "EAS")) {
        local <- "East.Asia"
    } else if (str_detect(species, "SAS")) {
        local <- "South.Asia"
    } else if (str_detect(species, "MDE")) {
        local <- "Middle.East"
    } else if (str_detect(species, "RBU")) {
        local <- "Russia"
    } else if (str_detect(species, "OCN")) {
        local <- "Ocean"
    } else if (str_detect(species, "RST")) {
        local <- "Rest"
    } else {
        local <- "No Match"
    }
    return (local)
} 

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

heatmap.plot.lines <- function () {
  list(geom_tile(colour = "white", size = 1),
    ylab("Receptor Region") ,
    xlab("Source Region"),
    scale_x_discrete(expand = c(0, 0), labels = c("North America", "Europe", "South Asia", "East Asia", "Middle East", "Russia", "Mexico\nCentral America", "North Africa", "South East\nAsia", "Rest", "Ocean", "Stratosphere", "Other")),
    #scale_x_discrete(expand = c(0, 0), labels = c("North America", "Europe", "South Asia", "East Asia", "Middle East", "Russia", "Mexico\nCentral America", "North Africa", "South East\nAsia", "Central\nAsia", "Rest", "Ocean", "Biogenic", "Biomass\nBurning", "Stratosphere", "Other")),
    scale_y_discrete(expand = c(0, 0)),
    geom_hline(yintercept = 7.5, size = 0.5), # NAM
    geom_hline(yintercept = 13.5, size = 0.5), # EUR
    geom_hline(yintercept = 16.5, size = 0.5), # SAS
    geom_hline(yintercept = 22.5, size = 0.5), # EAS
    geom_hline(yintercept = 25.5, size = 0.5), # MDE
    #geom_hline(yintercept = 29.5, size = 0.5), # RBU,
    geom_vline(xintercept = seq(1.5, 12.5, by = 1), size = 0.5),
    theme_bw(),
    theme(panel.border = element_rect(colour = "black")),
    theme(legend.key = element_blank()),
    theme(axis.text = element_text(colour = "black")),
    theme(axis.title = element_text(colour = "black", face = "bold")),
    theme(plot.title = element_text(face = "bold")),
    theme(legend.title = element_text(face = "bold"))
  )
}

real.metrics.plot.lines <- function () {
    list(
        geom_bar(stat = "identity", size = 0.9),
        facet_wrap(~ Region, nrow = 1),
        scale_x_discrete(expand = c(0, 0)),
        theme_bw(),
        theme(panel.border = element_rect(colour = "black")),
        theme(panel.grid = element_blank()),
        theme(legend.key = element_blank()),
        theme(axis.title = element_blank()),
        theme(legend.title = element_blank()),
        theme(strip.background = element_blank()),
        theme(strip.text = element_text(face = "bold")),
        theme(plot.title = element_text(face = "bold")),
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
}
