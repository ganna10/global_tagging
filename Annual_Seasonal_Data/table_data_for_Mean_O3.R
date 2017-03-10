setwd("~/Documents//Analysis//2016_HTAP//Annual_Seasonal_Data/")

get.data <- function (month) {
  month.text <- month.abb[month]
  file.name <- paste0("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/Mean/Data/HTAP_NOx_Tagging_", month.text, "_assigned_to_Tier2_regions.csv")
  data.df <- read.csv(file = file.name, header = TRUE)
  return(data.df)
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

with.sources <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
with.sources$Source <- factor(with.sources$Source, levels = c("Total", "Local", "North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Ocean", "Rest", "Stratosphere", "Other"))

#### calculate mean annual percentage contribution
annual.data <- with.sources %>%
  filter(Source != "Total") %>%
  group_by(Region, Source) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean)) %>%
  mutate(Total = sum(Zonal.Mean), Percent.Contribution = Zonal.Mean * 100 / Total) %>%
  dplyr::select(Receptor.Region = Region, Source, Percent.Contribution, -Total) %>%
  spread(Source, Percent.Contribution)
annual.data
write.csv(annual.data, file = "Mean_Annual_Percent_Contributions.csv", row.names = FALSE, quote = FALSE)

#### calculate mean seasonal percentage contribution
get.season <- function (month) {
  if (month %in% c("Jan", "Feb", "Mar")) {
    season = "JFM"
  } else if (month %in% c("Apr", "May", "Jun")) {
    season = "AMJ"
  } else if (month %in% c("Jul", "Aug", "Sep")) {
    season = "JUS"
  } else if (month %in% c("Oct", "Nov", "Dec")) {
    season = "OND"
  }
  return (season)
}

seasonal.data <- with.sources %>%
  filter(Source != "Total") %>%
  rowwise() %>%
  mutate(Season = get.season(Month)) %>%
  group_by(Season, Region, Source) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean)) %>%
  mutate(Total = sum(Zonal.Mean), Percent.Contribution = Zonal.Mean * 100 / Total) %>%
  dplyr::select(Season, Receptor.Region = Region, Source, Percent.Contribution, -Total) %>%
  spread(Source, Percent.Contribution)
seasonal.data$Season <- factor(seasonal.data$Season, levels = c("JFM", "AMJ", "JUS", "OND"))

write.csv(seasonal.data %>% arrange(Season), file = "Mean_Seasonal_Percent_Contributions.csv", row.names = FALSE, quote = FALSE)

#### testing that all sources match with total
# plot.test.df <- with.sources %>%
#   spread(Source, Zonal.Mean) %>%
#   gather(Source, Zonal.Mean, -Month, -Region, -Total)
# 
# p <- ggplot(data = plot.test.df, aes(x = Month))
# p <- p + geom_bar(aes(y = Zonal.Mean, fill = Source), stat = "identity")
# p <- p + geom_point(aes(y = Total))
# p <- p + facet_wrap(~ Region)
# p

#### get min max of stratosphere in each region
# min.max.str  <- with.sources %>%
#   filter(Source == "Stratosphere") %>%
#   group_by(Region) %>%
#   summarise(Min = min(Zonal.Mean), Max = max(Zonal.Mean))
# min.max.str$Region <- factor(min.max.str$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska", "NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus", "N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya", "NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya", "China; Tibet Himalaya", "Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq", "W Russia", "E Russia", "Belarus; Ukraine", "Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea", "Rest"))
# write.csv(min.max.str %>% arrange(Region), file = "Stratospheric_contributions.csv", quote = FALSE, row.names = FALSE)
