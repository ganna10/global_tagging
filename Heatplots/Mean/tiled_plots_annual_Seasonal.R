library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

# use my functions 
source ("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/R_functions/my_functions.R")

data <- tbl_df(read.csv("Mean_O3_data.csv", header = TRUE))
data$Month <- factor(data$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

local <- data %>%
    filter(Source == "Local") %>%
    rowwise() %>%
    mutate(Source = get.local.source(Species))

non.local <- data %>%
    filter(Source != "Local")

converted.data <- rbind(non.local, local)

plot.data <- converted.data %>%
  dplyr::select(-Metric) %>%
  filter(!Region %in% c("Indian Ocean", "Rest", "South Pacific", "North Pacific", "South Atlantic", "North Atlantic"), Source != "Total") %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
plot.data$Region <- factor(plot.data$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska", "Hudson Bay", "NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus", "Mediterranean Sea", "Baltic Sea", "N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya", "NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya", "Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq", "W Russia", "E Russia", "Belarus; Ukraine", "Black and Caspian Sea"))

### annual data - absolute
annual.data <- plot.data %>%
  group_by(Region, Source) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean))

bin.data <- function (number) {
    rounded <- round(number, 1)
    if (rounded < 1) {
        value <- "<1"
    } else if (rounded >= 1 & rounded < 2) {
        value <- "1-2"
    } else if (rounded >= 2 & rounded < 5) {
        value <- "2-5"
    } else if (rounded >= 5 & rounded < 10) {
        value <- "5-10"
    } else if (rounded >= 10 & rounded < 20) {
        value <- "10-20"
    } else if (rounded >= 20 & rounded < 30) {
        value <- "20-30"
    }
    return (value)
}

discrete <- annual.data %>%
    rowwise() %>%
    mutate(Cat = bin.data(Zonal.Mean))
discrete$Cat <- factor(discrete$Cat, levels = c("<1", "1-2", "2-5", "5-10", "10-20", "20-30"))
discrete$Source <- factor(discrete$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#discrete$Source <- factor(discrete$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))
discrete.colours <- c("#0352cb", "#86b650", "#f7c56c", "#cc6329", "#dc3522", "#6c254f")

p <- ggplot(data = discrete, aes(x = Source, y = Region, fill = Cat))
p <- p + ggtitle("Annual Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
p <- p + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(discrete$Cat))), name = "O3 (ppbv)")
p <- p + heatmap.plot.lines()

CairoPDF(file = "Annual_Mean_O3_Tiles.pdf", width = 16, height = 9)
print(p)
dev.off()

### annual data - % contribution
percent.bin.data <- function (number) {
    rounded <- round(number, 1)
    if (rounded < 5) {
        value <- "<5"
    } else if (rounded >= 5 & rounded < 10) {
        value <- "5-10"
    } else if (rounded >= 10 & rounded < 20) {
        value <- "10-20"
    } else if (rounded >= 20 & rounded < 30) {
        value <- "20-30"
    } else if (rounded >= 30 & rounded < 50) {
        value <- "30-50"
    } else if (rounded >= 50 & rounded < 70) {
        value <- "50-70"
    }
    return (value)
}

annual.contributions <- annual.data %>%
  mutate(Total = sum(Zonal.Mean), Percent.Contribution = Zonal.Mean * 100 / Total) %>%
  dplyr::select(Receptor.Region = Region, Source, Percent.Contribution, -Total) %>%
  rowwise() %>%
  mutate(Cat = percent.bin.data(Percent.Contribution))
annual.contributions$Cat <- factor(annual.contributions$Cat, levels = c("<5", "5-10", "10-20", "20-30", "30-50", "50-70"))
annual.contributions$Source <- factor(annual.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#annual.contributions$Source <- factor(annual.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))

p2 <- ggplot(data = annual.contributions, aes(x = Source, y = Receptor.Region, fill = Cat))
p2 <- p2 + ggtitle("Percent Contributions to Annual Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
p2 <- p2 + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(annual.contributions$Cat))), name = "% Contribution")
p2 <- p2 + heatmap.plot.lines()

CairoPDF(file = "Annual_Percent_Contributions_Mean_O3_Tiles.pdf", width = 16, height = 9)
print(p2)
dev.off()

### seasonal data
seasonal.bin.data <- function (number) {
    rounded <- round(number, 1)
    if (rounded < 2) {
        value <- "<2"
    } else if (rounded >= 2 & rounded < 5) {
        value <- "2-5"
    } else if (rounded >= 5 & rounded < 10) {
        value <- "5-10"
    } else if (rounded >= 10 & rounded < 20) {
        value <- "10-20"
    } else if (rounded >= 20 & rounded < 30) {
        value <- "20-30"
    } else if (rounded >= 30 & rounded <= 55) {
        value <- "30-55"
    }
    return (value)
}

seasonal.data <- plot.data %>%
  filter(Source != "Total") %>%
  rowwise() %>%
  mutate(Season = get.season(Month)) %>%
  dplyr::select(-Month) %>%
  group_by(Region, Source, Season) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean)) %>%
  #max(seasonal.data$Zonal.Mean)
  rowwise() %>%
  mutate(Cat = seasonal.bin.data(Zonal.Mean))
seasonal.data$Cat <- factor(seasonal.data$Cat, levels =  c("<2", "2-5", "5-10", "10-20", "20-30", "30-55"))
seasonal.data$Source <- factor(seasonal.data$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")) 
#seasonal.data$Source <- factor(seasonal.data$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other")) 

seasonal.plot <- function (df, season) {
  plot.df <- df %>% filter(Season == season)
  plot.title <- paste0(season, " Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
  
  p1 <- ggplot(data = plot.df, aes(x = Source, y = Region, fill = Cat))
  p1 <- p1 + ggtitle(plot.title)
  p1 <- p1 + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(plot.df$Cat))), name = "O3 (ppb)")
  p1 <- p1 + heatmap.plot.lines()
  
  file.name = paste0(season, "_mean_O3_Tiles.pdf")
  CairoPDF(file = file.name, width = 16, height = 9)
  print(p1)
  dev.off()
}
seasons <- levels(factor(seasonal.data$Season))
lapply(seasons, seasonal.plot, df = seasonal.data)

### seasonal data - % contribution
seasonal.percent.bin.data <- function (number) {
    rounded <- round(number, 1)
    if (rounded < 5) {
        value <- "<5"
    } else if (rounded >= 5 & rounded < 10) {
        value <- "5-10"
    } else if (rounded >= 10 & rounded < 20) {
        value <- "10-20"
    } else if (rounded >= 20 & rounded < 40) {
        value <- "20-40"
    } else if (rounded >= 40 & rounded < 65) {
        value <- "40-65"
    } else if (rounded >= 65 & rounded < 90) {
        value <- "65-90"
    }
    return (value)
}

seasonal.contributions <- seasonal.data %>%
  dplyr::select(-Cat) %>%
  spread(Source, Zonal.Mean) %>%
  mutate(Total = North.America + Europe + South.Asia + East.Asia + Middle.East + Russia + Rest + Ocean + Stratosphere + Other) %>%
  gather(Source, Zonal.Mean, -Region, -Season, -Total) %>%
  mutate(Percent.Contribution = Zonal.Mean * 100 / Total) %>%
  dplyr::select(Receptor.Region = Region, Source, Percent.Contribution, Season, -Total) %>%
  #max(seasonal.contributions$Percent.Contribution)
  rowwise() %>%
  mutate(Cat = seasonal.percent.bin.data(Percent.Contribution))
seasonal.contributions$Cat <- factor(seasonal.contributions$Cat, levels = c("<5", "5-10", "10-20", "20-40", "40-65", "65-90"))
seasonal.contributions$Source <- factor(seasonal.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#seasonal.contributions$Source <- factor(seasonal.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))

seasonal.contribution.plot <- function (df, season) {
  plot.df <- df %>% filter(Season == season)
  plot.title <- paste0(season, " Pecent Contributions to Mean O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
  
  p1 <- ggplot(data = plot.df, aes(x = Source, y = Receptor.Region, fill = Cat))
  p1 <- p1 + ggtitle(plot.title)
  p1 <- p1 + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(plot.df$Cat))), name = "% Contribution")
  p1 <- p1 + heatmap.plot.lines()
  
  file.name = paste0(season, "_Percent_Contribution_mean_O3_Tiles.pdf")
  CairoPDF(file = file.name, width = 16, height = 9)
  print(p1)
  dev.off()
}

lapply(seasons, seasonal.contribution.plot, df = seasonal.contributions)
