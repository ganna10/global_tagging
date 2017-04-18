library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

# use my functions 
source ("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/R_functions/my_functions.R")

data <- tbl_df(read.csv("AOT40N_O3_data.csv", header = TRUE))
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
    if (rounded < 100) {
        value <- "<100"
    } else if (rounded >= 100 & rounded < 250) {
        value <- "100-250"
    } else if (rounded >= 250 & rounded < 500) {
        value <- "250-500"
    } else if (rounded >= 500 & rounded < 1000) {
        value <- "500-1000"
    } else if (rounded >= 1000 & rounded < 2000) {
        value <- "1000-2000"
    } else if (rounded >= 2000 & rounded < 4500) {
        value <- "2000-4500"
    }
    return (value)
}

discrete <- annual.data %>%
    rowwise() %>%
    mutate(Cat = bin.data(Zonal.Mean))
discrete$Cat <- factor(discrete$Cat, levels = c("<100", "100-250", "250-500", "500-1000", "1000-2000", "2000-4500"))
discrete$Source <- factor(discrete$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#discrete$Source <- factor(discrete$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))
discrete.colours <- c("#0352cb", "#86b650", "#f7c56c", "#cc6329", "#dc3522", "#6c254f")

p <- ggplot(data = discrete, aes(x = Source, y = Region, fill = Cat))
p <- p + ggtitle("Annual AOT40N O3 (ppbv.hrs) in each Tier 2 Receptor Region from each Source Region")
p <- p + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(discrete$Cat))), name = "AOT40N (ppbv.hrs)")
p <- p + heatmap.plot.lines()

CairoPDF(file = "Annual_AOT40N_O3_Tiles.pdf", width = 16, height = 9)
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
    } else if (rounded >= 20 & rounded < 35) {
        value <- "20-35"
    } else if (rounded >= 35 & rounded < 60) {
        value <- "35-60"
    } else if (rounded >= 60 & rounded < 80) {
        value <- "60-80"
    }
    return (value)
}

annual.contributions <- annual.data %>%
  mutate(Total = sum(Zonal.Mean), Percent.Contribution = Zonal.Mean * 100 / Total) %>%
  dplyr::select(Receptor.Region = Region, Source, Percent.Contribution, -Total) %>%
  #max(annual.contributions$Percent.Contribution)
  rowwise() %>%
  mutate(Cat = percent.bin.data(Percent.Contribution))
annual.contributions$Cat <- factor(annual.contributions$Cat, levels = c("<5", "5-10", "10-20", "20-35", "35-60", "60-80"))
annual.contributions$Source <- factor(annual.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#annual.contributions$Source <- factor(annual.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))

p2 <- ggplot(data = annual.contributions, aes(x = Source, y = Receptor.Region, fill = Cat))
p2 <- p2 + ggtitle("Percent Contributions to Annual AOT40N O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
p2 <- p2 + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(annual.contributions$Cat))), name = "% Contribution")
p2 <- p2 + heatmap.plot.lines()

CairoPDF(file = "Annual_Percent_Contributions_AOT40N_O3_Tiles.pdf", width = 16, height = 9)
print(p2)
dev.off()

### seasonal data
seasonal.bin.data <- function (number) {
    rounded <- round(number, 1)
    if (rounded < 100) {
        value <- "<100"
    } else if (rounded >= 100 & rounded < 250) {
        value <- "100-250"
    } else if (rounded >= 250 & rounded < 1000) {
        value <- "250-1000"
    } else if (rounded >= 1000 & rounded < 3000) {
        value <- "1000-3000"
    } else if (rounded >= 3000 & rounded < 6000) {
        value <- "3000-6000"
    } else if (rounded >= 6000 & rounded <= 9000) {
        value <- "6000-9000"
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
seasonal.data$Cat <- factor(seasonal.data$Cat, levels =  c("<100", "100-250", "250-1000", "1000-3000", "3000-6000", "6000-9000"))
seasonal.data$Source <- factor(seasonal.data$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other")) 
#seasonal.data$Source <- factor(seasonal.data$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other")) 

seasonal.plot <- function (df, season) {
  plot.df <- df %>% filter(Season == season)
  plot.title <- paste0(season, " AOT40N O3 (ppbv.hrs) in each Tier 2 Receptor Region from each Source Region")

  discrete.colours <- c("<100" = "#0352cb", "100-250" = "#86b650", "250-1000" = "#f7c56c", "1000-3000" = "#cc6329", "3000-6000" = "#dc3522", "6000-9000" = "#6c254f")
  
  p1 <- ggplot(data = plot.df, aes(x = Source, y = Region, fill = Cat))
  p1 <- p1 + ggtitle(plot.title)
  p1 <- p1 + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(plot.df$Cat))), name = "AOT40N (ppb.hrs)")
  p1 <- p1 + heatmap.plot.lines()
  
  file.name = paste0(season, "_AOT40N_O3_Tiles.pdf")
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
    } else if (rounded >= 10 & rounded < 25) {
        value <- "10-25"
    } else if (rounded >= 25 & rounded < 50) {
        value <- "25-50"
    } else if (rounded >= 50 & rounded < 70) {
        value <- "50-70"
    } else if (rounded >= 70 & rounded < 95) {
        value <- "70-95"
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
seasonal.contributions$Cat <- factor(seasonal.contributions$Cat, levels = c("<5", "5-10", "10-25", "25-50", "50-70", "70-95"))
seasonal.contributions$Source <- factor(seasonal.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#seasonal.contributions$Source <- factor(seasonal.contributions$Source, levels = c("North.America", "Europe", "South.Asia", "East.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))

seasonal.contribution.plot <- function (df, season) {
  plot.df <- df %>% filter(Season == season)
  plot.title <- paste0(season, " Pecent Contributions to AOT40N O3 (ppbv) in each Tier 2 Receptor Region from each Source Region")
  
  p1 <- ggplot(data = plot.df, aes(x = Source, y = Receptor.Region, fill = Cat))
  p1 <- p1 + ggtitle(plot.title)
  p1 <- p1 + scale_fill_manual(values = rev(discrete.colours), limits = rev(levels(factor(plot.df$Cat))), name = "% Contribution")
  p1 <- p1 + heatmap.plot.lines()
  
  file.name = paste0(season, "_Percent_Contribution_AOT40N_O3_Tiles.pdf")
  CairoPDF(file = file.name, width = 16, height = 9)
  print(p1)
  dev.off()
}

lapply(seasons, seasonal.contribution.plot, df = seasonal.contributions)
