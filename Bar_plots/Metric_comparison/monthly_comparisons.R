library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

# use my functions 
source ("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/R_functions/my_functions.R")

get.data <- function (metric) {
  file.name <- paste0("../Data/", metric, "_O3_data.csv")
  data.df <- read.csv(file = file.name, header = TRUE)
  return (data.df)
}

metrics <- c("95th_Percentile", "AOT40D", "AOT40N", "MDA8", "Mean", "SOMO35", "W126")
data.list <- lapply(metrics, get.data)
data.df <- tbl_df(bind_rows(data.list))
data.df$Month <- factor(data.df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
data.df$Metric[data.df$Metric == "AOT40-D"] <- "AOT40D"
data.df$Metric[data.df$Metric == "AOT40-N"] <- "AOT40N"
data.df$Source <- factor(data.df$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#data.df$Source <- factor(data.df$Source, levels = c("Total", "Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))

scaled <- data.df %>%
  filter(Source != "Total") %>%
  spread(Metric, Zonal.Mean) %>%
  mutate(SOMO35 = SOMO35 / 10, AOT40D = AOT40D / 50, AOT40N = AOT40N / 50, W126 = W126 * 15) %>%
  gather(Metric, Zonal.Mean, -Month, -Region, -Source, -Species) %>%
  group_by(Month, Region, Source, Metric) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean))
scaled$Metric <- factor(scaled$Metric, levels = c("Mean", "MDA8", "Percentile.95", "SOMO35", "W126", "AOT40D", "AOT40N")) 

colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "Other" = "#b569b3", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#fdbf6f", "South.East.Asia" = "#fb9a99", "North.Africa" = "#b15928", "Mexico.Central.America" = "#33a02c", "Biogenic" = "#0e5c28", "Biomass.Burning" = "#2b9eb3", "Central.Asia" = "#898989")

title.base <- ": Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"

plotting <- function (data.frame, region) {
  if (region == "SAS") {
    df <- data.frame %>% filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
  } else if (region == "NAM") {
    df <- data.frame %>% filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
  } else if (region == "EUR") {
    df <- data.frame %>% filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
  } else if (region == "EAS") {
    df <- data.frame %>% filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
  } else if (region == "MDE") {
    df <- data.frame %>% filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
  } else if (region == "RBU") {
    df <- data.frame %>% filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
  } else if (region == "OCN") {
    df <- data.frame %>% filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
  } else if (region == "Rest") {
    df <- data.frame %>% filter(Region == "Rest")
  } else {
    stop("No region")
  }

  df[is.na(df)] <- 0
  plot.title <- paste0(plot.title(region), title.base)
  legend.levels <- unlist(plot.legend.levels(region))
  df$Region <- factor(df$Region, levels = unlist(plot.region.levels(region)))
  total <- df %>% group_by(Metric, Month) %>% summarise(Total = sum(Zonal.Mean))
  y.max = max(total$Total)
  custom.breaks <- seq(0, y.max, by = 10)

  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Metric, y = Zonal.Mean, fill = Source))
  p <- p + scale_y_continuous(expand = c(0, 0), breaks = custom.breaks, labels = every.nth(custom.breaks, 2, inverse = TRUE, percent = FALSE))
  p <- p + ggtitle(plot.title)
  p <- p + scale_fill_manual(values = colours, labels = rev(legend.levels))
  p <- p + metric.comparison.plot.lines()
  
  file.name <- paste0(region, "_metric_comparisons.pdf")
  CairoPDF(file = file.name, width = 16, height = 9)
  print(p)
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "RBU", "MDE", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = scaled)

## percent contributions
contribs <- scaled %>%
    spread(Source, Zonal.Mean, fill = 0) %>%
    mutate(Total = Local + North.America + Europe + East.Asia + South.Asia + Middle.East + Russia + Mexico.Central.America + North.Africa + South.East.Asia + Ocean + Rest + Other + Stratosphere) %>%
    #mutate(Total = Local + North.America + Europe + East.Asia + South.Asia + Middle.East + Russia + Mexico.Central.America + North.Africa + South.East.Asia + Central.Asia + Ocean + Rest + Other + Biogenic + Biomass.Burning + Stratosphere) %>%
    gather(Source, Zonal.Mean, -Month, -Region, -Metric, -Total) %>%
    mutate(Percent = ifelse(Total == 0, 0, Zonal.Mean / Total * 100)) %>%
    dplyr::select(-Zonal.Mean, -Total) 
contribs$Metric <- factor(contribs$Metric, levels = c("Mean", "MDA8", "Percentile.95", "SOMO35", "W126", "AOT40D", "AOT40N"))

title.base <- ": Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"

contribs.plotting <- function (data.frame, region) {
  file.name <- paste0(region, "_metrics_comparison_O3_percent_contribution.pdf")
  if (region == "SAS") {
    df <- data.frame %>% filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
  } else if (region == "NAM") {
    df <- data.frame %>% filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
  } else if (region == "EUR") {
    df <- data.frame %>% filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
  } else if (region == "EAS") {
    df <- data.frame %>% filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
  } else if (region == "MDE") {
    df <- data.frame %>% filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
  } else if (region == "RBU") {
    df <- data.frame %>% filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
  } else if (region == "OCN") {
    df <- data.frame %>% filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
  } else if (region == "Rest") {
    df <- data.frame %>% filter(Region == "Rest")
  } else {
    stop("No region")
  }

  custom.breaks <- seq(0, 100, 5)
  df <- df %>% filter(!is.na(Source))
  plot.title <- paste0(plot.title(region), title.base)
  legend.levels <- unlist(plot.legend.levels(region))
  df$Region <- factor(df$Region, levels = unlist(plot.region.levels(region)))
  df$Source <- factor(df$Source, levels = unlist(plot.source.levels(region)))

  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Metric, y = Percent, fill = Source))
  p <- p + scale_y_continuous(expand = c(0, 0), breaks = custom.breaks, labels = every.nth(custom.breaks, 2, inverse = TRUE, percent = TRUE))
  p <- p + ggtitle(plot.title)
  p <- p + scale_fill_manual(values = colours, labels = rev(legend.levels))
  p <- p + metric.comparison.plot.lines()
  
  CairoPDF(file = file.name, width = 16, height = 7)
  print(p)
  dev.off()
}

lapply(emission.regions, contribs.plotting, data.frame = contribs)
