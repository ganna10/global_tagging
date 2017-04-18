library("ggplot2")
library("dplyr")
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

get.data <- function (metric) {
  file.name <- paste0("../Data/", metric, "_O3_data.csv")
  data.df <- read.csv(file = file.name, header = TRUE)
  return (data.df)
}

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
  {
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

metrics <- c("95th_Percentile", "AOT40D", "AOT40N", "MDA8", "Mean", "SOMO35", "W126")
data.list <- lapply(metrics, get.data)
data.df <- tbl_df(bind_rows(data.list))
data.df$Month <- factor(data.df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
tbl_df(data.df)

data.df %>%
  filter(Source != "Total") %>%
  group_by(Metric) %>%
  summarise(Max = max(Zonal.Mean))
data.df$Metric[data.df$Metric == "AOT40-D"] <- "AOT40D"
data.df$Metric[data.df$Metric == "AOT40-N"] <- "AOT40N"
data.df$Source <- factor(data.df$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))

scaled <- data.df %>%
  filter(Source != "Total") %>%
  spread(Metric, Zonal.Mean) %>%
  mutate(SOMO35 = SOMO35 / 10, AOT40D = AOT40D / 50, AOT40N = AOT40N / 50, W126 = W126 * 15) %>%
  gather(Metric, Zonal.Mean, -Month, -Region, -Source, -Species) %>%
  group_by(Month, Region, Source, Metric) %>%
  summarise(Zonal.Mean = mean(Zonal.Mean))
scaled$Metric <- factor(scaled$Metric, levels = c("Mean", "MDA8", "Percentile.95", "SOMO35", "W126", "AOT40D", "AOT40N")) 

plotting <- function (data.frame, region) {
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
    plot.title <- "South Asia: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
    plot.title <- "North America: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
    plot.title <- "Europe: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
    plot.title <- "East Asia: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Middle East: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Russia, Belarus, Ukraine: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    legend.levels <- c("Other", "Stratosphere", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Oceans: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    legend.levels <- c("Other", "Stratosphere", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Rest of the World: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else {
    stop("No region")
  }

  colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "Other" = "#b569b3", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#fdbf6f", "South.East.Asia" = "#fb9a99", "North.Africa" = "#b15928", "Mexico.Central.America" = "#33a02c")

  df[is.na(df)] <- 0
  total <- df %>% group_by(Metric, Month) %>% summarise(Total = sum(Zonal.Mean))
  y.max = max(total$Total)
  custom.breaks <- seq(0, y.max, by = 10)

  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Metric, y = Zonal.Mean, fill = Source))
  p <- p + geom_bar(stat = "identity", size = 0.9)
  p <- p + facet_grid(Region ~ Month)
  p <- p + scale_y_continuous(expand = c(0, 0), breaks = custom.breaks, labels = every_nth(custom.breaks, 2, inverse = TRUE))
  p <- p + scale_x_discrete(expand = c(0, 0))
  p <- p + ylab("O3 (ppbv)") + ggtitle(plot.title)
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
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p <- p + theme(axis.line.x = element_line(colour = "black"))
  p <- p + theme(axis.line.y = element_line(colour = "black"))
  p <- p + theme(legend.position = "top")
  p <- p + guides(fill = guide_legend(nrow = 1))
  p <- p + scale_fill_manual(values = colours, labels = rev(legend.levels))
  
  file.name <- paste0(region, "_metric_comparisons.pdf")
  CairoPDF(file = file.name, width = 15, height = 9)
  print(p)
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "RBU", "MDE", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = scaled)

## percent contributions
contribs <- scaled %>%
    spread(Source, Zonal.Mean, fill = 0) %>%
    mutate(Total = Local + North.America + Europe + East.Asia + South.Asia + Middle.East + Russia + Mexico.Central.America + North.Africa + South.East.Asia + Ocean + Rest + Other + Stratosphere) %>%
    gather(Source, Zonal.Mean, -Month, -Region, -Metric, -Total) %>%
    mutate(Percent = ifelse(Total == 0, 0, Zonal.Mean / Total)) %>%
    dplyr::select(-Zonal.Mean, -Total) #%>%
    #group_by(Month, Region, Metric) %>%
    #summarise(Sum = sum(Percent))
contribs
contribs$Source <- factor(contribs$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
contribs$Metric <- factor(contribs$Metric, levels = c("Mean", "MDA8", "Percentile.95", "SOMO35", "W126", "AOT40D", "AOT40N"))

contribs.plotting <- function (data.frame, region) {
  file.name <- paste0(region, "_metrics_comparison_O3_percent_contribution.pdf")
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
    plot.title <- "South Asia: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
    plot.title <- "North America: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
    plot.title <- "Europe: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
    plot.title <- "East Asia: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Middle East: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Russia, Belarus, Ukraine: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    legend.levels <- c("Other", "Stratosphere", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Oceans: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    legend.levels <- c("Other", "Stratosphere", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Rest of the World: Monthly Comparison of % Contributions to O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/100, W126 (ppm.hrs)*15"
  } else {
    stop("No region")
  }

  colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "Other" = "#b569b3", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#fdbf6f", "South.East.Asia" = "#fb9a99", "North.Africa" = "#b15928", "Mexico.Central.America" = "#33a02c")
  custom.breaks <- seq(0, 1, 0.1)
  
  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Metric, y = Percent, fill = Source))
  p <- p + geom_bar(stat = "identity", size = 0.9)
  p <- p + facet_grid(Region ~ Month)
  p <- p + scale_y_continuous(expand = c(0, 0), breaks = custom.breaks, labels = scales::percent)
  p <- p + scale_x_discrete(expand = c(0, 0))
  p <- p + ggtitle(plot.title)
  p <- p + geom_vline(xintercept = 7.5, size = 1)
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
  p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p <- p + theme(axis.line.x = element_line(colour = "black"))
  p <- p + theme(axis.line.y = element_line(colour = "black"))
  p <- p + guides(fill = guide_legend(ncol = 1))
  p <- p + scale_fill_manual(values = colours)
  
  CairoPDF(file = file.name, width = 15, height = 7)
  print(p)
  dev.off()
}

#lapply(emission.regions, contribs.plotting, data.frame = contribs)
