setwd("~/Documents/Analysis/2016_HTAP/WRF/metric_data/")
library("ggplot2")
library("dplyr")
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

data.df <- tbl_df(read.csv("SOMO35_O3_data.csv", header = TRUE))
data.df
percent <- data.df %>%
  dplyr::select(-Metric) %>%
#   filter(Source != "Total") %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean)) %>%
  spread(Source, Zonal.Mean, fill = 0) %>%
  gather(Source, Zonal.Mean, -Month, -Region, -Total) %>%
#   group_by(Month, Region, Source) %>%
#   summarise(Total = sum(Total), Zonal.Mean = sum(Zonal.Mean)) %>%
  mutate(Percent.Contrib = ifelse(Total == 0, 0, Zonal.Mean / Total)) %>%
  dplyr::select(-Total, -Zonal.Mean)

print.data.frame(percent %>%
  group_by(Month, Region) %>%
  summarise(Sum = sum(Percent.Contrib)))

percent$Month <- factor(percent$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
percent$Source <- factor(percent$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))

plotting <- function (data.frame, region) {  
  print(region)
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean")) 
    df$Source <- factor(df$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
    df <- df %>% filter(!is.na(Source))
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "East Asia", "Europe", "North America", "Local")
    plot.title <- "South Asia: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))   
    df$Source <- factor(df$Source, levels = c("Local", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
    df <- df %>% filter(!is.na(Source))
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia",  "Europe", "Local")
    plot.title <- "North America: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))
    df$Source <- factor(df$Source, levels = c("Local", "North.America", "East.Asia", "South.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    df <- df %>% filter(!is.na(Source))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "North America", "Local")
    plot.title <- "Europe: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Source <- factor(df$Source, levels = c("Local", "North.America", "Europe", "South.Asia", "Middle.East", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    df <- df %>% filter(!is.na(Source))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "Europe", "North America", "Local")
    plot.title <- "East Asia: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Source <- factor(df$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Russia", "Rest", "Ocean", "Stratosphere", "Other"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    df <- df %>% filter(!is.na(Source))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Middle East: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Source <- factor(df$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Rest", "Ocean", "Stratosphere", "Other"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    df <- df %>% filter(!is.na(Source))
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Russia, Belarus, Ukraine: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Source <- factor(df$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Rest", "Stratosphere", "Other"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    df <- df %>% filter(!is.na(Source))
    legend.levels <- c("Other", "Stratosphere", "Rest", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    #legend.levels <- c("Other", "Stratosphere", "Rest", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Oceans: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    df$Source <- factor(df$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Ocean", "Stratosphere", "Other"))
    legend.levels <- c("Other", "Stratosphere", "Ocean", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    df <- df %>% filter(!is.na(Source))
    #legend.levels <- c("Other", "Stratosphere", "Ocean", "South East Asia", "North Africa", "Mexico, Central America", "Russia", "Middle East", "South Asia", "East Asia", "Europe", "North America", "Local")
    plot.title <- "Rest of the World: Percent Contributions to SOMO35 O3 for Emissions and Meteorology for Year 2010"
  } else {
    stop("No region")
  }
  
  colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "Other" = "#b569b3", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#fdbf6f", "South.East.Asia" = "#fb9a99", "North.Africa" = "#b15928", "Mexico.Central.America" = "#33a02c")
  
  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Month, y = Percent.Contrib, fill = Source))
  p <- p + geom_bar(stat = "identity", size = 0.9)
  p <- p + facet_wrap(~ Region, nrow = 1)
  p <- p + scale_y_continuous(expand = c(0, 0), labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, by = 0.1))
  p <- p + scale_x_discrete(expand = c(0, 0),  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p <- p + geom_vline(xintercept = 12.5, size = 2)
  p <- p + ggtitle(plot.title)
  p <- p + theme_bw()
  p <- p + theme(panel.border = element_blank())
  p <- p + theme(panel.grid = element_blank())
  p <- p + theme(legend.key = element_blank())
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(strip.background = element_blank())
  p <- p + theme(strip.text = element_text(face = "bold"))
  p <- p + theme(legend.title = element_blank())
  p <- p + theme(plot.title = element_text(face = "bold"))
  p <- p + theme(axis.line.x = element_line(colour = "black"))
  p <- p + theme(axis.line.y = element_line(colour = "black"))
  p <- p + guides(fill = guide_legend(ncol = 1))
  p <- p + scale_fill_manual(values = colours, labels = rev(legend.levels))
  
  file.name <- paste0(region, "_SOMO35_Percent_Contributions.pdf")
  CairoPDF(file = file.name, width = 14, height = 7)
  print(p)
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")                                                                      
lapply(emission.regions, plotting, data.frame = percent)
