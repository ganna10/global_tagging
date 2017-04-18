library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

# use my functions 
source ("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/R_functions/my_functions.R")

data.df <- tbl_df(read.csv("AOT40N_O3_data.csv", header = TRUE))
percent <- data.df %>%
  dplyr::select(-Metric) %>% 
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean)) %>%
  spread(Source, Zonal.Mean, fill = 0) %>%
  gather(Source, Zonal.Mean, -Month, -Region, -Total) %>%
  mutate(Percent.Contrib = ifelse(Total == 0, 0, Zonal.Mean / Total * 100)) %>%
  dplyr::select(-Total, -Zonal.Mean)

percent %>% group_by(Month, Region) %>% summarise(Sum = sum(Percent.Contrib))

percent$Month <- factor(percent$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
percent$Source <- factor(percent$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#percent$Source <- factor(percent$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))

title.base <- ": Percent Contributions to AOT40-N O3 for Emissions and Meteorology for Year 2010"

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
  
  colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "Other" = "#b569b3", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#fdbf6f", "South.East.Asia" = "#fb9a99", "North.Africa" = "#b15928", "Mexico.Central.America" = "#33a02c", "Biogenic" = "#0e5c28", "Biomass.Burning" = "#2b9eb3", "Central.Asia" = "#898989")

  custom.breaks <- seq(0, 100, by = 5)
  df <- df %>% filter(!is.na(Source))
  plot.title <- paste0(plot.title(region), title.base)
  legend.levels <- unlist(plot.legend.levels(region))
  df$Region <- factor(df$Region, levels = unlist(plot.region.levels(region)))
  df$Source <- factor(df$Source, levels = unlist(plot.source.levels(region)))
  
  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Month, y = Percent.Contrib, fill = Source))
  p <- p + geom_bar(stat = "identity", size = 0.9)
  p <- p + facet_wrap(~ Region, nrow = 1)
  p <- p + scale_y_continuous(expand = c(0, 0), labels = every.nth(custom.breaks, 2, inverse = TRUE, percent = TRUE), breaks = custom.breaks, limits = c(0, 100))
  p <- p + scale_x_discrete(expand = c(0, 0),  labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p <- p + ggtitle(plot.title)
  p <- p + theme_bw()
  p <- p + theme(panel.border = element_rect(colour = "black"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + theme(legend.key = element_blank())
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(strip.background = element_blank())
  p <- p + theme(strip.text = element_text(face = "bold"))
  p <- p + theme(legend.title = element_blank())
  p <- p + theme(plot.title = element_text(face = "bold"))
  p <- p + guides(fill = guide_legend(ncol = 1))
  p <- p + scale_fill_manual(values = colours, labels = rev(legend.levels))
  
  file.name <- paste0(region, "_AOT40N_Percent_Contributions.pdf")
  CairoPDF(file = file.name, width = 16, height = 9)
  print(p)
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")                                                                      
lapply(emission.regions, plotting, data.frame = percent)
