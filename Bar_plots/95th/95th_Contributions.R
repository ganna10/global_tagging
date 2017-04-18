library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

# use my functions 
source ("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/R_functions/my_functions.R")

get.data <- function (month) {
  month.text <- month.abb[month]
  file.name <- paste0("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/95th/Data/HTAP_95th_", month.text, "_assigned_to_Tier2_regions.csv")
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
zonal.mean$Month <- factor(zonal.mean$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 

with.sources <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  filter(Source != "Total") %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
with.sources$Source <- factor(with.sources$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Rest", "Ocean", "Stratosphere", "Other"))
#with.sources$Source <- factor(with.sources$Source, levels = c("Local", "North.America", "Europe", "East.Asia", "South.Asia", "Middle.East", "Russia", "Mexico.Central.America", "North.Africa", "South.East.Asia", "Central.Asia", "Rest", "Ocean", "Biogenic", "Biomass.Burning", "Stratosphere", "Other"))

metric.data <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region), Metric = "Percentile.95") 
write.csv(metric.data, file = "95th_Percentile_O3_data.csv", row.names = FALSE, quote = FALSE)

title.base <- ": Monthly 95th Percentile O3 for Emissions and Meteorology for Year 2010"

plotting <- function (region, data.frame) {
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

    y.max <- sum(df$Zonal.Mean)
    custom.breaks <- seq(0, 100, by = 2.5)
    plot.title <- paste0(plot.title(region), title.base)
    legend.levels <- unlist(plot.legend.levels(region))
    df$Region <- factor(df$Region, levels = unlist(plot.region.levels(region)))

    p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Month, y = Zonal.Mean, fill = Source))
    p <- p + geom_bar(stat = "identity", width = 0.9)
    p <- p + facet_wrap(~ Region, nrow = 1)
    p <- p + scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = custom.breaks, labels = every.nth(custom.breaks, 2, inverse = TRUE, percent = FALSE))
    p <- p + scale_x_discrete(expand = c(0, 0), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
    p <- p + ylab("O3 (ppbv)") + ggtitle(plot.title)
    p <- p + theme_bw()
    p <- p + theme(panel.border = element_rect(colour = "black"))
    p <- p + theme(panel.grid = element_blank())
    p <- p + theme(legend.key = element_blank())
    p <- p + theme(axis.title.x = element_blank())
    p <- p + theme(legend.title = element_blank())
    p <- p + theme(strip.background = element_blank())
    p <- p + theme(strip.text = element_text(face = "bold"))
    p <- p + theme(plot.title = element_text(face = "bold"))
    p <- p + theme(axis.title.y = element_text(face = "bold"))
    p <- p + scale_fill_manual(values = colours, labels = rev(legend.levels))

    file.name <- paste0(region, "_95th_O3_Contributions.pdf")
    CairoPDF(file = file.name, width = 16, height = 9)
    print(p)
    dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = with.sources)
