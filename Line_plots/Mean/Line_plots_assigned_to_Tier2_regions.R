library("ggplot2")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes") 
library("grid") 

# use my functions 
source ("/work/users/jco/tagging_global/Scripts/Tier2_Analysis/R_functions/my_functions.R")

data <- tbl_df(read.csv(file = "Mean_O3_data.csv"))
Total <- data %>% filter(Source == "Total") %>% dplyr::select(Month, Region, Source, Zonal.Mean)
summed.data <- data %>%
    dplyr::select(-Metric) %>%
    filter(Source != "Total") %>%
    group_by(Month, Region, Source) %>%
    summarise(Zonal.Mean = sum(Zonal.Mean)) %>%
    ungroup()
all.data <- rbind(summed.data, Total)
all.data$Month <- factor(all.data$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

title.base <- ": Monthly Mean for Emissions and Meteorology for Year 2010\nTotal Ozone Mean (ppbv) from NOx Sources in Tier 2 Receptor Regions"

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
  
  trans <- df %>% filter(!Source %in% c("Local", "Stratosphere", "Other", "Total")) 
  #trans <- df %>% filter(!Source %in% c("Local", "Stratosphere", "Biomass.Burning", "Biogenic", "Other", "Total")) 
  non.trans <- df %>% filter(Source %in% c("Local", "Stratosphere", "Other", "Total"))
  #non.trans <- df %>% filter(Source %in% c("Local", "Stratosphere", "Biomass.Burning", "Biogenic", "Other", "Total"))

  transported <- trans %>%
    spread(Source, Zonal.Mean) %>%
    ungroup %>%
    mutate(Transported = rowSums(.[3:9])) %>% #check that this is the correct sum!!
    dplyr::select(Month, Region, Transported) %>%
    gather(Source, Zonal.Mean, -Month, -Region)

  data <- rbind(non.trans, transported)
  data$Source <- factor(data$Source, levels = c("Total", "Local", "Transported", "Stratosphere", "Other"))
  #data$Source <- factor(data$Source, levels = c("Total", "Local", "Transported", "Stratosphere", "Biomass.Burning", "Biogenic", "Other"))
  data$Month <- factor(data$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  colours <- c("Total" = "#000000", "Local" = "#377eb8", "Stratosphere" = "#4daf4a", "Transported" = "#984ea3", "Other" = "#ff7f00", "Biogenic" = "#0e5c28", "Biomass.Burning" = "#2b9eb3")

  y.max <- max(data$Zonal.Mean)
  custom.breaks <- seq(0, y.max, by = 5)
  plot.title <- paste0(plot.title(region), title.base)
  data$Region <- factor(data$Region, levels = unlist(plot.region.levels(region)))
  
  p <- ggplot(data = data, aes(x = Month, y = Zonal.Mean, colour = Source, group = Source))
  p <- p + geom_point()
  p <- p + geom_line()
  p <- p + facet_wrap(~ Region, nrow = 1)
  p <- p + ylab("O3 Mean (ppbv)")
  p <- p + ggtitle(plot.title)
  p <- p + theme_bw()
  p <- p + scale_y_continuous(breaks = custom.breaks, labels = every.nth(custom.breaks, 2, inverse = TRUE, percent = FALSE))
  p <- p + theme(panel.border = element_rect(colour = "black"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + theme(legend.key = element_blank())
  p <- p + theme(axis.text = element_text(colour = "black"))
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(strip.text = element_text(face = "bold"))
  p <- p + theme(strip.background = element_blank())
  p <- p + scale_colour_manual(values = colours, limits = levels(factor(data$Source)))
  p <- p + theme(legend.title = element_blank())
  p <- p + theme(plot.title = element_text(size = 12, face = "bold"))
  p <- p + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  
  legend.levels <- unlist(transported.legend(region))
  trans$Source <- factor(trans$Source, levels = unlist(transported.levels(region)))
  trans$Region <- factor(trans$Region, levels = unlist(plot.region.levels(region)))
  country.colours = c("Total" = "#000000", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#fdbf6f", "South.East.Asia" = "#fb9a99", "North.Africa" = "#b15928", "Mexico.Central.America" = "#33a02c", "Central.Asia" = "#898989")
  y.max.1 <- max(trans$Zonal.Mean)
  custom.breaks.1 <- seq(0, y.max.1, by = 1)
  
  p2 <- ggplot(data = trans, aes(x = Month, y = Zonal.Mean, colour = Source, group = Source))
  p2 <- p2 + geom_point()
  p2 <- p2 + geom_line()
  p2 <- p2 + facet_wrap(~ Region, nrow = 1)
  p2 <- p2 + theme_bw()
  p2 <- p2 + theme(panel.border = element_rect(colour = "black"))
  p2 <- p2 + theme(panel.grid = element_blank())
  p2 <- p2 + theme(legend.key = element_blank())
  p2 <- p2 + scale_y_continuous(breaks = custom.breaks.1, labels = every.nth(custom.breaks.1, 2, inverse = TRUE, percent = FALSE))
  p2 <- p2 + theme(axis.text = element_text(colour = "black"))
  p2 <- p2 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p2 <- p2 + scale_colour_manual(values = country.colours, labels = legend.levels)
  p2 <- p2 + theme(axis.title = element_blank())
  p2 <- p2 + ggtitle("Monthly Mean O3 (ppbv) of Transported NOx Sources from Source Regions to Tier 2 Receptor Regions")
  p2 <- p2 + theme(legend.title = element_blank())
  p2 <- p2 + theme(strip.background = element_blank())
  p2 <- p2 + theme(strip.text = element_text(face = "bold"))
  p2 <- p2 + theme(plot.title = element_text(size = 12, face = "bold"))
  
  file.name <- paste0(region, "_Mean_O3_Yearly_Cycle_Total_plus_Contributions_plus_Country_Tier2.pdf")
  CairoPDF(file = file.name, width = 16, height = 9)
  print(grid.draw(rbind(ggplotGrob(p), ggplotGrob(p2), size = "last")))
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = all.data)
