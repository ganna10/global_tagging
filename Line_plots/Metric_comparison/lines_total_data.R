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

total <- data.df %>% 
  filter(Source == "Total") %>% 
  dplyr::select(-Species, -Source)
total$Metric[total$Metric == "AOT40-D"] <- "AOT40D"
total$Metric[total$Metric == "AOT40-N"] <- "AOT40N" 

scaled <- total %>%
  spread(Metric, Zonal.Mean) %>%
  mutate(SOMO35 = SOMO35 / 10, AOT40D = AOT40D / 50, AOT40N = AOT40N / 50, W126 = W126 * 15) %>%
  gather(Metric, Zonal.Mean, -Month, -Region)
scaled$Metric <- factor(scaled$Metric, levels = c("Mean", "MDA8", "Percentile.95", "SOMO35", "W126", "AOT40D", "AOT40N")) 

colours <- c("Mean" = "#e41a1c", "MDA8" = "#377eb8", "Percentile.95" = "#4daf4a", "AOT40D" = "#984ea3", "AOT40N" = "#ff7f00", "SOMO35" = "#f9c500", "W126" = "#a65628") 

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

  plot.title <- paste0(plot.title(region), title.base)
  legend.levels <- unlist(plot.legend.levels(region))
  df$Region <- factor(df$Region, levels = unlist(plot.region.levels(region)))
  custom.breaks <- seq(0, 300, 10)

  p <- ggplot(data = df, aes(x = Month, y = Zonal.Mean, colour = Metric, group = Metric))
  p <- p + geom_point()
  p <- p + geom_line()
  p <- p + facet_wrap(~ Region, nrow =1)
  p <- p + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p <- p + scale_y_continuous(limits = c(0, 300), breaks = custom.breaks, expand = c(0, 0), labels = every.nth(custom.breaks, 2, inverse = TRUE, percent = FALSE))
  p <- p + ggtitle(plot.title)
  p <- p + theme_bw()
  p <- p + theme(panel.border = element_rect(colour = "black"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + theme(legend.key = element_blank())
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(legend.title = element_blank())
  p <- p + theme(strip.background = element_blank())
  p <- p + theme(strip.text = element_text(face = "bold"))
  p <- p + theme(plot.title = element_text(face = "bold"))
  p <- p + scale_colour_manual(values = colours)
  
  file.name <- paste0(region, "_metric_comparisons_lines_total_values.pdf")
  CairoPDF(file = file.name, width = 16, height = 9)
  print(p)
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "RBU", "MDE", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = scaled)
