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
data.df

total <- data.df %>%
  filter(Source == "Total") %>%
  dplyr::select(-Species, -Source)
total$Metric[total$Metric == "AOT40-D"] <- "AOT40D"
total$Metric[total$Metric == "AOT40-N"] <- "AOT40N"
total

scaled <- total %>%
  spread(Metric, Zonal.Mean) %>%
  mutate(SOMO35 = SOMO35 / 10, AOT40D = AOT40D / 50, AOT40N = AOT40N / 50, W126 = W126 * 15) %>%
  gather(Metric, Zonal.Mean, -Month, -Region) #%>%
scaled$Metric <- factor(scaled$Metric, levels = c("Mean", "MDA8", "Percentile.95", "SOMO35", "W126", "AOT40D", "AOT40N")) 
tbl_df(scaled)

plotting <- function (data.frame, region) {
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    plot.title <- "South Asia: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    plot.title <- "North America: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    plot.title <- "Europe: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    plot.title <- "East Asia: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    plot.title <- "Middle East: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    plot.title <- "Russia, Belarus, Ukraine: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    plot.title <- "Oceans: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    plot.title <- "Rest of the World: Monthly Comparison of O3 Metrics for Emissions and Meteorology for Year 2010\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/10, AOT40D (ppbv.hrs)/50, AOT40N (ppbv.hrs)/50, W126 (ppm.hrs)*15"
  } else {
    stop("No region")
  }

  colours <- c("Mean" = "#e41a1c", "MDA8" = "#377eb8", "Percentile.95" = "#4daf4a", "AOT40D" = "#984ea3", "AOT40N" = "#ff7f00", "SOMO35" = "#f9c500", "W126" = "#a65628")
  custom.breaks <- seq(0, 300, 10)

  p <- ggplot(data = df, aes(x = Month, y = Zonal.Mean, colour = Metric, group = Metric))
  p <- p + geom_point()
  p <- p + geom_line()
  p <- p + facet_wrap(~ Region, nrow =1)
  p <- p + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p <- p + scale_y_continuous(limits = c(0, 300), breaks = custom.breaks, expand = c(0, 0), labels = every_nth(custom.breaks, 3, inverse = TRUE))
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
  p <- p + theme(axis.line.x = element_line(colour = "black"))
  p <- p + theme(axis.line.y = element_line(colour = "black"))
  p <- p + guides(fill = guide_legend(nrow = 1))
  p <- p + scale_colour_manual(values = colours)
  
  file.name <- paste0(region, "_metric_comparisons_lines_total_values.pdf")
  CairoPDF(file = file.name, width = 15, height = 9)
  print(p)
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "RBU", "MDE", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = scaled)
