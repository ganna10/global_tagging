setwd("~/Documents//Analysis//2016_HTAP//WRF//metric_data")

library("ggplot2")
library("dplyr")
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")

get.data <- function (metric) {
  file.name <- paste0(metric, "_O3_data.csv")
  data.df <- read.csv(file = file.name, header = TRUE)
  return (data.df)
}

metrics <- c("95th_percentile", "AOT40D", "AOT40N", "MDA8", "Mean", "SOMO35", "W126")
data.list <- lapply(metrics, get.data)
data.df <- tbl_df(bind_rows(data.list))
data.df$Month <- factor(data.df$Month, levels = c("May", "Jun", "Jul", "Aug", "Sep"))
tbl_df(data.df)

data.df %>%
  filter(Source != "Total") %>%
  group_by(Metric) %>%
  summarise(Max = max(Zonal.Mean))

scaled <- data.df %>%
  filter(Source != "Total") %>%
  spread(Metric, Zonal.Mean) %>%
  mutate(SOMO35 = SOMO35 / 5, AOT40D = AOT40D / 30, AOT40N = AOT40N / 10, W126 = W126 / 50) %>%
  gather(Metric, Zonal.Mean, -Month, -Region, -Source)
scaled %>%
  group_by(Metric) %>%
  summarise(Max = max(Zonal.Mean))
scaled$Source <- factor(scaled$Source, levels = c("Local", "UKI", "FRA", "GER", "ITA", "CEN", "BNL", "IBE", "RBU", "SEE", "SCA", "TCA", "BLT", "BNS", "MBS","BIO", "BMB", "NAMLBC", "OCNLBC", "RSTLBC", "SASLBC", "Stratosphere"))

plotting <- function (data.frame, region) {
  df <- data.frame %>% filter(Region == region)
  file.name <- paste0(region, "_metrics_comparison_O3.pdf")
  plot.title <- paste0("Comparison of O3 metrics (ppbv) in ", region, " with Contributions from different Emission Sources\n95th Percentile, MDA8, Mean (ppbv), SOMO35 (ppbv.days)/5, AOT40D (ppbv.hrs)/30, AOT40N (ppbv.hrs)/10, W126 (ppm.hrs)/50")
  
  colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "UKI" = "#b569b3", "FRA" = "#a6cee3", "GER" = "#b2df8a", "FRA" = "#ff7f00", "ITA" = "#e31a1c", "CEN" = "#6a3d9a", "BNL" = "#1f78b4", "IBE" = "#cab2d6", "RBU" = "#fdbf6f", "SEE" = "#fb9a99", "SCA" = "#b15928", "TCA" = "#33a02c", "BLT" = "#2b9eb3", "BNS" = "#ef6638", "MBS" = "#898989", "BIO" = "#86b650", "BMB" = "#0352cb", "NAMLBC" = "#f9c500", "OCNLBC" = "#77aecc", "RSTLBC" = "#8c1531", "SASLBC" = "#9bb18d")
  
  y.max <- sum(df$Zonal.Mean)
  
  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Metric, y = Zonal.Mean, fill = Source))
  p <- p + geom_bar(stat = "identity", size = 0.9)
  p <- p + facet_wrap(~ Month, nrow = 1)
  p <- p + scale_y_continuous(expand = c(0, 0), breaks = seq(0, y.max, by = 5))
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
  p <- p + guides(fill = guide_legend(ncol = 1))
  p <- p + scale_fill_manual(values = colours)
  
  CairoPDF(file = file.name, width = 14, height = 7)
  print(p)
  dev.off()
}

emission.regions <- levels(factor(scaled$Region))
lapply(emission.regions, plotting, data.frame = scaled)
