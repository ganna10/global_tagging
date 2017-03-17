setwd("~/Documents//Analysis//2016_HTAP//WRF//metric_data")

library("ggplot2")
library("dplyr")
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes")
library("gtable")
library("gridExtra")

get.data <- function (metric) {
  file.name <- paste0(metric, "_O3_data.csv")
  data.df <- read.csv(file = file.name, header = TRUE)
  return (data.df)
}

metrics <- c("95th_percentile", "AOT40D", "AOT40N", "MDA8", "Mean", "SOMO35", "W126")
data.list <- lapply(metrics, get.data)
data.df <- bind_rows(data.list)
data.df$Month <- factor(data.df$Month, levels = c("May", "Jun", "Jul", "Aug", "Sep"))
tbl_df(data.df)

test.plot <- data.df %>%
  filter(Region == "UK; Iceland", Metric %in% c("MDA8", "Mean", "Percentile.95"))
dummy.data <- data.df %>%
  filter(Region == "UK; Iceland", Metric %in% c("SOMO35", "AOT40D", "AOT40N", "W126"))
dummy.data$Zonal.Mean[dummy.data$Zonal.Mean < 1e9 ] <- 0
dummy.data <- unique.data.frame(dummy.data)

plot.data <- rbind(test.plot, dummy.data)

y.max.ppb <- max(plot.data$Zonal.Mean)
y.max.ppb

p <- ggplot(data = plot.data, aes(x = Metric, y = Zonal.Mean, fill = Source))
p <- p + geom_bar(stat = "identity", size = 0.9)
p <- p + facet_wrap(~ Month, nrow = 1)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# AOT plot
aot.data <- data.df %>%
  filter(Region == "UK; Iceland", Metric %in% c("AOT40D", "AOT40N"))
dummy.data.1 <- data.df %>%
  filter(Region == "UK; Iceland", !Metric %in% c("AOT40D", "AOT40N"))
dummy.data.1$Zonal.Mean[dummy.data.1$Zonal.Mean < 1e9 ] <- 0
dummy.data.1 <- unique.data.frame(dummy.data.1)

plot.data.1 <- rbind(aot.data, dummy.data.1)

p1 <- ggplot(plot.data.1, aes(x = Metric, y = Zonal.Mean, fill = Source))
p1 <- p1 + geom_bar(stat = "identity", size = 0.9)
p1 <- p1 + facet_wrap(~ Month, nrow = 1)
p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

# SOMO35 plot
somo.data <- data.df %>%
  filter(Region == "UK; Iceland", Metric == "SOMO35")
dummy.data.2 <- data.df %>%
  filter(Region == "UK; Iceland", Metric != "SOMO35")
dummy.data.2$Zonal.Mean[dummy.data.2$Zonal.Mean < 1e9 ] <- 0
dummy.data.2 <- unique.data.frame(dummy.data.2)

plot.data.2 <- rbind(somo.data, dummy.data.2)

p2 <- ggplot(plot.data.2, aes(x = Metric, y = Zonal.Mean, fill = Source))
p2 <- p2 + geom_bar(stat = "identity", size = 0.9)
p2 <- p2 + facet_wrap(~ Month, nrow = 1)
p2 <- p2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2

# W126 plot
w126.data <- data.df %>%
  filter(Region == "UK; Iceland", Metric == "W126")
dummy.data.3 <- data.df %>%
  filter(Region == "UK; Iceland", Metric != "W126")
dummy.data.3$Zonal.Mean[dummy.data.3$Zonal.Mean < 1e9 ] <- 0
dummy.data.3 <- unique.data.frame(dummy.data.3)

plot.data.3 <- rbind(somo.data, dummy.data.3)

p3 <- ggplot(plot.data.3, aes(x = Metric, y = Zonal.Mean, fill = Source))
p3 <- p3 + geom_bar(stat = "identity", size = 0.9)
p3 <- p3 + facet_wrap(~ Month, nrow = 1)
p3 <- p3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3

CairoPDF(file = "type1.pdf", width = 14, height = 9)
print(grid.draw(rbind(ggplotGrob(p), ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last")))
dev.off()
