setwd("~/Documents//Analysis//2016_HTAP//Aura_plots")

get.data <- function (month, metric) {
  month.txt <- month.abb[month]
  
  file.name <- paste0(tolower(month.txt), ".", metric, ".txt")
  data <- read.csv(file.name, header = TRUE, sep = " ")
  data$Receptor <- receptor.column # add receptor columns 
  arranged <- data %>%
    gather(Source, Value, -Receptor) %>%
    mutate(Month = month.txt, Metric = metric)
  return (arranged)
}

plot.regions <- function (df, region) {
  filtered <- df %>%
    filter(Receptor == region)
  plot.title <- paste0("Metrics from different sources in ", region)
  file.name <- paste0(region, "_metrics.png")
  
  p <- ggplot(filtered, aes(x = Month, y = Value, colour = Metric, group = Metric))
  p <- p + geom_point()
  p <- p + geom_line()
  p <- p + facet_wrap(~ Source, nrow = 4, scales = "free")
  p <- p + theme(panel.background = element_blank())
  p <- p + theme(strip.background = element_blank())
  p <- p + theme(axis.line = element_line(colour = "black"))
  p <- p + theme(axis.title = element_blank())
  p <- p + theme(legend.key = element_blank())
  p <- p + theme(axis.text = element_text(colour = "black"))
  p <- p + theme(strip.text = element_text(face = "bold"))
  p <- p + theme(legend.title = element_text(face = "bold"))
  p <- p + ggtitle(plot.title)
  p <- p + theme(plot.title = element_text(face = "bold", hjust = 0.5))
  p
  
  CairoPNG(file = file.name, width = 700, height = 700)
  print(p)
  dev.off()
}

receptor.column <- c("MBS", "IBE", "ITA", "SEE", "UKI", "FRA", "BNL", "GER", "CEN", "BNS", "SCA", "BLT", "RBU", "TCA")
months <- seq(5, 9, by = 1)
metrics <- c("mda8", "mda1", "mean", "somo35", "somo10", "perc95", "perc75", "perc50", "perc25", "perc05", "aotd", "aotn", "w126", "90")
all.data <- NULL

for (i in metrics) {
  data.list <- lapply(months, get.data, metric = i)
  data.df <- bind_rows(data.list)
  all.data <- rbind(all.data, data.df)
}
levels(factor(all.data$Metric))
all.data$Month <- factor(all.data$Month, levels = c("May", "Jun", "Jul", "Aug", "Sep"))
no.rest <- all.data %>%
  filter(Source != "RST")
no.rest$Source <- factor(no.rest$Source, levels = c("MBS", "IBE", "ITA", "SEE", "UKI", "FRA", "BNL", "GER", "CEN", "BNS", "SCA", "BLT", "RBU", "TCA", "STR", "BIO", "BMB", "NAM", "ASI", "OCN"))

lapply(receptor.column, plot.regions, df = no.rest)