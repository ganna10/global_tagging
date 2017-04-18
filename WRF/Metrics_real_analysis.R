setwd("~/Documents//Analysis//2016_HTAP//WRF")

# library("ggplot2")
# library("dplyr")
# library("tidyr")
# library("Cairo")
# library("stringr")
# library("ggthemes") 

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) {
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

data <- tbl_df(read.csv("/work/users/jco/WRF-HTAP_Analysis/95th/95th_percentile_O3_data.csv"))
data$Month <- factor(data$Month, levels = c("May", "Jun", "Jul", "Aug", "Sep"))
data$Region <- factor(data$Region, levels = c("Atlantic", "UK; Iceland", "Benelux", "France", "Iberia", "Scandinavia", "Baltic Sea", "Germany", "Central E Europe", "Italy", "Mediterranean + Black Sea", "Poland; NE Europe", "Russia", "Greece", "Turkey"))
data$Source <- factor(data$Source, levels = c("Total", "Local", "UKI", "FRA", "GER", "ITA", "CEN", "BNL", "IBE", "RBU", "SEE", "SCA", "TCA", "BLT", "BNS", "MBS","BIO", "BMB", "NAMLBC", "OCNLBC", "RSTLBC", "SASLBC", "Stratosphere"))

mjj.somo <- data %>%
  filter(Month %in% c("May", "June", "July")) %>%
  group_by(Region, Source, Metric) %>%
  summarise(AOT40D = sum(Zonal.Mean)) %>%
  mutate(Time = "MJJ")
mjj.somo

total.somo <- data %>%
  group_by(Region, Source, Metric) %>%
  summarise(AOT40D = sum(Zonal.Mean)) %>%
  mutate(Time = "All.Year")
total.somo

all.data <- rbind(mjj.somo, total.somo)

y.max <- max(mjj.somo$AOT40D)
custom.breaks <- seq(0, y.max, by = 100)

colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "UKI" = "#b569b3", "FRA" = "#a6cee3", "GER" = "#b2df8a", "FRA" = "#ff7f00", "ITA" = "#e31a1c", "CEN" = "#6a3d9a", "BNL" = "#1f78b4", "IBE" = "#cab2d6", "RBU" = "#fdbf6f", "SEE" = "#fb9a99", "SCA" = "#b15928", "TCA" = "#33a02c", "BLT" = "#2b9eb3", "BNS" = "#ef6638", "MBS" = "#898989", "BIO" = "#86b650", "BMB" = "#0352cb", "NAMLBC" = "#f9c500", "OCNLBC" = "#77aecc", "RSTLBC" = "#8c1531", "SASLBC" = "#9bb18d")

p <- ggplot()
p <- p + geom_bar(data = all.data %>% filter(Time == "MJJ", Source != "Total") %>% arrange(desc(Source)), aes(x = Region, y = AOT40D, fill = Source), stat = "identity", width = 0.9)
# p <- p + facet_wrap(~ Time, nrow = 1)
p <- p + scale_y_continuous(expand = c(0, 0), breaks = custom.breaks, labels = every_nth(custom.breaks, 2, inverse = TRUE))
p <- p + scale_x_discrete(expand = c(0, 0))
p <- p + ylab("AOT40D (ppbv.hrs)") + ggtitle("AOT40D (ppbv.hrs)")
p <- p + theme_bw()
p <- p + theme(panel.border = element_blank()) #element_rect(colour = "black"))
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
p <- p + scale_fill_manual(values = colours)
p <- p + guides(fill = guide_legend(ncol = 1))
p

# CairoPDF(file = "MJJ_AOT40D_all_regions.pdf", width = 16, height = 9)
# print(p)
# dev.off()
