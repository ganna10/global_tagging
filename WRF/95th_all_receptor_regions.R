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

test <- tbl_df(data.frame(Month = data$Month, Region = data$Region, Metric = data$Metric))
test$Source <- rep("Dummy", nrow(test))
test$Zonal.Mean <- rep(0.02, nrow(test))
test <- test %>% dplyr::select(Month, Region, Source, Zonal.Mean, Metric)

### data for lines
assign.cat <- function (Source) {
  if (Source %in% c("Stratosphere", "SASLBC", "RSTLBC", "OCNLBC", "NAMLBC")) {
    cat <- "cat.1"
  } else if (Source %in% c("BIO", "BMB")) {
    cat <- "cat.2"
  } else {
    cat <- "Others"
  }
  return (cat)
}

lines <- data %>%
  rowwise() %>%
  mutate(Category = assign.cat(Source)) %>%
  filter(Category != "Others") %>%
  group_by(Month, Region, Metric, Category) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
lines$Label <- rep("-----------", nrow(lines))

custom.breaks <- seq(0, 70, by = 5)

colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "UKI" = "#b569b3", "FRA" = "#a6cee3", "GER" = "#b2df8a", "FRA" = "#ff7f00", "ITA" = "#e31a1c", "CEN" = "#6a3d9a", "BNL" = "#1f78b4", "IBE" = "#cab2d6", "RBU" = "#fdbf6f", "SEE" = "#fb9a99", "SCA" = "#b15928", "TCA" = "#33a02c", "BLT" = "#2b9eb3", "BNS" = "#ef6638", "MBS" = "#898989", "BIO" = "#86b650", "BMB" = "#0352cb", "NAMLBC" = "#f9c500", "OCNLBC" = "#77aecc", "RSTLBC" = "#8c1531", "SASLBC" = "#9bb18d")

p <- ggplot()
p <- p + geom_bar(data = data %>% filter(Source != "Total") %>% arrange(desc(Source)), aes(x = Month, y = Zonal.Mean, fill = Source), stat = "identity", width = 0.9)
p <- p + geom_text(data = lines, aes(x = Month, y = Zonal.Mean, label = Label), position = "stack", colour = "white", size = 5)
p
p <- p + facet_wrap(~ Region, nrow = 3, scales = "free_x")
p <- p + scale_y_continuous(expand = c(0, 0), limits = c(0, 70), breaks = custom.breaks, labels = every_nth(custom.breaks, 2, inverse = TRUE))
p <- p + scale_x_discrete(expand = c(0, 0), labels = c("M", "J", "J", "A", "S"))
p <- p + ylab("O3 (ppbv)") + ggtitle("95th Percentile O3 (ppbv)")
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
p <- p + theme(axis.line.x = element_line(colour = "black"))
p <- p + theme(axis.line.y = element_line(colour = "black"))
p <- p + scale_fill_manual(values = colours, breaks = levels(factor(data$Source)))
p <- p + guides(fill = guide_legend(ncol = 1))
p

# CairoPDF(file = "95th_percentile_all_regions.pdf", width = 14, height = 7)
# print(p)
# dev.off()
