setwd("~/Documents//Analysis//2016_HTAP//Aura_plots")

data <- tbl_df(read.csv("input.csv"))
gathered <- data %>%
  gather(Source, Contribution, -Country, -Neigh.Country)

get.neigh <- gathered %>%
  filter(Source == "Neigh") %>%
  rowwise() %>%
  mutate(Source = paste0(Neigh.Country, ".", Source))

get.local <- gathered %>%
  filter(Source == "Local") %>%
  rowwise() %>%
  mutate(Source = paste0(Country, ".", Source))

non.neigh <- gathered %>%
  filter(Source != "Neigh" & Source != "Local")

plot.data <- rbind(non.neigh, get.neigh, get.local)
plot.data <- plot.data %>%
  dplyr::select(-Neigh.Country)
plot.data$Source[plot.data$Source == "GER.Local"] <- "GER"
plot.data$Source[plot.data$Source == "GER.Neigh"] <- "GER"
plot.data$Source[plot.data$Source == "BLT.Local"] <- "BLT"
plot.data$Source[plot.data$Source == "BNL.Local"] <- "BNL"
plot.data$Source[plot.data$Source == "BNL.Neigh"] <- "BNL"
plot.data$Source[plot.data$Source == "CEN.Neigh"] <- "CEN"
plot.data$Source[plot.data$Source == "CEN.Local"] <- "CEN"
plot.data$Source[plot.data$Source == "FRA.Neigh"] <- "FRA"
plot.data$Source[plot.data$Source == "FRA.Local"] <- "FRA"
plot.data$Source[plot.data$Source == "IBE.Local"] <- "IBE"
plot.data$Source[plot.data$Source == "ITA.Neigh"] <- "ITA"
plot.data$Source[plot.data$Source == "ITA.Local"] <- "ITA"
plot.data$Source[plot.data$Source == "RBU.Neigh"] <- "RBU"
plot.data$Source[plot.data$Source == "RBU.Local"] <- "RBU"
plot.data$Source[plot.data$Source == "SCA.Local"] <- "SCA"
plot.data$Source[plot.data$Source == "SEE.Neigh"] <- "SEE"
plot.data$Source[plot.data$Source == "SEE.Local"] <- "SEE"
plot.data$Source[plot.data$Source == "TCA.Neigh"] <- "TCA"
plot.data$Source[plot.data$Source == "TCA.Local"] <- "TCA"
plot.data$Source[plot.data$Source == "UKI.Local"] <- "UKI"

plot.data$Source[plot.data$Source == "RST"] <- "Other Sources"
plot.data$Source[plot.data$Source == "NAM"] <- "North America"
plot.data$Source[plot.data$Source == "ROE"] <- "Rest of Europe"
plot.data$Source[plot.data$Source == "GER"] <- "Germany"
plot.data$Source[plot.data$Source == "BLT"] <- "Baltics"
plot.data$Source[plot.data$Source == "BNL"] <- "BeNeLuxs"
plot.data$Source[plot.data$Source == "CEN"] <- "CEN-Eur"
plot.data$Source[plot.data$Source == "FRA"] <- "France"
plot.data$Source[plot.data$Source == "IBE"] <- "Iberics"
plot.data$Source[plot.data$Source == "ITA"] <- "Italy"
plot.data$Source[plot.data$Source == "RBU"] <- "RuBeUkr"
plot.data$Source[plot.data$Source == "SCA"] <- "Nordics"
plot.data$Source[plot.data$Source == "SEE"] <- "SE-Eur"
plot.data$Source[plot.data$Source == "TCA"] <- "Tur-Cau"
plot.data$Source[plot.data$Source == "UKI"] <- "UK-Ire"

plot.data$Country <- as.character(plot.data$Country)
plot.data$Country[plot.data$Country == "GER"] <- "Germany"
plot.data$Country[plot.data$Country == "BLT"] <- "Baltics"
plot.data$Country[plot.data$Country == "BNL"] <- "BeNeLuxs"
plot.data$Country[plot.data$Country == "CEN"] <- "CEN-Eur"
plot.data$Country[plot.data$Country == "FRA"] <- "France"
plot.data$Country[plot.data$Country == "IBE"] <- "Iberics"
plot.data$Country[plot.data$Country == "ITA"] <- "Italy"
plot.data$Country[plot.data$Country == "RBU"] <- "RuBeUkr"
plot.data$Country[plot.data$Country == "SCA"] <- "Nordics"
plot.data$Country[plot.data$Country == "SEE"] <- "SE-Eur"
plot.data$Country[plot.data$Country == "TCA"] <- "Tur-Cau"
plot.data$Country[plot.data$Country == "UKI"] <- "UK-Ire"

plot.data$Country <- factor(plot.data$Country, levels = c("Iberics", "Italy", "SE-Eur", "UK-Ire", "France", "BeNeLuxs", "Germany", "CEN-Eur", "Nordics", "Baltics", "RuBeUkr", "Tur-Cau"))
plot.data$Source <- factor(plot.data$Source, levels = c("Other Sources", "North America", "Rest of Europe", "Tur-Cau", "RuBeUkr", "Baltics", "Nordics", "CEN-Eur", "Germany", "BeNeLuxs", "France", "UK-Ire", "SE-Eur", "Italy", "Iberics"))

plot.colours <- c("Iberics" = "#1f78b4", "Italy" = "#b2df8a", "SE-Eur" = "#33a02c", "UK-Ire" = "#fb9a99", "France" = "#e31a1c", "BeNeLuxs" = "#fdbf6f", "Germany" = "#ff7f00", "CEN-Eur" = "#cab2d6", "Nordics" = "#ce6283", "Baltics" = "#b15928", "RuBeUkr" = "#1b3b7c", "Tur-Cau" = "#fb8072", "Rest of Europe" = "#6a3d9a", "North America" = "#898989", "Other Sources" = "#a6cee3")

p <- ggplot(plot.data %>% arrange(desc(Source)), aes(x = Country, y = Contribution, fill = Source))
p <- p + geom_bar(stat = "identity")
p <- p + scale_fill_manual(values = plot.colours, limits = levels(factor(plot.data$Source)))
p <- p + theme_tufte()
p <- p + xlab("") + ylab("% Contribution")
p <- p + theme(axis.title.y = element_text(size = 30, face = "bold"))
p <- p + theme(axis.ticks.x = element_blank())
p <- p + scale_x_discrete(expand = c(0, 0))
p <- p + scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 20))
p <- p + theme(axis.text.x = element_text(size = 30, colour = "black", face = "bold", angle = 90))
p <- p + theme(axis.text.y = element_text(size = 28, colour = "black"))
p <- p + theme(legend.title = element_blank())
p <- p + theme(legend.text = element_text(size = 28))
p <- p + theme(legend.key.size = unit(1, "cm"))

CairoPNG(file = "AGU_plot.png", width = 1300, height = 900, units = "px")
print(p)
dev.off()
