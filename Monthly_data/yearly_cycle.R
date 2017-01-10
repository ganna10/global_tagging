setwd("~/Documents//Analysis//2016_HTAP//Monthly_data")

assigned.df <- read.csv(file = "Monthly_assigned_MDA8_to_regions.csv", header = TRUE)

zonal.mean <- assigned.df %>%
  group_by(Species, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio)) 
tbl_df(zonal.mean)

zonal.mean$Month <- factor(zonal.mean$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
zonal.mean$Region <- factor(zonal.mean$Region, levels = c("North America", "Europe", "Middle East", "Russia", "South Asia", "East Asia", "Ocean", "Rest"))

p <- ggplot(data = zonal.mean %>% filter(Species == "O3"), aes(x = Month, y = Zonal.Mean, group = Species))
p <- p + geom_point()
p <- p + geom_line()
p <- p + facet_wrap(~ Region, scales = "free", nrow = 2)
p <- p + plot_theme()
p <- p + ylab("Mixing Ratio (ppbv)")
p <- p + theme(axis.title.x = element_blank())
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
p <- p + ggtitle("Yearly Cycle of Ozone")
p

CairoPDF(file = "O3_Yearly_cycle.pdf", width = 10, height = 7)
print(p)
dev.off()

# tagged less non-tagged
non.tagged <- zonal.mean %>% 
  filter(Species == "O3")
non.tagged$Species <- as.character(non.tagged$Species)

tagged <- zonal.mean %>%
  filter(Species != "O3") %>%
  group_by(Month, Region) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean)) %>%
  mutate(Species = "Tagged.O3")
tagged <- tagged[,c(4, 1, 2, 3)]

check <- rbind(non.tagged, tagged)
diff <- check %>%
  spread(Species, Zonal.Mean) %>%
  mutate(Diff = O3 - Tagged.O3) %>%
  dplyr::select(-O3, -Tagged.O3)
diff$Month <- factor(diff$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
diff$Region <- factor(diff$Region, levels = c("North America", "Europe", "Middle East", "Russia", "South Asia", "East Asia", "Ocean", "Rest"))

ggplot(data = diff, aes(x = Month, y = Diff)) + geom_bar(stat = "identity") + plot_theme() + facet_wrap(~ Region, nrow = 2, scales = "free")

# assign to total, local, transported, natural, other
get.source <- function (species, Region) {
  # convert Region to tag format for matching
  if (Region == "Europe") {
    match.Region = "EUR"
  } else if (Region == "East Asia") {
    match.Region = "EAS"
  } else if (Region == "South Asia") {
    match.Region = "SAS"
  } else if (Region == "Middle East") {
    match.Region = "MDE"
  } else if (Region == "North America") {
    match.Region = "NAM"
  } else if (Region == "Ocean") {
    match.Region = "OCN"
  } else if (Region == "Rest") {
    match.Region = "RST"
  } else if (Region == "Russia") {
    match.Region = "RBU"
  }
    
  if (species == "O3") { # total ozone
    source.emiss = "Total"
  } else if (str_detect(species, 'LGT') | str_detect(species, 'STR')) { # natural emissions: lightning and stratosphere
    source.emiss = "Natural"
  } else if (str_detect(species, 'INI') | str_detect(species, 'AIR') | str_detect(species, 'XTR')) { # other sources: aircraft, initial conditions and chemical source
    source.emiss = "Other"
  } else if (str_detect(species, match.Region)) { # check for local or transported emissions
    source.emiss = "Local"
  } else if (!str_detect(species, match.Region)) { # check for local or transported emissions
    source.emiss = "Transported"
  } else {
    source.emiss = "No Match"
  }
  return (source.emiss)
}

with.sources <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
with.sources

with.sources$Source <- factor(with.sources$Source, levels = c("Total", "Local", "Transported", "Natural", "Other"))
colours <- c("Total" = "#000000", "Local" = "#377eb8", "Natural" = "#4daf4a", "Transported" = "#984ea3", "Other" = "#ff7f00")

p <- ggplot(data = with.sources, aes(x = Month, y = Zonal.Mean, colour = Source, group = Source))
p <- p + geom_point()
p <- p + geom_path()
p <- p + facet_wrap(~ Region, nrow = 1)
p <- p + plot_theme()
p <- p + ylab("O3 Mixing Ratio (ppbv)")
p <- p + theme(axis.title.x = element_blank())
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
p <- p + ggtitle("Yearly Cycle of Total Ozone Mixing Ratios and NOx Sources")
p <- p + scale_colour_manual(values = colours, limits = levels(factor(with.sources$Source)))
p <- p + scale_y_continuous(limits = c(0, 65))
p <- p + theme(legend.title = element_blank())
p

CairoPDF(file = "O3_yearly_cycle_sources.pdf", width = 10, height = 7)
print(p)
dev.off()

# contributions to total
contributions <- with.sources %>%
  spread(Source, Zonal.Mean) %>%
  gather(Source, Mixing.Ratio, -Month, -Region, -Total) %>%
  mutate(Contribution = Mixing.Ratio / Total) 

contributions %>%
  filter(Month == "Aug", Region == "Middle East") 

contributions$Source <- factor(contributions$Source, levels = c("Other", "Natural", "Transported", "Local"))

p1 <- ggplot(data = contributions %>% arrange(Source), aes(x = Month, y = Contribution, fill = Source))
p1 <- p1 + geom_bar(stat = "identity")
p1 <- p1 + facet_wrap(~ Region, nrow = 1)
p1 <- p1 + plot_theme()
p1 <- p1 + scale_y_continuous(label = percent, expand = c(0, 0))
p1 <- p1 + scale_x_discrete(expand = c(0, 0))
p1 <- p1 + scale_fill_manual(values = colours, limits = levels(factor(contributions$Source)))
p1 <- p1 + ylab("Percent Contribution to Total O3")
p1 <- p1 + theme(axis.title.x = element_blank())
p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
p1 <- p1 + ggtitle("Yearly Cycle of Percent Contributions of NOx Sources to Total Ozone")
p1 <- p1 + theme(legend.title = element_blank())
p1

CairoPDF(file = "O3_Yearly_Cycle_Percent_Contributions.pdf", width = 10, height = 7)
print(p1)
dev.off()

# grid.arrange(p, p1, nrow = 2) # doesn't align y-axes
grid.newpage()
grid.draw(rbind(ggplotGrob(p), ggplotGrob(p1), size = "last"))

CairoPDF(file = "O3_Yearly_Cycle_Total_plus_Contributions.pdf", width = 10, height = 7)
print(grid.draw(rbind(ggplotGrob(p), ggplotGrob(p1), size = "last")))
dev.off()
