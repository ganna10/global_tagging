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
p <- p + ggtitle("Yearly Cycle of Total Ozone Mixing Ratios and from NOx Sources")
p <- p + scale_colour_manual(values = colours, limits = levels(factor(with.sources$Source)))
p <- p + scale_y_continuous(limits = c(0, 65))
p <- p + theme(legend.title = element_blank())
p <- p + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
p

CairoPDF(file = "O3_yearly_cycle_sources.pdf", width = 10, height = 7)
print(p)
dev.off()

# contributions to total
contributions <- with.sources %>%
  spread(Source, Zonal.Mean) %>%
  gather(Source, Mixing.Ratio, -Month, -Region, -Total) %>%
  mutate(Contribution = Mixing.Ratio / Total) 

contributions$Source <- factor(contributions$Source, levels = c("Other", "Natural", "Transported", "Local"))

p1 <- ggplot(data = contributions %>% arrange(Source), aes(x = Month, y = Contribution, fill = Source))
p1 <- p1 + geom_bar(stat = "identity")
p1 <- p1 + facet_wrap(~ Region, nrow = 1)
p1 <- p1 + plot_theme()
p1 <- p1 + scale_y_continuous(limits = c(0, 1), label = percent, expand = c(0, 0))
p1 <- p1 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), expand = c(0, 0))
p1 <- p1 + scale_fill_manual(values = colours, limits = levels(factor(contributions$Source)))
p1 <- p1 + ylab("Percent Contribution to Total O3")
p1 <- p1 + theme(axis.title.x = element_blank())
p1 <- p1 + ggtitle("Yearly Cycle of Percent Contributions of NOx Sources to Total Ozone")
p1 <- p1 + theme(legend.title = element_blank())
p1

CairoPDF(file = "O3_Yearly_Cycle_Percent_Contributions.pdf", width = 10, height = 7)
print(p1)
dev.off()

# grid.arrange(p, p1, nrow = 2) # doesn't align y-axes
grid.newpage()
grid.draw(rbind(ggplotGrob(p), ggplotGrob(p1), size = "last"))

CairoPDF(file = "O3_Yearly_Cycle_Total_plus_Contributions.pdf", width = 13, height = 7)
print(grid.draw(rbind(ggplotGrob(p), ggplotGrob(p1), size = "last")))
dev.off()

get.data.type <- function (Species) {
  if (str_detect(Species, 'ANT')) {
    type = "Anthropogenic"
  } else if (str_detect(Species, 'SOI')) {
    type = "Biogenic"
  } else if (str_detect(Species, 'BMB')) {
    type = "Biomass.Burning" 
  } else {
    type = "Error"
  }
  return (type)
}

# look at components of local NOx sources to O3
sources <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  filter(Source %in% c("Local", "Transported")) %>% 
  rowwise() %>%
  mutate(Type = get.data.type(Species)) %>%
  group_by(Month, Region, Type, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean)) %>%
  spread(Type, Zonal.Mean) %>%
  mutate(Total = Anthropogenic + Biogenic + Biomass.Burning) %>%
  gather(Type, Mixing.Ratio, -Month, -Region, -Source)

sources$Type <- factor(sources$Type, levels = c("Total", "Anthropogenic", "Biogenic", "Biomass.Burning"))

type.colours <- c("Total" = "#000000", "Anthropogenic" = "#80b1d3", "Biogenic" = "#b3d369", "Biomass.Burning" = "#fb8072")

p2 <- ggplot(data = sources, aes(x = Month, y = Mixing.Ratio, colour = Type, group = Type))
p2 <- p2 + geom_path()
p2 <- p2 + geom_point()
p2 <- p2 + facet_grid(Source ~ Region)
p2 <- p2 + plot_theme()
p2 <- p2 + scale_colour_manual(values = type.colours, labels = c("Total", "Anthropogenic", "Biogenic", "Biomass Burning"))
p2 <- p2 + theme(legend.title = element_blank())
p2 <- p2 + theme(axis.title.x = element_blank())
p2 <- p2 + ylab("O3 Mixing Ratio (ppbv)")
p2 <- p2 + ggtitle("O3 Mixing Ratios of Local and Transported Sources in Receptor Regions")
p2 <- p2 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
p2 <- p2 + theme(legend.position = "top")
p2

CairoPDF(file = "Local_Transported_O3_Mixing_Ratios_in_Year.pdf", width = 13, height = 7)
print(p2)
dev.off()

# Contributions of Sources to Local and Transported O3
l.t.contributions <- sources %>%
  spread(Type, Mixing.Ratio) %>%
  gather(Type, Mixing.Ratio, -Month, -Region, -Total, -Source) %>%
  mutate(Contribution = Mixing.Ratio / Total) 

l.t.contributions$Type <- factor(l.t.contributions$Type, levels = c("Biogenic", "Biomass.Burning", "Anthropogenic"))

p3 <- ggplot(data = l.t.contributions, aes(x = Month, y = Contribution, fill = Type))
p3 <- p3 + geom_bar(stat = "identity")
p3 <- p3 + facet_grid(Source ~ Region)
p3 <- p3 + plot_theme()
p3 <- p3 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), expand = c(0, 0))
p3 <- p3 + scale_fill_manual(values = type.colours, labels = c("Biogenic", "Biomass Burning", "Anthropogenic"))
p3 <- p3 + scale_y_continuous(limits = c(0, 1.01), label = percent, expand = c(0, 0))
p3 <- p3 + ylab("Percent Contributions of NOx Sources to Local and Transported O3")
p3 <- p3 + theme(axis.title.x = element_blank())
p3 <- p3 + ggtitle("Yearly Cycle of Percent Contributions of NOx Sources to Local and Transported Ozone")
p3 <- p3 + theme(legend.title = element_blank())
p3

CairoPDF(file = "Local_Transported_O3_Contributions_in_Year.pdf", width = 13, height = 7)
print(p3)
dev.off()

# transport from which regions influence local ozone
get.emission.source <- function (Species) {
  if (str_detect(Species, 'EAS')) {
    Source = "East.Asia" 
  } else if (str_detect(Species, 'EUR')) {
    Source = "Europe"
  } else if (str_detect(Species, 'NAM')) {
    Source = "North.America"
  } else if (str_detect(Species, 'MDE')) {
    Source = "Middle.East"
  } else if (str_detect(Species, 'RBU')) {
    Source = "Russia"
  } else if (str_detect(Species, 'SAS')) {
    Source = "South.Asia"
  } else if (str_detect(Species, 'OCN')) {
    Source = "Ocean"
  } else if (str_detect(Species, 'RST')) {
    Source = "Rest"
  } else {
    Source = "Error"
  }
  return (Source)
}

transport <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  filter(Source == "Transported") %>%
  dplyr::select(-Source) %>%
  rowwise() %>%
  mutate(Source = get.emission.source(Species)) %>%
  group_by(Month, Region, Source) %>%
  summarise(Mixing.Ratio = sum(Zonal.Mean)) %>%
  spread(Source, Mixing.Ratio)
transport[is.na(transport)] <- 0
transport <- transport %>%
  mutate(Total = North.America + Europe + Middle.East + Russia + South.Asia + East.Asia + Ocean + Rest) %>%
  gather(Source, Mixing.Ratio, -Month, -Region)
transport

transport$Source <- factor(transport$Source, levels = c("Total", "North.America", "Europe", "Middle.East", "Russia", "South.Asia", "East.Asia", "Ocean", "Rest"))

country.colours = c("Total" = "#000000", "North.America" = "#a6cee3", "Europe" = "#b2df8a", "Middle.East" = "#ff7f00", "Russia" = "#e31a1c", "South.Asia" = "#6a3d9a", "East.Asia" = "#1f78b4", "Ocean" = "#cab2d6", "Rest" = "#33a02c")

p4 <- ggplot(data = transport, aes(x = Month, y = Mixing.Ratio, colour = Source, group = Source))
p4 <- p4 + geom_point()
p4 <- p4 + geom_path()
p4 <- p4 + facet_wrap(~ Region, nrow = 2, scales = "free_y")
p4 <- p4 + plot_theme()
p4 <- p4 + theme(legend.title = element_blank())
p4 <- p4 + theme(axis.title.x = element_blank())
p4 <- p4 + ylab("O3 Mixing Ratio (ppbv)")
p4 <- p4 + ggtitle("O3 Mixing Ratios of Transported Sources in Receptor Regions")
p4 <- p4 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
p4 <- p4 + scale_colour_manual(values = country.colours, labels = c("Total", "North America", "Europe", "Middle East", "Russia", "South Asia", "East Asia", "Ocean", "Rest"))
p4

CairoPDF(file = "Yearly_Transported_O3_from_countries_to_Receptors.pdf", width = 13, height = 7)
print(p4)
dev.off()

transported.contributions <- transport %>%
  spread(Source, Mixing.Ratio) %>%
  gather(Source, Mixing.Ratio, -Month, -Region, -Total) %>%
  mutate(Contribution = Mixing.Ratio / Total) %>%
  filter(Mixing.Ratio != 0)
transported.contributions

transported.contributions$Source <- factor(transported.contributions$Source, levels = c("Rest", "Ocean", "East.Asia", "South.Asia", "Russia", "Middle.East", "Europe", "North.America"))

p5 <- ggplot(data = transported.contributions, aes(x = Month, y = Contribution, fill = Source))
p5 <- p5 + geom_bar(stat = "identity")
p5 <- p5 + facet_wrap( ~ Region, nrow = 2)
p5 <- p5 + plot_theme()
p5 <- p5 + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), expand = c(0, 0))
p5 <- p5 + scale_fill_manual(values = country.colours, labels = c("Rest", "Ocean", "East Asia", "South Asia", "Russia", "Middle East", "Europe", "North America"))
p5 <- p5 + scale_y_continuous(limits = c(0, 1.01), label = percent, expand = c(0, 0))
p5 <- p5 + ylab("Percent Contributions of NOx Sources to Transported O3")
p5 <- p5 + theme(axis.title.x = element_blank())
p5 <- p5 + ggtitle("Yearly Cycle of Percent Contributions of Transported NOx Sources in Receptor Regions")
p5 <- p5 + theme(legend.title = element_blank())
p5

CairoPDF(file = "Yearly_Transported_O3_from_countries_to_Receptors_contributions.pdf", width = 13, height = 7)
print(p5)
dev.off()
