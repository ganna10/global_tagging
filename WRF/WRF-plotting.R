setwd("~/Documents//Analysis//2016_HTAP//WRF")

library("ggplot2")
library("dplyr")
library("tidyr")
library("Cairo")
library("stringr")
library("ggthemes") 

get.data <- function (month) {
  month.text <- month.abb[month]
  file.name <- paste0("/work/users/jco/WRF-HTAP_Analysis/95th/", month.text, "_assigned_to_regions.csv")
  file.name <- paste0(month.text, "_assigned_to_regions.csv")
  data.df <- read.csv(file = file.name, header = TRUE)
  return(data.df)
}

# assign to total, local, transported, natural, other
get.source <- function (species, Region) {
  # convert Region to tag format for matching
  if (str_detect(Region, "Atlantic")) {
    match.Region = "OCNLBC"
  } else if (str_detect(Region, "Baltic")) {
    match.Region = "BLT"
  } else if (str_detect(Region, "Benelux")) {
    match.Region = "BNL"
  } else if (str_detect(Region, "Central E Europe")) {
    match.Region = "CEN"
  } else if (str_detect(Region, "France")) {
    match.Region = "FRA"
  } else if (str_detect(Region, "Germany")) {
    match.Region = "GER"
  } else if (Region == "Greece") {
    match.Region = "SEE"
  } else if (Region == "Iberia") {
    match.Region = "IBE"
  } else if (Region == "Italy") {
    match.Region = "ITA"
  } else if (str_detect(Region, "Mediterranean")) {
    match.Region = "MBS"
  } else if (str_detect(Region, "Poland")) {
    match.Region = "BNS"
  } else if (Region == "Russia") {
    match.Region = "RBU"
  } else if (Region == "Scandinavia") {
    match.Region = "SCA"
  } else if (Region == "Turkey") {
    match.Region = "TCA"
  } else if (str_detect(Region, "UK")) {
    match.Region = "UKI"  
  }
  
  if (species == "O3") { # total ozone
    source.emiss = "Total"
  } else if (str_detect(species, 'STR')) { # stratosphere
    source.emiss = "Stratosphere"
  } else if (str_detect(species, 'LGT') | str_detect(species, 'INI') | str_detect(species, 'AIR') | str_detect(species, 'XTR')) { # other sources: aircraft, lightning, initial conditions and chemical source
    source.emiss = "Other"
  } else if (str_detect(species, match.Region)) { # check for local or transported emissions
    source.emiss = "Local"
  } else if (!str_detect(species, match.Region)) { # check for local or transported emissions
    source.emiss = get.emission.source(species)
  } else {
    source.emiss = "No Match"
  }
  return (source.emiss)
}

# transport from which regions influence local ozone
get.emission.source <- function (Species) {
  if (str_detect(Species, 'BLT')) {
    Source = "BLT" 
  } else if (str_detect(Species, 'OCNLBC')) {
    Source = "OCNLBC"
  } else if (str_detect(Species, 'NAMLBC')) {
    Source = "NAMLBC"
  } else if (str_detect(Species, 'RSTLBC')) {
    Source = "RSTLBC"
  } else if (str_detect(Species, 'SASLBC')) {
    Source = "SASLBC"
  } else if (str_detect(Species, 'BIO')) {
    Source = "BIO"
  } else if (str_detect(Species, 'BMB')) {
    Source = "BMB"
  } else if (str_detect(Species, 'BNL')) {
    Source = "BNL"
  } else if (str_detect(Species, 'BNS')) {
    Source = "BNS"
  } else if (str_detect(Species, 'CEN')) {
    Source = "CEN"
  } else if (str_detect(Species, 'FRA')) {
    Source = "FRA"
  } else if (str_detect(Species, 'GER')) {
    Source = "GER"
  } else if (str_detect(Species, 'IBE')) {
    Source = "IBE"
  } else if (str_detect(Species, 'ITA')) {
    Source = "ITA"
  } else if (str_detect(Species, 'MBS')) {
    Source = "MBS"
  } else if (str_detect(Species, 'RBU')) {
    Source = "RBU"
  } else if (str_detect(Species, 'SCA')) {
    Source = "SCA"
  } else if (str_detect(Species, 'SEE')) {
    Source = "SEE"
  } else if (str_detect(Species, 'TCA')) {
    Source = "TCA"
  } else if (str_detect(Species, 'UKI')) {
    Source = "UKI"
  } else {
    Source = "Error"
  }
  return (Source)
}

assign.tag.text <- function (spc) {
  if (spc == "o3") {
    tag = "O3"
  } else if (spc == "o3_x_c1") {
    tag = "O3_X_ATL"
  } else if (spc == "o3_x_c2") {
    tag = "O3_X_MBS"
  } else if (spc == "o3_x_c3") {
    tag = "O3_X_BNS"
  } else if (spc == "o3_x_c4") {
    tag = "O3_X_CEN"
  } else if (spc == "o3_x_c5") {
    tag = "O3_X_BNL"
  } else if (spc == "o3_x_c6") {
    tag = "O3_X_SEE"
  } else if (spc == "o3_x_c7") {
    tag = "O3_X_GER"
  } else if (spc == "o3_x_c8") {
    tag = "O3_X_IBE"
  } else if (spc == "o3_x_c9") {
    tag = "O3_X_SCA"
  } else if (spc == "o3_x_c10") {
    tag = "O3_X_FRA"
  } else if (spc == "o3_x_c11") {
    tag = "O3_X_UKI"
  } else if (spc == "o3_x_c12") {
    tag = "O3_X_ITA"
  } else if (spc == "o3_x_c13") {
    tag = "O3_X_BLT"
  } else if (spc == "o3_x_c14") {
    tag = "O3_X_RBU"
  } else if (spc == "o3_x_c15") {
    tag = "O3_X_TCA"
  } else if (spc == "o3_x_str") {
    tag = "O3_X_STR"
  } else if (spc == "o3_x_xtr") {
    tag = "O3_X_XTR"
  } else if (spc == "o3_x_bio") {
    tag = "O3_X_BIO"
  } else if (spc == "o3_x_bmb") {
    tag = "O3_X_BMB"
  } else if (spc == "o3_x_namlbc") {
    tag = "O3_X_NAMLBC"
  } else if (spc == "o3_x_saslbc") {
    tag = "O3_X_SASLBC"
  } else if (spc == "o3_x_eurlbc") {
    tag = "O3_X_EURLBC"
  } else if (spc == "o3_x_ocnlbc") {
    tag = "O3_X_OCNLBC"
  } else if (spc == "o3_x_rbulbc") {
    tag = "O3_X_RBULBC"
  } else if (spc == "o3_x_rstlbc") {
    tag = "O3_X_RSTLBC"
  } else {
    tag <- "No match"
  }
  return (tag)
}

months <- seq(5, 5, by = 1) 
data.list <- lapply(months, get.data)
data.df <- tbl_df(bind_rows(data.list))
data.df$Month <- factor(data.df$Month, levels = c("May", "Jun", "Jul", "Aug", "Sep"))
data.df <- data.df %>% filter(!Species %in% c("o3_x_eurini", "o3_x_namini", "o3_x_ocnini", "o3_x_rbuini", "o3_x_rstini", "o3_x_sasini")) %>% mutate(O3 = O3 * 1000)  

tagged.data <- data.df %>%
  filter(Region != "Remove") %>%
  rowwise() %>%
  mutate(Tagged.Spc = assign.tag.text(Species))

combined.data <- tagged.data %>%
  dplyr::select(-Species) %>%
  spread(Tagged.Spc, Mixing.Ratio, fill = 0) %>%
  mutate(O3_X_RBU = O3_X_RBU + O3_X_RBULBC, O3_X_OCNLBC = O3_X_OCNLBC + O3_X_ATL, O3_X_RSTLBC = O3_X_RSTLBC + O3_X_EURLBC + O3_X_XTR) %>%
  gather(Species, Mixing.Ratio, -x, -y, -Month, -Region) %>%
  filter(!Species %in% c("O3_X_ATL", "O3_X_RBULBC", "O3_X_XTR", "O3_X_EURLBC"))
combined.data

zonal.mean <- combined.data %>%
  group_by(Month, Region, Species) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio))
zonal.mean

with.sources <- zonal.mean %>%
  rowwise() %>%
  mutate(Source = get.source(Species, Region)) %>%
  group_by(Month, Region, Source) %>%
  summarise(Zonal.Mean = sum(Zonal.Mean))
with.sources
with.sources$Source <- factor(with.sources$Source, levels = c("Total", "Local", "UKI", "FRA", "GER", "ITA", "CEN", "BNL", "IBE", "RBU", "SEE", "SCA", "TCA", "BLT", "BNS", "MBS","BIO", "BMB", "NAMLBC", "OCNLBC", "RSTLBC", "SASLBC", "Stratosphere"))
with.sources$Region <- factor(with.sources$Region, levels = c("UK; Iceland", "France", "Germany", "Italy", "Central E Europe", "Benelux", "Iberia", "Russia", "Greece", "Scandinavia", "Turkey", "Baltic Sea", "Poland; NE Europe", "Mediterranean + Black Sea", "Atlantic"))

plotting <- function (data.frame, region) { 
  df <- data.frame %>% filter(Region == region, Source != "Total")
  file.name <- paste0(region, "_95th_O3.pdf")
  plot.title <- paste0(" 95th Percentile O3 (ppbv) in ", region, " with Contributions from different Emission Sources")
  
  colours <- c("Local" = "#6c254f", "Stratosphere" = "#4daf4a", "UKI" = "#b569b3", "FRA" = "#a6cee3", "GER" = "#b2df8a", "FRA" = "#ff7f00", "ITA" = "#e31a1c", "CEN" = "#6a3d9a", "BNL" = "#1f78b4", "IBE" = "#cab2d6", "RBU" = "#fdbf6f", "SEE" = "#fb9a99", "SCA" = "#b15928", "TCA" = "#33a02c", "BLT" = "#2b9eb3", "BNS" = "#ef6638", "MBS" = "#898989", "BIO" = "#86b650", "BMB" = "#0352cb", "NAMLBC" = "#f9c500", "OCNLBC" = "#77aecc", "RSTLBC" = "#8c1531", "SASLBC" = "#9bb18d")
  
  y.max <- sum(df$Zonal.Mean)
  
  p <- ggplot(data = df %>% arrange(desc(Source)), aes(x = Month, y = Zonal.Mean, fill = Source))
  p <- p + geom_bar(stat = "identity", width = 0.9)
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
  p <- p + theme(axis.line.x = element_line(colour = "black"))
  p <- p + theme(axis.line.y = element_line(colour = "black"))
  p <- p + guides(fill = guide_legend(ncol = 1))
  p <- p + scale_fill_manual(values = colours)
  
  CairoPDF(file = file.name, width = 10, height = 7)
  print(p)
  dev.off()
}
  
emission.regions <- levels(factor(with.sources$Region))
lapply(emission.regions, plotting, data.frame = with.sources)

#### only total O3
p <- ggplot(with.sources %>% filter(Source == "Total"), aes(x = Month, y = Zonal.Mean, group = 1))
p <- p + geom_point()
p <- p + geom_line()
p <- p + facet_wrap(~ Region, scales = "free")

CairoPDF(file = "Total_O3.pdf", width = 10, height = 7)
print(p)
dev.off()
