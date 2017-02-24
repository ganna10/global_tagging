setwd("~/Documents/Analysis/2016_HTAP/Regridding_Tim/")

plotting <- function (region, data.frame) {
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    plot.title <- "South Asia: Monthly Average O3 for Emissions and Meteorology for Year 2010"    
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    plot.title <- "North America: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    plot.title <- "Europe: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    plot.title <- "East Asia: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    plot.title <- "Middle East: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    plot.title <- "Russia, Belarus, Ukraine: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    plot.title <- "Oceans: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    plot.title <- "Rest of the World: Monthly Average O3 for Emissions and Meteorology for Year 2010"
  } else {
    stop("No region")
  }
  
  colours <- c("Total.Tagged" = "#000000", "Base.Run" = "#e31a1c", "HTAP.Max" = "#377eb8", "HTAP.Min" = "#ff7f00", "HTAP.Mean" = "#898989", "CAMchem" = "#984ea3")
  
  p <- ggplot(data = df, aes(x = Month, y = Mixing.Ratio, colour = Type, group = Type))
  p <- p + geom_point()
  p <- p + geom_line()
  p <- p + facet_wrap(~ Region, nrow = 1)
  p <- p + plot_theme()
  p <- p + ylab("O3 (ppbv)")
  p <- p + theme(axis.title = element_blank())
  p <- p + ggtitle(plot.title, subtitle = "\nMean Ozone (ppbv) in Tier 2 Receptor Regions from NOx sources.\nComparison with Min and Max of HTAP ensemble and Base Tagged run")
  p <- p + scale_colour_manual(values = colours, limits = levels(factor(df$Type)))
  p <- p + theme(legend.title = element_blank())
  p <- p + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  p <- p + theme(plot.subtitle = element_text(face = "bold", size = 12))

  file.name <- paste0(region, "_O3_Yearly_Cycle_Tier2_Ensemble_Tagged_Base.pdf")
  CairoPDF(file = file.name, width = 11, height = 7)
  print(p)
  dev.off()
}

# Sort out HTAP ensemble data
ensemble.data <- tbl_df(read.csv(file = "HTAP_ensemble_assigned_Tier2_regions.csv", header = TRUE))
df.ensemble <- ensemble.data %>%
  filter(Model != "GEOSCHEMADJOINT") %>%
  gather(Month, Mixing.Ratio, -Model, -Region) %>%
  mutate(Mixing.Ratio = Mixing.Ratio * 1e9) %>%
  group_by(Model, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio))

camchem <- df.ensemble %>%
  filter(Model == "CAMchem") %>%
  dplyr::select(Month, Region, Type = Model, Mixing.Ratio = Zonal.Mean)
  
# Calculate Min and Max of HTAP ensemble
final.htap <- df.ensemble %>%
  group_by(Month, Region) %>%
  summarise(HTAP.Max = max(Zonal.Mean), HTAP.Min = min(Zonal.Mean)) %>%
  gather(Type, Mixing.Ratio, -Month, -Region)
final.htap <- rbind(final.htap, camchem)
final.htap

# NOx Tagging data
NOx.tagging <- tbl_df(read.csv(file = "HTAP_NOx_Tagging_2010_assigned_to_Tier2_regions.csv", header = TRUE))
zonal.mean <- NOx.tagging %>%  
  filter(Species == "O3") %>%
  mutate(Mixing.Ratio = Mixing.Ratio * 1e9, Type = "Total.Tagged") %>%
  group_by(Month, Region, Type) %>%
  summarise(Mixing.Ratio = mean(Mixing.Ratio)) 
tbl_df(zonal.mean)

# HTAP Base Run
base.data <- tbl_df(read.csv(file = "HTAP_base_2010_assigned_to_Tier2_regions.csv", header = TRUE))
base.df <- base.data %>%
  mutate(Mixing.Ratio = Mixing.Ratio * 1e9, Type = "Base.Run") %>%
  group_by(Month, Region, Type) %>%
  summarise(Mixing.Ratio = mean(Mixing.Ratio))
base.df

# combine all data
all.data <- rbind(final.htap, zonal.mean) #, base.df)
all.data$Month <- factor(all.data$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
all.data$Type <- factor(all.data$Type, levels = c("HTAP.Max", "CAMchem", "Total.Tagged", "HTAP.Min")) #, "Base.Run"
all.data

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")
lapply(emission.regions, plotting, data.frame = all.data)
