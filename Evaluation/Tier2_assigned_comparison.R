setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

data.files <- list.files(pattern = "*_Tier2.csv")
data.files

test <- data.files[1]
# get assigned data
file.data <- tbl_df(read.csv(file = test, header = TRUE))
file.data

zonal.mean <- file.data %>%
  group_by(Model, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio) * 1e9) 
tbl_df(zonal.mean)

plotting <- function (region, data.frame) {
  if (region == "SAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "India") & !str_detect(Region, "Ocean"))    
    df$Region <- factor(df$Region, levels = c("N India; Nepal; Bangladesh; Afghanistan; Pakistan", "S India; Sri Lanka", "Indian Himalaya"))
    plot.title <- "South Asia: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"    
  } else if (region == "NAM") {
    df <- data.frame %>%
      filter(str_detect(Region, " US") | str_detect(Region, "Canada"))    
    df$Region <- factor(df$Region, levels = c("NE US", "SE US", "NW US", "SW US", "E Canada", "W Canada and Alaska"))
    plot.title <- "North America: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"
  } else if (region == "EUR") {
    df <- data.frame %>%
      filter(str_detect(Region, "Europe") | str_detect(Region, "Greece"))    
    df$Region <- factor(df$Region, levels = c("NW Europe", "SW Europe", "E Europe", "Greece; Turkey; Cyprus"))
    plot.title <- "Europe: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"
  } else if (region == "EAS") {
    df <- data.frame %>%
      filter(str_detect(Region, "China") | str_detect(Region, "Korea") | str_detect(Region, "Japan"))
    df$Region <- factor(df$Region, levels = c("NE China", "SE China", "W China; Mongolia", "N Korea; S Korea", "Japan", "China; Tibet Himalaya"))
    plot.title <- "East Asia: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"
  } else if (region == "MDE") {
    df <- data.frame %>%
      filter(str_detect(Region, "Lebanon") | str_detect(Region, "Oman") | str_detect(Region, "Iran"))
    df$Region <- factor(df$Region, levels = c("Lebanon; Israel; Jordan; Syria", "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain", "Iran; Iraq"))
    plot.title <- "Middle East: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"
  } else if (region == "RBU") {
    df <- data.frame %>%
      filter(str_detect(Region, "Russia") | str_detect(Region, "Ukraine"))
    df$Region <- factor(df$Region, levels = c("W Russia", "E Russia", "Belarus; Ukraine"))
    plot.title <- "Russia, Belarus, Ukraine: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"
  } else if (region == "OCN") {
    df <- data.frame %>%
      filter(str_detect(Region, "Ocean") | str_detect(Region, " Sea") | str_detect(Region, " Bay") | str_detect(Region, "Atlantic") | str_detect(Region, "Pacific"))
    df$Region <- factor(df$Region, levels = c("Baltic Sea", "North Atlantic", "South Atlantic", "North Pacific", "South Pacific", "Indian Ocean", "Hudson Bay", "Mediterranean Sea", "Black and Caspian Sea"))
    plot.title <- "Oceans: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"
  } else if (region == "Rest") {
    df <- data.frame %>%
      filter(Region == "Rest")
    plot.title <- "Rest of the World: Monthly Average O3 (ppbv) in Tier 2 Receptor Regions. Emissions and Meteorology for Year 2010"
  } else {
    stop("No region")
  }
  
  df$Month <- factor(df$Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
  
  p <- ggplot(data = df, aes(x = Month, y = Zonal.Mean, colour = Model, group = Model))
  p <- p + geom_point()
  p <- p + geom_path()
  p <- p + facet_wrap(~ Region, nrow = 1)
  p <- p + plot_theme()
  p <- p + ylab("O3 (ppbv)")
  p <- p + ggtitle(plot.title)
  p <- p + scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))

  file.name <- paste0(region, "_O3_Yearly_Cycle_HTAP_ensemble_Tier2.pdf")
  CairoPDF(file = file.name, width = 14, height = 9)
  print(p)
  dev.off()
}

emission.regions <- c("SAS", "NAM", "EUR", "EAS", "MDE", "RBU", "OCN", "Rest")
emission.regions <- c("SAS")
lapply(emission.regions, plotting, data.frame = zonal.mean)