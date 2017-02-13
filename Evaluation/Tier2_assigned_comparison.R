setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

data.path <- "/work/users/jco/HTAP2_Model_Output/"
data.dirs <- list.dirs(path = data.path)
data.dirs <- data.dirs[-1] # remove data.path directory
data.dirs

dir <- data.dirs[1]
# get assigned data
model.name <- str_split_fixed(dir, pattern = "Output//", n = 2)[2]
file.name <- paste0(dir, "/htap2_", model.name, "_vmro3_ModelLevel_2010-JAN_assigned_Tier2.csv")
file.data <- tbl_df(read.csv(file = "htap2_CAMchem_BASE_vmro3_ModelLevel_2010-JAN_assigned_Tier2.csv", header = TRUE))
file.data

zonal.mean <- file.data %>%
  group_by(Model, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio) * 1e9) 
tbl_df(zonal.mean)

get.tier1 <- function (region) {
  if (str_detect(region, " Sea") | str_detect(region, "Ocean") | str_detect(region, "Atlantic") | str_detect(region, "Pacific") | str_detect(region, "Hudson")) {
    tier1 <- "Ocean"
  } else if (str_detect(region, " US") | str_detect(region, "Canada")) {
    tier1 <- "North.America"
  } else if (str_detect(region, "Europe") | str_detect(region, "Greece")) {
    tier1 <- "Europe"
  } else if (str_detect(region, "India")) {
    tier1 <- "South.Asia"
  } else if (str_detect(region, "China") | str_detect(region, "Korea") | str_detect(region, "Japan")) {
    tier1 <- "East.Asia"
  } else if (str_detect(region, "Lebanon") | str_detect(region, "Oman") | str_detect(region, "Iraq")) {
    tier1 <- "Middle.East"
  } else if (str_detect(region, "Russia") | str_detect(region, "Ukraine")) {
    tier1 <- "Russia"
  } else if (str_detect(region, "Rest")) {
    tier1 <- "Rest"
  } else {
      tier1 <- "no.match"
  }
  return(tier1)
}

tiered <- zonal.mean %>%
  rowwise() %>%
  mutate(Tier1.Region = get.tier1(Region)) %>%
  dplyr::rename(Tier2.Region = Region)

p <- ggplot(data = tiered, aes(x = Month, y = Zonal.Mean, colour = Tier1.Region, group = Tier1.Region))
p <- p + geom_point()
p <- p + geom_line()
p <- p + facet_wrap( ~ Tier2.Region, nrow = 3)
p
