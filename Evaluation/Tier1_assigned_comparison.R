setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

data.path <- "/work/users/jco/HTAP2_Model_Output/"
data.dirs <- list.dirs(path = data.path)
data.dirs <- data.dirs[-1] # remove data.path directory
data.dirs

dir <- data.dirs[1]
# get assigned data
model.name <- str_split_fixed(dir, pattern = "Output//", n = 2)[2]
file.name <- paste0(dir, "/htap2_", model.name, "_vmro3_ModelLevel_2010-JAN_assigned_Tier1.csv")
file.data <- tbl_df(read.csv(file = "htap2_CAMchem_BASE_vmro3_ModelLevel_2010-JAN_assigned_Tier1.csv", header = TRUE))
file.data

zonal.mean <- file.data %>%
  group_by(Model, Month, Region) %>%
  summarise(Zonal.Mean = mean(Mixing.Ratio) * 1e9) 
tbl_df(zonal.mean)

p <- ggplot(data = zonal.mean, aes(x = Region, y = Zonal.Mean))
p <- p + geom_bar(stat = "identity")
p
