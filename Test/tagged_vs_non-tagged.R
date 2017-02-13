setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

file <- "/worknb//users//jco//HTAP//HTAP_VOC_Tagging//voc_new_htap.cam.h1.2010-07-30-64800.nc"
nc <- nc_open(file)
all.vars <- as.data.frame(names(nc$var))
colnames(all.vars) <- c("Vars")
o3.vars.df <- all.vars %>%
  filter(str_detect(Vars, 'O3'))
o3.vars <- as.vector(o3.vars.df$Vars)
o3.vars

get.data <- function (variable, file.name) {
  var.raster <- raster(file.name, varname = variable, lev = 56)
  var.rotated.raster <- rotate(var.raster)
  var.raster.ppbv <- var.rotated.raster * 1e9
  
  var.df <- as.data.frame(rasterToPoints(var.raster.ppbv))
  colnames(var.df) <- c("lon", "lat", "O3")
  
  if (variable == "O3") {
    emission.region = "All"
    emission.type = "All"
  } else if (str_detect(variable, '_X_')) {
    tag <- str_sub(variable, start = 6, end = str_length(variable))
    split.tag <- strsplit(tag, split = "")[[1]]
    strings <- paste0(split.tag[c(TRUE, FALSE, FALSE)], split.tag[c(FALSE, TRUE, FALSE)], split.tag[c(FALSE, FALSE, TRUE)])
    emission.region = strings[1]
    emission.type = strings[2]
  } else {
    emission.region = "ERROR"
    emission.type = "ERROR"
  }
  var.df$Emission.Region = rep(emission.region, length(var.df$lon))
  var.df$Emission.Type = rep(emission.type, length(var.df$lon))
  return(var.df)
}

data.list <- lapply(o3.vars, FUN = get.data, file.name = file)
data.df <- tbl_df(do.call("rbind", data.list))
data.df

# real - tagged
non.tagged.o3 <- data.df %>%
  filter(Emission.Region == "All") %>%
  dplyr::select(-Emission.Type)
tagged.o3 <- data.df %>%
  filter(Emission.Region != "All") %>%
  dplyr::select(-Emission.Type) %>%
  mutate(Emission.Region = "Tagged") %>% 
  group_by(lon, lat) %>%
  mutate(O3 = sum(O3)) %>%
  distinct(lon, lat, .keep_all = TRUE)

# plot diff between tagged and non-tagged
all.df <- tbl_df(rbind(as.data.frame(non.tagged.o3), as.data.frame(tagged.o3)))
all.df
diff.df <- all.df %>%
 spread(Emission.Region, O3) %>%
 mutate(Diff = All - Tagged)
diff.df
ggplot() + geom_raster(data = diff.df, aes(x = lon, y = lat, fill = Diff), hjust = 0, vjust = 0) + geom_path(data = map_data("world"), aes(x = long, y = lat, group = group)) + scale_fill_distiller(palette = "Spectral") + theme_tufte()
