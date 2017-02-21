setwd("~/Documents//Analysis//2016_HTAP//Evaluation")

get.data <- function (month) {
  file.month <- sprintf("%02d", month)
  
  # base data
#   base.file.name <- paste0("/worknb/users/jco/cesm/archive/HTAP_2010_base/atm/hist/HTAP_2010_base.2010-", file.month, ".mean.nc")
  base.file.name <- paste0("HTAP_tagged_base.2010-", file.month, ".mean.nc")
  base.raster <- raster(base.file.name, lev = 56, varname = "O3")
  base.df <- tbl_df(as.data.frame(rasterToPoints(rotate(base.raster))))
  base.df <- base.df %>%
    mutate(O3 = O3.concentration * 1e9, Type = "IASS.Base", Month = month.abb[month]) %>%
    dplyr::select(lon = x, lat = y, O3, Type, Month)
print(base.df)
  
  # NOx tagging data
#   nox.file.name <- paste0("/worknb/users/jco/HTAP//evaluation_with_ncar/HTAP_NOx_Tagging.cam.h2.2010-", file.month, ".O3mean.nc")
  nox.file.name <- paste0("HTAP_NOx_Tagging.cam.h2.2010-", file.month, ".O3mean.nc")
  nox.raster <- raster(nox.file.name, varname = "O3", lev = 56)
  nox.df <- tbl_df(as.data.frame(rasterToPoints(rotate(nox.raster))))
  nox.df <- nox.df %>%
    mutate(O3 = O3.concentration * 1e9, Type = "NOx.Tagging", Month = month.abb[month]) %>%
    dplyr::select(lon = x, lat = y, O3, Type, Month)
  
  all.data <- rbind(base.df, nox.df) 
  global.mean <- all.data %>% 
    group_by(Month, Type) %>% 
    summarise(Global.Mean = mean(O3))
  return(global.mean)
}

get.htap.data <- function (month, file) {
  file.month <- toupper(month.abb[month]) 
  if (str_detect(file, file.month)) {
    model.name <- str_split_fixed(file, pattern = "Output//", n = 2)[2]
    model <- str_split_fixed(model.name, patter = "_BASE/htap2", n = 2)[1]
    if (str_detect(file, "GEMMACH")) {
      htap.raster <- raster(file, lev = 79, varname = "vmro3", stopIfNotEqualSpaced=FALSE)
    } else if (str_detect(file, "MOZART")) {
      htap.raster <- raster(file, lev = 56, varname = "vmro3", stopIfNotEqualSpaced=FALSE)
    } else {
      htap.raster <- raster(file, lev = 1, varname = "vmro3", stopIfNotEqualSpaced=FALSE)
    }
    htap.df <- tbl_df(as.data.frame(rasterToPoints(htap.raster)))
    htap.df <- htap.df %>%                                                                                                                                                                                      
      mutate(O3 = O3.Volume.Mixing.Ratio * 1e9, Type = model, Month = month.abb[month]) %>%
      dplyr::select(lon = x, lat = y, O3, Type, Month)
  } else {
    htap.df <- data.frame(lon = c(1), lat = c(1), O3 = 1, Type = "No.Match", Month = month.abb[month])
  }
  return(htap.df)
}

months <- seq(1, 1, by = 1)

# HTAP ensemble data
htap.path <- "/work/users/jco/HTAP2_Model_Output/"
dirs <- list.dirs(htap.path)[-1]

htap.list.null <- lapply(dirs, function (dir) { files <- list.files(path = dir, pattern = "*.nc") ; lapply(files, function (x) { file.name <- paste0(dir, "/", x); lapply(months, get.htap.data, file = file.name) }) })
htap.list <- unlist(htap.list.null, recursive = FALSE)
htap.list.1 <- unlist(htap.list, recursive = FALSE)
htap.df <- tbl_df(bind_rows(htap.list.1))

levels(factor(htap.df$Type))
levels(factor(htap.df$Month))

htap.df <- htap.df %>%
  filter(Type != "No.Match")
htap.df

global.htap.mean <- htap.df %>%
  group_by(Month, Type) %>%
  summarise(Global.Mean = mean(O3))

# IASS Data
iass.list <- lapply(months, get.data)
iass.df <- tbl_df(bind_rows(iass.list))

# all data
all.data <- bind_rows(global.htap.mean, iass.df)
all.data$Month <- factor(all.data$Month, levels = c("Jan", "Feb"))
all.data$mysize <- rep(1, nrow(all.data))
all.data$mysize[all.data$Type == "IASS.Base"] <- 2
all.data$mysize[all.data$Type == "NOx.Tagging"] <- 2

colours <- c("IASS.Base" = "#dc3522", "NOx.Tagging" = "#000000", "CAMchem" = "#9bb18d", "CHASER_re1" = "#e7e85e", "CHASER_t106" = "#6c254f", "C-IFS" = "#0e5c28", "C-IFS_v2" = "#ba8b01", "EMEP_rv4.5" = "#0c3f78", "EMEP_rv48" = "#898989", "GEMMACH" = "#8c6238", "GEOS-Chem" = "#1c3e3d", "GEOSCHEMADJOINT" = "#cc6329", "HadGEM2-ES" = "#f9c500", "HCMAQ" = "#0352cb", "MOZART-4" = "#8ed6d2", "OsloCTM3.v2" = "#8c1531", "RAQMS" = "#86b650")

p <- ggplot(data = all.data, aes(x = Month, y = Global.Mean, colour = Type, group = Type, size = mysize))
p <- p + geom_point()
p <- p + geom_path()
p <- p + theme_tufte()
#p <- p + scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10), expand = c(0, 0))
p <- p + ylab("Global Mean Surface Ozone (ppbv)")
p <- p + theme(axis.line = element_line(colour = "black"))
p <- p + theme(axis.title.x = element_blank())
p <- p + theme(axis.title.y = element_text(face = "bold"))
p <- p + scale_colour_manual(name = "Model", values = colours)
p <- p + scale_size(range = c(1, 3), guide = "none")
p  <- p + theme()
