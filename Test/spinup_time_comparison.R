
get.monthly.diffs <- function (month) {
  mon <- sprintf("%02d", month)
  file.name <- paste0("/worknb/users/jco/cesm/archive/HTAP_2010_base/atm/hist/HTAP_2010_base.2010-", mon, ".timavg.nc")
  diff.raster <- rotate(raster(file.name, varname = "O3", lev = 56))
  diff.df <- tbl_df(as.data.frame(rasterToPoints(diff.raster)))
  return.df <- diff.df %>%
    mutate(Month = month.abb[month], O3 = O3.concentration * 1e9) %>%
    dplyr::select(lon = x, lat = y, Month, O3)
  return(return.df)
}

months <- seq(1, 12, by = 1)
list.data <- lapply(months, get.monthly.diffs)
list.df <- bind_rows(list.data)
list.df$Month <- factor(list.df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

means <- list.df %>%
  group_by(Month) %>%
  summarise(Mean = mean(O3)) %>%
  mutate(Type = "1yrSpinUp")
means

NOx.df <- NOx.tagging %>%
  filter(Species == "O3") %>%
  group_by(Month) %>%
  summarise(Mean = mean(Mixing.Ratio) * 1e9) %>%
  mutate(Type = "NOx.Tagged")
NOx.df$Month <- factor(NOx.df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
NOx.df

all.df <- rbind(means, NOx.df)
ggplot(all.df, aes(x = Month, y = Mean, colour = Type, group = Type)) + geom_point() + geom_line()
