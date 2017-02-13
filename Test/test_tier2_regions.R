setwd("/work/users/jco/tagging_global/land_mask")

nc <- raster("Tier2_receptor_regions_1.9x2.5.nc")
df <- tbl_df(as.data.frame(rasterToPoints(nc)))
levels(factor(df$region_codes))

p <- ggplot()
p <- p + geom_raster(data = df, aes(x = x, y = y, fill = factor(region_codes)))
p <- p + geom_path(data = map_data("world2"), aes(x = long, y = lat, group = group))
p <- p + plot_theme()
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + theme(axis.line = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(legend.title = element_blank())
p

CairoPNG(file = "/local//home/coates/Documents//Analysis//2016_HTAP//Test//test.png", width = 2000, height = 1700)
print(p)
dev.off()

# comparison to Tier 1
tier1 <- raster("/local//home/coates//Documents//Analysis//2016_HTAP//Tier1_receptor_regions_1.9x2.5.nc")
df.tier1 <- tbl_df(as.data.frame(rasterToPoints(tier1)))

p <- ggplot()
p <- p + geom_raster(data = df.tier1, aes(x = x, y = y, fill = factor(region_codes)))
p <- p + geom_path(data = map_data("world2"), aes(x = long, y = lat, group = group))
# p <- p + facet_wrap(~ region_codes)
p <- p + plot_theme()
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + theme(axis.line = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(legend.title = element_blank())
p

# original Tier 2 data
tier2 <- raster("HTAP_Phase2_tier2NC01x01.nc")
tier2.df <- tbl_df(as.data.frame(rasterToPoints(tier2)))
levels(factor(tier2.df$region_code))

p <- ggplot()
p <- p + geom_raster(data = tier2.df %>% filter(region_code == 021), aes(x = x, y = y, fill = factor(region_code)))
p <- p + geom_path(data = map_data("world"), aes(x = long, y = lat, group = group))
p <- p + plot_theme()
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + theme(axis.line = element_blank())
p <- p + theme(axis.title = element_blank())
p <- p + theme(legend.title = element_blank())
p
