library(geospaar)

roads <- read_sf(system.file("extdata/roads.shp", package = "geospaar"))
districts <- st_read(
  system.file("extdata/districts.shp", package = "geospaar")
)

districts <- districts %>% mutate(ID = 1:nrow(.))
distsr <- districts %>% rasterize(x = ., y = raintot, field = "ID") %>% print()
plot(distsr)

# Download DEM
dem <- getData(name = "alt", country = "ZMB", path = tempdir())

# calculate slope
slope <- terrain(x = dem, opt = 'slope', unit = 'degrees')
plot(terrain(x = dem, opt = 'aspect', unit = 'degrees'))

# calculate mean by district
distsr_rs <- resample(x = distsr, y = slope, method = "ngb")
zoneslope <- zonal(x = slope, z = distsr_rs, fun = "mean")
zoneelevation <- zonal(x = dem, z = distsr_rs, fun = "mean")

# map zonal statistics
distr_slopezone <- zoneslope %>% data.frame %>%
  subs(x = distsr_rs, y = ., by = "zone")
distr_elezone <- zoneelevation %>% data.frame %>%
  subs(x = distsr_rs, y = ., by = "zone")

# plot
l <- list(distr_elezone, distr_slopezone)
titles <- c("Elevation", "Slope")
par(mfrow = c(1, 2))
for(i in 1:length(l)) {
  plot_noaxes(l[[i]], main = titles[i])
}




dt <- as.Date(gsub("Y", "", names(chirpsz)), format = "%y%j")
dt <- lubridate::parse_date_time(gsub("Y", "", names(chirpsz)), orders = "yj")
wk <- lubridate::week(dt)
weekly_rainfall <- lapply(unique(wk), function(x) {
  calc(chirpsz[[which(wk == x)]], sum)
}) %>% stack(.)
rflim <- range(cellStats(weekly_rainfall, range))
plot(stack(weekly_rainfall), zlim = rflim)
