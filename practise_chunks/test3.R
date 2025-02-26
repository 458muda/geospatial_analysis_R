library(geospaar)
roads <- read_sf(system.file("extdata/roads.shp", package = "geospaar"))
districts <- st_read(
  system.file("extdata/districts.shp", package = "geospaar")
)

#1
r <- raster(extent(districts[49,]$geometry %>% st_bbox()), res = 0.1,  crs = 4326)
values(r) <- sample(1:100, size = ncell(r), replace = TRUE)

#2
rt <- projectRaster(r,crs = crs(roads))
districts_alb <- districts %>% st_transform(st_crs(roads))
plot(st_geometry(districts_alb), col = "grey")
plot(rt, col = "grey40", add = TRUE)

#3
rt_dis1 <- rt %>% disaggregate(., fact=5, method="bilinear")
plot(rt_dis1)
rt_dis2 <- rt %>% resample(., rt_dis1, method = "ngb")
plot(rt_dis2)

#4
data(chirps)
chirpsz <- mask(chirps, districts)
chirpsz %>% projectRaster(., crs = crs(districts_alb)) -> chirpsz_alb
rainsum <- calc(chirpsz, fun = sum)
plot_noaxes(rainsum)

#5
min10mm <- rainsum < 10
is.na(min10mm) <- 0
minrain <- mask(rainsum, min10mm)
plot_noaxes(minrain)

cellStats(chirpsz_alb[[15]], mean, na.rm = TRUE)
norain <- chirpsz_alb[[15]] == 0
norain <- mask(chirpsz_alb[[15]], norain)
mean <- cellStats(norain, mean)
mean

#6
rainmean <- cellStats(chirpsz_alb[[15]], stat = mean)
rainmean


districts %>% st_union %>% plot(add = TRUE)

plot(filter_image2, col = "black")


farmers <- system.file("extdata/farmer_spatial.csv", package = "geospaar") %>%
  read_csv() %>% st_as_sf(coords = c("x", "y"))


farmers


roads <- read_sf(system.file("extdata/roads.shp", package = "geospaar"))

roads

set.seed(2)
districts2 <- cbind(districts, st_centroid(districts) %>% st_coordinates()) %>%
  mutate(yield = (7 - 0.25 * -Y) * runif(n = nrow(.), min = 0.9, max = 1.2)) %>%
  mutate(yld_cat = ifelse(yield > 4.5, "high", "other"),
         yld_cat = ifelse(between(yield, 3.5, 4.5), "medium", yld_cat),
         yld_cat = ifelse(yield < 3.5, "low", yld_cat)) %>%
  dplyr::select(distName, X, Y, yield, yld_cat)

districts2


districts2 %>% group_by(yld_cat) %>%
  summarize(yield = mean(yield)) %>%
  ggplot() + geom_sf(aes(fill = yield))

p <- ggplot(districts2) + geom_sf(fill = "grey")
p
# farmer_ct <- farmers %>% group_by(uuid) %>% count()
# st_crs(farmer_ct) <- st_crs(districts)
farmer_ct <- farmers %>% group_by(uuid) %>% count() %>%
  st_set_crs(st_crs(districts))
farmers_districts <- st_join(farmer_ct, districts2, largest = TRUE)

p + geom_sf(data = farmers_districts, aes(color = n)) +
  scale_color_viridis_c()



# lapply(farmers_per_dist$distName, function(x) {
#   farmers_per_dist %>% filter(distName == x) %>% st_cast("POINT")
# }) %>% do.call(rbind, .)
# farmers_per_dist %>% st_cast("POINT") %>% plot()
farmers_per_dist <- farmers_districts %>% group_by(distName) %>% count()
p + geom_sf(data = farmers_per_dist) +
  geom_sf(data = farmers_per_dist %>% st_centroid(), col = "blue")


dmed <- districts3 %>% #mutate(area = as.numeric(st_area(.)) / 10^6) %>%
  mutate(area = as.numeric(units::set_units(st_area(.), "km2"))) %>%
  filter(area == quantile(area, probs = 0.5, type = 1))
p2 <- p + geom_sf(data = dmed, fill = "red")

dmax <- districts3 %>% mutate(area = as.numeric(st_area(.)) / 10^6) %>%
  # arrange(area) %>% slice(nrow(.)) %>%
  arrange(desc(area)) %>% slice(1)
# slice_max(order_by = area) #
p2 + geom_sf(data = dmax, fill = "blue")

districts3 %>% mutate(area = as.numeric(st_area(.)) / 10^6) %>%
  arrange(area) %>% pull(distName)



pols <- st_multipolygon(list(list(cbind(x = c(26.5, 27.5, 27, 26, 26.5),
                                        y = c(-15.5, -16.5, -17, -16, -15.5))),
                             list(cbind(x = c(26.5, 27.5, 27, 26, 26.5) + 1,
                                        y = c(-15.5, -16.5, -17, -16, -15.5) -
                                          1))))
pols <- pols %>% st_geometry %>% st_cast("POLYGON") %>%
  st_as_sf(crs = 4326) %>%
  mutate(ID = 1:nrow(.)) %>% #as.data.frame() %>%
  dplyr::select(ID)

p + geom_sf(data = pols, aes(fill = as.factor(ID))) +
  geom_sf(data = st_union(pols),
          fill = rgb(1, 0, 1, alpha = 0.7), col = "purple")




int_dists <- districts2 %>%
  slice(which(st_intersects(., st_union(pols), sparse = FALSE)))
p + geom_sf(data = int_dists, fill = "red") +
  geom_sf(data = pols, fill = "transparent", col = "blue")


districts_pol <- st_intersection(districts2, st_union(pols))
ggplot(districts_pol) + geom_sf(aes(fill = distName))
p + geom_sf(data = districts_pol, aes(fill = distName))



roads30 <- roads %>% mutate(km = as.numeric(st_length(.)) / 1000) %>%
  filter(km > 100) %>%
  st_buffer(dist = 30000) %>% st_transform(crs = 4326)

roads30

roads10 <- roads %>% mutate(km = as.numeric(st_length(.)) / 1000) %>%
  filter(km > 100) %>%
  st_buffer(dist = 10000) %>% st_transform(crs = 4326)

roads10

dif <- sf::st_difference(roads30, roads10)

ggplot(dif) + geom_sf()


strat_sample <- roads30 %>%
  st_sample(size = 100, exact = TRUE)

ggplot() + geom_sf(data = districts2) + geom_sf(data = roads30, fill = "red") + geom_sf(data = strat_sample, color = "blue")

st_crs(districts) <- st_crs(roads)



roads_dist <- roads %>% mutate(length = as.numeric(st_length(.))/1000) %>%
  filter(length == max(length)) %>% st_intersection(districts)

