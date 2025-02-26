set.seed(100)
m1 <- cbind(V1 = 1:20, V2 = sample(1:100, size = 20, replace = TRUE),
            V3 = rnorm(n = 20, mean = 500, sd = 100))

set.seed(200)
m2 <- cbind(V1 = 1:20, V2 = sample(1:100, size = 20, replace = TRUE),
            V3 = rnorm(n = 20, mean = 500, sd = 100))

set.seed(300)
m3 <- cbind(V1 = 1:20, V2 = sample(1:100, size = 20, replace = TRUE),
            V3 = rnorm(n = 20, mean = 500, sd = 100))

l1 = list(m1, m2, m3)

l1
l2 <- lapply(l1, function(x) {
  x[1:2,1:3]
})
l2

GRP = sample(LETTERS[1:5], size = nrow(m1), replace = TRUE)
GRP

l3 <- lapply( l1, function(x) {
  data.frame(x, GRP = sample(LETTERS[1:5], size = nrow(m1), replace = TRUE))
})


l3

d <- do.call(rbind, l3)
d


for(i in 1:length(l3)) {
  write.csv(l3[[i]], file = paste0("C:\\Users\\svroc\\Documents\\geospar_data\\dummy", i, ".csv"))
}


library(dplyr)
library(tidyr)
library(ggplot2)
# d1 <- data.frame(a = 1:10, grp1 = rep("a", 10), grp2 = rep("b", 10))
d1 <- data.frame(i = 1:10, a = 1:10, grp = rep("a", 10))
d1

d2 <- data.frame(i = 1:10, a = 11:20, grp = rep("b", 10))
d2
DF <- rbind(d1, d2)
DF
df_wide <- DF %>% pivot_wider(names_from = grp, values_from = a)

df_wide

df_long <- df_wide %>%
  pivot_longer(cols = a:b, names_to = "grp", values_to = "a") %>% select(i, a, grp) %>% arrange(grp)
df_long

set.seed(110)
price_weight <- tibble(
  year = 1951:2000,
  price = runif(n = length(year), 20, 50),
  weight = (price * 10) * runif(n = length(year), 0.8, 1.2)
)


price_weight

plot(price_weight$price, price_weight$weight)

price_weight_long <- price_weight %>%
  pivot_longer(cols = "price":"weight", names_to = "element", values_to = "value")

price_weight_long


price_weight_wide <- price_weight_long %>%
  pivot_wider(names_from = "element", values_from = value)

price_weight_wide



do.call(rbind, lapply(unique(df_long$grp), function(x) {
  # df_long %>%
  #   filter(grp == x) %>%
  #   summarize(mean = mean(a), stdev = sd(a)) %>%
  #   mutate(grp = x) %>% select(grp, mean, stdev)
  dat <- df_long[df_long$grp == x, ]
  data.frame(grp = x, mean = mean(dat$a), stdev = sd(dat$a))
}))


lapply(unique(df_long$grp), function(x) { # x <- "a"
  dat <- df_long[df_long$grp == x, ]
  x_bar <- mean(dat$a)
  s <- sd(dat$a)
  data.frame(grp = x , mean = x_bar, stdev = s)
})

price_weight_long

lapply(unique(price_weight_long$element), function(x) { # x <- "a"
  dat <- price_weight_long[price_weight_long$element == x, ]
  x_bar <- mean(dat$value)
  s <- sd(dat$value)
  data.frame(grp = x , mean = x_bar, stdev = s)
})


price_weight_long %>%
  pivot_wider(names_from = element, values_from = value) %>%
  mutate(wt_price = weight / price) %>%
  ggplot() +
  geom_histogram(aes(x = wt_price), color = "black", fill = "blue", bins = 20)
