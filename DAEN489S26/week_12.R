##################

library(dplyr)
library(geostan)
library(ggplot2)
library(gridExtra)

data("georgia")
summary(georgia$college)
hist(georgia$college)

sp_diag(georgia$college, georgia, name = "College (%)")
W <- shape2mat(georgia, style = "W")
moran_plot(georgia$college, W)
mc(georgia$college, W)

sp_diag(georgia$income, georgia, name = "Median Income (USD)")
moran_plot(georgia$income, W)
mc(georgia$income, W)
gr(georgia$income, W)

hist(georgia$income)

W <- shape2mat(georgia, "W")
x <- log(georgia$income)

Ii <- lisa(x, W)
head(Ii)

Ci <- lg(x, W)
head(Ci)

Ci_map <- ggplot(georgia) + 
  geom_sf(aes(fill=Ci)) +
  scale_fill_gradient(high = "navy",  
                      low = "white") +  theme_void()

Li_map <- ggplot(georgia) + 
  geom_sf(aes(fill=Ii$Li)) +
  scale_fill_gradient2(name = "Ii") + theme_void()

gridExtra::grid.arrange(Ci_map, Li_map, nrow = 1)

x <- log(georgia$income)
rho <- aple(x, W)
n <- nrow(georgia)
ess <- n_eff(rho = rho, n = n)
c(nominal_n = n, rho = rho, MC = mc(x, W), ESS = ess)

##################
##################

library(tidycensus)
library(tidyverse)

v22 <- load_variables(2022, "acs5", cache = TRUE)
View(v22)

# B03002_001: Total Population
# B03002_012: Hispanic or Latino Population
# B19013_001: Median Household Income
# B15003_001: Total population 25 years and over (for Education)
# B15003_017: Count of people with High School Diploma
# ...
vars <- c(
  total_pop = "B03002_001",
  hispanic_pop = "B03002_012",
  med_income = "B19013_001",
  edu_total = "B15003_001",
  diploma1 = "B15003_017",
  diploma2 = "B15003_018",
  diploma3 = "B15003_019",
  diploma4 = "B15003_020",
  diploma5 = "B15003_021",
  diploma6 = "B15003_022",
  diploma7 = "B15003_023",
  diploma8 = "B15003_024",
  diploma9 = "B15003_025"
  )

tx_county_data <- get_acs(
  geography = "county",
  variables = vars,
  state = "TX",
  year = 2022,
  output = "wide"
)

tx_final <- tx_county_data %>%
  mutate(
    pct_hispanic = (hispanic_popE / total_popE) * 100,
    pct_diploma = ((diploma1E + diploma2E + diploma3E + diploma4E + diploma5E + diploma6E + diploma7E + diploma8E + diploma9E) / edu_totalE) * 100
  ) %>%
  select(
    NAME, 
    median_income = med_incomeE, 
    pct_hispanic, 
    pct_diploma
  )

print(head(tx_final))

lm(median_income ~ pct_hispanic, data = tx_final)
lm(median_income ~ pct_diploma, data = tx_final)
lm(median_income ~ pct_hispanic + pct_diploma, data = tx_final)
lm(median_income ~ pct_hispanic * pct_diploma, data = tx_final)
summary(lm(median_income ~ pct_hispanic * pct_diploma, data = tx_final))

tx_county_data <- get_acs(
  geography = "county",
  variables = vars,
  state = "TX",
  year = 2022,
  output = "wide",
  geometry = TRUE
)

tx_final <- tx_county_data %>%
  mutate(
    pct_hispanic = (hispanic_popE / total_popE) * 100,
    pct_diploma = ((diploma1E + diploma2E + diploma3E + diploma4E + diploma5E + diploma6E + diploma7E + diploma8E + diploma9E) / edu_totalE) * 100
  ) %>%
  select(
    NAME, 
    median_income = med_incomeE, 
    pct_hispanic, 
    pct_diploma
  )

library(geostan)

tx_final_clean <- tx_final %>%
  filter(!is.na(median_income))

sp_diag(tx_final_clean$pct_hispanic, tx_final_clean, name = "pct_hispanic")

sp_diag(tx_final_clean$pct_diploma, tx_final_clean, name = "pct_diploma")

sp_diag(tx_final_clean$median_income, tx_final_clean, name = "income USD")

W <- shape2mat(tx_final_clean, "W")
x <- tx_final_clean$median_income

rho <- aple(x, W)
n <- nrow(tx_final_clean)
ess <- n_eff(rho = rho, n = n)
c(nominal_n = n, rho = rho, MC = mc(x, W), ESS = ess)

##################
##################

tx_tract_data <- get_acs(
  geography = "tract",
  variables = vars,
  state = "TX",
  year = 2022,
  output = "wide",
  geometry = TRUE
)

tx_tract_clean <- tx_tract_data %>%
  mutate(
    pct_hispanic = (hispanic_popE / total_popE) * 100,
    pct_diploma = ((diploma1E + diploma2E + diploma3E + diploma4E + diploma5E + diploma6E + diploma7E + diploma8E + diploma9E) / edu_totalE) * 100
  ) %>%
  select(
    NAME, 
    median_income = med_incomeE, 
    pct_hispanic, 
    pct_diploma
  ) %>%
  drop_na(median_income, pct_hispanic, pct_diploma)

lm(median_income ~ pct_hispanic, data = tx_tract_clean)
lm(median_income ~ pct_diploma, data = tx_tract_clean)
lm(median_income ~ pct_hispanic + pct_diploma, data = tx_tract_clean)
lm(median_income ~ pct_hispanic * pct_diploma, data = tx_tract_clean)

sp_diag(tx_tract_clean$pct_hispanic, tx_tract_clean, name = "pct_hispanic")
sp_diag(tx_tract_clean$pct_diploma, tx_tract_clean, name = "pct_diploma")
sp_diag(tx_tract_clean$median_income, tx_tract_clean, name = "income USD")

##################

library(sf)

n <- 30
set.seed(13531)
xy <- data.frame(x = runif(n), y = runif(n)) |> 
  st_as_sf(coords = c("x", "y"))

w1 <- st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)) |> 
  st_as_sfc() 
w2 <- st_sfc(st_point(c(1, 0.5))) |> st_buffer(1.2)

par(mfrow = c(1, 2), mar = c(2.1, 2.1, 0.1, 0.5), xaxs = "i", yaxs = "i")
plot(w1, axes = TRUE, col = 'grey')
plot(xy, add = TRUE)
plot(w2, axes = TRUE, col = 'grey')
plot(xy, add = TRUE, cex = .5)

##################

library(spatstat) |> suppressPackageStartupMessages()
as.ppp(xy)

(pp1 <- c(w1, st_geometry(xy)) |> as.ppp())

c1 <- st_buffer(st_centroid(w2), 1.2)
(pp2 <- c(c1, st_geometry(xy)) |> as.ppp())

par(mfrow = c(1, 2), mar = rep(0, 4))
q1 <- quadratcount(pp1, nx=3, ny=3)
q2 <- quadratcount(pp2, nx=3, ny=3)
plot(q1, main = "")
plot(xy, add = TRUE)
plot(q2, main = "")
plot(xy, add = TRUE)

quadrat.test(pp1, nx=3, ny=3)
quadrat.test(pp2, nx=3, ny=3)

den1 <- density(pp1, sigma = bw.diggle)
den2 <- density(pp2, sigma = bw.diggle)

par(mfrow = c(1, 2), mar = c(0,0,1.1,2))
plot(den1)
plot(pp1, add=TRUE)
plot(den2)
plot(pp1, add=TRUE)

##################

library(stars)

s1 <- st_as_stars(den1)
(s2 <- st_as_stars(den2))

s1$a <- st_area(s1) |> suppressMessages()
s2$a <- st_area(s2) |> suppressMessages()
with(s1, sum(v * a, na.rm = TRUE))
with(s2, sum(v * a, na.rm = TRUE))

pt <- st_sfc(st_point(c(0.5, 0.5)))
st_as_sf(s2, as_points = TRUE, na.rm = FALSE) |>
  st_distance(pt) -> s2$dist

(m <- ppm(pp2 ~ dist, data = list(dist = as.im(s2["dist"]))))

plot(m, se = FALSE)

predict(m, covariates = list(dist = as.im(s2["dist"]))) |>
  st_as_stars()

##################
