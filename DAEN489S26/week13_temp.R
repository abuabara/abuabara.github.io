library(tidycensus)
library(tidyverse)
library(sf)

# v22 <- load_variables(2022, "acs5", cache = TRUE)
# View(v22)

vars <- c(
  MedianIncome = "B19013_001",
  PopTotal = "B03002_001",
  hispanic_pop = "B03002_012",
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

tract_data <- get_acs(
  geography = "tract",
  variables = vars,
  state = "TX",
  year = 2022,
  geometry = TRUE,
  output = "wide"
) %>%
  mutate(
    PctHispanicE = (hispanic_popE / PopTotalE) * 100,
    PctDiplomaE = ((diploma1E + diploma2E + diploma3E + diploma4E + diploma5E + diploma6E + diploma7E + diploma8E + diploma9E) / edu_totalE) * 100
  ) %>% # select(-NAME)
  select(GEOID, PopTotalE, MedianIncomeE, PctHispanicE, PctDiplomaE)

head(tract_data)

file_path <- "/Users/abuabara/GitHub/abuabara.github.io/DAEN489S26/TX_A.CSV"

life_exp <- read_csv(file_path,
                     col_types = cols(
                       `Tract ID` = col_character(),
                       `e(0)` = col_double(),
                       `se(e(0))` = col_double(),
                       `Abridged life table flag` = col_double()
                     )) %>%
  rename(
    GEOID = `Tract ID`,
    LifeExpE = `e(0)`,
    LifeExpM = `se(e(0))`,
    Abridged = `Abridged life table flag`
  ) %>%
  mutate(LifeExpM = LifeExpM * 1.645) %>%
  select(-STATE2KX, -CNTY2KX, -TRACT2KX, -Abridged, -LifeExpM)

head(life_exp)

data1 <- tract_data %>%
  left_join(life_exp, by = "GEOID")

head(data1)

data1 <- st_transform(data1, 3083)
data1 <- data1[!st_is_empty(data1), ]
data1 <- st_make_valid(data1)

################

library(ggplot2)
library(patchwork)
library(viridis)
library(scales)

map_income <- ggplot(data1) +
  geom_sf(aes(fill = MedianIncomeE), color = NA, size = 0) +
  scale_fill_viridis_c(
    option = "viridis", 
    labels = scales::dollar_format(),
    name = "Income"
  ) +
  theme_void() +
  labs(title = "Median Household Income") +
  theme(
    legend.position = "left",
    legend.key.width = unit(.5, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

map_life <- ggplot(data1) +
  geom_sf(aes(fill = LifeExpE), color = NA, size = 0) +
  scale_fill_viridis_c(
    option = "magma",
    name = "Years"
  ) +
  theme_void() +
  labs(title = "Life Expectancy at Birth") +
  theme(
    legend.position = "right",
    legend.key.width = unit(.5, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

(plot <- map_income + map_life + 
    plot_annotation(
      title = "Texas Census Tracts",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    ))

################

ggplot() +
  geom_sf(data = data1, fill = NA, color = "black", size = .05) +
  geom_sf(data = subset(data1, is.na(MedianIncomeE) | is.na(LifeExpE) | is.na(PopTotalE)),
          fill = "red", color = NA, size = 0) +
  theme_void()

# Little's MCAR test
library(naniar)

mcar_test(st_drop_geometry(select(data1, MedianIncomeE, LifeExpE, PopTotalE)))
# statistic    df p.value missing.patterns
#      389.     5       0                4
# p-value > 0.05: You fail to reject the null. It is safe to assume the data is missing randomly.
# p-value < 0.05: The missingness is likely correlated with other variables in your dataset (it is not completely random)

vis_miss(st_drop_geometry(select(data1, MedianIncomeE, LifeExpE, PopTotalE)))

mcar_test(st_drop_geometry(select(data1, MedianIncomeE, LifeExpE)))
vis_miss(st_drop_geometry(select(data1, MedianIncomeE, LifeExpE)))

data_test <- data1 %>%
  mutate(LifeExpMissing = ifelse(is.na(LifeExpE), 1, 0))

# A simple logistic regression (or t-test): does MedianIncome predict whether LifeExp is missing?
summary(glm(LifeExpMissing ~ MedianIncomeE, data = data_test, family = "binomial"))

################

# remove missing values
data2 <- subset(data1, !is.na(MedianIncomeE) & !is.na(LifeExpE) & !is.na(PopTotalE))

nrow(data1) - nrow(data2) # droping: 3510 tracts

cor.test(log10(data2$MedianIncomeE), data2$LifeExpE)

################

library(geostan)

sp_diag(data2$PopTotalE, data2, name = "pct_hispanic")
sp_diag(data2$LifeExpE, data2, name = "pct_diploma")
sp_diag(data2$MedianIncomeE, data2, name = "income USD")

A <- shape2mat(data2, "B", method = "rook")

A <- shape2mat(data2, "B", method = "rook", snap = 20000)

E <- edges(A, shape = data2) # edges with geometry

graph <- st_geometry(E)

ogpar <- par(mar = rep(0, 4))
plot(st_geometry(data2), lwd = .1) # plot countries
plot(graph, add = TRUE, type = 'p') # add graph nodes
plot(graph, add = TRUE, type = 'l') # add graph edges
par(ogpar)

################

summary(glm(LifeExpE ~ log(MedianIncomeE), weights = PopTotalE, data = data2))

fit_lm <- stan_glm(LifeExpE ~ log(MedianIncomeE),
                   data = data2, iter = 800, quiet = TRUE)

print(fit_lm)

plot(fit_lm)

fdf <- fitted(fit_lm)
head(fdf)

rdf <- resid(fit_lm)
moran_plot(rdf$mean, A)

cars <- prep_car_data(A, quiet = TRUE)

fit_car <- stan_car(LifeExpE ~ 1, data = data2,
                    car_parts = cars, iter = 800, quiet = TRUE)

print(fit_car)











