library(dplyr)
library(tidyverse)
library(tidycensus)
library(stargazer)
library(lme4)
library(broom)
library(broom.mixed)
library(ggplot2)
library(ggExtra)
library(sf)
library(geostan)
library(spatialreg)
library(spdep)

options(digits = 4,
        scipen = 999,
        sf_use_s2 = FALSE, # spherical geometry switched off
        tigris_use_cache = TRUE,
        stringsAsFactors = FALSE)

theme_set(theme_minimal(12))

setwd("~/GitHub/abuabara.github.io/DAEN489S26")

v22 <- load_variables(2022, "acs5", cache = TRUE)

# fetch data (requires Census API)
vars <- c(
  med_income  = "B19013_001",
  pop_total   = "B03002_001",
  pop_hisp    = "B03002_012",
  pop_over_25 = "B15003_001",
  # diploma1    = "B15003_017",
  # diploma2    = "B15003_018",
  # diploma3    = "B15003_019",
  # diploma4    = "B15003_020",
  diploma5    = "B15003_021", # Associate's degree
  diploma6    = "B15003_022", # Bachelor's degree
  diploma7    = "B15003_023", # Master's degree
  diploma8    = "B15003_024", # Professional school degree
  diploma9    = "B15003_025"  # Doctorate degree
)

tx_data <- get_acs(
  geography = "county",
  variables = vars,
  state     = "TX",
  year      = 2022,
  geometry  = TRUE,
  output    = "wide"
)

# clean / engineer variables
data <- tx_data %>%
  filter(!st_is_empty(geometry)) %>%
  transmute(
    GEOID,
    NAME             = str_remove_all(NAME, " County, Texas"),
    myplace          = as.integer((grepl("Brazos", NAME))),
    pct_hisp         = (pop_hispE / pop_totalE) * 100,
    pct_with_diploma = ((diploma5E + diploma6E + diploma7E + diploma8E + diploma9E) / pop_over_25E) * 100,
    med_income_k     = med_incomeE / 1000,
    # simulate a "region" grouping variable for the multilevel model
    # (in a real analysis, you would join this with a dataset of Texas COGs or MSAs)
    region_id = as.factor(kmeans(st_coordinates(st_centroid(geometry)), centers = 5)$cluster)
  ) %>% na.omit()# filter(!is.na(med_income_k), !is.na(pct_with_diploma), !is.na(pct_hisp))

# ggplot(data, aes(x = pct_with_diploma, y = "")) +
#   geom_boxplot(color = "navy", fill = "navy",
#                alpha = 0.5, width = 0.25,
#                outliers = FALSE) +
#   labs(title = "Distribution of Bachelor Degree or Higher Education",
#        subtitle = "Texas Counties",
#        caption = "Source: ACS 5-Year Data (2008-2022)",
#        y = "",
#        x = "Percent With Bachelor Degree or Higher Education") +
#   theme_minimal()

dev.off()
ggplot(data, aes(x = med_income_k, y = "")) +
  geom_boxplot(color = "navy",
               fill  = "navy",
               alpha = 0.5,
               width = 0.5,
               outliers = FALSE) +
  labs(title    = "Distribution of Median Household Income in the Past 12 Months",
       subtitle = "Texas Counties",
       caption  = "Source: ACS 5-Year Data (2008-2022)",
       y = "",
       x = "Median Household Income ($k)") + theme_minimal()

# standard linear model (baseline) -- ignoring both spatial and regional nesting

cor.test(data$med_income_k, data$pct_with_diploma)

# cor.test(data$med_income_k, data$pct_hisp)

# cor.test(data$pct_with_diploma, data$pct_hisp)

dev.off()
(plot <-
    ggplot(data,
           aes(x = pct_with_diploma,
               y = med_income_k)) +
    geom_point(alpha = .5) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_point(data = subset(data, myplace == 1),
               aes(color = "1"),
               size  = 2,
               alpha = .9) +
    scale_color_manual(name   = "",
                       labels = c("0" = "Other",
                                  "1" = "Brazos"),
                       values = c("0" = "black",
                                  "1" = "orange")) +
    scale_y_continuous(labels = scales::label_dollar(),
                       limits = c(min(data$med_income_k), max(data$med_income_k))) +
    labs(
      title    = "The Impact of Education on Household Income",
      subtitle = "Texas Counties",
      caption  = "Source: ACS 5-Year Data (2008-2022)",
      x        = "Percent With Bachelor Degree or Higher Education",
      y        = "Median Household Income ($k)") +
    # theme_minimal() +
    theme(legend.position="none") +
    coord_fixed(ratio = .4))

dev.off()
(plot2 <- ggMarginal(plot, type="boxplot", size = 10, colour = "grey10", fill = "royalblue", alpha = .5))

model0 <- lm(med_income_k ~ 1, data = data)
model1 <- lm(med_income_k ~ pct_with_diploma, data = data)

summary(model0)
summary(model1)

anova(model0, model1)

stargazer(model0, model1, type = "text",
          report = "vc*", digits = 2)

##################

# fit a random intercept model
# this allows the starting point (intercept) of income to vary across regions,
# while holding the effect of pct_with_diploma constant

dev.off()
plot(data[, "region_id"], main = "Regions ID")

plot(data[, "pct_with_diploma"], main = "Percent with Diploma")

# plot(
#   st_boundary(data %>% group_by(region_id) %>% summarise()),
#   add = TRUE, border = "red")

plot(data[, "med_income_k"], main = "Median Income ($k)")

multilevel_model1 <- lmer(
  med_income_k ~ pct_with_diploma + (1 | region_id), 
  data = data)

summary(multilevel_model1)

model0 <- glm(med_income_k ~ 1, data = data)

model1 <- glm(med_income_k ~ pct_with_diploma, data = data)

stargazer(model0, model1, multilevel_model1,
          type = "text", report = "vc*", digits = 2)

##################

data$ID <- 1:nrow(data)

target <- data[data$ID == 1, ]

neighbor_indices <- unlist(st_touches(target, data))

zoom_area <- data[c(which(data$ID == 1), neighbor_indices), ]

dev.off()
plot(data, main = "Region ID / Neighbors")

dev.off()
plot(zoom_area[, "ID"], main = "Region ID / Neighbors", reset = FALSE)
centers <- st_centroid(st_geometry(zoom_area))
coords <- st_coordinates(centers)
text(x      = coords[, 1], 
     y      = coords[, 2], 
     labels = zoom_area$ID, 
     cex    = 1.4,
     font   = 2,
     col    = "green")
# text(coords[,1], coords[,2], labels = zoom_area$ID, cex = 0.8, col = "white")
# text(coords[,1], coords[,2], labels = zoom_area$ID, cex = 0.7, col = "black")

dev.off()
plot(zoom_area[, "med_income_k"], main = "Region ID / Neighbors", reset = FALSE)

# define "neighbors" (queen contiguity: counties that share boundaries/vertices)
neighbors <- poly2nb(data, queen = TRUE)
coords_94 <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(data)))

dev.off()
plot(sf::st_geometry(data), border = "lightgray")
plot(neighbors, coords_94, add = TRUE, col = "red", pch = 19, cex = 0.5)

# assign weights to those neighbors (row-standardized)
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# convert the weights list into a "Spatial Network" dataframe
# this creates a table with columns: 'from', 'to', and 'weights'
spatial_net <- listw2sn(weights)

data.frame(spatial_net) %>% filter(from == 1)

dev.off()
plot(zoom_area[, "med_income_k"], main = "med_income_k", reset = FALSE)
text(x      = coords[, 1], 
     y      = coords[, 2], 
     labels = zoom_area$ID, 
     cex    = 1.4,
     font   = 2,
     col    = "green")

data.frame(spatial_net) %>% filter(from == 198)

target <- data[data$ID == 198, ]

neighbor_indices <- unlist(st_touches(target, data))

zoom_area <- data[c(which(data$ID == 198), neighbor_indices), ]

dev.off()
plot(zoom_area[, "med_income_k"], main = "med_income_k", reset = FALSE)

# fit the spatial lag model
spatial_model <-
  lagsarlm(
    med_income_k ~ pct_with_diploma, 
    data        = data, 
    listw       = weights, 
    zero.policy = TRUE
  )

summary(spatial_model)

stargazer(model0, model1, multilevel_model1, spatial_model,
          type = "text", report = "vc*", digits = 2)

# higher value = better fit
# lower (more negative) = worse fit

##################

sp_diag(data$med_income_k, data, name = "med_income_k")

# extract the data (estimates and 95% confidence intervals) from each model
tidy_base <- tidy(model1, conf.int = TRUE) %>% mutate(model = "1. Base")
tidy_multi <- tidy(multilevel_model1, conf.int = TRUE) %>% mutate(model = "2. Multilevel")
tidy_spatial <- tidy(spatial_model, conf.int = TRUE) %>% mutate(model = "3. Spatial Lag")

# combine them and filter out the intercepts (only want to look at the effect of the diploma)
coef_comparison <- bind_rows(tidy_base, tidy_multi, tidy_spatial) %>%
  filter(term == "pct_with_diploma")

# coefficients side-by-side
ggplot(coef_comparison, aes(x = estimate, y = model, xmin = conf.low, xmax = conf.high, color = model)) +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_pointrange(size = 1, linewidth = 1) +
  theme_minimal() +
  labs(
    title    = "Comparing the Effect of High Education on Income",
    subtitle = "Estimates and 95% Confidence Intervals",
    x        = "Coefficient Estimate (Effect on Median Income in $1,000s)",
    y        = "") + theme(legend.position = "none",
                           axis.text.y = element_text(size = 12, face = "bold"))

# base model (OLS)  → ignores spatial structure
# Multilevel model  → accounts for grouping (e.g., regions), but not spatial spillovers
# Spatial lag model → explicitly models influence from neighboring units

# When the coefficient for pct_with_diploma shrinks in the spatial lag model, it usually means:
# that pattern is actually very common in spatial models—and it's telling you something important about spatial dependence
# some of the effect you previously attributed to education is actually due to nearby areas influencing each other.

# Conclusion: education still matters, but not as much once we account for spatial context.

W <- shape2mat(data, "W")
x <- log(data$med_income_k)

# rho <- aple(x, W)
# x_centered <- scale(x, center = TRUE, scale = FALSE)
rho <- aple(x - mean(x, na.rm = TRUE), W)

n <- nrow(data)
ess <- n_eff(rho = rho, n = n)

c(nominal_n = n, rho = rho, MC = mc(x, W), ESS = ess)

C <- shape2mat(data)

fit <- stan_glm(
  med_income_k ~ offset(pct_with_diploma), 
  data    = data, 
  re      = ~ GEOID, 
  family  = gaussian(), 
  C       = C,
  refresh = 0) # this line silences stan printing

print(fit)

sp_diag(fit, data, rates = FALSE)















# sp_diag(georgia$college, georgia, name = "College (%)")
# 
# georgia_nb <- poly2nb(georgia, queen = TRUE)
# 
# georgia_w <- nb2listw(georgia_nb, style = "W", zero.policy = TRUE)
# 
# georgia$neighbor_avg <- lag.listw(georgia_w, georgia$college)
# 
# ggplot(georgia, aes(x = college, y = neighbor_avg)) +
#   geom_point(color = "darkred") +
#   geom_smooth(method = "lm", se = FALSE, color = "black") +
#   labs(x = "College (%)", 
#        y = "Mean Neighbor College (%)",
#        title = "Spatial Relationship (Raw Scales)") +
#   theme_minimal()

# convert the weights list into a "Spatial Network" dataframe
# this creates a table with columns: 'from', 'to', and 'weights'
spatial_net <- listw2sn(weights)

# extract the centroid coordinates (x and y)
coords <- st_coordinates(st_centroid(st_geometry(data)))

plot(
  st_geometry(data), 
  border = "darkgray", 
  col = "white",
  main = "Texas Counties: Weighted Spatial Network"
)

segments(
  x0 = coords[spatial_net$from, 1], # Start X
  y0 = coords[spatial_net$from, 2], # Start Y
  x1 = coords[spatial_net$to, 1],   # End X
  y1 = coords[spatial_net$to, 2],   # End Y
  col = "red",
  # scale the line width (lwd) by the actual weight!
  # We multiply by 8 because row-standardized weights are small fractions 
  # (e.g., 0.15), which would be nearly invisible without scaling up.
  lwd = spatial_net$weights * 4
)

points(coords, pch = 19, cex = 0.4, col = "black")
