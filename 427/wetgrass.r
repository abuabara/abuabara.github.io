dat <- read.csv("/Users/abuabara/Desktop/wetgrass/wetgrass_microdata.csv")
head(dat)

# CPTs
prop.table(table(Cloudy = dat$Cloudy_TF))
prop.table(table(Cloudy = dat$Cloudy_TF, Rain = dat$Rain_TF), margin = 1)
prop.table(table(Cloudy = dat$Cloudy_TF, Sprinkler = dat$Sprinkler_TF), margin = 1)
prop.table(table(Rain     = dat$Rain_TF[dat$Sprinkler_TF == TRUE],
                 WetGrass = dat$WetGrass_TF[dat$Sprinkler_TF == TRUE]), margin = 1)
prop.table(table(Rain     = dat$Rain_TF[dat$Sprinkler_TF == FALSE],
                 WetGrass = dat$WetGrass_TF[dat$Sprinkler_TF == FALSE]), margin = 1)           

# Q9a) Calculate marginal probability that WetGrass, 1 = True
(p_wet <- mean(dat$WetGrass))
# 0.6462

# This is the empirical marginal probability P(W=T) from the data,
# which should be close to the theoretical value implied by the Bayesian network.

# Q9b) Compute P (Cloudy= True | Sprinkler= True), 0 = False, 1 = True
num <- sum(dat$Cloudy == 1 & dat$Sprinkler == 1)
den <- sum(dat$Sprinkler == 1)
(p_C_given_S <- num / den)
# 0.1602

# Q9c) Given that the grass is wet, compute the posterior probability that it is cloudy.

num <- sum(dat$Cloudy == 1 & dat$WetGrass == 1)
den <- sum(dat$WetGrass == 1)
(p_C_given_W <- num / den)
# 0.5797

1-p_C_given_W
# 0.4203

sum(dat$Cloudy == 0 & dat$WetGrass == 1) / sum(dat$WetGrass == 1)
# 0.4203

# linear regression
# lm(WetGrass ~ 1, data = dat)
# 0.6462

# model1 = lm(WetGrass ~ 1,                         data = dat)
# model2 = lm(WetGrass ~ Sprinkler + Rain,          data = dat)
# model3 = lm(WetGrass ~ Sprinkler + Rain + Cloudy, data = dat)

# library(stargazer)
# stargazer(model1, model2, model3,
#           type = "text")

lm(Cloudy ~ 1, data = dat)
0.5015

model <- lm(Cloudy ~ WetGrass + Sprinkler + Rain, data = dat)
model
# Coefficients:
#   (Intercept)     WetGrass    Sprinkler         Rain  
#        0.3291       0.0795      -0.3755       0.4639

# logistic regression
# model <- glm(Cloudy ~ WetGrass + Sprinkler + Rain, data = dat, family = binomial)

# new data for prediction
(newdata <- data.frame(
  WetGrass = mean(dat$WetGrass),
  # WetGrass = 1,
  Sprinkler = mean(dat$Sprinkler),  # or 0/1 depending on scenario
  Rain = mean(dat$Rain)             # or 0/1 depending on scenario
))
# WetGrass Sprinkler   Rain
#   0.6462    0.2947 0.4994
#        1    0.2947 0.4994

# Predict probability
predict(model, newdata = newdata, type = "response")
# 0.5015
# 0.5296

predict(model, newdata = data.frame(WetGrass = 1,
                                    Sprinkler = 0,
                                    Rain = 1),
        type = "response")
# 0.8725

# Numerator: count of all rows with Cloudy=1, WetGrass=1, Sprinkler=0, Rain=1
num <- sum(dat$Cloudy == 1 &
             dat$WetGrass == 1 &
             dat$Sprinkler == 0 &
             dat$Rain == 1)

# Denominator: count of all rows with WetGrass=1, Sprinkler=0, Rain=1
den <- sum(dat$WetGrass == 1 &
             dat$Sprinkler == 0 &
             dat$Rain == 1)

# Conditional probability
num / den
# 0.8784398

# newdata <- dat
# newdata$WetGrass <- 1
# probs <- predict(model, newdata = newdata, type = "response")
# mean(probs)
