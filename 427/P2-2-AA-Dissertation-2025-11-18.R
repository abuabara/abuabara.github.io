# title:  "Dissertation Paper 2 - Part 2"
# author: "Alexander Abuabara"
# date:   "Feb-22-2022"

###### Preamble ######
# if (!require("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("Rgraphviz")

library(bnlearn)      # Bayesian Network Structure Learning, Parameter Learning and Inference
library(corrplot)     # Visualization of a Correlation Matrix
library(DescTools)    # Tools for Descriptive Statistics
library(gRain)        # Graphical Independence Networks
library(gtsummary)    # Presentation-Ready Data Summary and Analytic Result Tables
library(haven)        # Import and Export "SPSS", "Stata" and "SAS" Files
library(labelled)     # Manipulating Labelled Data
library(modelsummary) # Summary Tables and Plots for Statistical Models and Data: Beautiful, Customizable, and Publication-Ready
library(plyr)         # Tools for Splitting, Applying and Combining Data
library(rstatix)      # Pipe-Friendly Framework for Basic Statistical Tests
library(tidyverse)    # Easily Install and Load the "Tidyverse"

setwd("~/Downloads/P2 script")
# setwd("/Users/alexander/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Research/3-Dissertation/P2 script/")
options(digits = 2, scipen = 9999, na.strings = "NA")

###### Data ######
# survey_ <- read_sav("./P2 data/Coastal Bend Hurricane Evacuation Behavior Generation 2.sav") %>%
#    mutate(county_aux = as.factor(str_remove_all(ActualCounty, " County")),
#           # data cleaning for structure type (suggested by Peacock)
#           Q31aux    = case_when(Q31 == 1        ~ 1,
#                                 Q31 %in% c(2,3) ~ 2,
#                                 Q31 == 4        ~ 3,
#                                 Q31 == 5        ~ 4),
#           Q31aux    = ifelse(InformID %in% c(576,224,154,613,333,287,672,194,29,58,306,758,885,5,
#                                              392,260,458,192,580,834,481,65,805,426,408), 1, Q31aux),
#           Q31aux    = ifelse(InformID %in% c(830,249,272,421,317,446,279,894,78,815,46,
#                                              493,764,690,848,782,879,407,502),            2, Q31aux),
#           Q31aux    = ifelse(InformID %in% c(114,881,582,701,127,239,634,877,71,84,261),  3, Q31aux),
#           Q31aux    = ifelse(InformID %in% c(666,616,146),                                4, Q31aux),
#           Q31aux    = ifelse(InformID %in% c(899),                                       NA, Q31aux),
#           structure = labelled(Q31aux, c("Single_family" = 1,
#                                          "Multi_family"  = 2,
#                                          "Mobile_home"   = 3,
#                                          "Other"         = 4)))

# survey <- survey_ %>%
#    remove_var_label() %>%
#    filter(Use == 1) %>%
#    remove_attributes("format.spss") %>%
#    transmute(evacuation = factor(tolower(as_factor(Q5Mod))),
#              age = as.numeric(InfAge),
#              consult_info = as.numeric(rowMeans(select(., Q1_1, Q1_2, Q1_3, Q1_4, Q1_5, Q1_6), na.rm = TRUE)),
#              evac_orders = factor(case_when(grepl(c("Matagorda|Calhoun|Refugio|Aransas|San Patricio"), county_aux) ~ "mandatory",
#                                             grepl("Nueces", county_aux) & Q31aux == 3 ~ "mandatory",
#                                             TRUE ~ "voluntary"), ordered = TRUE, levels = c("mandatory", "voluntary")),
#              expected_hh_impacts = as.numeric(rowMeans(select(., Q3_3, Q3_4, Q3_5, Q3_6), na.rm = TRUE)),
#              gender = factor(case_when(Gender == 1 ~ "male",
#                                        Gender == 0 ~ "female"), ordered = TRUE, levels = c("male", "female")),
#              signif_evac_orders = as.numeric(rowMeans(select(., Q4_4), na.rm = TRUE)),
#              # education = as.numeric(rowMeans(select(., Q36), na.rm = TRUE)),
#              education = as.numeric(case_when(Q36 == 1 ~ 10,
#                                               Q36 == 2 ~ 12,
#                                               Q36 == 3 ~ 16,
#                                               Q36 == 4 ~ 18,
#                                               Q36 == 5 ~ 21)),
#              multiple_concerns = as.numeric(rowMeans(select(., Q4_7, Q4_8, Q4_9, Q4_11, Q4_12), na.rm = TRUE)),
#              risk_area = factor(case_when(EvacZoneOld %in% c("A", "B", "C", "D", "E",
#                                                              "Risk 1", "Risk 2", "Risk 3", "Risk 4", "Risk 5",
#                                                              "Coastal", "Zone 1-2", "Zone 3", "Zone 4-5") ~ "yes",
#                                           TRUE ~ "no"), ordered = TRUE, levels = c("yes", "no")),
#              social_cues = as.numeric(rowMeans(select(., Q4_1, Q4_2), na.rm = TRUE)),
#              unnecessary_evac_exp = as.numeric(rowMeans(select(., Q4_6), na.rm = FALSE)),
#    ) %>%
#    mutate_all(~ case_when(!is.nan(.x) ~ .x),) %>%
#    mutate_if(is.numeric, signif, 3)

survey <- read.csv('/Users/abuabara/Downloads/P2 script/DAEN 427/survey_short.csv')

PlotMiss(survey, main = "Missing survey data (clustered)", clust = TRUE)

dat_bn_dicretized <-
   survey %>%
   as.data.frame() %>%
   transmute(
      evacuation = case_when(evacuation == "yes" ~ 1,
                             evacuation == "no" ~ 0),
      expected_hh_impacts,
      signif_evac_orders,
      evac_orders = case_when(evac_orders == "mandatory" ~ 1,
                              evac_orders == "voluntary" ~ 0),
      unnecessary_evac_exp,
      multiple_concerns,
      risk_area = case_when(risk_area == "yes" ~ 1,
                            risk_area == "no" ~ 0),
      social_cues,
      consult_info,
      age = age,
      education,
      gender = case_when(gender == "male" ~ 1,
                         gender == "female" ~ 0),
   ) %>%
   filter(if_all(everything(), ~!is.na(.x))) %>% # na.omit()
   mutate_if(is.integer, as.double) %>%
   mutate(age = round_any(age, 10, floor)) %>%
   mutate(across(where(is.numeric), round, 0)) %>%
   mutate_if(is.double, as.ordered)

dat_bn_dicretized %>% glimpse()
dat_bn_dicretized %>% tbl_summary() %>% as_hux_table()

dat_dicretized <-
   dat_bn_dicretized %>%
   mutate_if(is.ordered, as.character) %>%
   mutate_if(is.character, as.double)

dat_dicretized %>%
   tbl_summary(
      type = list(where(is.numeric) ~ "continuous"),
      statistic = list(all_continuous() ~ "mean {mean} (sd {sd})"),
      missing_text = "(Missing)"
   ) %>% as_hux_table()

###### Descriptive ######
# Empirical CDF of discretized data
par(mfrow = c(2, 6), mar = c(1, 3, 1, 1), pty = "s")
for (var in colnames(dat_dicretized %>%
                     # select(-evacuation) %>%
                     mutate_if(is.ordered, as.character) %>%
                     mutate_if(is.character, as.double))){
   x = dat_dicretized[, var]
   plot(ecdf(x),
        col = "black", lwd = 1, lty = 1, xaxt = "n", yaxt = "n",
        verticals = TRUE, do.points = FALSE, col.01line = NULL,
        main = "", xlab = "", ylab = "", add = FALSE)
   axis(1, at = min(x):max(x))
   axis(2, seq(0, 1, by = .5))
   mtext(var, side = 1, line = 2.4)
}
title(main = "Empirical Cumulative Distribution Function of Each Variable", line = -3, cex.main = 2, outer = TRUE)

for (var in colnames(dat_bn_dicretized %>% select(-evacuation))){
   x = dat_dicretized[dat_dicretized$evacuation == 1, var]
   y = dat_dicretized[dat_dicretized$evacuation == 0, var]
   plot(ecdf(x),
        col = "blue", lwd = 1.5, lty = 1, xaxt = "n", yaxt = "n",
        verticals = TRUE, do.points = FALSE, col.01line = NULL,
        main = "", xlab = "", ylab = "", add = FALSE)
   plot(ecdf(y),
        col = "red", lwd = 0.75, lty = 1, xaxt = "n", yaxt = "n",
        verticals = TRUE, do.points = FALSE, col.01line = NULL,
        main = "", xlab = "", ylab = "", add = TRUE)
   axis(1, at = min(x):max(x))
   axis(2, seq(0, 1, by = .5))
   mtext(var, side = 1, line = 2.2, cex = .9)
   legend("bottomright",
          legend = c("1", "0"),
          col    = c("blue", "red"),
          pch    = 15, cex = .9)
}
title(main = "Empirical Cumulative Distribution Function of Each Variable Conditional to Evacuation", line = -3, cex.main = 2, outer = TRUE)

# Correlation
corr <- cor_test(data   = dat_dicretized,
                 vars   = evacuation,
                 method = "pearson",
                 use    = "pairwise.complete.obs") %>% arrange(-cor, p)

# Matrix of p-values
p.mat <- cor.mtest(dat_dicretized, conf.level = 0.95)
# p.mat %>% view()
round(cor(dat_dicretized), 2)

par(mfrow = c(1, 1), mar = c(1, 1, 10, 1), bg = "white", pty = "m")
corrplot(cor(dat_dicretized, method = "pearson"),
         method = "color", type = "upper", tl.srt = 40, tl.col = "black",
         p.mat  = p.mat$p, sig.level = 0.05, # insig = "blank",
         order  = "original", col = RColorBrewer::brewer.pal(n = 10, name = "RdBu"))

# Detect multicollinearity
eigen(cor(dat_dicretized))$values
kappa(cor(dat_dicretized), exact = TRUE)

###### Logistic Regression ######
summary(model_1 <- glm(evacuation ~ social_cues,
                     data = dat_dicretized,
                     family = binomial(link = "logit")))

summary(logit <- glm(evacuation ~ social_cues + unnecessary_evac_exp,
                     data = dat_dicretized,
                     family = binomial(link = "logit")))

summary(logit <- glm(evacuation ~ social_cues + unnecessary_evac_exp + signif_evac_orders + multiple_concerns,
                     data = dat_dicretized,
                     family = binomial(link = "logit")))

summary(model_1 <- glm(evacuation ~ expected_hh_impacts + signif_evac_orders,
                       data = dat_dicretized,
                       family = binomial(link = "logit")
                       ))

summary(logit <- glm(evacuation ~ .,
                     data = dat_dicretized,
                     family = binomial(link = "logit")
                     ))

anova(model_1, logit)
# hypothesis test:
# H0 = the two models are equally useful for predicting the outcome
# H1 = the larger model is significantly better than the smaller model
# canno reject the null hypothesis, and prefer to use the first model?

lmtest::lrtest(model_1, logit)
# likelihood-ratio test

###### Standardized and performance ######
summary(lm.beta::lm.beta(logit))
library(tidymodels)
performance::check_model(logit)

broom::tidy(logit, exponentiate = TRUE, conf.level = 0.95)
performance::r2_nagelkerke(logit)
VIF(logit)
epiDisplay::logistic.display(logit, simplified = TRUE)[["table"]] %>% as.data.frame() %>%
   rownames_to_column("var") %>% mutate(signif = case_when(`Pr(>|Z|)` <= 0.001 ~ "Signif. 0.001",
                                                           `Pr(>|Z|)` > 0.001  & `Pr(>|Z|)` <= 0.01  ~ "Signif. 0.01",
                                                           `Pr(>|Z|)` > 0.01   & `Pr(>|Z|)` <= 0.05  ~ "Signif. 0.05",
                                                           TRUE ~ "Not signif."),
                                        signif = factor(signif, levels = c("Not signif.", "Signif. 0.05", "Signif. 0.01", "Signif. 0.001"))) %>%
   ggplot(aes(x = OR, y = fct_reorder(var, OR), fill = signif, color = signif)) +
   geom_point(shape = 21, size = 3) +
   geom_errorbar(aes(xmin = lower95ci, xmax = upper95ci), width = .1) +
   scale_colour_manual(values = rev(c("red", "green4", "blue", "black")),
                       breaks = c("Not signif.","Signif. 0.05", "Signif. 0.01", "Signif. 0.001")) +
   scale_fill_manual(values   = rev(c("red", "green4", "blue", "black")),
                     breaks = c("Not signif.", "Signif. 0.05", "Signif. 0.01", "Signif. 0.001")) +
   geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
   coord_trans(x = "log10") +
   scale_x_continuous(breaks = seq(0, 10, 1) ) +
   labs(title = "Logit regression predicting evacuation", x = "Odds ratio and 95% confidence intervals (log scale)",
        y = "", color = "", fill = "") + theme_bw()

effects_logit = margins::margins(logit)
summary(effects_logit)

par(new = TRUE, mfrow = c(1, 1), mar = c(3, 0, 1, 0), bg = "white", pty = "s")
plot(effects_logit) # las = 3

par(new = TRUE, mfrow = c(1, 2), mar = c(7, 3, 3, 2), bg = "white", pty = "s")
for (var in c("expected_hh_impacts", "signif_evac_orders")){
   visreg::visreg(logit, var, scale = "response", partial = FALSE, rug = 2, xlab = paste(var), ylab = "P(evacuation)")
}

###### BN ######
dag = model2network("[age][consult_info][evac_orders][gender][education][risk_area][social_cues][unnecessary_evac_exp][signif_evac_orders|evac_orders][multiple_concerns|age:gender:education][expected_hh_impacts|age:consult_info:gender:education:risk_area:social_cues][evacuation|expected_hh_impacts:signif_evac_orders:multiple_concerns:unnecessary_evac_exp]")
par(new = TRUE, mfrow = c(1, 1), bg = "white", pty = "m")
graphviz.plot(dag, shape = "ellipse")
bn = bn.fit(dag, dat_bn_dicretized)
coefficients(bn)
par(mfrow = c(1, 1), bg = "white", pty = "m")
# graphviz.chart(bn)
graphviz.chart(bn, type = "barprob", grid = TRUE, scale = c(1, 1.2)) # c(1.2,2))
(pvalues = arc.strength(dag, data = dat_bn_dicretized))
par(new = TRUE, mfrow = c(1, 1), bg = "white", pty = "m")
strength.plot(dag, strength = pvalues, shape = "ellipse")
LL  = logLik(dag, dat_bn_dicretized)
k   = log(nrow(dat_bn_dicretized))/2
N   = nparams(dag, dat_bn_dicretized)
(BIC = LL - N * k)
score(dag, dat_bn_dicretized)

###### Exp.1 ######
dag = model2network("[age][consult_info][evac_orders][gender][education][risk_area][social_cues][unnecessary_evac_exp][signif_evac_orders|evac_orders][multiple_concerns|age:gender:education][expected_hh_impacts|age:consult_info:gender:education:risk_area:social_cues][evacuation|expected_hh_impacts:signif_evac_orders:multiple_concerns:risk_area:unnecessary_evac_exp]")
par(new = TRUE, mfrow = c(1, 1), bg = "white")
graphviz.plot(dag, shape = "ellipse")
bn = bn.fit(dag, dat_bn_dicretized)
pvalues = arc.strength(dag, data = dat_bn_dicretized)
par(new = TRUE, mfrow = c(1, 1), bg = "white")
strength.plot(dag, strength = pvalues, shape = "ellipse")
LL  = logLik(dag, dat_bn_dicretized)
k   = log(nrow(dat_bn_dicretized))/2
N   = nparams(dag, dat_bn_dicretized)
(BIC = LL - N * k)
score(dag, dat_bn_dicretized)

###### Exp.2: Risk area ######
dag = model2network("[age][consult_info][evac_orders][gender][education|age:gender][risk_area][social_cues][unnecessary_evac_exp|age][signif_evac_orders|age:gender:education:evac_orders][multiple_concerns|age:gender:education][expected_hh_impacts|age:consult_info:gender:education:risk_area:social_cues][evacuation|expected_hh_impacts:signif_evac_orders:multiple_concerns:risk_area:unnecessary_evac_exp]")
par(new = TRUE, mfrow = c(1, 1), bg = "white")
graphviz.plot(dag, shape = "ellipse")
bn = bn.fit(dag, dat_bn_dicretized)
pvalues = arc.strength(dag, data = dat_bn_dicretized)
par(new = TRUE, mfrow = c(1, 1), bg = "white")
strength.plot(dag, strength = pvalues, shape = "ellipse")
LL  = logLik(dag, dat_bn_dicretized)
k   = log(nrow(dat_bn_dicretized))/2
N   = nparams(dag, dat_bn_dicretized)
(BIC = LL - N * k)
score(dag, dat_bn_dicretized)

dag = model2network("[consult_info][evac_orders][gender][education][risk_area][social_cues][signif_evac_orders|gender:evac_orders][multiple_concerns|education][expected_hh_impacts|consult_info:risk_area:social_cues][evacuation|expected_hh_impacts:signif_evac_orders:multiple_concerns:risk_area]")
par(new = TRUE, mfrow = c(1, 1), bg = "white")
graphviz.plot(dag, shape = "ellipse")
bn = bn.fit(dag, dat_bn_dicretized %>% select(-unnecessary_evac_exp, -age))
par(new = TRUE, mfrow = c(1, 1), bg = "white")
graphviz.chart(bn, type = "barprob", grid = TRUE, draw.levels = TRUE, scale = c(1, 1.2)) # c(1.2,2))
pvalues = arc.strength(dag, data = dat_bn_dicretized %>% select(-unnecessary_evac_exp, -age))
par(new = TRUE, mfrow = c(1, 1), bg = "white")
strength.plot(dag, strength = pvalues, shape = "ellipse")
LL  = logLik(dag, dat_bn_dicretized %>% select(-unnecessary_evac_exp, -age))
k   = log(nrow(dat_bn_dicretized))/2
N   = nparams(dag, dat_bn_dicretized %>% select(-unnecessary_evac_exp, -age))
(BIC = LL - N * k)
score(dag, dat_bn_dicretized %>% select(-unnecessary_evac_exp, -age))

###### Exp.3: Soft-evidence ######
dag = model2network("[consult_info][evac_orders][gender][education][risk_area][social_cues][signif_evac_orders|gender:evac_orders][multiple_concerns|education][expected_hh_impacts|consult_info:risk_area:social_cues][evacuation|expected_hh_impacts:signif_evac_orders:multiple_concerns:risk_area]")
par(new = TRUE, mfrow = c(1, 1), bg = "white")
graphviz.plot(dag, shape = "ellipse")
bn = bn.fit(dag, dat_bn_dicretized %>% select(-unnecessary_evac_exp, -age))

ev <- list(multiple_concerns = "1")                                                          # evidence vector
updated_dat <- cpdist(bn, nodes = bnlearn::nodes(bn), evidence = ev, method = "lw", n = 1e6) # draw samples
updated_fit <- bn.fit(dag, data = updated_dat)                                               # refit: you'll get warnings over missing levels
par(new = TRUE, mfrow = c(1, 1), bg = "white")                                               # plot
graphviz.chart(updated_fit, type = "barprob", grid = TRUE, scale = c(1, 1.2))

junction <- compile(as.grain(bn))
multiple_concerns_low <- setEvidence(junction,
                                     nodes = "multiple_concerns",
                                     states = "1")
querygrain(multiple_concerns_low)$evacuation
querygrain(multiple_concerns_low)$education

###### Exp.4: Soft-evidence ######
dag = model2network("[consult_info][evac_orders][gender][education][risk_area][social_cues][signif_evac_orders|gender:evac_orders][multiple_concerns|education][expected_hh_impacts|consult_info:risk_area:social_cues][evacuation|expected_hh_impacts:signif_evac_orders:multiple_concerns:risk_area]")
par(new = TRUE, mfrow = c(1, 1), bg = "white")
graphviz.plot(dag, shape = "ellipse")
bn = bn.fit(dag, dat_bn_dicretized %>% select(-unnecessary_evac_exp, -age))

ev <- list(evacuation = "1")
updated_dat <- cpdist(bn, nodes = bnlearn::nodes(bn), evidence = ev, method = "lw", n = 1e6)
updated_fit <- bn.fit(dag, data = updated_dat)
par(new = TRUE, mfrow = c(1, 1), bg = "white")
graphviz.chart(updated_fit, type = "barprob", grid = TRUE, scale = c(1, 1.2))

junction <- compile(as.grain(bn))
evac_yes <- setEvidence(junction,
                        nodes = "evacuation",
                        states = "1")
querygrain(evac_yes)$expected_hh_impacts
querygrain(evac_yes)$signif_evac_orders
querygrain(evac_yes)$risk_area

# sessionInfo()