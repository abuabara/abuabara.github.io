# Import data
library(tidyverse)
library(janitor)

data(bechdel, package = "bayesrules")

# Take a sample of 20 movies
set.seed(84735)

bechdel_20 <- bechdel %>% 
  sample_n(20)

bechdel_20 %>% 
  head(3)
# A tibble: 3 x 3
#    year title      binary
#    <dbl> <chr>      <chr> 
# 1  2005 King Kong  FAIL  
# 2  1983 Flashdance PASS  
# 3  2013 The Purge  FAIL  

bechdel_20 %>% 
  tabyl(binary) %>% 
  adorn_totals("row")
#  binary  n percent
#  FAIL   11    0.55
#  PASS    9    0.45
#  Total  20    1.00

bechdel %>% 
  filter(year == 1991) %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

bechdel %>% 
  filter(year == 2000) %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

bechdel %>% 
  filter(year == 2013) %>% 
  tabyl(binary) %>% 
  adorn_totals("row")

###################
###################
library(tidyverse)

priors <- tibble(
  analyst = c("Skeptic", "Uncertain", "Optimist"),
  alpha   = c(5, 1, 14),
  beta    = c(11, 1, 1)
)

grid <- crossing(priors, pi = seq(0, 1, length.out = 501)) |>
  mutate(density = dbeta(pi, alpha, beta))

ggplot(grid, aes(pi, density, color = analyst)) +
  geom_line(linewidth = 1) + theme_minimal()


###################
###################
library(bayesrules)

plot_beta_binomial(alpha = 14, beta = 1,
                   y = 6, n = 13)

summarize_beta_binomial(alpha = 14, beta = 1,
                        y = 6, n = 13)

plot_beta_binomial(alpha = 14, beta = 1,
                   y = 46, n = 99)

###################
###################
batches <- tibble(day = 1:3,
                  y = c(1, 17, 8),
                  n = c(10, 20, 10))

alpha <- 1; beta <- 10
history <- tibble(day = 0, alpha = alpha, beta = beta)

for (i in seq_len(nrow(batches))) {
  alpha <- alpha + batches$y[i]
  beta  <- beta + batches$n[i] - batches$y[i]
  history <- rbind(history,
                   tibble(day = i, alpha = alpha, beta = beta))
}

history |> mutate(mean = alpha / (alpha + beta))
