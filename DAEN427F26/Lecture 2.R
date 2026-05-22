# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Import article data
data(fake_news)

fake_news %>% 
  tabyl(type) %>% 
  adorn_totals("row")
#  type   n percent
#  fake  60     0.4
#  real  90     0.6
# Total 150     1.0

# Transposed version of head()
# makes possible to see all columns in a data frame
glimpse(fake_news)

# Tabulate exclamation usage and article type
fake_news %>% 
  tabyl(title_has_excl, type) %>% 
  adorn_totals("row")
# title_has_excl fake real
#          FALSE   44   88
#           TRUE   16    2
#          Total   60   90