# Load packages
library(bayesrules)
library(tidyverse)
library(janitor)

# Import article data
data(fake_news)

fake_news

head(fake_news)

# Transposed version of head()
# makes possible to see all columns in a data frame
glimpse(fake_news)

fake_news %>%
  glimpse()

fake_news %>% 
  tabyl(type)

?adorn_totals

fake_news %>% 
  tabyl(type) %>% 
  adorn_totals("row")
#  type   n percent
#  fake  60     0.4
#  real  90     0.6
# Total 150     1.0

fake_news %>% 
  tabyl(type) %>% 
  adorn_totals("row")

# Tabulate exclamation usage and article type
fake_news %>% 
  tabyl(title_has_excl, type) %>% 
  adorn_totals("row", "col")

?adorn_totals

fake_news %>% 
  tabyl(title_has_excl, type) %>% 
  adorn_totals(c("row", "col"))
