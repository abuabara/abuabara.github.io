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

addmargins(table(fake_news$type))

fake_news %>%
  select(type) %>%
  table() %>%
  addmargins()

fake_news %>%
  tabyl(type)

?adorn_totals

fake_news %>%
  tabyl(type) %>%
  adorn_totals("row")

# Tabulate exclamation usage and article type
fake_news %>%
  select(title_has_excl, type) %>%
  table() %>%
  addmargins()

fake_news %>%
  tabyl(title_has_excl, type) %>%
  adorn_totals("row")

?adorn_totals

fake_news %>%
  tabyl(title_has_excl, type) %>%
  adorn_totals(c("row", "col"))
# title_has_excl fake real Total
#          FALSE   44   88   132
#           TRUE   16    2    18
#          Total   60   90   150
