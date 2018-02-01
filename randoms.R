## Datasets and functions that facilitate data analysis labs in data science courses
## and workshops.
library(dslabs)
library(tidyverse)
library(ggrepel)
library(ggthemes)

## list all the R scripts used to wrangle data from their original source
list.files(system.file("script", package = "dslabs"))

data("murders")
glimpse(murders)
dim(murders)

## Total sum of population and murder cases, and the murder rate per million
death_rate <- murders %>%
  summarize(pop = sum(population),
            tot = sum(total)) %>%
  mutate(rate = tot / pop * 10 ^ 6) %>%
  .$rate

ds_theme_set()

murders %>%
  ggplot(aes(population / 10 ^ 6, total, label = abb)) +
  geom_abline(intercept = log10(death_rate), lty = 2, col = "darkgrey") +
  geom_point(aes(colour = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Population in millions (Log scale)", 
       y = "Total number of murders (Log scale)",
       title = "US Gun murders in 2010") +
  scale_colour_discrete(name = "Region")

data("gapminder")
glimpse(gapminder) 

gapminder <- gapminder %>%
  mutate(group = case_when(
    region %in% west ~ "The West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribean", "Central America", "South America") ~ "Latin America",
    continent %in% "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"
  ))
  