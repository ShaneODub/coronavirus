
# Setup and libraries -----------------------------------------------------
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("stringr")
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

# Get the data and transform it --------------------------------------------

confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

countries_regions <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
names(countries_regions)[1] <- 'Country.Region'
countries_regions <- countries_regions %>% select(c(1,6:8))
countries_regions$Country.Region <- as.character(countries_regions$Country.Region)

countries_regions <- 
  countries_regions %>% 
  mutate(
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Bolivia (Plurinational State of)',
      'Bolivia'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Brunei Darussalam',
      'Brunei'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Congo',
      'Congo (Kinshasa)'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == "Côte d'Ivoire",
      "Cote d'Ivoire"
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Iran (Islamic Republic of)',
      'Iran'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Korea, Republic of',
      'Korea, South'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Moldova, Republic of',
      'Moldova'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Russian Federation',
      'Russia'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Taiwan, Province of China',
      'Taiwan*'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'United Kingdom of Great Britain and Northern Ireland',
      'United Kingdom'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'United States of America',
      'US'
    ),
    Country.Region = replace(
      Country.Region,
      Country.Region == 'Viet Nam',
      'Vietnam'
    )
  )

# Define function to pivot the time series columns to a long format.
corona_pivot <- function(corona_data){
  pivot_longer(
    corona_data,
    cols = starts_with("X"),
    names_to = "date",
    values_to = deparse(substitute(corona_data))) 
}

# Make new dataframe with figures for confirmed cases, recovered cases and deaths.
all_data <-
  cbind(
    corona_pivot(confirmed),
    corona_pivot(recovered)[,6],
    corona_pivot(deaths)[,6]
  )

# Fix the data column: e.g. X1.22.30 becomes 2020-01-22
all_data <- 
  all_data %>% 
  mutate(date = str_remove(date, "X"),
         date = as.Date(date, '%m.%d.%y' ))

# Left join the data to a dataframe of countries and regions, subregions
all_data <-
  merge(all_data,
        countries_regions,
        by="Country.Region",
        all.x=TRUE)

# After joining, some countries had no matching regions because the names mismatched.
# This was then fixed using the long mutate-replace statement above, and the join was
# done again, successfully.
#
# all_data %>%
#   filter(is.na(region)) %>%
#   pull(Country.Region) %>%
#   unique()
#
# # [1] Bolivia          Brunei           Congo (Kinshasa) Cote d'Ivoire   
# # [5] Cruise Ship      Iran             Korea, South     Moldova         
# # [9] Reunion          Russia           Taiwan*          United Kingdom  
# # [13] US              Vietnam  



# Plot the data -----------------------------------------------------------

ggplot(all_data %>% filter(Country.Region == 'China',
                           Province.State != 'Hubei'),
       aes(x = date , y = confirmed, group = Province.State)) +
  geom_line()

