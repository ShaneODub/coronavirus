
# Setup and libraries -----------------------------------------------------
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("directlabels")
# install.packages("gganimate")
# install.packages("ggiraph")

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(directlabels)
library(gganimate)
library(ggrepel)
library(ggiraph)

# Get the data -----------------------------------------------------------

# confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

# recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

# New data structure
confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


countries_regions <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
names(countries_regions)[1] <- 'Country.Region'
countries_regions <- countries_regions %>% select(c(1:3,6:8))
countries_regions$Country.Region <- as.character(countries_regions$Country.Region)


# Fix up the data. Join confirmed, recovered, deaths & regions. --------------------

countries_regions <- 
  countries_regions %>% 
  mutate(
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Bolivia (Plurinational State of)', 'Bolivia'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Brunei Darussalam', 'Brunei'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Congo', 'Congo (Kinshasa)'),
    Country.Region = replace(Country.Region, 
                             Country.Region == "Côte d'Ivoire", "Cote d'Ivoire"),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Iran (Islamic Republic of)', 'Iran'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Korea, Republic of', 'Korea, South'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Moldova, Republic of', 'Moldova'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Russian Federation', 'Russia'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Taiwan, Province of China', 'Taiwan*'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'United Kingdom of Great Britain and Northern Ireland', 'United Kingdom'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'United States of America', 'US'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Viet Nam', 'Vietnam'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Venezuela (Bolivarian Republic of)', 'Venezuela'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Curaçao', 'Curacao'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Réunion', 'Reunion'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Palestine, State of', 'occupied Palestinian territory'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Congo, Democratic Republic of the', 'Congo (Brazzaville)'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Syrian Arab Republic', 'Syria'),
    Country.Region = replace(Country.Region, 
                             Country.Region == "Lao People's Democratic Republic", 'Laos'),
    Country.Region = replace(Country.Region, 
                             Country.Region == 'Tanzania, United Republic of', 'Tanzania')
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
    # corona_pivot(recovered)[,6],
    corona_pivot(deaths)[,6]
  )

# There are both Cabo Verde and Cape Verde in all_data. Removing one of them.
# all_data <- 
#   all_data[all_data$Country.Region != 'Cape Verde',]

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

all_data %>%
  filter(is.na(region)) %>%
  pull(Country.Region) %>%
  unique()
#
# # [1] Bolivia          Brunei           Congo (Kinshasa) Cote d'Ivoire   
# # [5] Cruise Ship      Iran             Korea, South     Moldova         
# # [9] Reunion          Russia           Taiwan*          United Kingdom  
# # [13] US              Vietnam  

# Sort it correctly
all_data <-
  all_data[order(all_data$Country.Region,all_data$Province.State, all_data$date),]

# Fixing some individual figures that I noticed were wrong or missing
all_data <- 
  all_data %>% 
  group_by(Country.Region, Province.State) %>% 
  mutate(
    confirmed = case_when(
      date == '2020-03-12' & Country.Region == 'Ireland' ~ as.integer(70),
      date == '2020-03-15' & Country.Region == 'Ireland' ~ as.integer(169),
      date == '2020-03-16' & Country.Region == 'Ireland' ~ as.integer(223),
      date == '2020-03-17' & Country.Region == 'Ireland' ~ as.integer(292),
      date == '2020-03-18' & Country.Region == 'Ireland' ~ as.integer(366),
      TRUE ~ confirmed
    ),
    deaths = case_when(
      date == '2020-03-23' & Country.Region == 'Ireland' ~ as.integer(6),
      date == '2020-03-15' & Country.Region == 'Iceland' ~ as.integer(0),
      deaths < lag(deaths) ~ lag(deaths),
      TRUE ~ deaths
      )
    )

# Add a column for active cases
# all_data$active <- all_data$confirmed - all_data$recovered - all_data$deaths

# Drop the rows with no cases, and insert 'days_since_first_case' column
all_data <- 
  all_data %>% 
  filter(confirmed > 0) %>% 
  mutate(days_since_first_case = 0:(n() - 1))

# Include a logical expression (deaths > 0) in the group_by 
# This puts some meaningless values in rows where deaths = 0, but we fix that in the next step.
all_data <- 
  all_data %>%
  group_by(Country.Region, Province.State, deaths > 0) %>%
  mutate(days_since_first_death = 0:(n() - 1),)%>%
  group_by(Country.Region, Province.State)

# Get rid of the meaningless 'days_since_first_death' values where deaths = 0
all_data[all_data$deaths == 0,]$days_since_first_death <- NA

# Plot the data -----------------------------------------------------------
# 
# areas_of_interest <- 
#   all_data %>% 
#   group_by(Country.Region, Province.State) %>% 
#   filter(#date == max(date),
#          max(deaths) >= 4,
#          region == 'Europe',
#          (as.character(Province.State) == as.character(Country.Region))
#          |as.character(Province.State) == '')
# 
# x <- 'days_since_first_death'
# y <- 'deaths'
# group_1 <- 'Country.Region'
# group_2 <- 'alpha.2'
# colour_group <- areas_of_interest$Country.Region == 'Ireland'
# label_group_1 <- areas_of_interest %>% filter(days_since_first_death > 5 | deaths > 5,
#                                               date == max(date))
# label_group_2 <- areas_of_interest %>% filter(days_since_first_death <= 5 & deaths <= 5,
#                                               date == max(date))
# 
# myplot <- 
# ggplot(areas_of_interest,
#        aes_string(x = x,
#                   y = y,
#                   group = group_1)) +
#   geom_line(aes(colour = colour_group)) +
#   geom_point_interactive(aes(colour = Country.Region == 'Ireland',
#                              tooltip = deaths,
#                              data_id = deaths),
#                          data = areas_of_interest %>%
#                            filter(date == max(date) 
#                                   | Country.Region == 'Ireland')) +
#   scale_y_log10() +
#   scale_colour_manual(values = c('grey','red'), guide = 'none') +
#   # geom_dl(aes_string(label = group_1),
#   #         method = list(dl.trans(x = x + 0.2, y = y + 0.2), "last.bumpup", cex = 0.8))
#   geom_dl(aes_string(label = group_1),
#           data = label_group_1,
#           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8)) +
#   geom_dl(aes_string(label = group_2),
#           data = label_group_2,
#           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8))


# Europe deaths plot ------------------------------------------------------

irish_deaths <-
  all_data %>%
  filter(Country.Region == 'Ireland') %>% 
  pull(deaths) %>% 
  max()

countries_of_interest <- 
  all_data %>% 
  filter(max(deaths) >= irish_deaths |
           max(days_since_first_death) >= 20,
         Country.Region == 'Ireland' |
           (max(deaths) < 100 & date == max(date)) |
              (max(deaths) >= 100 &
                 (deaths > lag(deaths) | deaths < 30))
                  # deaths > lag(deaths) ignores days with no new deaths
         ) 

points_of_interest <- 
  countries_of_interest %>% 
  filter(date == max(date) | Country.Region == 'Ireland')

# Log-scale horizontal gridlines
breaks <- sort(c(10^(0:10),10^(0:10)*5))
# minor_breaks <- 10^(0:10) * 5
minor_breaks <- rep(1:9, 11)*(10^rep(0:10, each=9))

# Make 'Ireland' the last level of Country.Region for display purposes
countries_of_interest$Country.Region  <- 
  countries_of_interest$Country.Region %>% 
  factor(levels = countries_of_interest$Country.Region %>% 
           levels() %>% 
           setdiff('Ireland') %>% 
           append('Ireland'))

myplot <- 
  ggplot(data = countries_of_interest,
         aes(x = days_since_first_death,
             y = deaths,
             group = interaction(Country.Region, Province.State))) +
  geom_line(aes(colour = Country.Region == 'Ireland')) +
  geom_point_interactive(data = points_of_interest,
                         aes(fill = Country.Region == 'Ireland',
                             tooltip = deaths,
                             data_id = deaths),
                         shape = 21, size = 2) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_colour_manual(values = c('grey','red'), guide = 'none') +
  scale_fill_manual(values = c('black', 'red'), guide = 'none') +
  geom_dl(aes(label = paste(Country.Region, Province.State)),
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6))

myplot
girafe(ggobj = myplot, width_svg = 7, height_svg = 4.5)
