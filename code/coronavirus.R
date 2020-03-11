
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

corona_pivot <- function(corona_data){
  pivot_longer(
    corona_data,
    cols = starts_with("X"),
    names_to = "date",
    values_to = deparse(substitute(corona_data))) 
}

all_data <-
  cbind(
    corona_pivot(confirmed),
    corona_pivot(recovered)[,6],
    corona_pivot(deaths)[,6]
  )

# Plot the data -----------------------------------------------------------

all_data <- 
  all_data %>% 
  mutate(date = str_remove(date, "X")) %>%
  View()

ggplot(all_data %>% 
         filter(Country.Region == "Mainland China"),
       mapping = aes(x = date, y = confirmed, group = "Province.State")) +
  geom_line()
