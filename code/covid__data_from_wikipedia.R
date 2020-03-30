


# Get the data ------------------------------------------------------------


# Northern Ireland --------------------------------------------------------

url_northern_ireland <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Northern_Ireland'

northern_ireland <-
  url_northern_ireland %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>% 
  html_table() %>% 
  .[[1]] %>%
  select(Date, Cases, Deaths)

names(northern_ireland) <- c('date','new_cases','new_deaths')

northern_ireland <- 
  northern_ireland %>%
  mutate(total_cases = cumsum(new_cases),
         total_deaths = cumsum(new_deaths))

northern_ireland$date <-
  as.Date(northern_ireland$date, format = '%d %b %Y')
  

# Scotland ----------------------------------------------------------------

url_scotland <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Scotland'

scotland <-
  url_scotland %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] 

names(scotland) <- make.unique(names(scotland))

scotland <- 
  scotland %>% 
  rename(date = Date,
         new_cases = Confirmed,
         total_cases = Confirmed.1,
         new_tested = Tested,
         total_tested = Tested.1,
         new_deaths = Deaths,
         total_deaths = Deaths.1)

scotland <- 
  scotland %>% 
  filter(str_detect(.$date, '2020/\\d{2}/\\d{2}'))

scotland <- 
  scotland %>% 
  select(date, new_cases, total_cases, new_deaths, total_deaths)

scotland$date <- as.Date(scotland$date)

scotland[2:5] <- lapply(scotland[2:5], as.integer)
  
# Wales -------------------------------------------------------------------

url_wales <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Wales'

wales <-
  url_wales %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/div[3]/div/table') %>% 
  html_table()

names(wales) <- make.unique(names(wales))

wales <- 
  wales %>% 
  select(1,4)

names(wales) <- 
  c('date', 'cases')

wales$new_cases <- 
  wales$cases %>% 
  str_extract('\\(\\+\\d+\\)') %>% 
  str_extract('\\d+') %>% 
  as.integer()

wales$total_cases <- 
  wales$cases %>% 
  str_extract('\\d+\\(') %>% 
  str_extract('\\d+') %>% 
  as.integer()

wales <- 
  wales %>% 
  filter(str_detect(.$date, '2020-\\d{2}-\\d{2}'))

wales$date <- as.Date(wales$date)

# United Kingdom ----------------------------------------------------------

url_united_kingdom <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_the_United_Kingdom'

united_kingdom <-
  url_united_kingdom %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] 

names(united_kingdom) <- make.unique(names(united_kingdom))

united_kingdom <- 
  united_kingdom %>% 
  rename(date = Date,
         england_east = England,
         england_london = England.1,
         england_midlands = England.2,
         england_ne_yorks = England.3,
         england_north_west = England.4,
         england_south_east = England.5,
         england_south_west = England.6,
         england_unclassified = England.7,
         new_cases = `Confirmed cases`,
         total_cases = `Confirmed cases.1`,
         new_deaths = Deaths,
         total_deaths = Deaths.1,
         new_tested = Tested,
         total_tested = Tested.1)

united_kingdom <- 
  united_kingdom %>% 
  filter(str_detect(.$date, '^\\d{1,2}\\s[A-z]{3}$')) %>% 
  select(-Sources)

united_kingdom$date <- 
  as.Date(united_kingdom$date, format = '%d %b' )
# 
# united_kingdom <- 
#   united_kingdom %>%
#   mutate_all(funs(gsub(',','', .))) %>% 
#   mutate_all(funs(gsub('\\s\\(\\d+\\)','',.))) %>% 
#   mutate_if(is.character, as.integer)

# Republic of Ireland -----------------------------------------------------


url_ireland <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_the_Republic_of_Ireland'

ireland <-
  url_ireland %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/div[5]/div/table') %>% 
  html_table()

names(ireland) <- make.unique(names(ireland))

ireland <- 
  ireland %>% 
  select(1,4,5)

names(ireland) <- 
  c('date', 'cases', 'deaths')

ireland$total_cases <- 
  ireland$cases %>% 
  str_remove_all(',') %>% 
  str_extract('^\\d+\\(') %>% 
  str_extract('\\d+') %>% 
  as.numeric()

ireland$total_deaths <- 
  ireland$deaths %>% 
  str_remove_all(',') %>% 
  str_extract('^\\d+\\(') %>% 
  str_extract('\\d+') %>% 
  as.numeric()

ireland$new_cases <- 
  ireland$total_cases - 
  lag(ireland$total_cases, default = 0)

ireland$new_deaths <- 
  ireland$total_deaths - 
  lag(ireland$total_deaths, default = 0)

ireland <- 
  ireland %>% 
  filter(str_detect(.$date, '2020-\\d{2}-\\d{2}'))

ireland$date <- as.Date(ireland$date)
  

