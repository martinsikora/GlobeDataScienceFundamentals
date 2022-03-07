
## Basics

### set up workspace and read tutorial data
library(tidyverse)
library(lubridate)
library(slider)

covid <- read_csv("https://www.dropbox.com/s/v3677c38ux5sjpd/owid-covid-data_subset.csv?dl=1", guess_max = 10000)
pop <- read_csv("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1")

## tibbles

### explore data

covid
covid %>%
    head()
covid %>%
    colnames()

## Select variables

### selecting individual columns
covid %>%
    colnames()
select(covid, country) ## by name
select(covid, total_tests)
select(covid, 1, 5) ## by position
select(covid, country, total_tests)
select(covid, total_tests, country)
select(covid, 1, country)

### selecting column ranges
covid %>%
    colnames()
select(covid, new_cases:total_cases_per_million) ## by positions
select(covid, 6:11) ## by positions

### exclude columns
covid %>%
    colnames()
select(covid, -iso_code) ## by positions
select(covid, -date:-human_development_index) ## by positions
select(covid, -4:-59) ## by positions

### helper functions
covid %>%
    colnames()
select(covid, starts_with("new"))
select(covid, ends_with("rate"))
select(covid, contains("case"))
select(covid, everything())
select(covid, country, everything())


## Arrange observations
covid %>%
    colnames()
arrange(covid, new_cases)
arrange(covid, desc(new_cases))
arrange(covid, desc(date), desc(new_cases))

## Short exercises

## Select all columns containing "total"
select(covid, contains("total"))



## Filter observations

### basic filters
covid %>%
    colnames()
filter(covid, country == "Denmark")
filter(covid, continent != "Europe")
filter(covid, total_cases < 100)
filter(covid, new_cases > 1000)
filter(covid, date %in% c(ymd(20200301), ymd(20200302)))

### multiple filters
covid %>%
    colnames()
filter(covid, country == "Denmark", date == "2020-03-01")
filter(covid, country == "Denmark", date == "2018-03-01")
filter(covid, country == "Denmark" | date == "2018-03-01")
filter(covid, !(country == "Denmark" | date == "2018-03-01"))
filter(covid, is.na(new_cases_smoothed))
filter(covid, !is.na(new_cases_smoothed))

### other useful filters
slice(covid, 10)
slice(covid, 1:10)
distinct(covid, country)
distinct(covid, country, continent)
top_n(covid, 10, new_cases)
top_n(covid, 10, country)


## Create variables

### create a subset of the data
covid %>%
    colnames()
covid_small <- select(covid, country:new_cases, population) %>%
    filter(date == "2021-01-01")

### single new variable
mutate(covid_small, total_cases_thousands = total_cases / 1000)
mutate(covid_small, previous_cases = total_cases - new_cases)
mutate(covid_small, new_cases_per_100k = new_cases / population * 100000)

### reuse created variable
mutate(covid_small, total_cases_fraction = total_cases / population, total_cases_percent = total_cases_fraction * 100)

### show only new variables
transmute(covid_small, total_cases_fraction = total_cases / population, total_cases_percent = total_cases_fraction * 100)


## summarise

### basic summarise
summarise(covid, max_new_cases = max(new_cases))
summarise(covid, max_new_cases = max(new_cases, na.rm = TRUE))

## grouped summaries
covid_by_country <- group_by(covid, country)
covid_by_country

summarise(covid, max_new_cases = max(new_cases, na.rm = TRUE))
summarise(covid_by_country, max_new_cases = max(new_cases, na.rm = TRUE))

## counts
count(covid, country)
count(covid, date)
count(covid, date, sort = TRUE)
count(covid, country, date)


## pipes
covid_by_country %>%
    summarise(max_new_cases = max(new_cases, na.rm = TRUE))

covid_by_country %>%
    summarise(max_new_cases = max(new_cases, na.rm = TRUE)) %>%
    arrange(desc(max_new_cases))

select(covid, country, date, new_cases, population) %>%
    filter(date > ymd(20201224), date < ymd(20210101)) %>%
    group_by(country, population) %>%
    summarise(avg_new_cases = mean(new_cases)) %>%
    mutate(avg_new_cases_per_100k = avg_new_cases / population * 100000) %>%
    ungroup() %>%
    arrange(desc(avg_new_cases_per_100k))


## data import / tidy data

### reading files
pop <- read.csv("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1")
str(pop)
pop <- read_csv("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1")
str(pop)


### import options
pop <- read_delim("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1", delim = ",")
pop <- read_delim("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1", delim = ",", col_names = c("a", "b", "c", "d", "e", "f", "g"))
pop <- read_delim("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1", delim = ",", col_names = c("a", "b", "c", "d", "e", "f", "g"), skip= 1)
pop <- read_delim("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1", delim = ",", col_types = "cdddddd")
pop <- read_delim("https://www.dropbox.com/s/ntdh4coibs7er2g/un_pop.csv?dl=1", delim = ",", col_types = "fiiiiii")


### write files
write_tsv(pop, path = "~/pop.tsv")
write_tsv(pop, path = "~/pop_nocolnames.tsv", col_names = FALSE)
write_tsv(pop, path = "~/pop.tsv.gz")


## Tidy data

### pivot longer
pop
pop %>%
    pivot_longer(pop_65_plus:pop_12_24)
pop %>%
    pivot_longer(pop_65_plus:pop_12_24, names_to = "age_group", values_to = "population")

### pivot wider
pop
pop %>%
    select(country, year, pop_65_plus) %>%
    pivot_wider(names_from = "year", values_from = "pop_65_plus")

### separate
pop %>%
    select(country, year, pop_6_11, pop_12_24, pop_25_64) %>%
    pivot_longer(-country:-year) %>%
    mutate(name = gsub("pop_", "", name)) %>%
    separate(name, into = c("age_low", "age_high"))


## Joining tables

covid_small <- select(covid, country:new_cases, population) %>%
    filter(date == "2021-01-01") %>%
    mutate(yr = year(ymd(date)))

### Mutating join
covid_small
pop
covid_small %>%
    inner_join(pop)
covid_small %>%
    inner_join(pop, by = c("country" = "country"))
covid_small %>%
    inner_join(pop, by = c("country" = "country", "yr" = "year"))
covid_small %>%
    left_join(pop, by = c("country" = "country", "yr" = "year"))
covid_small %>%
    right_join(pop, by = c("country" = "country", "yr" = "year"))
covid_small %>%
    full_join(pop, by = c("country" = "country", "yr" = "year"))

### filtering joins
covid_small %>%
    semi_join(pop, by = c("country" = "country"))

df <- tibble(country = "Denmark", date = ymd(20210101))
covid_small %>%
    semi_join(df)

covid_small %>%
    anti_join(df)


## larger exercises

## Exercise 1

### Calculate the test positivity percent, averaged per month for each year and country. arrange by country, year and date
d <- covid %>%
    select(date, country, new_cases, new_tests) %>%
    separate(date, into = c("year", "month", "day")) %>%
    group_by(country, month, year) %>%
    summarise(cases_per_test = mean(new_cases/new_tests, na.rm = TRUE)) %>%
    mutate(pos_rate = cases_per_test * 100) %>%
    arrange(country, year, month) %>%
    ungroup()

## which month had the lowest positivity rate in Denmark?
filter(d, country == "Denmark") %>%
    arrange(pos_rate) %>%
    head(2)

## in which countries occurred the overall top five months with the highes positivity rates?
top_n(d, 5, pos_rate) %>%
    distinct(country)

## create and plot a 7 day average new cases curve for Denmark and Sweden
d <- filter(covid, country %in% c("Denmark", "Sweden")) %>%
    select(date, country, new_cases) %>%
    group_by(country) %>%
    mutate(new_cases_avg = slide_dbl(new_cases, ~mean(.x, na.rm = TRUE), .before = 3, .after = 3))

ggplot(d, aes(x = date, y = new_cases_avg, color = country)) +
    geom_point(size = 1) +
    geom_line()
