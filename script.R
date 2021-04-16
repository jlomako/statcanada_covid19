# data from Statistics Canada:
# Table 13-10-0768-01 Weekly death counts, by age group and sex
# DOI: https://doi.org/10.25318/1310076801-eng
# https://www150.statcan.gc.ca/n1/tbl/csv/13100768-eng.zip
# downloaded 24 sep 2020 --> updated on 16 apr 2021
# to do: update plots for 2021
#
# Table 17-10-0009-01 Population estimates, quarterly
# DOI: https://doi.org/10.25318/1710000901-eng, 
# downloaded 30.09.2020


rm(list=ls())

# install.packages("tidyverse")
library(tidyverse)


# import death counts:
data <- read.csv("13100768-eng/13100768.csv")
data <- as_tibble(data)

# view data structure
str(data) # 121,590 x 17

# select and rename columns: geo, date, age, deaths (=number of deaths)
data <- data %>% 
  select(geo = GEO, date = ï..REF_DATE, age = Age.at.time.of.death, deaths = VALUE, sex = Sex)

# clean up and convert variables
data$geo <- str_replace(data$geo, ", place of occurrence", "")
data$geo <- as.factor(data$geo)
levels(data$geo)

data$age <- str_replace(data$age, "Age at time of death, ", "")
data$age <- as.factor(data$age)
levels(data$age) # 0-44  45-64  65-84  85+  all ages

data$sex <- as.factor(data$sex)
levels(data$sex) # Both sexes  Females  Males

# convert date
data$date <- as.Date(data$date)

# extract years and weeks
data$year <- format(data$date, "%Y")
data$week <- format(data$date, "%U")

str(data)

## tibble [121,590 x 7] (S3: tbl_df/tbl/data.frame)
## $ geo   : Factor w/ 14 levels "Alberta","British Columbia",..: 3 3 3 3 3 3 3 3 3 3 ...
## $ date  : Date[1:121590], format: "2010-01-09" "2010-01-09" "2010-01-09" ...
## $ age   : Factor w/ 5 levels "0 to 44 years",..: 5 5 5 1 1 1 2 2 2 3 ...
## $ deaths: int [1:121590] 4955 2535 2420 240 140 100 790 505 285 2255 ...
## $ sex   : Factor w/ 3 levels "Both sexes","Females",..: 1 3 2 1 3 2 1 3 2 1 ...
## $ year  : chr [1:121590] "2010" "2010" "2010" "2010" ...
## $ week  : chr [1:121590] "01" "01" "01" "01" ...

# checking last available date & available years in dataset
tail(data$date)
table(data$year)

# checking for missing values (NAs)
summary(data$deaths)

# what data is missing? 
data %>%
  filter(is.na(deaths)) %>%
  count(year, geo) %>%
  print(n=30)
# mostly 2020 + 2021
# no data available for some (northern) provinces, esp. Yukon

# drop rows containing missing values
data <- data %>% drop_na()
summary(data$deaths)


# drop also year 2021 (for now)
data <- data %>% filter(year != "2021") 

data %>%
  count(year) %>%
  print(n=130)


# import population data:
population <- read.csv("population/1710000901.csv")
population <- as_tibble(population)

# select and rename columns, select rows for january 01
population <- population %>% 
  select(geo = GEO, year = ï..REF_DATE, pop = VALUE) %>%
  filter(str_detect(year, "-01$")) %>%
  mutate(year = as.numeric(str_replace(year, "-01", ""))) 

str(population)

# checking for missing data
population %>%
  filter(is.na(pop)) %>%
  count(geo)


###
# Quebec was the most affected province in Canada by COVID19 in 2020
# First, i want to compare Quebec and Canada in terms of weekly death rate

# merge data (weekly deaths) and population data sets
canada <- merge(data, population, by=c("geo","year"))

# include numbers for "both sexes" and "all ages" only
canada <- canada %>% filter(sex == "Both sexes" & age == "all ages")

# plot data for Canada and Quebec for 2010 to 2020 
canada %>% filter(geo == "Canada" | geo =="Quebec") %>%
  # calculate death rate per 100.000
  mutate(death_rate = (deaths / pop) * 100000) %>%
  arrange(geo, date) %>%
  # CommonDate and year2020 dummy variables for better plotting
  mutate(CommonDate = as.Date(paste0("2000-", format(date, "%j")), "%Y-%j")) %>%
  mutate(year2020 = ifelse(year == "2020", T, F)) %>% 
     ggplot(aes(x = CommonDate, y = death_rate, colour = year, alpha = year2020)) +
     # add smooth line
     geom_line(size = 1.1, stat = "smooth", method = "lm", formula = y ~ poly(x,16), se = F) +
     # add datapoints to plot
     # geom_point() + 
     scale_alpha_manual(values = c(.3, 1)) +
     ylim(5,25) +
     scale_x_date(date_labels = "%b %d", date_breaks = "2 month") +
     labs(title = "Fig 1: Weekly death rate per 100.000 in Canada and Quebec from 2010 to 2020", 
       caption = "based on data from Stats Canada, downloaded on 16 Apr 2021", 
       y = "death rate per 100.000", x = "") +
     theme_bw() +
     guides(alpha = FALSE) + # remove alpha from legend
     facet_wrap(geo ~ .)

ggsave("Fig 1.png")


# next I want to have a closer look at Quebec
# and calculate the average death rate for 
# 2010 - 2019 to compare it to 2020

# select rows for Quebec
# calculate death rate for each month
quebec <- canada %>% 
  filter(geo == "Quebec") %>%
  arrange(date) %>%
  mutate(death_rate = (deaths / pop) * 100000) %>%
  mutate(year = as.factor(year), week = as.factor(week), deaths = as.numeric(deaths))
str(quebec)


# calculate average death_rate per week 2010-2019 and save in "avg_year" variable
avg_year <- quebec %>%
  group_by(geo, age, sex, week) %>%
  summarize(date = date[str_detect(year, "2019")], # use date from 2019 (for plotting)
            deaths = mean(deaths[str_detect(year, "201")]), # calculate mean deaths 2010-2019
            death_rate = mean(death_rate[str_detect(year, "201")]), # calculate mean death rate 2010-2019
            year = 2000) # save as year 2000 (dummy year for mean values)

glimpse(avg_year)
glimpse(quebec)

# adjust variables for merging
avg_year <- avg_year %>%
  mutate(year = as.factor(year), week = as.factor(week), death_rate = as.numeric(death_rate), deaths = as.numeric(deaths))

# merge data/ add new rows for average year to dataframe
quebec <- union_all(quebec, avg_year)

# quebec data now includes also rows with average data (saved under year 2000)
# plot data to compare average year with year 2020
# plot all years 2010 to 2019 in grey, average in black, 2020 in red
quebec %>%  
  mutate(CommonDate = as.Date(paste0("2000-", format(date, "%j")), "%Y-%j")) %>%
  mutate(year2020 = ifelse(year == "2020" | year == "2000", T, F)) %>% 
  ggplot(aes(x = CommonDate, y = death_rate, colour = year, alpha = year2020)) +
    geom_line(size=1) +
    ylim(10,30) +
    scale_alpha_manual(values = c(.5, 1)) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
    # set coulors manually: 10x grey, 2020 = red and average = black
    scale_colour_manual(values=c(rep("grey", 10), "red", "black")) +
    labs(title = "Fig 2: Weekly death rate in Quebec compared to average death rate (2010-2019) during COVID-19 pandemic in 2020", 
       caption = "based on data from Stats Canada, downloaded on 16 Apr 2021", 
       y = "death rate per 100.000", x = "") +
    theme_bw() +
    guides(alpha = F, colour = F) + # remove legends 
    # add simpler legend
    annotate("text", x = as.Date("2000-10-01"), y = 30, label = "__", hjust = 0, size = 6, colour = "red") +
    annotate("text", x = as.Date("2000-10-01"), y = 29, label = "__", hjust = 0, size = 6, colour = "grey") +
    annotate("text", x = as.Date("2000-10-01"), y = 28, label = "__", hjust = 0, size = 6, colour = "black") +
    # add text
    annotate("text", x = as.Date("2000-10-17"), y = 29.3, label = "year 2020", hjust = 0, size = 4, colour = "black") +
    annotate("text", x = as.Date("2000-10-17"), y = 28.3, label = "years 2010 - 2019", hjust = 0, size = 4, colour = "black") +
    annotate("text", x = as.Date("2000-10-17"), y = 27.3, label = "average death rate", hjust = 0, size = 4, colour = "black") 

ggsave("Fig 2.png")


# Finally, I'm interested in the number of total excess deaths in Quebec in 2020.
# For this I have to transform the death rate to total numbers: (pop / 100000) * death_rate
# OBS: i cannot use original death numbers here because of the changes in population over the years

# calculate excess_deaths for quebec (difference between average and 2020):
excess_deaths_qc <- quebec %>%
  select(year, week, death_rate, pop) %>%
  filter(year == 2020 | year == 2000) %>% # select 2020 and 2000 (year with averages)
  mutate(pop = pop[year == 2020][1]) %>% # use population from 2020
  group_by(week) %>% 
  summarise(difference = death_rate[year == 2020] - death_rate[year == 2000],
            excess_deaths = (pop[year == 2020]/100000) * difference)


# calculate total excess_deaths:
# and save in variable "total" for plotting
total <- round(sum(excess_deaths_qc$excess_deaths))
# 6748 excess deaths from Jan 1 to Dec 31, 2020 in Quebec

# round variable "total" to nearest 100
total <- 100*round(total/100)
# 6700

# add dates to dataframe
excess_deaths_qc$date <- quebec$date[quebec$year == 2020]


# plot excess deaths for Quebec
excess_deaths_qc %>%
  mutate(pos = difference >= 0) %>% # add variable for pos/neg plotting
  ggplot(aes(x = week, y = excess_deaths, fill = pos)) +
    geom_col(position = "identity", size = 0.5) +
    scale_y_continuous(breaks=seq(-250, 750, 50)) +
    scale_fill_manual(values = c("black", "red"), guide = F) +
    annotate("text", x= 40, y = 730, size = 5,
             label = paste("~", total, "more people than expected died \n in 2020 during the COVID-19 pandemic")) +
    labs(title = "Fig 3: Excess mortality in Quebec during the COVID-19 pandemic in 2020", 
       caption = "based on data from Stats Canada, downloaded on 16 Apr 2021", 
       y = "number of deaths below or above average", 
       x = "week") +
    theme_bw() 

ggsave("Fig 3.png")
