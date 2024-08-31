rm(list=ls())
library(tidyverse)
library(lubridate)
library(dplyr)
library(magrittr)
library(readr)
library(ggplot2)
library(tidyr)
library(janitor)

#1a
fcc <- clean_names(read_csv("fcc_complaints_CA-2021.csv"))

#1b
fcc_clean <- fcc %>% 
  filter(!is.na(method)) %>%
  filter(zip != "00000")

#1c
fcc_clean %<>% tidyr::extract(col = caller_id_number,
                              into = "area_code",
                              regex = "(\\d\\d\\d)",
                              remove = F)

#1d
fcc_clean %<>% mutate(month_of_issue = month(date_of_issue)) %>%
  mutate(year_of_issue = year(date_of_issue) )

#1e
fcc_clean %<>% mutate(time_of_issue_clean = str_replace_all(time_of_issue, "\\.", "")) %>%
  mutate(time_of_issue_clean = parse_time(time_of_issue_clean, format = ""))
## shows seconds? but prompt only shows hours and mins

call_distribution <- fcc_clean %>%
  filter(form == "Phone") %>%
  ggplot(aes(time_of_issue_clean, fill = method)) + 
  geom_histogram(alpha = 0.8) +
  theme_minimal() +
  scale_x_time(breaks = scales::date_breaks("3 hour"),
               labels = scales::time_format("%I:%M %p")) + #edit time format
  theme(legend.position = "bottom") + #edit
  labs(x = "", y = "Number of FCC Complaints", fill = "",
       title = "Distribution of Phone Complaints to the FCC",
       subtitle = "From January 2021 - July 2022") +
  ggthemes::scale_fill_fivethirtyeight()

#2a
acs <- read_csv("acs_data.csv")

#2b
acs %<>% tidyr::extract(col = name,
                        into = "zip",
                        regex = "(\\d\\d\\d\\d\\d)",
                        remove = F)

#2c
fcc_joined <- fcc_clean %>%
  left_join(acs, by = c("zip" = "zip")) %>%
  filter(year_of_issue == 2021) %>%
  filter(total_pop != 0)


#2d
fcc_joined %<>%
  mutate(low_income_zip = ifelse(median_income < mean(median_income, na.rm = T),
                                 "Below Average Median Income",
                                 "Above Average Median Income")) %>%
  mutate(high_age_zip = ifelse(median_age < mean(median_age, na.rm = T),
                               "Below Average Median Age",
                               "Above Average Median Age"))

fcc_joined %>%
  summary()

#2e
aggregate <- fcc_joined %>%
  group_by(median_age, median_income, total_pop, zip, low_income_zip) %>%
  summarize(n_complaints = n())

#2f
aggregate %<>% 
  mutate(complaints_per_1000 = (n_complaints/total_pop)*1000)

#-------------------------
#write up code

#joined_fcc presentable
pretty_joined <- stargazer(fcc_joined,
                        summary = F,
                        type = "text",
                        out = "joined_tibble.txt")
pretty_joined <- fcc_joined %>%
  select(time_of_issue_clean, method, zip, median_income,
         median_age, total_pop)

#for the acs data

#making tibble that averages median income for each age group

aggregate %<>%
  mutate(age_group = cut(median_age, breaks = c(-Inf, 20, 30, 40, 50, 60, 70, Inf),
                         labels = c("Below 19 Years Old",
                                    "20-29 Years Old",
                                    "30-39 Years Old",
                                    "40-49 Years Old",
                                    "50-59 Years Old",
                                    "60-69 Years Old",
                                    "Above 70 Years Old")))

age_tibble <- aggregate %>% group_by(age_group) %>%
  summarise(mean_income = round(mean(median_income, na.rm = T), digits=2),
            population = sum(total_pop, na.rm = T),
            complaints = sum(n_complaints))
age_tibble 

install.packages("stargazer")
library(stargazer)
pretty_tib <- stargazer(age_tibble,
          summary = T,
          type = "text",
          out = "age_tibble.txt")
pretty_tib
#creating a map

install.packages("zipcodeR")
library(zipcodeR)
geocode_zip("93117")

map_tibble <- aggregate %>%
  group_by(zip) %>%
  summarise(mean_age = mean(median_age, na.rm = T),
            mean_income = mean(median_income, na.rm = T),
            total_population = sum(total_pop, na.rm = T),
            n_complaints = sum(n_complaints))

# above shows that all observations in aggregate have different zip codes

lat_long_tibble <- aggregate %>%
  mutate(latitude = geocode_zip(zip)$lat,
         longitude = geocode_zip(zip)$lng)

# this works!!

lat_long_tibble_trans <- usmap_transform(lat_long_tibble,
                                         input_names = c("longitude", "latitude"),
                                         output_names = c("x", "y"))

complaint_map <- plot_usmap(include = c("CA")) +
  geom_point(data = lat_long_tibble_trans, aes(x = x, y = y, size = complaints_per_1000),
             color = "blue", alpha = .4) +
  scale_fill_continuous(low = "white", high = "blue", name = "Unwanted Call Complaints (per 1000)",label = scales::comma) +
  labs(title = "Unwanted Calls Complaint Distribution in California", subtitle = "Year 2021") +
  theme(legend.position = "right")

complaint_map

pop_map <- plot_usmap(include = c("CA")) +
  geom_point(data = lat_long_tibble_trans, aes(x = x, y = y, size = total_pop),
             color = "purple", alpha = .4) +
  scale_fill_continuous(low = "white", high = "purple", name = "Unwanted Call Complaints (per 1000)",label = scales::comma) +
  labs(title = "Population Distribution in California", subtitle = "Year 2021") +
  theme(legend.position = "right")
pop_map
