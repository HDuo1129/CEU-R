# Data Import
## Option 1: tidytuesdayR package 
tuesdata <- tidytuesdayR::tt_load('2024-09-24')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 39)

country_results_df <- tuesdata$country_results_df
individual_results_df <- tuesdata$individual_results_df
timeline_df <- tuesdata$timeline_df

# Option 2: Read directly from GitHub
country_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/country_results_df.csv')
individual_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/individual_results_df.csv')
timeline_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/timeline_df.csv')

# Data Cleaning
## Scraping IMO results data
library(tidyverse)
library(rvest)
library(janitor)
library(httr2)

timeline_df <- read_html("https://www.imo-official.org/organizers.aspx") %>%
  html_table() %>%
  .[[1]] %>%
  clean_names() %>%
  rename(
    "all_contestant" = contestants,
    "male_contestant" = contestants_2,
    "female_contestant" = contestants_3,
    "edition" = number
  ) %>%
  filter(edition != "#") %>%
  mutate(
    start_date = paste0(gsub("(.*)(-)(.*)", "\\1", date),year),
    end_date = paste0(gsub("(.*)(-)(.*)", "\\3", date),year),
    across(
      c(start_date, end_date),
      ~as.Date(.x, format = "%d.%m.%Y")
    ),
    across(
      c(edition, year, countries, all_contestant, male_contestant, female_contestant),
      as.integer
    )
  ) %>%
  select(-date) %>%
  # only keeping records till current year
  filter(year < 2025)

# circulate through country results link and rbind tables
scrape_country <- function(year) {
  paste0("https://www.imo-official.org/year_country_r.aspx?year=", year) %>%
    read_html() %>%
    html_table() %>%
    .[[1]] %>%
    clean_names() %>%
    filter(country != "Country") %>%
    mutate(year = year, .before = "country") 
}

country_results_df <- map_df(timeline_df$year, scrape_country) %>%
  select(
    year,
    country,
    team_size_all = team_size,
    team_size_male = team_size_2,
    team_size_female = team_size_3,
    starts_with("p"),
    awards_gold = awards,
    awards_silver = awards_2,
    awards_bronze = awards_3,
    awards_honorable_mentions = awards_4,
    leader,
    deputy_leader
  ) %>% 
  mutate(
    across(
      c(team_size_all:awards_honorable_mentions),
      as.integer
    )
  )

## circulate through individual results link and rbind tables
scrape_individual <- function(year) {
  # These can time out, so we'll use httr2 to retry.
  paste0("https://www.imo-official.org/year_individual_r.aspx?year=", year) %>%
    httr2::request() %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform() %>%
    httr2::resp_body_html() %>%
    html_table() %>%
    .[[1]] %>%
    clean_names() %>%
    mutate(year = year, .before = "contestant") 
}

individual_results_df <- map_df(timeline_df$year, scrape_individual) %>%
  select(
    year:p6, p7, total,
    individual_rank = number_rank,
    award
  ) %>%
  mutate(
    across(
      c(year, p1:individual_rank),
      as.integer
    )
  )