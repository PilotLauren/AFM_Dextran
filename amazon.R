# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(robotstxt)

# check that we can scrape data from the chronicle -----------------------------

paths_allowed("https://www.amazon.com/s?me=ACKT49FTL26DZ&marketplaceID=ATVPDKIKX0DER")


# read page --------------------------------------------------------------------

page <- read_html("https://www.dukechronicle.com/section/opinion?page=1&per_page=500")

# parse components -------------------------------------------------------------

titles <- page |>
  html_elements(".headline a") |>
  html_text()

columns <- page |>
  html_elements(".col-md-8 .kicker") |>
  html_text()|>
  str_remove("\n ")

abstracts <- page |>
  html_elements(".article-abstract") |>
  html_text()


authors_dates <- page |>
  html_elements(".col-md-8 .dateline") |>
  html_text2() |>
  str_remove("By ")

urls <- page |>
  html_elements(".headline a") |>
  html_attr(name = "href")

# create a data frame ----------------------------------------------------------

chronicle_raw <- tibble(
  title = titles,
  author_date = authors_dates,
  abstract = abstracts,
  column = columns,
  url = urls
)
chronicle_raw

# clean up data ----------------------------------------------------------------

chronicle <- chronicle_raw |>
  # separate author_date into author and date
  separate_wider_delim(cols = author_date, delim = "| ", names = c("author", "date"), too_few = "align_end")|>
  mutate(column = if_else(column == "OPINION", "OPINION |    ", column)) |>
  mutate(
    date = case_when(
      date == "12 hours ago" ~ "February 27, 2024",
      date == "Yesterday" ~ "February 26, 2024",
      date == "4 days ago" ~ "February 23, 2024",
      date == "5 days ago" ~ "February 22, 2024",
      date == "6 days ago" ~ "February 21, 2024",
      .default = date
    ),
    author = str_trim(author)
  )|>
  separate_wider_delim(cols = column, delim = "|", names = c("opinion", "column"), too_few = "align_start")|>
  mutate(column = str_trim(column))

# trim white space in author names
# fix dates and their type


# write data -------------------------------------------------------------------

write_csv(chronicle, file = "~/ae-PilotLauren2/data/chronicle.csv")