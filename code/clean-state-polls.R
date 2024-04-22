## Attach required packages
library(dplyr)
library(tidyr)
library(readr)

## Read in the data pasted from Wikipedia
dat = read_csv("~/tmp-polls.csv")

## Add a few fixed variables and rename/reclass others
dat = dat %>%
    mutate(election = 2016, race_id = 2016) %>%
    mutate(question_id = as.numeric(question_id)) %>%
    rename(sample_size = `Sample size`)

## Fix the dates
date_range_pattern = "([A-Za-z]+) ([0-9]+)–([0-9]+)"
make_date = function(x) as.Date(paste0(trimws(x), " 16"), format = "%B %d %y")
dat = dat %>%
    mutate(date = gsub(", 2016", "", `Date administered`)) %>%
    mutate(date = gsub(date_range_pattern, "\\1 \\2 – \\1 \\3", date)) %>%
    mutate(date = gsub("^([A-Za-z]+ [0-9]+)$", "\\1 – \\1", date)) %>%
    separate_wider_delim(
        cols = date, delim = "–", names = c("start_date", "end_date"),
        cols_remove = FALSE
    ) %>%
    mutate(start_date = make_date(start_date)) %>%
    mutate(end_date = make_date(end_date))

## Get the poll ID (via footnote marks)
dat = dat %>%
    mutate(fn = as.numeric(gsub(".*\\[([0-9]+)\\]$", "\\1", `Poll source`))) %>%
    mutate(poll_id = as.numeric(paste0("2016", sprintf("%03.0f", fn))))

## Get poll URLs
URL = paste0(
    "https://en.wikipedia.org/wiki/",
    "Statewide_opinion_polling_for_the_2016_United_States_presidential_election"
)
# polite_object = polite::bow(URL)
# scraped_site  = polite::scrape(polite_object)
fns = sort(unique(dat$fn))
link_selector = "cite.citation>a.external.text:first-of-type"
links = scraped_site %>% rvest::html_elements(link_selector)
links = sapply(links[fns], rvest::html_attr, name = "href")
links = data.frame(fn = fns, URL = links)
dat = dat %>% left_join(links)

## Get results in votes rather than percents
dat = dat %>%
    mutate(Dem = round(0.01 * as.numeric(gsub("%", "", Dem)) * sample_size)) %>%
    mutate(GOP = round(0.01 * as.numeric(gsub("%", "", GOP)) * sample_size))

## Add the state name and subset down to the columns we need
dat = dat %>%
    mutate(state = "Wyoming") %>%
    select(
        election, race_id, poll_id, question_id, state, start_date, end_date,
        sample_size, Dem, GOP, URL
    )

## Write out the results
readr::write_csv(dat, file = "tmp-polls-processed.csv")
