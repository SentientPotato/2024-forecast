library(dplyr)
library(tidyr)
# require(polite); require(rvest)
URL = paste0(
    "https://en.wikipedia.org/wiki/",
    "Opinion_polling_for_the_2016_United_States_presidential_election_in_Florida"
)
polite_object = polite::bow(URL)
scraped_site  = polite::scrape(polite_object)
tables = scraped_site %>%
    rvest::html_nodes("table.wikitable") %>%
    rvest::html_table(fill = TRUE)
date_range_pattern = "([A-Za-z]+) ([0-9]+)–([0-9]+)"
make_date = function(x) as.Date(paste0(trimws(x), " 16"), format = "%B %d %y")
dat = bind_rows(tables[1:3], .id = "question_id") %>%
    mutate(election = 2016, race_id = 2016) %>%
    mutate(question_id = as.numeric(question_id) + 1) %>%
    mutate(fn = as.numeric(gsub(".*\\[([0-9]+)\\]$", "\\1", `Poll source`))) %>%
    mutate(poll_id = as.numeric(paste0("20169", sprintf("%02.0f", fn)))) %>%
    rename(sample_size = `Sample size`) %>%
    mutate(date = gsub(", 2016", "", `Date administered`)) %>%
    mutate(date = gsub(date_range_pattern, "\\1 \\2 – \\1 \\3", date)) %>%
    mutate(date = gsub("^([A-Za-z]+ [0-9]+)$", "\\1 – \\1", date)) %>%
    separate_wider_delim(
        cols = date, delim = "–", names = c("start_date", "end_date"),
        cols_remove = FALSE
    ) %>%
    mutate(start_date = make_date(start_date)) %>%
    mutate(end_date = make_date(end_date))
fns = sort(unique(dat$fn))
link_selector = "cite.citation>a.external.text:first-of-type"
links = scraped_site %>% rvest::html_elements(link_selector)
links = sapply(links[fns], rvest::html_attr, name = "href")
links = data.frame(fn = seq_along(links), URL = links)
dat = dat %>% left_join(links)
dat = dat %>%
    mutate(sample_size = as.numeric(gsub(",", "", sample_size))) %>%
    mutate(`Hillary Clinton` = as.numeric(gsub("%", "", `Hillary Clinton`))) %>%
    mutate(Dem = round(0.01 * `Hillary Clinton` * sample_size)) %>%
    mutate(`Donald Trump` = as.numeric(gsub("%", "", `Donald Trump`))) %>%
    mutate(GOP = round(0.01 * `Donald Trump` * sample_size)) %>%
    mutate(state = "Florida") %>%
    select(
        election, race_id, poll_id, question_id, state, start_date, end_date,
        sample_size, Dem, GOP, URL
    )
readr::write_csv(dat, file = "florida-polls.csv")
