## Attach required packages
library(dplyr)
library(readr)
library(tidyr)

## Read in state-level return data from https://doi.org/10.7910/DVN/42MVDX
## (NOTE: I won't be keeping this file in the repo, you'll have to download it
##        yourself & change the filepath below as appropriate if necessary)
returns = read_csv("~/Downloads/1976-2020-president.csv")

## Fix party_simplified (so e.g. DFL is DEM rather than IND)
dems = c(
    "KERRY, JOHN", "OBAMA, BARACK H.", "CLINTON, HILLARY", "BIDEN, JOSEPH R. JR"
)
reps = c(
    "BUSH, GEORGE W.", "MCCAIN, JOHN", "ROMNEY, MITT", "MITT, ROMNEY",
    "TRUMP, DONALD J."
)
returns = returns %>%
    filter(year >= 2004) %>%
    mutate(party = case_when(
        candidate %in% dems ~ "Dem",
        candidate %in% reps ~ "GOP",
        TRUE ~ NA_character_
    ))

## Go from that to Democratic two-way voteshare in each state in each election
returns = returns %>%
    filter(!is.na(party)) %>%
    pivot_wider(
        id_cols = c(year, state),
        names_from = party,
        values_from = candidatevotes,
        values_fn = sum
    ) %>%
    mutate(twoway = Dem / (Dem + GOP))

## Subset down to identifiers and voteshare, get state names out of all caps
returns = returns %>%
    rename(election = year) %>%
    mutate(state = tools::toTitleCase(tolower(state))) %>%
    select(election, state, twoway)

## Write out results
write_csv(returns, "data/voteshare.csv")
